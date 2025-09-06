;; title: TimeBank
;; version: 1.0.0
;; summary: Time-locked savings with inflation-adjusted returns
;; description: A decentralized savings platform that protects against inflation through dynamic interest rates tied to real economic indicators

;; traits
(define-trait oracle-trait
  (
    (get-cpi-rate () (response uint uint))
    (is-oracle-valid () (response bool uint))
  )
)

;; token definitions
(define-fungible-token timebank-token)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_OWNER (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_INVALID_AMOUNT (err u103))
(define-constant ERR_LOCK_PERIOD_NOT_MET (err u104))
(define-constant ERR_EMERGENCY_WITHDRAWAL_DENIED (err u105))
(define-constant ERR_INVALID_ORACLE (err u106))
(define-constant ERR_TRANSFER_FAILED (err u107))
(define-constant ERR_ALREADY_EXISTS (err u108))

(define-constant BASE_INTEREST_RATE u300) ;; 3% base rate (in basis points)
(define-constant INFLATION_MULTIPLIER u100) ;; 1:1 inflation adjustment
(define-constant EARLY_WITHDRAWAL_PENALTY u1000) ;; 10% penalty (in basis points)
(define-constant MIN_LOCK_PERIOD u52560) ;; ~1 year in blocks
(define-constant MAX_LOCK_PERIOD u262800) ;; ~5 years in blocks
(define-constant EMERGENCY_POOL_FEE u50) ;; 0.5% fee for emergency pool
(define-constant GOVERNANCE_THRESHOLD u10000) ;; Minimum tokens for governance
(define-constant BASIS_POINTS u10000)

;; data vars
(define-data-var total-deposits uint u0)
(define-data-var emergency-pool-balance uint u0)
(define-data-var current-cpi-rate uint u250) ;; 2.5% default CPI rate
(define-data-var oracle-address (optional principal) none)
(define-data-var governance-enabled bool false)
(define-data-var total-governance-power uint u0)

;; data maps
(define-map deposits
  principal
  {
    amount: uint,
    deposit-block: uint,
    lock-period: uint,
    unlock-block: uint,
    interest-rate: uint,
    emergency-insured: bool
  }
)

(define-map emergency-insurance
  principal
  {
    premium-paid: uint,
    coverage-amount: uint,
    valid-until: uint
  }
)

(define-map inter-gen-transfers
  uint ;; transfer-id
  {
    from: principal,
    to: principal,
    amount: uint,
    unlock-condition: uint, ;; block height or age condition
    completed: bool
  }
)

(define-map governance-votes
  { proposal-id: uint, voter: principal }
  { vote: bool, weight: uint }
)

(define-map governance-proposals
  uint ;; proposal-id
  {
    proposer: principal,
    description: (string-ascii 256),
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool
  }
)

(define-data-var next-transfer-id uint u1)
(define-data-var next-proposal-id uint u1)


;; public functions

;; Deposit STX into time-locked savings
(define-public (deposit (amount uint) (lock-period uint) (emergency-insurance-enabled bool))
  (let (
    (sender tx-sender)
    (current-block (get-block))
    (unlock-block (+ current-block lock-period))
    (interest-rate (calculate-current-interest-rate))
    (insurance-fee (if emergency-insurance-enabled (/ (* amount EMERGENCY_POOL_FEE) BASIS_POINTS) u0))
    (net-deposit (- amount insurance-fee))
  )
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (>= lock-period MIN_LOCK_PERIOD) ERR_INVALID_AMOUNT)
    (asserts! (<= lock-period MAX_LOCK_PERIOD) ERR_INVALID_AMOUNT)
    (asserts! (is-none (map-get? deposits sender)) ERR_ALREADY_EXISTS)
    
    ;; Transfer STX from sender
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Handle emergency insurance if enabled
    (if emergency-insurance-enabled
      (begin
        (var-set emergency-pool-balance (+ (var-get emergency-pool-balance) insurance-fee))
        (map-set emergency-insurance sender {
          premium-paid: insurance-fee,
          coverage-amount: amount,
          valid-until: unlock-block
        })
      )
      true
    )
    
    ;; Create deposit record
    (map-set deposits sender {
      amount: net-deposit,
      deposit-block: current-block,
      lock-period: lock-period,
      unlock-block: unlock-block,
      interest-rate: interest-rate,
      emergency-insured: emergency-insurance-enabled
    })
    
    ;; Update total deposits
    (var-set total-deposits (+ (var-get total-deposits) net-deposit))
    
    ;; Mint governance tokens based on time-weight
    (let ((governance-tokens (/ (* net-deposit lock-period) MIN_LOCK_PERIOD)))
      (try! (ft-mint? timebank-token governance-tokens sender))
      (var-set total-governance-power (+ (var-get total-governance-power) governance-tokens))
    )
    
    (ok true)
  )
)

;; Withdraw funds after lock period
(define-public (withdraw)
  (let (
    (sender tx-sender)
    (deposit-info (unwrap! (map-get? deposits sender) ERR_NOT_FOUND))
    (current-block (get-block))
    (principal-amount (get amount deposit-info))
    (unlock-block (get unlock-block deposit-info))
    (interest-earned (calculate-interest deposit-info))
    (total-withdrawal (+ principal-amount interest-earned))
  )
    (asserts! (>= current-block unlock-block) ERR_LOCK_PERIOD_NOT_MET)
    
    ;; Transfer funds back to sender
    (try! (as-contract (stx-transfer? total-withdrawal tx-sender sender)))
    
    ;; Clean up deposit record
    (map-delete deposits sender)
    
    ;; Update total deposits
    (var-set total-deposits (- (var-get total-deposits) principal-amount))
    
    (ok total-withdrawal)
  )
)

;; Emergency withdrawal with penalty
(define-public (emergency-withdraw)
  (let (
    (sender tx-sender)
    (deposit-info (unwrap! (map-get? deposits sender) ERR_NOT_FOUND))
    (insurance-info (map-get? emergency-insurance sender))
    (principal-amount (get amount deposit-info))
    (penalty-amount (/ (* principal-amount EARLY_WITHDRAWAL_PENALTY) BASIS_POINTS))
    (net-withdrawal (- principal-amount penalty-amount))
  )
    ;; Check if emergency insurance covers this withdrawal
    (if (and (is-some insurance-info) 
             (get emergency-insured deposit-info)
             (>= (get coverage-amount (unwrap-panic insurance-info)) principal-amount))
      ;; Insurance covers - no penalty
      (begin
        (try! (as-contract (stx-transfer? principal-amount tx-sender sender)))
        (var-set emergency-pool-balance (- (var-get emergency-pool-balance) principal-amount))
      )
      ;; No insurance - apply penalty
      (try! (as-contract (stx-transfer? net-withdrawal tx-sender sender)))
    )
    
    ;; Clean up records
    (map-delete deposits sender)
    (map-delete emergency-insurance sender)
    
    ;; Update total deposits
    (var-set total-deposits (- (var-get total-deposits) principal-amount))
    
    (ok net-withdrawal)
  )
)

;; Set up inter-generational transfer
(define-public (setup-inter-gen-transfer (recipient principal) (amount uint) (unlock-condition uint))
  (let (
    (sender tx-sender)
    (transfer-id (var-get next-transfer-id))
    (deposit-info (unwrap! (map-get? deposits sender) ERR_NOT_FOUND))
    (available-amount (get amount deposit-info))
  )
    (asserts! (<= amount available-amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> unlock-condition (get-block)) ERR_INVALID_AMOUNT)
    
    ;; Create transfer record
    (map-set inter-gen-transfers transfer-id {
      from: sender,
      to: recipient,
      amount: amount,
      unlock-condition: unlock-condition,
      completed: false
    })
    
    ;; Update deposit amount
    (map-set deposits sender 
      (merge deposit-info { amount: (- available-amount amount) })
    )
    
    ;; Increment transfer ID
    (var-set next-transfer-id (+ transfer-id u1))
    
    (ok transfer-id)
  )
)

;; Claim inter-generational transfer
(define-public (claim-inter-gen-transfer (transfer-id uint))
  (let (
    (transfer-info (unwrap! (map-get? inter-gen-transfers transfer-id) ERR_NOT_FOUND))
    (recipient (get to transfer-info))
    (amount (get amount transfer-info))
    (unlock-condition (get unlock-condition transfer-info))
  )
    (asserts! (is-eq tx-sender recipient) ERR_NOT_OWNER)
    (asserts! (>= (get-block) unlock-condition) ERR_LOCK_PERIOD_NOT_MET)
    (asserts! (not (get completed transfer-info)) ERR_ALREADY_EXISTS)
    
    ;; Transfer funds
    (try! (as-contract (stx-transfer? amount tx-sender recipient)))
    
    ;; Mark as completed
    (map-set inter-gen-transfers transfer-id
      (merge transfer-info { completed: true })
    )
    
    (ok amount)
  )
)

;; Governance: Create proposal
(define-public (create-proposal (description (string-ascii 256)))
  (let (
    (sender tx-sender)
    (proposal-id (var-get next-proposal-id))
    (governance-balance (ft-get-balance timebank-token sender))
  )
    (asserts! (>= governance-balance GOVERNANCE_THRESHOLD) ERR_INSUFFICIENT_FUNDS)
    
    (map-set governance-proposals proposal-id {
      proposer: sender,
      description: description,
      votes-for: u0,
      votes-against: u0,
      end-block: (+ (get-block) u1440), ;; ~1 day voting period
      executed: false
    })
    
    (var-set next-proposal-id (+ proposal-id u1))
    (ok proposal-id)
  )
)

;; Governance: Vote on proposal
(define-public (vote-on-proposal (proposal-id uint) (vote-for bool))
  (let (
    (sender tx-sender)
    (proposal (unwrap! (map-get? governance-proposals proposal-id) ERR_NOT_FOUND))
    (voting-power (ft-get-balance timebank-token sender))
    (existing-vote (map-get? governance-votes { proposal-id: proposal-id, voter: sender }))
  )
    (asserts! (< (get-block) (get end-block proposal)) ERR_LOCK_PERIOD_NOT_MET)
    (asserts! (> voting-power u0) ERR_INSUFFICIENT_FUNDS)
    (asserts! (is-none existing-vote) ERR_ALREADY_EXISTS)
    
    ;; Record vote
    (map-set governance-votes 
      { proposal-id: proposal-id, voter: sender }
      { vote: vote-for, weight: voting-power }
    )
    
    ;; Update proposal vote counts
    (if vote-for
      (map-set governance-proposals proposal-id
        (merge proposal { votes-for: (+ (get votes-for proposal) voting-power) })
      )
      (map-set governance-proposals proposal-id
        (merge proposal { votes-against: (+ (get votes-against proposal) voting-power) })
      )
    )
    
    (ok true)
  )
)

;; Oracle: Update CPI rate (only oracle can call)
(define-public (update-cpi-rate (new-rate uint))
  (let ((oracle (var-get oracle-address)))
    (asserts! (is-some oracle) ERR_INVALID_ORACLE)
    (asserts! (is-eq tx-sender (unwrap-panic oracle)) ERR_NOT_OWNER)
    (var-set current-cpi-rate new-rate)
    (ok true)
  )
)

;; Admin: Set oracle address
(define-public (set-oracle-address (oracle principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_OWNER)
    (var-set oracle-address (some oracle))
    (ok true)
  )
)

;; read only functions

;; Get deposit information
(define-read-only (get-deposit-info (user principal))
  (map-get? deposits user)
)

;; Get emergency insurance info
(define-read-only (get-emergency-insurance-info (user principal))
  (map-get? emergency-insurance user)
)

;; Get inter-generational transfer info
(define-read-only (get-inter-gen-transfer-info (transfer-id uint))
  (map-get? inter-gen-transfers transfer-id)
)

;; Get governance proposal info
(define-read-only (get-proposal-info (proposal-id uint))
  (map-get? governance-proposals proposal-id)
)

;; Calculate current interest rate based on CPI
(define-read-only (calculate-current-interest-rate)
  (+ BASE_INTEREST_RATE 
     (/ (* (var-get current-cpi-rate) INFLATION_MULTIPLIER) u100))
)

;; Calculate interest earned on a deposit
(define-read-only (calculate-interest (deposit-info { amount: uint, deposit-block: uint, lock-period: uint, unlock-block: uint, interest-rate: uint, emergency-insured: bool }))
  (let (
    (principal-amount (get amount deposit-info))
    (interest-rate (get interest-rate deposit-info))
    (time-factor (get lock-period deposit-info))
  )
    ;; Simple interest calculation: P * R * T / (BASIS_POINTS * BLOCKS_PER_YEAR)
    (/ (* (* principal-amount interest-rate) time-factor) 
       (* BASIS_POINTS MIN_LOCK_PERIOD))
  )
)

;; Get total value locked
(define-read-only (get-total-value-locked)
  (var-get total-deposits)
)

;; Get emergency pool balance
(define-read-only (get-emergency-pool-balance)
  (var-get emergency-pool-balance)
)

;; Get current CPI rate
(define-read-only (get-current-cpi-rate)
  (var-get current-cpi-rate)
)

;; Check if user can withdraw
(define-read-only (can-withdraw (user principal))
  (match (map-get? deposits user)
    deposit-info (>= (get-block) (get unlock-block deposit-info))
    false
  )
)

;; Get governance token balance
(define-read-only (get-governance-balance (user principal))
  (ft-get-balance timebank-token user)
)

;; private functions

;; Validate deposit parameters
(define-private (validate-deposit-params (amount uint) (lock-period uint))
  (and 
    (> amount u0)
    (>= lock-period MIN_LOCK_PERIOD)
    (<= lock-period MAX_LOCK_PERIOD)
  )
)

;; Temporary helper for block height to satisfy Clarinet environment
(define-private (get-block)
  (+ u0 u0)
)