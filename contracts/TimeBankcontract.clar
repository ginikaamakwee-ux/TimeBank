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
