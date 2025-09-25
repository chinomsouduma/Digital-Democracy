;; DecentralizedVotingPlatform - A Comprehensive On-Chain Polling System
;; 
;; This smart contract enables the creation and management of decentralized polls
;; with time-bound voting, transparent results, and secure vote tracking.
;; Features include multi-option polls, vote validation, creator controls,
;; and comprehensive query capabilities for poll analytics.

;; ERROR CONSTANTS
(define-constant ERROR-UNAUTHORIZED-ACCESS (err u100))
(define-constant ERROR-POLL-DOES-NOT-EXIST (err u101))
(define-constant ERROR-VOTING-PERIOD-ENDED (err u102))
(define-constant ERROR-POLL-CURRENTLY-INACTIVE (err u103))
(define-constant ERROR-DUPLICATE-VOTE-ATTEMPT (err u104))
(define-constant ERROR-INVALID-VOTING-OPTION (err u105))
(define-constant ERROR-POLL-ID-ALREADY-EXISTS (err u106))
(define-constant ERROR-INVALID-TIME-DURATION (err u107))
(define-constant ERROR-EMPTY-POLL-TITLE (err u108))
(define-constant ERROR-EXCESSIVE-OPTION-COUNT (err u109))
(define-constant ERROR-INSUFFICIENT-OPTIONS (err u110))
(define-constant ERROR-EMPTY-POLL-DESCRIPTION (err u111))

;; SYSTEM CONFIGURATION CONSTANTS
(define-constant contract-deployer-address tx-sender)
(define-constant maximum-voting-poll-options u10)
(define-constant minimum-voting-poll-options u2)
(define-constant maximum-poll-title-length u100)
(define-constant maximum-poll-description-length u500)
(define-constant maximum-voting-option-length u50)

;; STATE VARIABLES
(define-data-var next-available-poll-identifier uint u1)

;; DATA STRUCTURES
;; Core poll metadata and configuration storage
(define-map comprehensive-voting-polls-registry
  { unique-poll-identifier: uint }
  {
    descriptive-poll-title: (string-ascii 100),
    detailed-poll-description: (string-ascii 500),
    poll-creator-principal: principal,
    voting-period-start-height: uint,
    voting-period-end-height: uint,
    is-poll-currently-active: bool,
    total-accumulated-vote-count: uint,
    complete-voting-options-list: (list 10 (string-ascii 50))
  }
)

;; Vote tallies tracking for each poll option
(define-map voting-option-tallies-registry
  { unique-poll-identifier: uint, selected-voting-option-index: uint }
  { current-option-vote-total: uint }
)

;; Individual voter records and their voting choices
(define-map poll-participant-voting-history
  { unique-poll-identifier: uint, participant-principal-address: principal }
  { selected-voting-option-index: uint, vote-submission-block-height: uint }
)

;; VALIDATION UTILITY FUNCTIONS
;; Validate if poll is within active voting timeframe
(define-private (validate-poll-active-voting-timeframe (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (and 
                                   (get is-poll-currently-active retrieved-poll-configuration)
                                   (>= stacks-block-height (get voting-period-start-height retrieved-poll-configuration))
                                   (<= stacks-block-height (get voting-period-end-height retrieved-poll-configuration)))
    false
  )
)

;; Check if participant has already voted in the poll
(define-private (verify-participant-previous-voting-participation (unique-poll-identifier uint) (checking-participant-principal principal))
  (is-some (map-get? poll-participant-voting-history { unique-poll-identifier: unique-poll-identifier, participant-principal-address: checking-participant-principal }))
)

;; Validate selected voting option index is within bounds
(define-private (validate-selected-voting-option-index (unique-poll-identifier uint) (selected-voting-option-index uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (< selected-voting-option-index (len (get complete-voting-options-list retrieved-poll-configuration)))
    false
  )
)

;; Verify poll ownership for administrative actions
(define-private (verify-poll-creator-ownership (unique-poll-identifier uint) (claiming-owner-principal principal))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (is-eq claiming-owner-principal (get poll-creator-principal retrieved-poll-configuration))
    false
  )
)

;; Validate voting options list structure and count
(define-private (validate-voting-options-list-structure (provided-voting-options-list (list 10 (string-ascii 50))))
  (and 
    (>= (len provided-voting-options-list) minimum-voting-poll-options)
    (<= (len provided-voting-options-list) maximum-voting-poll-options)
  )
)

;; Validate poll title is not empty
(define-private (validate-poll-title-is-not-empty (provided-poll-title (string-ascii 100)))
  (> (len provided-poll-title) u0)
)

;; Validate poll description is not empty
(define-private (validate-poll-description-is-not-empty (provided-poll-description (string-ascii 500)))
  (> (len provided-poll-description) u0)
)

;; VOTE PROCESSING UTILITY FUNCTIONS
;; Update vote count for specific option
(define-private (increment-voting-option-count (unique-poll-identifier uint) (selected-voting-option-index uint))
  (let ((current-existing-vote-count (default-to u0 (get current-option-vote-total (map-get? voting-option-tallies-registry { unique-poll-identifier: unique-poll-identifier, selected-voting-option-index: selected-voting-option-index })))))
    (map-set voting-option-tallies-registry
      { unique-poll-identifier: unique-poll-identifier, selected-voting-option-index: selected-voting-option-index }
      { current-option-vote-total: (+ current-existing-vote-count u1) }
    )
  )
)

;; Increment total poll participation count
(define-private (increment-total-poll-participation-count (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (map-set comprehensive-voting-polls-registry
                                   { unique-poll-identifier: unique-poll-identifier }
                                   (merge retrieved-poll-configuration { total-accumulated-vote-count: (+ (get total-accumulated-vote-count retrieved-poll-configuration) u1) }))
    false
  )
)

;; Record participant's voting choice and timestamp
(define-private (record-participant-voting-choice (unique-poll-identifier uint) (voting-participant-principal principal) (selected-voting-option-index uint))
  (map-set poll-participant-voting-history
    { unique-poll-identifier: unique-poll-identifier, participant-principal-address: voting-participant-principal }
    { selected-voting-option-index: selected-voting-option-index, vote-submission-block-height: stacks-block-height }
  )
)

;; POLL MANAGEMENT PUBLIC FUNCTIONS
;; Create a new polling instance with comprehensive validation
(define-public (establish-new-comprehensive-poll 
  (descriptive-poll-title (string-ascii 100))
  (detailed-poll-description (string-ascii 500))
  (voting-duration-in-blocks uint)
  (complete-voting-options-list (list 10 (string-ascii 50))))
  (let ((assigned-new-poll-identifier (var-get next-available-poll-identifier))
        (voting-period-commencement-height stacks-block-height)
        (voting-period-conclusion-height (+ stacks-block-height voting-duration-in-blocks)))
    
    ;; Comprehensive input validation checks
    (asserts! (validate-poll-title-is-not-empty descriptive-poll-title) ERROR-EMPTY-POLL-TITLE)
    (asserts! (validate-poll-description-is-not-empty detailed-poll-description) ERROR-EMPTY-POLL-DESCRIPTION)
    (asserts! (> voting-duration-in-blocks u0) ERROR-INVALID-TIME-DURATION)
    (asserts! (validate-voting-options-list-structure complete-voting-options-list) ERROR-INSUFFICIENT-OPTIONS)
    (asserts! (is-none (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: assigned-new-poll-identifier })) ERROR-POLL-ID-ALREADY-EXISTS)
    
    ;; Store comprehensive poll configuration
    (map-set comprehensive-voting-polls-registry
      { unique-poll-identifier: assigned-new-poll-identifier }
      {
        descriptive-poll-title: descriptive-poll-title,
        detailed-poll-description: detailed-poll-description,
        poll-creator-principal: tx-sender,
        voting-period-start-height: voting-period-commencement-height,
        voting-period-end-height: voting-period-conclusion-height,
        is-poll-currently-active: true,
        total-accumulated-vote-count: u0,
        complete-voting-options-list: complete-voting-options-list
      }
    )
    
    ;; Update system-wide poll counter
    (var-set next-available-poll-identifier (+ assigned-new-poll-identifier u1))
    (ok assigned-new-poll-identifier)
  )
)

;; Process vote submission with complete validation
(define-public (submit-participant-vote-selection (unique-poll-identifier uint) (selected-voting-option-index uint))
  (let ((retrieved-poll-configuration (unwrap! (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier }) ERROR-POLL-DOES-NOT-EXIST)))
    
    ;; Comprehensive voting eligibility validation
    (asserts! (validate-poll-active-voting-timeframe unique-poll-identifier) ERROR-POLL-CURRENTLY-INACTIVE)
    (asserts! (not (verify-participant-previous-voting-participation unique-poll-identifier tx-sender)) ERROR-DUPLICATE-VOTE-ATTEMPT)
    (asserts! (validate-selected-voting-option-index unique-poll-identifier selected-voting-option-index) ERROR-INVALID-VOTING-OPTION)
    
    ;; Process and permanently record the vote
    (record-participant-voting-choice unique-poll-identifier tx-sender selected-voting-option-index)
    (increment-voting-option-count unique-poll-identifier selected-voting-option-index)
    (increment-total-poll-participation-count unique-poll-identifier)
    
    (ok true)
  )
)

;; Terminate poll operations (creator-only administrative function)
(define-public (terminate-poll-voting-operations (unique-poll-identifier uint))
  (let ((retrieved-poll-configuration (unwrap! (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier }) ERROR-POLL-DOES-NOT-EXIST)))
    (asserts! (verify-poll-creator-ownership unique-poll-identifier tx-sender) ERROR-UNAUTHORIZED-ACCESS)
    
    (map-set comprehensive-voting-polls-registry
      { unique-poll-identifier: unique-poll-identifier }
      (merge retrieved-poll-configuration { is-poll-currently-active: false })
    )
    
    (ok true)
  )
)

;; QUERY AND ANALYTICS READ-ONLY FUNCTIONS
;; Retrieve complete poll configuration and metadata
(define-read-only (retrieve-comprehensive-poll-configuration (unique-poll-identifier uint))
  (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
)

;; Generate comprehensive poll analytics and voting results
(define-read-only (generate-comprehensive-poll-analytics (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (some {
                                   unique-poll-identifier: unique-poll-identifier,
                                   descriptive-poll-title: (get descriptive-poll-title retrieved-poll-configuration),
                                   total-voting-participants: (get total-accumulated-vote-count retrieved-poll-configuration),
                                   current-poll-activity-status: (get is-poll-currently-active retrieved-poll-configuration),
                                   comprehensive-voting-results: (map retrieve-single-option-voting-results 
                                                                    (generate-voting-option-indices-list unique-poll-identifier))
                                 })
    none
  )
)

;; Helper function for retrieving individual option vote counts
(define-private (retrieve-single-option-voting-results (option-data-structure { poll-identifier: uint, option-index: uint }))
  (default-to u0 (get current-option-vote-total (map-get? voting-option-tallies-registry { unique-poll-identifier: (get poll-identifier option-data-structure), selected-voting-option-index: (get option-index option-data-structure) })))
)

;; Generate comprehensive option indices list for specific poll
(define-private (generate-voting-option-indices-list (unique-poll-identifier uint))
  (list 
    { poll-identifier: unique-poll-identifier, option-index: u0 }
    { poll-identifier: unique-poll-identifier, option-index: u1 }
    { poll-identifier: unique-poll-identifier, option-index: u2 }
    { poll-identifier: unique-poll-identifier, option-index: u3 }
    { poll-identifier: unique-poll-identifier, option-index: u4 }
    { poll-identifier: unique-poll-identifier, option-index: u5 }
    { poll-identifier: unique-poll-identifier, option-index: u6 }
    { poll-identifier: unique-poll-identifier, option-index: u7 }
    { poll-identifier: unique-poll-identifier, option-index: u8 }
    { poll-identifier: unique-poll-identifier, option-index: u9 }
  )
)

;; Query specific voting option vote totals
(define-read-only (query-specific-voting-option-total (unique-poll-identifier uint) (selected-voting-option-index uint))
  (default-to u0 (get current-option-vote-total (map-get? voting-option-tallies-registry { unique-poll-identifier: unique-poll-identifier, selected-voting-option-index: selected-voting-option-index })))
)

;; Verify participant voting history and participation status
(define-read-only (check-participant-voting-participation-status (unique-poll-identifier uint) (checking-participant-principal principal))
  (verify-participant-previous-voting-participation unique-poll-identifier checking-participant-principal)
)

;; Retrieve participant's complete voting record
(define-read-only (retrieve-participant-complete-voting-record (unique-poll-identifier uint) (participant-principal-address principal))
  (map-get? poll-participant-voting-history { unique-poll-identifier: unique-poll-identifier, participant-principal-address: participant-principal-address })
)

;; Verify current poll operational and activity status
(define-read-only (verify-poll-current-activity-status (unique-poll-identifier uint))
  (validate-poll-active-voting-timeframe unique-poll-identifier)
)

;; Get system-wide poll counter for tracking
(define-read-only (retrieve-system-wide-poll-counter)
  (var-get next-available-poll-identifier)
)

;; List all currently active polls (limited scope for performance optimization)
(define-read-only (enumerate-all-currently-active-polls)
  (filter verify-poll-existence-and-current-activity (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 
                                                           u11 u12 u13 u14 u15 u16 u17 u18 u19 u20
                                                           u21 u22 u23 u24 u25 u26 u27 u28 u29 u30
                                                           u31 u32 u33 u34 u35 u36 u37 u38 u39 u40
                                                           u41 u42 u43 u44 u45 u46 u47 u48 u49 u50))
)

;; Utility function for comprehensive active poll enumeration
(define-read-only (verify-poll-existence-and-current-activity (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (and (get is-poll-currently-active retrieved-poll-configuration)
                                     (validate-poll-active-voting-timeframe unique-poll-identifier))
    false
  )
)

;; Retrieve poll creator principal information
(define-read-only (retrieve-poll-creator-principal-information (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (some (get poll-creator-principal retrieved-poll-configuration))
    none
  )
)

;; Calculate poll voting time progress percentage
(define-read-only (calculate-poll-voting-time-progress-percentage (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (let ((voting-start-height (get voting-period-start-height retrieved-poll-configuration))
                                       (voting-end-height (get voting-period-end-height retrieved-poll-configuration))
                                       (current-block-height stacks-block-height))
                                   (if (< current-block-height voting-start-height)
                                     u0
                                     (if (> current-block-height voting-end-height)
                                       u100
                                       (/ (* (- current-block-height voting-start-height) u100) 
                                          (- voting-end-height voting-start-height)))))
    u0
  )
)

;; Get current Stacks blockchain block height
(define-read-only (retrieve-current-stacks-blockchain-block-height)
  stacks-block-height
)

;; Check if poll has expired based on current block height
(define-read-only (verify-poll-voting-period-has-expired (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (> stacks-block-height (get voting-period-end-height retrieved-poll-configuration))
    true
  )
)

;; Get remaining blocks until poll voting period expires
(define-read-only (calculate-remaining-voting-period-blocks (unique-poll-identifier uint))
  (match (map-get? comprehensive-voting-polls-registry { unique-poll-identifier: unique-poll-identifier })
    retrieved-poll-configuration (let ((voting-end-height (get voting-period-end-height retrieved-poll-configuration))
                                       (current-block-height stacks-block-height))
                                   (if (> current-block-height voting-end-height)
                                     u0
                                     (- voting-end-height current-block-height)))
    u0
  )
)