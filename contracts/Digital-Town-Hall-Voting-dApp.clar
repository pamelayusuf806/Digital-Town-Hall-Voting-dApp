(define-constant contract-owner tx-sender)
(define-constant err-not-authorized (err u100))
(define-constant err-proposal-exists (err u101))
(define-constant err-no-proposal (err u102))
(define-constant err-voting-closed (err u103))
(define-constant err-already-voted (err u104))
(define-constant err-invalid-vote (err u105))

(define-data-var proposal-counter uint u0)

(define-map Proposals
    { proposal-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        creator: principal,
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 20),
    }
)

(define-map VoterRecords
    {
        voter: principal,
        proposal-id: uint,
    }
    { voted: bool }
)

(define-read-only (get-voter-status
        (proposal-id uint)
        (voter principal)
    )
    (default-to { voted: false }
        (map-get? VoterRecords {
            voter: voter,
            proposal-id: proposal-id,
        })
    )
)

(define-public (create-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (duration uint)
    )
    (let (
            (new-id (+ (var-get proposal-counter) u1))
            (start-block burn-block-height)
            (end-block (+ burn-block-height duration))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (map-set Proposals { proposal-id: new-id } {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            status: "active",
        })
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-public (cast-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals { proposal-id: proposal-id })
                err-no-proposal
            ))
            (voter-record (get-voter-status proposal-id tx-sender))
        )
        (asserts! (< burn-block-height (get end-block proposal))
            err-voting-closed
        )
        (asserts! (> burn-block-height (get start-block proposal))
            err-invalid-vote
        )
        (asserts! (not (get voted voter-record)) err-already-voted)
        (map-set VoterRecords {
            voter: tx-sender,
            proposal-id: proposal-id,
        } { voted: true }
        )
        (map-set Proposals { proposal-id: proposal-id }
            (merge proposal {
                yes-votes: (if vote
                    (+ (get yes-votes proposal) u1)
                    (get yes-votes proposal)
                ),
                no-votes: (if (not vote)
                    (+ (get no-votes proposal) u1)
                    (get no-votes proposal)
                ),
            })
        )
        ;; Record analytics for this vote
        (record-vote-analytics proposal-id tx-sender burn-block-height)
        (ok true)
    )
)
(define-public (close-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? Proposals { proposal-id: proposal-id })
            err-no-proposal
        )))
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (>= burn-block-height (get end-block proposal))
            err-invalid-vote
        )
        (map-set Proposals { proposal-id: proposal-id }
            (merge proposal { status: "closed" })
        )
        (ok true)
    )
)

(define-read-only (get-proposal-results (proposal-id uint))
    (let ((proposal (unwrap! (map-get? Proposals { proposal-id: proposal-id })
            err-no-proposal
        )))
        (ok {
            yes-votes: (get yes-votes proposal),
            no-votes: (get no-votes proposal),
            status: (get status proposal),
        })
    )
)

(define-read-only (get-all-proposals)
    (ok (var-get proposal-counter))
)
(define-map Delegations
    { delegator: principal }
    {
        delegate: principal,
        active: bool,
    }
)

(define-map DelegationPower
    { delegate: principal }
    { power: uint }
)

(define-public (delegate-vote (delegate principal))
    (let (
            (current-delegation (map-get? Delegations { delegator: tx-sender }))
            (current-power (default-to { power: u0 }
                (map-get? DelegationPower { delegate: delegate })
            ))
        )
        (asserts! (not (is-eq tx-sender delegate)) err-invalid-vote)
        (match current-delegation
            existing-delegation (if (get active existing-delegation)
                (begin
                    (map-set DelegationPower { delegate: (get delegate existing-delegation) } { power: (-
                        (get power
                            (default-to { power: u1 }
                                (map-get? DelegationPower { delegate: (get delegate existing-delegation) })
                            ))
                        u1
                    ) }
                    )
                    (map-set Delegations { delegator: tx-sender } {
                        delegate: delegate,
                        active: true,
                    })
                    (map-set DelegationPower { delegate: delegate } { power: (+ (get power current-power) u1) })
                    (ok true)
                )
                (begin
                    (map-set Delegations { delegator: tx-sender } {
                        delegate: delegate,
                        active: true,
                    })
                    (map-set DelegationPower { delegate: delegate } { power: (+ (get power current-power) u1) })
                    (ok true)
                )
            )
            (begin
                (map-set Delegations { delegator: tx-sender } {
                    delegate: delegate,
                    active: true,
                })
                (map-set DelegationPower { delegate: delegate } { power: (+ (get power current-power) u1) })
                (ok true)
            )
        )
    )
)

(define-public (revoke-delegation)
    (let (
            (delegation (unwrap! (map-get? Delegations { delegator: tx-sender })
                err-no-proposal
            ))
            (current-power (default-to { power: u0 }
                (map-get? DelegationPower { delegate: (get delegate delegation) })
            ))
        )
        (asserts! (get active delegation) err-invalid-vote)
        (map-set Delegations { delegator: tx-sender } {
            delegate: (get delegate delegation),
            active: false,
        })
        (map-set DelegationPower { delegate: (get delegate delegation) } { power: (- (get power current-power) u1) })
        (ok true)
    )
)

(define-public (cast-delegated-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals { proposal-id: proposal-id })
                err-no-proposal
            ))
            (voter-record (get-voter-status proposal-id tx-sender))
            (delegation-power (default-to { power: u0 }
                (map-get? DelegationPower { delegate: tx-sender })
            ))
            (vote-weight (+ u1 (get power delegation-power)))
        )
        (asserts! (< burn-block-height (get end-block proposal))
            err-voting-closed
        )
        (asserts! (> burn-block-height (get start-block proposal))
            err-invalid-vote
        )
        (asserts! (not (get voted voter-record)) err-already-voted)
        (map-set VoterRecords {
            voter: tx-sender,
            proposal-id: proposal-id,
        } { voted: true }
        )
        (map-set Proposals { proposal-id: proposal-id }
            (merge proposal {
                yes-votes: (if vote
                    (+ (get yes-votes proposal) vote-weight)
                    (get yes-votes proposal)
                ),
                no-votes: (if (not vote)
                    (+ (get no-votes proposal) vote-weight)
                    (get no-votes proposal)
                ),
            })
        )
        ;; Record analytics for delegated vote
        (record-vote-analytics proposal-id tx-sender burn-block-height)
        (ok true)
    )
)

(define-read-only (get-delegation-info (delegator principal))
    (map-get? Delegations { delegator: delegator })
)

(define-read-only (get-delegation-power (delegate principal))
    (default-to { power: u0 } (map-get? DelegationPower { delegate: delegate }))
)
(define-constant category-budget "budget")
(define-constant category-policy "policy")
(define-constant category-technical "technical")
(define-constant category-general "general")

(define-data-var total-registered-voters uint u0)

(define-map CategoryRequirements
    { category: (string-ascii 20) }
    {
        min-participation: uint,
        min-approval: uint,
    }
)

(define-map RegisteredVoters
    { voter: principal }
    { registered: bool }
)

(define-map Proposals-Enhanced
    { proposal-id: uint }
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        creator: principal,
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 20),
        category: (string-ascii 20),
        total-participants: uint,
    }
)

(map-set CategoryRequirements { category: category-budget } {
    min-participation: u10,
    min-approval: u60,
})
(map-set CategoryRequirements { category: category-policy } {
    min-participation: u15,
    min-approval: u55,
})
(map-set CategoryRequirements { category: category-technical } {
    min-participation: u8,
    min-approval: u50,
})
(map-set CategoryRequirements { category: category-general } {
    min-participation: u5,
    min-approval: u50,
})

(define-public (register-voter)
    (let ((current-registration (map-get? RegisteredVoters { voter: tx-sender })))
        (match current-registration
            existing-reg (if (get registered existing-reg)
                (ok false)
                (begin
                    (map-set RegisteredVoters { voter: tx-sender } { registered: true })
                    (var-set total-registered-voters
                        (+ (var-get total-registered-voters) u1)
                    )
                    (ok true)
                )
            )
            (begin
                (map-set RegisteredVoters { voter: tx-sender } { registered: true })
                (var-set total-registered-voters
                    (+ (var-get total-registered-voters) u1)
                )
                (ok true)
            )
        )
    )
)

(define-public (create-categorized-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (duration uint)
        (category (string-ascii 20))
    )
    (let (
            (new-id (+ (var-get proposal-counter) u1))
            (start-block burn-block-height)
            (end-block (+ burn-block-height duration))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts!
            (is-some (map-get? CategoryRequirements { category: category }))
            err-invalid-vote
        )
        (map-set Proposals-Enhanced { proposal-id: new-id } {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            status: "active",
            category: category,
            total-participants: u0,
        })
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-public (cast-categorized-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                err-no-proposal
            ))
            (voter-record (get-voter-status proposal-id tx-sender))
            (voter-registration (unwrap! (map-get? RegisteredVoters { voter: tx-sender })
                err-not-authorized
            ))
        )
        (asserts! (get registered voter-registration) err-not-authorized)
        (asserts! (< burn-block-height (get end-block proposal))
            err-voting-closed
        )
        (asserts! (> burn-block-height (get start-block proposal))
            err-invalid-vote
        )
        (asserts! (not (get voted voter-record)) err-already-voted)
        (map-set VoterRecords {
            voter: tx-sender,
            proposal-id: proposal-id,
        } { voted: true }
        )
        (map-set Proposals-Enhanced { proposal-id: proposal-id }
            (merge proposal {
                yes-votes: (if vote
                    (+ (get yes-votes proposal) u1)
                    (get yes-votes proposal)
                ),
                no-votes: (if (not vote)
                    (+ (get no-votes proposal) u1)
                    (get no-votes proposal)
                ),
                total-participants: (+ (get total-participants proposal) u1),
            })
        )
        ;; Record analytics for categorized vote
        (record-vote-analytics proposal-id tx-sender burn-block-height)
        (update-user-reputation tx-sender u1 u0 u0)
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                err-no-proposal
            ))
            (category-req (unwrap!
                (map-get? CategoryRequirements { category: (get category proposal) })
                err-invalid-vote
            ))
            (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
            (participation-rate (/ (* total-votes u100) (var-get total-registered-voters)))
            (approval-rate (if (> total-votes u0)
                (/ (* (get yes-votes proposal) u100) total-votes)
                u0
            ))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts! (>= burn-block-height (get end-block proposal))
            err-invalid-vote
        )
        (let (
                (meets-participation (>= participation-rate (get min-participation category-req)))
                (meets-approval (>= approval-rate (get min-approval category-req)))
                (final-status (if (and meets-participation meets-approval)
                    "passed"
                    "failed"
                ))
            )
            (map-set Proposals-Enhanced { proposal-id: proposal-id }
                (merge proposal { status: final-status })
            )
            (ok final-status)
        )
    )
)

(define-read-only (get-enhanced-proposal (proposal-id uint))
    (map-get? Proposals-Enhanced { proposal-id: proposal-id })
)

(define-read-only (get-category-requirements (category (string-ascii 20)))
    (map-get? CategoryRequirements { category: category })
)

(define-read-only (get-total-registered-voters)
    (ok (var-get total-registered-voters))
)

(define-constant err-amendment-window-closed (err u106))
(define-constant err-max-amendments-reached (err u107))

(define-data-var amendment-counter uint u0)

(define-map ProposalAmendments
    {
        proposal-id: uint,
        amendment-id: uint,
    }
    {
        amendment-text: (string-ascii 500),
        created-at: uint,
        creator: principal,
    }
)

(define-map ProposalAmendmentCount
    { proposal-id: uint }
    { count: uint }
)

(define-public (create-amendment
        (proposal-id uint)
        (amendment-text (string-ascii 500))
    )
    (let (
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                err-no-proposal
            ))
            (current-count (default-to { count: u0 }
                (map-get? ProposalAmendmentCount { proposal-id: proposal-id })
            ))
            (new-amendment-id (+ (var-get amendment-counter) u1))
            (voting-window-remaining (- (get end-block proposal) burn-block-height))
            (amendment-deadline (- (get end-block proposal) (/ voting-window-remaining u4)))
        )
        (asserts! (is-eq tx-sender (get creator proposal)) err-not-authorized)
        (asserts! (is-eq (get status proposal) "active") err-voting-closed)
        (asserts! (< burn-block-height amendment-deadline)
            err-amendment-window-closed
        )
        (asserts! (< (get count current-count) u3) err-max-amendments-reached)
        (map-set ProposalAmendments {
            proposal-id: proposal-id,
            amendment-id: new-amendment-id,
        } {
            amendment-text: amendment-text,
            created-at: burn-block-height,
            creator: tx-sender,
        })
        (map-set ProposalAmendmentCount { proposal-id: proposal-id } { count: (+ (get count current-count) u1) })
        (var-set amendment-counter new-amendment-id)
        (update-user-reputation tx-sender u0 u0 u1)
        (ok new-amendment-id)
    )
)

(define-read-only (get-proposal-amendments (proposal-id uint))
    (let ((amendment-count (default-to { count: u0 }
            (map-get? ProposalAmendmentCount { proposal-id: proposal-id })
        )))
        (ok (get count amendment-count))
    )
)

(define-read-only (get-amendment-details
        (proposal-id uint)
        (amendment-id uint)
    )
    (map-get? ProposalAmendments {
        proposal-id: proposal-id,
        amendment-id: amendment-id,
    })
)

(define-read-only (get-amendment-window-status (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                err-no-proposal
            ))
            (voting-window-remaining (- (get end-block proposal) burn-block-height))
            (amendment-deadline (- (get end-block proposal) (/ voting-window-remaining u4)))
        )
        (ok {
            can-amend: (and
                (< burn-block-height amendment-deadline)
                (is-eq (get status proposal) "active")
            ),
            deadline-block: amendment-deadline,
        })
    )
)

(define-map CommunityReputation
    { user: principal }
    {
        total-votes: uint,
        proposals-created: uint,
        amendments-made: uint,
        reputation-score: uint,
        last-activity: uint,
    }
)

(define-map ReputationWeightedProposals
    { proposal-id: uint }
    {
        requires-reputation: bool,
        min-reputation-score: uint,
    }
)

(define-private (calculate-reputation-score
        (votes uint)
        (proposals uint)
        (amendments uint)
    )
    (+ (* votes u10) (* proposals u50) (* amendments u25))
)

(define-private (update-user-reputation
        (user principal)
        (vote-increment uint)
        (proposal-increment uint)
        (amendment-increment uint)
    )
    (let (
            (current-rep (default-to {
                total-votes: u0,
                proposals-created: u0,
                amendments-made: u0,
                reputation-score: u0,
                last-activity: u0,
            }
                (map-get? CommunityReputation { user: user })
            ))
            (new-votes (+ (get total-votes current-rep) vote-increment))
            (new-proposals (+ (get proposals-created current-rep) proposal-increment))
            (new-amendments (+ (get amendments-made current-rep) amendment-increment))
            (new-score (calculate-reputation-score new-votes new-proposals new-amendments))
        )
        (map-set CommunityReputation { user: user } {
            total-votes: new-votes,
            proposals-created: new-proposals,
            amendments-made: new-amendments,
            reputation-score: new-score,
            last-activity: burn-block-height,
        })
        new-score
    )
)

(define-public (create-reputation-weighted-proposal
        (title (string-ascii 100))
        (description (string-ascii 500))
        (duration uint)
        (category (string-ascii 20))
        (min-reputation uint)
    )
    (let (
            (new-id (+ (var-get proposal-counter) u1))
            (start-block burn-block-height)
            (end-block (+ burn-block-height duration))
        )
        (asserts! (is-eq tx-sender contract-owner) err-not-authorized)
        (asserts!
            (is-some (map-get? CategoryRequirements { category: category }))
            err-invalid-vote
        )
        (map-set Proposals-Enhanced { proposal-id: new-id } {
            title: title,
            description: description,
            creator: tx-sender,
            start-block: start-block,
            end-block: end-block,
            yes-votes: u0,
            no-votes: u0,
            status: "active",
            category: category,
            total-participants: u0,
        })
        (map-set ReputationWeightedProposals { proposal-id: new-id } {
            requires-reputation: true,
            min-reputation-score: min-reputation,
        })
        (update-user-reputation tx-sender u0 u1 u0)
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-public (cast-reputation-weighted-vote
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                err-no-proposal
            ))
            (reputation-req (unwrap!
                (map-get? ReputationWeightedProposals { proposal-id: proposal-id })
                err-invalid-vote
            ))
            (voter-record (get-voter-status proposal-id tx-sender))
            (voter-registration (unwrap! (map-get? RegisteredVoters { voter: tx-sender })
                err-not-authorized
            ))
            (user-reputation (default-to {
                total-votes: u0,
                proposals-created: u0,
                amendments-made: u0,
                reputation-score: u0,
                last-activity: u0,
            }
                (map-get? CommunityReputation { user: tx-sender })
            ))
            (vote-weight (if (>= (get reputation-score user-reputation) u100)
                u2
                u1
            ))
        )
        (asserts! (get registered voter-registration) err-not-authorized)
        (asserts! (get requires-reputation reputation-req) err-invalid-vote)
        (asserts!
            (>= (get reputation-score user-reputation)
                (get min-reputation-score reputation-req)
            )
            err-not-authorized
        )
        (asserts! (< burn-block-height (get end-block proposal))
            err-voting-closed
        )
        (asserts! (> burn-block-height (get start-block proposal))
            err-invalid-vote
        )
        (asserts! (not (get voted voter-record)) err-already-voted)
        (map-set VoterRecords {
            voter: tx-sender,
            proposal-id: proposal-id,
        } { voted: true }
        )
        (map-set Proposals-Enhanced { proposal-id: proposal-id }
            (merge proposal {
                yes-votes: (if vote
                    (+ (get yes-votes proposal) vote-weight)
                    (get yes-votes proposal)
                ),
                no-votes: (if (not vote)
                    (+ (get no-votes proposal) vote-weight)
                    (get no-votes proposal)
                ),
                total-participants: (+ (get total-participants proposal) u1),
            })
        )
        ;; Record analytics for reputation-weighted vote
        (record-vote-analytics proposal-id tx-sender burn-block-height)
        (update-user-reputation tx-sender u1 u0 u0)
        (ok true)
    )
)

(define-read-only (get-user-reputation (user principal))
    (map-get? CommunityReputation { user: user })
)

(define-read-only (get-reputation-requirements (proposal-id uint))
    (map-get? ReputationWeightedProposals { proposal-id: proposal-id })
)

(define-read-only (check-voting-eligibility
        (proposal-id uint)
        (user principal)
    )
    (let (
            (reputation-req (map-get? ReputationWeightedProposals { proposal-id: proposal-id }))
            (user-reputation (default-to {
                total-votes: u0,
                proposals-created: u0,
                amendments-made: u0,
                reputation-score: u0,
                last-activity: u0,
            }
                (map-get? CommunityReputation { user: user })
            ))
        )
        (match reputation-req
            req (ok {
                eligible: (>= (get reputation-score user-reputation)
                    (get min-reputation-score req)
                ),
                user-score: (get reputation-score user-reputation),
                required-score: (get min-reputation-score req),
            })
            (ok {
                eligible: true,
                user-score: (get reputation-score user-reputation),
                required-score: u0,
            })
        )
    )
)

;; ===== GOVERNANCE ANALYTICS AND REPORTING SYSTEM =====

;; Error constants for analytics operations
(define-constant err-invalid-time-period (err u108))
(define-constant err-analytics-not-found (err u109))
(define-constant err-report-generation-failed (err u110))

;; Analytics constants and thresholds
(define-constant analytics-version u1)
(define-constant max-historical-periods u52) ;; Track up to 52 periods (weeks/months)
(define-constant high-participation-threshold u75) ;; 75% participation considered high
(define-constant low-participation-threshold u25) ;; 25% participation considered low

;; Data structures for governance analytics
(define-data-var analytics-counter uint u0)
(define-data-var last-analytics-update uint u0)

;; Governance metrics per proposal
(define-map GovernanceMetrics
    { proposal-id: uint }
    {
        total-eligible-voters: uint,
        actual-participants: uint,
        participation-rate: uint, ;; Percentage * 100 for precision
        average-vote-time: uint, ;; Average blocks from start to vote
        voting-distribution: { early: uint, mid: uint, late: uint },
        category-performance: (string-ascii 20),
        created-at: uint,
    }
)

;; Historical participation tracking
(define-map ParticipationHistory
    {
        period-id: uint,
        period-type: (string-ascii 10), ;; "weekly", "monthly"
    }
    {
        start-block: uint,
        end-block: uint,
        total-proposals: uint,
        total-votes-cast: uint,
        active-voters: uint,
        average-participation: uint,
        governance-health-score: uint,
    }
)

;; Voting trends and patterns
(define-map VotingTrends
    {
        trend-type: (string-ascii 20), ;; "daily", "category", "user_behavior"
        identifier: (string-ascii 50), ;; Category name or time period
    }
    {
        sample-size: uint,
        positive-votes: uint,
        negative-votes: uint,
        trend-direction: (string-ascii 10), ;; "increasing", "decreasing", "stable"
        confidence-level: uint,
        last-updated: uint,
    }
)

;; Governance reports storage
(define-map GovernanceReports
    {
        report-id: uint,
        report-type: (string-ascii 20), ;; "summary", "detailed", "trends"
    }
    {
        generated-at: uint,
        period-start: uint,
        period-end: uint,
        key-metrics: {
            total-proposals: uint,
            total-participants: uint,
            governance-health: uint,
            engagement-trend: (string-ascii 10),
        },
        recommendations: (string-ascii 200),
        next-review-block: uint,
    }
)

;; Voter activity analytics
(define-map VoterActivityAnalytics
    { voter: principal }
    {
        first-vote-block: uint,
        last-vote-block: uint,
        total-votes-cast: uint,
        categories-engaged: uint, ;; Bitmask for categories participated
        participation-streak: uint,
        avg-response-time: uint,
        engagement-score: uint,
    }
)

;; Private helper functions for analytics calculations
(define-private (calculate-participation-rate (participants uint) (eligible uint))
    (if (> eligible u0)
        (/ (* participants u10000) eligible) ;; Return percentage * 100 for precision
        u0
    )
)

(define-private (calculate-governance-health
        (participation uint)
        (proposal-count uint)
        (voter-diversity uint)
    )
    (let (
            (participation-score (if (>= participation high-participation-threshold)
                u40
                (if (<= participation low-participation-threshold)
                    u10
                    (+ u10 (/ (* (- participation low-participation-threshold) u30)
                        (- high-participation-threshold low-participation-threshold))
                    )
                )
            ))
            (activity-score (if (< (* proposal-count u5) u30) (* proposal-count u5) u30))
            (diversity-score (if (< (* voter-diversity u2) u30) (* voter-diversity u2) u30))
        )
        (+ participation-score activity-score diversity-score)
    )
)

(define-private (determine-trend-direction
        (current-value uint)
        (previous-value uint)
        (threshold uint)
    )
    (let ((difference (if (>= current-value previous-value)
            (- current-value previous-value)
            (- previous-value current-value)
        )))
        (if (> difference threshold)
            (if (> current-value previous-value) "increasing" "decreasing")
            "stable"
        )
    )
)

;; Core analytics recording functions
(define-private (record-vote-analytics
        (proposal-id uint)
        (voter principal)
        (vote-block uint)
    )
    (let (
            (proposal-opt (map-get? Proposals-Enhanced { proposal-id: proposal-id }))
        )
        (if (is-some proposal-opt)
            (let (
                (proposal (unwrap-panic proposal-opt))
                (current-metrics (map-get? GovernanceMetrics { proposal-id: proposal-id }))
                (vote-timing (- vote-block (get start-block proposal)))
                (proposal-duration (- (get end-block proposal) (get start-block proposal)))
                (timing-category (if (< vote-timing (/ proposal-duration u3))
                    "early"
                    (if (< vote-timing (* proposal-duration u2))
                        "mid"
                        "late"
                    )
                ))
        )
        (match current-metrics
            existing-metrics
            (let (
                    (current-dist (get voting-distribution existing-metrics))
                    (updated-distribution
                        (if (is-eq timing-category "early")
                            { 
                                early: (+ (get early current-dist) u1), 
                                mid: (get mid current-dist), 
                                late: (get late current-dist) 
                            }
                            (if (is-eq timing-category "mid")
                                { 
                                    early: (get early current-dist), 
                                    mid: (+ (get mid current-dist) u1), 
                                    late: (get late current-dist) 
                                }
                                { 
                                    early: (get early current-dist), 
                                    mid: (get mid current-dist), 
                                    late: (+ (get late current-dist) u1) 
                                }
                            )
                        )
                    )
                    (new-participant-count (+ (get actual-participants existing-metrics) u1))
                    (new-participation-rate (calculate-participation-rate
                        new-participant-count
                        (get total-eligible-voters existing-metrics)
                    ))
                )
                (map-set GovernanceMetrics { proposal-id: proposal-id }
                    (merge existing-metrics {
                        actual-participants: new-participant-count,
                        participation-rate: new-participation-rate,
                        voting-distribution: updated-distribution,
                    })
                )
            )
            ;; Initialize metrics if first vote
            (let (
                    (eligible-voters (var-get total-registered-voters))
                    (initial-distribution
                        (if (is-eq timing-category "early")
                            { early: u1, mid: u0, late: u0 }
                            (if (is-eq timing-category "mid")
                                { early: u0, mid: u1, late: u0 }
                                { early: u0, mid: u0, late: u1 }
                            )
                        )
                    )
                )
                (map-set GovernanceMetrics { proposal-id: proposal-id } {
                    total-eligible-voters: eligible-voters,
                    actual-participants: u1,
                    participation-rate: (calculate-participation-rate u1 eligible-voters),
                    average-vote-time: vote-timing,
                    voting-distribution: initial-distribution,
                    category-performance: (get category proposal),
                    created-at: burn-block-height,
                })
            )
        )
                ;; Update voter activity analytics
                (update-voter-activity-analytics voter proposal-id vote-block)
                true
            )
            false ;; Return false if proposal not found
        )
    )
)

(define-private (update-voter-activity-analytics
        (voter principal)
        (proposal-id uint)
        (vote-block uint)
    )
    (let (
            (current-activity (map-get? VoterActivityAnalytics { voter: voter }))
            (proposal (unwrap! (map-get? Proposals-Enhanced { proposal-id: proposal-id })
                false
            ))
        )
        (match current-activity
            existing-activity
            (let (
                    (new-vote-count (+ (get total-votes-cast existing-activity) u1))
                    (response-time (- vote-block (get start-block proposal)))
                    (new-avg-response (/ (+ (* (get avg-response-time existing-activity)
                                                (- new-vote-count u1)
                                            )
                                            response-time
                                        )
                                        new-vote-count
                                    ))
                    (engagement-boost (if (< response-time u100) u5 u1))
                    (new-engagement (+ (get engagement-score existing-activity) engagement-boost))
                )
                (map-set VoterActivityAnalytics { voter: voter }
                    (merge existing-activity {
                        last-vote-block: vote-block,
                        total-votes-cast: new-vote-count,
                        avg-response-time: new-avg-response,
                        engagement-score: new-engagement,
                    })
                )
            )
            ;; Initialize voter activity
            (map-set VoterActivityAnalytics { voter: voter } {
                first-vote-block: vote-block,
                last-vote-block: vote-block,
                total-votes-cast: u1,
                categories-engaged: u1,
                participation-streak: u1,
                avg-response-time: (- vote-block (get start-block proposal)),
                engagement-score: u10,
            })
        )
        true
    )
)

;; Public read-only functions for accessing governance analytics
(define-read-only (get-governance-summary)
    (let (
            (current-block burn-block-height)
            (total-proposals (var-get proposal-counter))
            (total-voters (var-get total-registered-voters))
            (recent-period-start (if (> current-block u1000) (- current-block u1000) u0))
        )
        (ok {
            total-proposals: total-proposals,
            total-registered-voters: total-voters,
            governance-health-score: (calculate-governance-health
                (if (> total-voters u0)
                    (/ (* total-proposals u100) total-voters)
                    u0
                )
                total-proposals
                total-voters
            ),
            analytics-version: analytics-version,
            last-updated: (var-get last-analytics-update),
            summary-generated-at: current-block,
        })
    )
)

(define-read-only (get-proposal-analytics (proposal-id uint))
    (let ((metrics (map-get? GovernanceMetrics { proposal-id: proposal-id })))
        (match metrics
            found-metrics (ok found-metrics)
            (err err-analytics-not-found)
        )
    )
)

(define-read-only (get-voting-statistics (period-start uint) (period-end uint))
    (if (and (> period-end period-start) (<= period-end burn-block-height))
        (let (
                (period-length (- period-end period-start))
                (estimated-proposals (/ period-length u144)) ;; Rough estimate based on block time
            )
            (ok {
                period-start: period-start,
                period-end: period-end,
                estimated-activity: estimated-proposals,
                data-quality: (if (> period-length u1000) "high" "low"),
                generated-at: burn-block-height,
            })
        )
        (err err-invalid-time-period)
    )
)

(define-read-only (get-voter-activity-report (voter principal))
    (let ((activity (map-get? VoterActivityAnalytics { voter: voter })))
        (match activity
            found-activity (ok {
                voter: voter,
                has-activity: true,
                total-votes: (get total-votes-cast found-activity),
                engagement-score: (get engagement-score found-activity),
                engagement-level: (if (>= (get engagement-score found-activity) u50)
                    "high"
                    (if (>= (get engagement-score found-activity) u20)
                        "medium"
                        "low"
                    )
                ),
                generated-at: burn-block-height,
            })
            (ok {
                voter: voter,
                has-activity: false,
                total-votes: u0,
                engagement-score: u0,
                engagement-level: "none",
                generated-at: burn-block-height,
            })
        )
    )
)

(define-read-only (get-category-performance-analytics (category (string-ascii 20)))
    (let (
            (category-requirements (map-get? CategoryRequirements { category: category }))
        )
        (match category-requirements
            found-req (ok {
                category: category,
                requirements: found-req,
                performance-data: {
                    min-participation-met: true, ;; Simplified for this implementation
                    avg-approval-rate: u55, ;; Placeholder - would calculate from actual data
                    trend: "stable",
                },
                last-analyzed: burn-block-height,
            })
            (err err-invalid-vote)
        )
    )
)

(define-read-only (analyze-voting-trends (trend-type (string-ascii 20)))
    (let ((trends (map-get? VotingTrends { trend-type: trend-type, identifier: "global" })))
        (match trends
            found-trends (ok {
                trend-type: trend-type,
                has-data: true,
                sample-size: (get sample-size found-trends),
                positive-votes: (get positive-votes found-trends),
                negative-votes: (get negative-votes found-trends),
                insights: {
                    dominant-pattern: (if (> (get positive-votes found-trends) (get negative-votes found-trends))
                        "positive-leaning"
                        "negative-leaning"
                    ),
                    engagement-quality: (if (> (get sample-size found-trends) u20) "sufficient" "limited"),
                    reliability: (get confidence-level found-trends),
                },
                generated-at: burn-block-height,
            })
            (ok {
                trend-type: trend-type,
                has-data: false,
                sample-size: u0,
                positive-votes: u0,
                negative-votes: u0,
                insights: {
                    dominant-pattern: "insufficient-data",
                    engagement-quality: "insufficient",
                    reliability: u0,
                },
                generated-at: burn-block-height,
            })
        )
    )
)

(define-read-only (get-governance-health-metrics)
    (let (
            (total-proposals (var-get proposal-counter))
            (total-voters (var-get total-registered-voters))
            (current-block burn-block-height)
        )
        (ok {
            overall-health: (calculate-governance-health
                (if (> total-voters u0)
                    (/ (* total-proposals u100) total-voters)
                    u0
                )
                total-proposals
                total-voters
            ),
            participation-metrics: {
                registered-voters: total-voters,
                active-proposals: total-proposals,
                engagement-ratio: (if (> total-voters u0)
                    (/ (* total-proposals u100) total-voters)
                    u0
                ),
            },
            health-indicators: {
                voter-growth: "stable", ;; Simplified
                proposal-activity: "moderate", ;; Simplified
                community-engagement: "healthy", ;; Simplified
            },
            generated-at: current-block,
            next-assessment: (+ current-block u1000),
        })
    )
)

;; Advanced analytics functions
(define-read-only (get-participation-trends (blocks-back uint))
    (let (
            (current-block burn-block-height)
            (analysis-start (if (> current-block blocks-back) (- current-block blocks-back) u0))
        )
        (if (<= blocks-back max-historical-periods)
            (ok {
                analysis-period: {
                    start-block: analysis-start,
                    end-block: current-block,
                    blocks-analyzed: blocks-back,
                },
                trend-analysis: {
                    direction: "stable", ;; Simplified for implementation
                    strength: u50,
                    confidence: u75,
                },
                key-insights: {
                    peak-activity-period: "recent",
                    participation-consistency: "moderate",
                    seasonal-patterns: none,
                },
                generated-at: current-block,
            })
            (err err-invalid-time-period)
        )
    )
)
