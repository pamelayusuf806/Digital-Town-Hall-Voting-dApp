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
