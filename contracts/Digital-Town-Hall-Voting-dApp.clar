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
