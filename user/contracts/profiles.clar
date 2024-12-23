;; User Profile Smart Contract
;; Implements user profile management with robust error handling and security features

;; Contract Owner
(define-data-var contract-owner principal tx-sender)

;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-USER-EXISTS (err u101))
(define-constant ERR-USER-NOT-FOUND (err u102))
(define-constant ERR-INVALID-USERNAME (err u103))
(define-constant ERR-INVALID-BIO-LENGTH (err u104))
(define-constant ERR-INVALID-OPERATION (err u105))
(define-constant ERR-INVALID-ADDRESS (err u106))

;; Data maps
(define-map user-profiles
    principal
    {
        username: (string-utf8 50),
        bio: (string-utf8 280),
        avatar-url: (optional (string-utf8 200)),
        created-at: uint,
        last-updated: uint,
        followers: uint,
        following: uint,
        is-verified: bool
    }
)

(define-map following-data
    { follower: principal, following: principal }
    bool
)

;; Read-only functions

(define-read-only (get-contract-owner)
    (ok (var-get contract-owner))
)

(define-read-only (get-user-profile (user principal))
    (match (map-get? user-profiles user)
        profile (ok profile)
        ERR-USER-NOT-FOUND
    )
)

(define-read-only (is-following (follower principal) (user principal))
    (default-to false
        (map-get? following-data { follower: follower, following: user })
    )
)

(define-read-only (get-follower-count (user principal))
    (match (map-get? user-profiles user)
        profile (ok (get followers profile))
        ERR-USER-NOT-FOUND
    )
)

;; Private functions

(define-private (is-contract-owner)
    (is-eq tx-sender (var-get contract-owner))
)

(define-private (is-valid-principal (address principal))
    (not (is-eq address tx-sender))  ;; Basic check to ensure address is not current sender
)

;; Public functions

(define-public (set-contract-owner (new-owner principal))
    (begin
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (not (is-eq new-owner tx-sender)) ERR-INVALID-ADDRESS)
        (asserts! (not (is-eq new-owner (var-get contract-owner))) ERR-INVALID-OPERATION)
        (ok (var-set contract-owner new-owner))
    )
)

(define-public (create-profile (username (string-utf8 50)) (bio (string-utf8 280)))
    (let (
        (caller tx-sender)
        (exists (map-get? user-profiles caller))
    )
        (asserts! (is-none exists) ERR-USER-EXISTS)
        (asserts! (> (len username) u0) ERR-INVALID-USERNAME)
        (asserts! (<= (len bio) u280) ERR-INVALID-BIO-LENGTH)
        
        (ok (map-set user-profiles caller {
            username: username,
            bio: bio,
            avatar-url: none,
            created-at: block-height,
            last-updated: block-height,
            followers: u0,
            following: u0,
            is-verified: false
        }))
    )
)

(define-public (update-profile (username (string-utf8 50)) (bio (string-utf8 280)) (avatar-url (optional (string-utf8 200))))
    (let (
        (caller tx-sender)
        (existing-profile (map-get? user-profiles caller))
    )
        (asserts! (is-some existing-profile) ERR-USER-NOT-FOUND)
        (asserts! (> (len username) u0) ERR-INVALID-USERNAME)
        (asserts! (<= (len bio) u280) ERR-INVALID-BIO-LENGTH)
        
        (ok (map-set user-profiles caller (merge (unwrap! existing-profile ERR-USER-NOT-FOUND) {
            username: username,
            bio: bio,
            avatar-url: avatar-url,
            last-updated: block-height
        })))
    )
)

(define-public (follow-user (user principal))
    (let (
        (caller tx-sender)
        (follower-profile (map-get? user-profiles caller))
        (following-profile (map-get? user-profiles user))
    )
        (asserts! (not (is-eq caller user)) ERR-INVALID-OPERATION)
        (asserts! (and (is-some follower-profile) (is-some following-profile)) ERR-USER-NOT-FOUND)
        (asserts! (not (is-following caller user)) ERR-USER-EXISTS)
        
        (map-set following-data { follower: caller, following: user } true)
        
        ;; Update follower counts
        (map-set user-profiles caller (merge (unwrap! follower-profile ERR-USER-NOT-FOUND) {
            following: (+ (get following (unwrap! follower-profile ERR-USER-NOT-FOUND)) u1)
        }))
        (map-set user-profiles user (merge (unwrap! following-profile ERR-USER-NOT-FOUND) {
            followers: (+ (get followers (unwrap! following-profile ERR-USER-NOT-FOUND)) u1)
        }))
        
        (ok true)
    )
)

(define-public (unfollow-user (user principal))
    (let (
        (caller tx-sender)
        (follower-profile (map-get? user-profiles caller))
        (following-profile (map-get? user-profiles user))
    )
        (asserts! (and (is-some follower-profile) (is-some following-profile)) ERR-USER-NOT-FOUND)
        (asserts! (is-following caller user) ERR-USER-NOT-FOUND)
        
        (map-delete following-data { follower: caller, following: user })
        
        ;; Update follower counts
        (map-set user-profiles caller (merge (unwrap! follower-profile ERR-USER-NOT-FOUND) {
            following: (- (get following (unwrap! follower-profile ERR-USER-NOT-FOUND)) u1)
        }))
        (map-set user-profiles user (merge (unwrap! following-profile ERR-USER-NOT-FOUND) {
            followers: (- (get followers (unwrap! following-profile ERR-USER-NOT-FOUND)) u1)
        }))
        
        (ok true)
    )
)

;; Admin functions

(define-public (verify-user (user principal))
    (let (
        (existing-profile (map-get? user-profiles user))
    )
        (asserts! (is-contract-owner) ERR-NOT-AUTHORIZED)
        (asserts! (is-some existing-profile) ERR-USER-NOT-FOUND)
        
        (match existing-profile
            profile (ok (map-set user-profiles user (merge profile {
                is-verified: true,
                last-updated: block-height
            })))
            ERR-USER-NOT-FOUND
        )
    )
)