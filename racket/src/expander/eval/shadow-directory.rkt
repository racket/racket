#lang racket/base
(require racket/private/place-local
         ffi/unsafe/atomic
         ;; Avoid keyword-argument variant:
         (only-in '#%kernel directory-list))

(provide directory-exists?/shadow-filesystem
         shadow-directory-place-init!)

;; On Windows, we can shadow the filesystem and stay in sync via a
;; filesystem-change evt, it's worth caching subpath results with
;; respect to collection-root directories. The number of relevant
;; directories in practice will be large enough to take a while to
;; check the filesystem. At the same time, the number is als small
;; enough to keep filesystem change events for each directory.

(struct shadow-directory (evt    ; filesystem-change event that determines whether `table` is valid
                          table) ; set of normalized path elements for directory content
  #:authentic)

(define use-shadow-directory?
  (let ([v (system-type 'fs-change)])
    (and (eq? 'scalable (vector-ref v 1))
         (eq? 'low-latency (vector-ref v 2)))))

(define (make-cache)
  (and use-shadow-directory?
       (make-weak-hash)))

;; map from paths to cached directory-existence information:
(define-place-local shadow-directory-cache (make-cache))

(define (shadow-directory-place-init!)
  (set! shadow-directory-cache (make-cache)))

(define (lookup-shadow-directory orig)
  (define sd (call-as-atomic (lambda () (hash-ref shadow-directory-cache orig #f))))
  (cond
    [sd
     (cond
       [(sync/timeout 0 (shadow-directory-evt sd))
        ;; Cached information is out of date, so reset it
        (call-as-atomic (lambda () (hash-remove! shadow-directory-cache orig)))
        (lookup-shadow-directory orig)]
       [else sd])]
    [else
     (define evt (filesystem-change-evt orig (lambda () #f)))
     (cond
       [evt
        (define table (for/hash ([p (in-list (directory-list orig))]
                                 #:when (directory-exists? (build-path orig p)))
                        (values (normal-case-path p) #t)))
        (define sd (shadow-directory evt table))
        (call-as-atomic (lambda () (hash-set! shadow-directory-cache orig sd)))
        sd]
       [else #f])]))

;; Check for `p`, which is `(build-path orig subpath)` and `subpath`
;; is a path element derived from a collection name (which avoids
;; various normalization issues, but still must be case-normalized)
(define (directory-exists?/shadow-filesystem p orig subpath)
  (cond
    [(not shadow-directory-cache)
     (directory-exists? p)]
    [else
     (define sd (lookup-shadow-directory orig))
     (cond
       [(not sd)
        ;; Something went wrong trying to get a filesystem-change event,
        ;; so fall back to `directory-exists?`
        (directory-exists? p)]
       [sd
        (define ht (shadow-directory-table sd))
        (hash-ref ht (normal-case-path subpath) #f)])]))
