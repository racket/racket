#lang racket

(module+ test
  (main))

;; Runs 3 places perfoming the test suite simultaneously. Each
;;  thread creates a directory sub<n> to run in, so that filesystem
;;  tests don't collide.

(define (go n test)
  (let ([dirname (path->complete-path (format "sub~s" n))])
    (when (directory-exists? dirname)
      (delete-directory/files dirname))
    (make-directory dirname)
    (current-directory dirname)
    (namespace-require '(lib "racket/init"))
    (load (build-path 'up test))
    (when (directory-exists? dirname)
      (delete-directory/files dirname))))

(define (main)
  (define ps
    (for/list ([i 3])
      (let ([p (place ch (go (place-channel-get ch)
                             (place-channel-get ch)))])
        (place-channel-put p i)
        (place-channel-put p "quiet.rktl")
        p)))
  (map place-wait ps))

