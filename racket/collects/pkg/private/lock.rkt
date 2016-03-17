#lang racket/base
(require racket/file
         racket/format
         "params.rkt"
         "print.rkt"
         "dirs.rkt"
         "path.rkt")

(provide pkg-lock-held
         with-pkg-lock
         with-pkg-lock/read-only
         ;; Checks that the lock is held:
         write-file-hash!
         ;; For migrate:
         call-with-separate-lock)

(define pkg-lock-held (make-parameter #f))
(define pkg-lock-scope (make-parameter #f))

;; Call `t' with lock held for the current scope. The intent is that
;; `t' reads and writes package information in the curent scope. It
;; may also *read* package information for wider package scopes
;; without a further lock --- which is questionable, but modification
;; of a shared scope while others are running can create trouble,
;; anyway.
(define (with-pkg-lock* read-only? t)
  (define mode (if read-only? 'shared 'exclusive))
  (define held-mode (pkg-lock-held))
  (define now-scope (current-pkg-scope))
  (define held-scope (pkg-lock-scope))
  (when (and held-scope
             (not (eq? held-scope now-scope)))
    (pkg-error "lock mismatch\n  held scope: ~a\n  requested scope: ~a"
               held-scope
               now-scope))
  (if (or (eq? mode held-mode)
          (eq? 'exclusive held-mode))
      (t)
      (let ([d (pkg-dir #f)])
        (unless read-only? (make-directory* d))
        (if (directory-exists? d)
            ;; If the directory exists, assume that a lock file is
            ;; available or creatable:
            (call-with-file-lock/timeout
             #f 
             mode
             (lambda ()
               (parameterize ([pkg-lock-held mode]
                              [pkg-lock-scope now-scope]
                              [current-no-pkg-db #f])
                 (t)))
             (λ () (pkg-error  (~a "could not acquire package lock\n"
                                   "  lock file: ~a")
                               (pkg-lock-file)))
             #:lock-file (pkg-lock-file))
            ;; Directory does not exist; we must be in read-only mode.
            ;; Run `t' under the claim that no database is available
            ;; (in case the database is created concurrently):
            (parameterize ([current-no-pkg-db now-scope])
              (parameterize ([pkg-lock-held mode])
                (t)))))))
(define-syntax-rule (with-pkg-lock e ...)
  (with-pkg-lock* #f (λ () e ...)))
(define-syntax-rule (with-pkg-lock/read-only e ...)
  (with-pkg-lock* #t (λ () e ...)))

;; Intended for use with `pkg-migrate`, which needs to
;; read a different version than it writes to:
(define (call-with-separate-lock f)
  (parameterize ([pkg-lock-held #f]
                 [pkg-lock-scope #f])
    (f)))

(define (write-file-hash! file new-db)
  (unless (eq? (pkg-lock-held) 'exclusive)
    (pkg-error "attempt to write package database without write lock"))
  (make-parent-directory* file)
  (call-with-atomic-output-file
   file
   (λ (o tmp-path) (write new-db o) (newline o))))
