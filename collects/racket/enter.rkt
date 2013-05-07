#lang racket/base

(require "rerequire.rkt"
         racket/list
         (for-syntax racket/base))

(provide enter!)

(define-syntax (enter! stx)
  (syntax-protect
   (syntax-case stx ()
     [(enter! mod flag ...) (andmap keyword? (syntax->datum #'(flag ...)))
      #'(do-enter! 'mod '(flag ...))]
     [_ (raise-syntax-error
         #f "bad syntax; should be `(enter! <module-path-or-#f> [flag...])'"
         stx)])))

(define orig-namespace (current-namespace))

(define-logger enter!)

(define (check-flags flags)
  ;; check that all flags are known, that at most one of the noise flags is
  ;; present, and add #:verbose-reload if none are (could be done at the macro
  ;; level, but this is intended for interactive use anyway)
  (let loop ([flags (remove-duplicates flags eq?)] [noise #f])
    (cond [(null? flags)
           (if noise '() '(#:verbose-reload))]
          [(eq? (car flags) '#:dont-re-require-enter)
           (cons (car flags) (loop (cdr flags) noise))]
          [(not (memq (car flags) '(#:verbose #:quiet #:verbose-reload)))
           (error 'enter! "unknown flag: ~e" (car flags))]
          [noise (error 'enter! "contradicting noise flags: ~e and ~e"
                        noise (car flags))]
          [else (cons (car flags) (loop (cdr flags) (car flags)))])))

(define (do-enter! mod flags)
  (let ([flags (check-flags flags)])
    (if mod
      (let* ([none (gensym)]
             [exn (with-handlers ([void (lambda (exn) exn)])
                    (if (module-path? mod)
                        (dynamic-rerequire
                         mod
                         #:verbosity
                         (cond
                          [(memq '#:verbose flags) 'all]
                          [(memq '#:verbose-reload flags) 'reload]
                          [else 'none]))
                        (raise-argument-error 'enter! "module-path?" mod))
                    none)])
        ;; Try to switch to the module namespace,
        ;; even if there was an exception, because the
        ;; idea is to allow debugging from inside the
        ;; module. If any excepiton happens in trying to
        ;; switch to the declared module, log that as
        ;; an internal exception.
        (with-handlers ([void (lambda (exn)
                                (if (exn? exn)
                                    (log-enter!-error (exn-message exn))
                                    (log-enter!-error "~s" exn)))])
          (when (and (module-path? mod)
                     (module-declared? mod #f))
            (let ([ns (module->namespace mod)])
              (current-namespace ns)
              (unless (memq '#:dont-re-require-enter flags)
                (namespace-require 'racket/enter)))))
        (unless (eq? none exn) (raise exn)))
      (current-namespace orig-namespace))))
