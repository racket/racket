#lang racket/base
(require "test-util.rkt"
         racket/list)

(define (test-errortrace-has-name #:name name
                                  #:target-name [target-name name]
                                  expr0 . exprs)
  (define all-exprs (append (list expr0) exprs))
  (define init-exprs (drop-right all-exprs 1))
  (define expr (last all-exprs))
  (define test-case-name (string->symbol (format "checking-arrow-src-locs.~a" name)))

  (for ([index (in-naturals)]
        [init-expr (in-list init-exprs)])
    (define sp (open-input-string (format "~s\n" init-expr)))
    (define init-stx (read-syntax (string->symbol (format "~a.~a" name index)) sp))
    (close-input-port sp)
    (contract-eval init-stx #:test-case-name test-case-name))

  (define sp (open-input-string (format "~s\n" expr)))
  (define stx (read-syntax name sp))
  (define exn
    (with-handlers ((exn:fail? values))
      (contract-eval stx)))
  (define sp2 (open-output-string))
  (parameterize ([current-error-port sp2])
    ((error-display-handler) (exn-message exn) exn))
  (define matches?
    (regexp-match? (pregexp
                    (string-append (regexp-quote (format "~a:" target-name))
                                   "[[:digit:]]*"
                                   ":"
                                   "[[:digit:]]*"
                                   ":"
                                   "[^\n]*"
                                   "->"))
                   (get-output-string sp2)))
  (unless matches?
    (display (get-output-string sp2)))
  (test #t 
        test-case-name
        matches?))

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace 'racket/contract)])
  ;; have to do this here so that errortrace is loaded
  ;; in the right context (cannot put 'errortrace into 
  ;; the argument to make-basic-contract-namespace)
  (parameterize ([current-namespace (current-contract-namespace)])
    (dynamic-require 'errortrace #f))

  (test-errortrace-has-name
   #:name 'whereitsat
   '(-> (λ (a b c) #f) any))

  (test-errortrace-has-name
   #:name 'whereitsat-star
   '(->* ((λ (a b c) #f)) any))

  (test-errortrace-has-name
   #:name 'whereitsat-mod
   #:target-name 'whereitsat-mod.0
   '(module anon-mod1 racket/base
      (require racket/contract)
      (-> (λ (a b c) #f) any))
   '(require 'anon-mod1))

  (test-errortrace-has-name
   #:name 'whereitsat-mod-star
   #:target-name 'whereitsat-mod-star.0
   '(module anon-mod2 racket/base
      (require racket/contract)
      (->* ((λ (a b c) #f)) any))
   '(require 'anon-mod2)))
