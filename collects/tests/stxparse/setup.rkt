#lang racket/base
(require rackunit
         syntax/parse
         syntax/parse/private/rep-attrs
         (only-in syntax/parse/private/runtime attribute-binding)
         (for-syntax racket/base))

(provide tok
         terx
         terx*
         tcerr

         bound
         s=
         a=
         convert-syntax-error)

#|
Testing forms
-------------

(tok stx-template pattern [#:pre pre-pattern ...] [#:post post-pattern ...])
-- pattern should succeed parsing stx (pre and post patterns should fail)

(terx stx-template pattern ErrorPattern ...)
(terx* stx-template (pattern ...) ErrorPattern ...)
  where ErrorPattern is regexp | (not regexp)
-- pattern should fail with exn message matching every ErrorPattern

(tcerr tc-name-expr expr ErrorPattern ...)
-- delays syntax errors in expr until runtime, error msg must every pattern


Auxiliaries
-----------

(bound (name depth [syntax?]) ...)
-- checks that name is an attr w/ proper depth and syntax?

(s= stx-template sexpr)
-- checks that stx-template produces stx w/ datum equivalent to sexpr

(a= attr expr)
-- checks that attr has value equal to expr

|#

;; tok = test pattern ok
(define-syntax tok
  (syntax-rules ()
    [(tok s p expr #:pre [pre-p ...] #:post [post-p ...])
     (test-case (format "line ~s: ~s match ~s"
                        (syntax-line (quote-syntax s))
                        's 'p)
       (syntax-parse (quote-syntax s)
         [pre-p (error 'wrong-pattern "~s" 'pre-p)] ...
         [p expr]
         [post-p (error 'wrong-pattern "~s" 'post-p)] ...)
       (void))]
    [(tok s p expr)
     (tok s p expr #:pre () #:post ())]
    [(tok s p)
     (tok s p 'ok)]))

(define-syntax-rule (bound b ...)
  (begin (bound1 b) ...))

(define-syntax bound1
  (syntax-rules ()
    [(bound1 (name depth))
     (let ([a (attribute-binding name)])
       (check-pred attr? a)
       (when (attr? a)
         (check-equal? (attr-depth a) 'depth)))]
    [(bound1 (name depth syntax?))
     (let ([a (attribute-binding name)])
       (check-pred attr? a)
       (when (attr? a)
         (check-equal? (attr-depth a) 'depth)
         (check-equal? (attr-syntax? a) 'syntax?)))]))

(define-syntax-rule (s= t v)
  (check-equal? (syntax->datum #'t) v))

(define-syntax-rule (a= a v)
  (check-equal? (attribute a) v))

(define-syntax-rule (terx s p rx ...)
  (terx* s [p] rx ...))

(define-syntax terx*
  (syntax-rules ()
    [(terx s [p ...] rx ...)
     (test-case (format "line ~s: ~a match ~s for error"
                        (syntax-line (quote-syntax s))
                        's '(p ...))
       (check-exn (lambda (exn)
                    (erx rx (exn-message exn)) ... #t)
                  (lambda ()
                    (syntax-parse (quote-syntax s)
                      [p 'ok] ...)))
       (void))]))

(define-syntax erx
  (syntax-rules (not)
    [(erx (not rx) msg)
     (check (compose not regexp-match?) rx msg)]
    [(erx rx msg)
     (check regexp-match? rx msg)]))

;; ====

(define-syntax-rule (tcerr name expr rx ...)
  (test-case name
    (check-exn (lambda (exn)
                 (define msg (exn-message exn))
                 (erx rx msg) ...
                 #t)
               (lambda ()
                 (parameterize ((error-print-source-location #f))
                   (convert-syntax-error expr))))
    (void)))

(define-syntax (convert-syntax-error stx)
  (syntax-case stx ()
    [(_ expr)
     (with-handlers ([exn:fail:syntax?
                      (lambda (e)
                        #`(error '#,(exn-message e)))])
       (parameterize ((error-print-source-location #f))
         (local-expand #'expr 'expression null)))]))
