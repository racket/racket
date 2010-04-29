#lang scheme
(require rktunit
         syntax/parse)
(require (for-syntax syntax/parse))
(provide (all-defined-out))

;; Error selection tests

(error-print-source-location #f)

(define-syntax-rule (terx s p stuff ...)
  (terx* s [p] stuff ...))

(define-syntax terx*
  (syntax-parser
    [(terx s [p ...] (~optional (~seq #:term term) #:defaults ([term #'#f])) rx ...)
     #'(test-case (format "line ~s: ~a match ~s for error"
                          (syntax-line (quote-syntax s))
                          's '(p ...))
         (let ([exn (let/ec escape
                      (check-exn (lambda (exn)
                                   (escape exn))
                                 (lambda ()
                                   (syntax-parse (quote-syntax s)
                                     [p 'ok] ...))))])
           (let ([msg (exn-message exn)]
                 [stxs (and (exn:fail:syntax? exn)
                            (exn:fail:syntax-exprs exn))])
             (when 'term
               (check-equal? (and (pair? stxs) (syntax->datum (car stxs))) 'term))
             (erx rx (exn-message exn)) ... #t))
         'ok)]))

(define-syntax erx
  (syntax-rules (not)
    [(erx (not rx) msg)
     (check (compose not regexp-match?) rx msg)]
    [(erx rx msg)
     (check regexp-match? rx msg)]))

;; ----

(terx* (1 2) [x:nat (y:id z:id)]
       #:term 1
       #rx"expected identifier")

;; --

(define-syntax-class bindings
  (pattern ((var:id rhs:expr) ...)))

(terx* ((x 1 2)) [x:id bs:bindings]
       #:term 2
       #rx"unexpected term")

;; --

(terx ((a 1) (a 2))
      ((~or (~once ((~datum a) x) #:name "A clause")
            (~optional ((~datum b) y) #:name "B clause"))
       ...)
      ;; #:term (a 2)
      #rx"too many occurrences of A clause")

;; --

(define-syntax-class A
  (pattern ((~datum a) x)))
(define-syntax-class B
  (pattern ((~datum b) y)))

(terx ((a 1) (a 2))
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"too many occurrences of A clause")

(terx ((a 1 2) _)
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"unexpected term")

(terx ((b 1 2) _)
      ((~or (~once a:A #:name "A clause")
            (~optional b:B #:name "B clause"))
       ...)
      #rx"unexpected term")
