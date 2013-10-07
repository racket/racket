#lang scheme
(require rackunit
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
     #`(test-case (format "line ~s: ~a match ~s for error"
                          '#,(syntax-line #'s)
                          's '(p ...))
         (let ([exn (let/ec escape
                      (check-exn (lambda (exn)
                                   (escape exn))
                                 (lambda ()
                                   (syntax-parse (quote-syntax s)
                                     [p (void)] ...))))])
           (let ([msg (exn-message exn)]
                 [stxs (and (exn:fail:syntax? exn)
                            (exn:fail:syntax-exprs exn))])
             (when 'term
               (check-equal? (and (pair? stxs) (syntax->datum (car stxs))) 'term))
             (erx rx (exn-message exn)) ... #t))
         (void))]))

(define-syntax erx
  (syntax-rules (not)
    [(erx (not rx) msg)
     (check (compose not regexp-match?) rx msg)]
    [(erx rx msg)
     (check regexp-match? rx msg)]))

;; ----

(terx (a b c 7) (x:id ...)
      #:term 7
      #rx"expected identifier")

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

;; Ellipses

(terx (a b c 4)
      (x:id ...)
      #rx"expected identifier")

;; Repetition constraints

(terx (1 2)
      ((~or (~once x:id #:name "identifier") n:nat) ...)
      #rx"missing required occurrence of identifier")

(terx (1 a 2 b)
      ((~or (~once x:id #:name "identifier") n:nat) ...)
      #rx"too many occurrences of identifier")

;; Roles

(terx 1
      (~var x id #:role "var")
      #rx"expected identifier for var")
(terx 1
      (~describe #:opaque #:role "R" "D" (_))
      #rx"expected D for R")
(terx 1
      (~describe #:role "R" "D" (_))
      #rx"expected D for R")

(test-case "#:describe #:role"
  (check-exn #rx"expected identifier for var"
             (lambda ()
               (syntax-parse #'1
                 [x
                  #:declare x id #:role "var"
                  'ok]))))

(test-case "role coalescing"
  (check-exn #rx"^m: expected identifier for thing$" ;; not repeated
             (lambda ()
               (syntax-parse #'(m 0 b)
                 [(_ x y:nat)
                  #:declare x id #:role "thing"
                  'a]
                 [(_ x y:id)
                  #:declare x id #:role "thing"
                  'b]))))

;; Expected more terms

(terx (1)
      (a b)
      #rx"expected more terms starting with any term$")

(terx (1)
      (a b:id)
      #rx"expected more terms starting with identifier$")

(terx (1)
      (a (~describe "thing" b))
      #rx"expected more terms starting with thing$")
