#lang racket/base

(require (for-syntax racket/base)
         racket/match)

;;;;;;;;;;
;;
;;  paul graham's [ _ ] macro
;;
;;;;;;;;;;

(provide lx)

(define-syntax (lx stx)
  (syntax-case stx ()
    [(lx term)
     (with-syntax ([binder (datum->syntax (syntax term) `_)])
       (syntax (lambda (binder) term)))]))
  


;;;;;;;;;;
;;
;;  ccond implementation
;; 
;;;;;;;;;;

(provide ccond)

(define-syntax (ccond stx)
  (syntax-case stx ()
    [(_ (question answer) ...)
     (syntax
      (cond
        (question answer) ...
        (else (error 'ccond "fell off end of cond expression"))))]))



;;;;;;;;;;
;;
;;  2vals implementation
;; 
;;;;;;;;;;


;; honestly, match-let* supersedes all of this, if I ever have time to redo it...

(provide 2vals-map apply-to-first-of-2vals)

(define (apply-to-first-of-2vals proc 2vals)
  (vector (proc (vector-ref 2vals 0))
          (vector-ref 2vals 1)))

; 2vals-map : (('a -> (2vals 'b 'c)) ('a list)) -> (2vals ('b list) ('c list))
;  dual-map is like map, only for a procedure that returns (values a b), and its
;  result is (values a-list b-list)... the contract specifies this more clearly.

(define (2vals-map f . lsts)
  (if (null? (car lsts))
      (vector null null)
      (match-let* ([(vector a b) (apply f (map car lsts))]
                   [(vector a-rest b-rest) (apply 2vals-map f (map cdr lsts))])
        (vector (cons a a-rest) (cons b b-rest)))))

; test cases
; (require my-macros)
;
;(= (2vals-first (2vals 3 4)) 3)
;(= (2vals-second (2vals 3 4)) 4)
;
;(= 
; (let*-2vals
;     ([a (2vals 3 4)]
;      [(b c) a])
;   a
;   c)
; 4)
;
;(make-contract-checker my-type (lambda (x) (= x 3)))
;
;(contract-check-my-type? 3 'second-arg)
;;(contract-check-my-type? 14 'first-arg)
;
;((checked-lambda (x (y my-type) (z my-type))
;    (+ x y z))
; 3 3 5)
;
