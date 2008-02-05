
#lang scheme/base
(require (for-syntax scheme/base)
         (for-syntax scheme/private/struct-info)
         scheme/list
         scheme/match
         "deriv.ss")

(provide make

         Wrap
         
         ok-node?
         interrupted-node?

         wderiv-e1
         wderiv-e2
         wlderiv-es1
         wlderiv-es2
         wbderiv-es1
         wbderiv-es2

         wderivlist-es2)

;; Wrap matcher
;; Matches unwrapped, interrupted wrapped, or error wrapped
(define-match-expander Wrap
  (lambda (stx)
    (syntax-case stx ()
      [(Wrap S (var ...))
       (syntax/loc stx (struct S (var ...)))])))

;; ----

(define (check sym pred type x)
  (unless (pred x)
    (raise-type-error sym type x)))

(define (ok-node? x)
  (check 'ok-node? node? "node" x)
  (and (node-z1 x) #t))
(define (interrupted-node? x)
  (check 'interrupted-node? node? "node" x)
  (not (node-z2 x)))


(define (wderiv-e1 x)
  (check 'wderiv-e1 deriv? "deriv" x)
  (node-z1 x))
(define (wderiv-e2 x)
  (check 'wderiv-e2 deriv? "deriv" x)
  (node-z2 x))

(define (wlderiv-es1 x)
  (check 'wlderiv-es1 lderiv? "lderiv" x)
  (node-z1 x))
(define (wlderiv-es2 x)
  (check 'wlderiv-es2 lderiv? "lderiv" x)
  (node-z2 x))

(define (wbderiv-es1 x)
  (check 'wbderiv-es1 bderiv? "bderiv" x)
  (node-z1 x))
(define (wbderiv-es2 x)
  (check 'wbderiv-es2 bderiv? "bderiv" x))

;; wderivlist-es2 : (list-of WDeriv) -> (list-of Stx)/#f
(define (wderivlist-es2 xs)
  (let ([es2 (map wderiv-e2 xs)])
    (and (andmap syntax? es2) es2)))

;; ----

(define-syntax (make stx)
  (syntax-case stx ()
    [(make S expr ...)
     (unless (identifier? #'S)
       (raise-syntax-error #f "not an identifier" stx #'S))
     (let ()
       (define (no-info) (raise-syntax-error #f "not a struct" stx #'S))
       (define info
         (extract-struct-info
          (syntax-local-value #'S no-info)))
       (define constructor (list-ref info 1))
       (define accessors (list-ref info 3))
       (unless (identifier? #'constructor)
         (raise-syntax-error #f "constructor not available for struct" stx #'S))
       (unless (andmap identifier? accessors)
         (raise-syntax-error #f "incomplete info for struct type" stx #'S))
       (let ([num-slots (length accessors)]
             [num-provided (length (syntax->list #'(expr ...)))])
         (unless (= num-provided num-slots)
           (raise-syntax-error
            #f
            (format "wrong number of arguments for struct ~s (expected ~s)"
                    (syntax-e #'S)
                    num-slots)
            stx)))
       (with-syntax ([constructor constructor])
         #'(constructor expr ...)))]))
