#lang scheme/base
(require scheme/contract)

(define path-element?
  (or/c path-string? (symbols 'up 'same)))
;; Eli: We already have a notion of "path element" which is different
;;   from this (see `string->path-element') .

(define port-number? (between/c 1 65535))

(define non-empty-string/c
  (and/c string?
         (lambda (s) (not (zero? (string-length s))))))
;; Eli: If this gets in, there should also be versions for bytes, lists, and
;;   vectors.

;; ryanc added:

;; (if/c predicate then/c else/c) applies then/c to satisfying
;;   predicate, else/c to those that don't.
(define (if/c predicate then/c else/c)
  #|
  Naive version:
    (or/c (and/c predicate then/c)
          (and/c (not/c predicate) else/c))
  But that applies predicate twice.
  |#
  (let ([then-ctc (coerce-contract 'if/c then/c)]
        [else-ctc (coerce-contract 'if/c else/c)])
    (define name (build-compound-type-name 'if/c predicate then-ctc else-ctc))
    ;; Special case: if both flat contracts, make a flat contract.
    (if (and (flat-contract? then-ctc)
             (flat-contract? else-ctc))
        ;; flat contract
        (let ([then-pred (flat-contract-predicate then-ctc)]
              [else-pred (flat-contract-predicate else-ctc)])
          (define (pred x)
            (if (predicate x) (then-pred x) (else-pred x)))
          (flat-named-contract name pred))
        ;; ho contract
        (let ([then-proj ((proj-get then-ctc) then-ctc)]
              [then-fo ((first-order-get then-ctc) then-ctc)]
              [else-proj ((proj-get else-ctc) else-ctc)]
              [else-fo ((first-order-get else-ctc) else-ctc)])
          (define ((proj pos neg srcinfo name pos?) x)
            (if (predicate x)
                ((then-proj pos neg srcinfo name pos?) x)
                ((else-proj pos neg srcinfo name pos?) x)))
          (make-proj-contract
           name
           proj
           (lambda (x) (if (predicate x) (then-fo x) (else-fo x))))))))

(provide/contract
 [non-empty-string/c contract?]
 [path-element? contract?]
 [port-number? contract?]
 [if/c (-> procedure? contract? contract? contract?)])
