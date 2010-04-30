#lang racket/base
(require racket/contract)

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
        (let ([then-proj (contract-projection then-ctc)]
              [then-fo (contract-first-order then-ctc)]
              [else-proj (contract-projection else-ctc)]
              [else-fo (contract-first-order else-ctc)])
          (define ((proj blame) x)
            (if (predicate x)
                ((then-proj blame) x)
                ((else-proj blame) x)))
          (make-contract
           #:name name
           #:projection proj
           #:first-order
           (lambda (x) (if (predicate x) (then-fo x) (else-fo x))))))))

(define (rename-contract ctc name)
  (let ([ctc (coerce-contract 'rename-contract ctc)])
    (if (flat-contract? ctc)
        (flat-named-contract name (flat-contract-predicate ctc))
        (let* ([ctc-fo (contract-first-order ctc)]
               [proj (contract-projection ctc)])
          (make-contract #:name name
                           #:projection proj
                           #:first-order ctc-fo)))))

(provide/contract
 [non-empty-string/c contract?]
 [path-element? contract?]
 [port-number? contract?]
 [if/c (-> procedure? contract? contract? contract?)]
 [rename-contract (-> contract? any/c contract?)])
