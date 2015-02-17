#lang racket/load

(require "test-harness.rkt"
         racket/private/unit-runtime)

;; check-unit
(test-runtime-error exn:fail:contract?
                    "result of unit expression was not a unit"
  (check-unit 1 'check-unit))

(test (void)
  (check-unit (make-unit 1 2 3 4 5) 'check-unit))

;; check-helper
(define sub-vector
  #((a . #((t . r1) (t . r2) (t . r3)))
    (a . #((#f . r1) (#f . r2) (#f . r3)))))

(test (void)
  (check-helper sub-vector #() 'check-helper #f))

(test (void)
  (check-helper sub-vector sub-vector 'check-helper #f))

(test (void)
  (check-helper sub-vector
                #((d . #((t . r2) (t . r3))))
                'check-helper
                #f))

(test-runtime-error exn:fail:contract?
                    "expects a unit with an export for tag t with signature c, which the given unit does not supply"
  (check-helper sub-vector
                #((c . #((t . r4) (t . r1) (t . r2) (t . r3))))
                'check-helper
                #f))
(define sub-vector2
  #((a . #((t . r5) (t . r2) (t . r3)))
    (b . #((t . r1) (t . r2) (t . r3)))))

(test (void)
      (check-helper sub-vector2 sub-vector2 'check-helper #f))

(test (void)
      (check-helper sub-vector2 
                    #((a . #((t . r5) (t . r2) (t . r3))))
                    'check-helper #f))

(test-runtime-error exn:fail:contract?
                    "expects a unit with an export for tag t with signature c, which the given unit supplies multiple times"
      (check-helper sub-vector2 
                    #((c . #((t . r2) (t . r3))))
                    'check-helper #f))

;; check-deps
;;UNTESTED
