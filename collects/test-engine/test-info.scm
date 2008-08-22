#lang scheme/base

(require scheme/class)

(provide (all-defined-out))

;; (make-failed-check src (listof (U string snip%)) (U #f exn))
(define-struct failed-check (src msg exn?))

(define test-info-base%
  (class* object% ()
    (super-instantiate ())

    (init-field (style 'check-base))
    (field [analyses null])

    (define total-tsts 0)
    (define failed-tsts 0)
    (define total-cks 0)
    (define failed-cks 0)

    (define failures null)

    (define/public (test-style) style)
    (define/public (tests-run) total-tsts)
    (define/public (tests-failed) failed-tsts)
    (define/public (checks-run) total-cks)
    (define/public (checks-failed) failed-cks)
    (define/public (summarize-results)
      (cond [(and (zero? total-tsts) (zero? total-cks)) 'no-tests]
            [(and (zero? failed-cks) (zero? failed-tsts)) 'all-passed]
            [else 'mixed-results]))

    (define/public (failed-checks) failures)

    (define/pubment (add-check)
      (set! total-cks (add1 total-cks))
      (inner (void) add-check))

    (define/pubment (add-test)
      (set! total-tsts (add1 total-tsts))
      (inner (void) add-test))

    ;; check-failed: (list (U string snip%)) src (U exn false) -> void
    (define/pubment (check-failed msg src exn?)
      (set! failed-cks (add1 failed-cks))
      (set! failures (cons (make-failed-check src msg exn?) failures))
      (inner (void) check-failed msg src exn?))

    (define/pubment (test-failed failed-info)
      (set! failed-tsts (add1 failed-tsts))
      (inner (void) test-failed failed-info))

    (define/public (add-analysis a) (set! analyses (cons a analyses)))

    (define/public (analyze-position src . vals)
      (for ([a analyses]) (send a analyze src vals)))
    (define/public (extract-info pred?)
      (filter pred? (map (lambda (a) (send a provide-info)) analyses)))))
