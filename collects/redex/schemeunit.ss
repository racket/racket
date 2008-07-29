#lang scheme

(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         (except-in "reduction-semantics.ss" check))

(provide test-reduces 
         check-reduces 
         test-reduces/multiple
         check-reduces/multiple)

(define-shortcut (test-reduces reds from to) (check-reduces reds from to))

(define-check (check-reduces reds from to)
  (let ([all (apply-reduction-relation* reds from)])
    (cond
      [(null? (cdr all))
       (unless (equal? (car all) to)
         (with-check-info
          (('expected to)
           ('actual (car all)))
          (fail-check)))]
      [else
       (with-check-info
        (('multiple-results all))
        (fail-check))])))

(define-shortcut (test-reduces/multiple reds from to) (check-reduces/multiple reds from to))

(define-check (check-reduces/multiple reds from to)
  (let ([all (apply-reduction-relation* reds from)])
    (unless (set-equal? all to)
      (with-check-info
       (('expecteds to)
        ('actuals all))
       (fail-check)))))

(define (set-equal? s1 s2)
  (define (subset? a b)
    (let ([ht (make-hash)])
      (for-each (λ (x) (hash-set! ht x #t)) a)
      (andmap (λ (x) (hash-ref ht x #f)) b)))
  (and (subset? s1 s2)
       (subset? s2 s1)))