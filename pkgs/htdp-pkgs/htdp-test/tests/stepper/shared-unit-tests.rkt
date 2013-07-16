#lang racket

(require rackunit
         stepper/private/shared
         stepper/private/syntax-property)
  
; test cases taken from shared.rkt
;
(define (lifted-name sym) 
  (syntax->datum (get-lifted-var sym)))
(define cd-stx 
  (datum->syntax #f 'cd))

(check-equal? (lifted-name (datum->syntax #f 'ab)) 'lifter-ab-0)
(check-equal? (lifted-name cd-stx) 'lifter-cd-1)
(check-equal? (lifted-name (datum->syntax #f 'ef)) 'lifter-ef-2)
(check-equal? (lifted-name cd-stx) 'lifter-cd-1)

(check-equal? (map syntax-e (arglist->ilist #'(a b c))) '(a b c))
(check-equal? (map syntax-e (arglist->ilist #'(a . (b c)))) '(a b c))
(check-equal? (syntax-e (arglist->ilist #'a)) 'a)
(let ([result (arglist->ilist #' (a b . c))])
  (check-equal? (syntax-e (car result)) 'a)
  (check-equal? (syntax-e (cadr result)) 'b)
  (check-equal? (syntax-e (cddr result)) 'c))
(check-equal? (map syntax-e (arglist-flatten #'(a b c))) '(a b c))
(check-equal? (map syntax-e (arglist-flatten #'(a . (b c)))) '(a b c))
(check-equal? (map syntax-e (arglist-flatten #'(a b . c))) '(a b c))
(check-equal? (map syntax-e (arglist-flatten #'a)) '(a))

(define new-queue (make-queue))
(check-equal? (queue-push new-queue 1) (void))
(check-equal? (queue-push new-queue 2) (void))
(check-equal? (queue-pop new-queue) 1)
(check-equal? (queue-push new-queue 3) (void))
(check-equal? (queue-pop new-queue) 2)
(check-equal? (queue-pop new-queue) 3)
(check-exn exn:fail? (lambda () (queue-pop new-queue)))

(check-equal?
 (call-with-values (lambda ()
                     (values-map (lambda (a b) (values (+ a b) (- a b)))
                                 `(1 2 3 4 5)
                                 `(9 8 7 6 5)))
                   (lambda (sums diffs)
                     (list sums diffs)))
 `((10 10 10 10 10)
   (-8 -6 -4 -2 0)))

(check-exn exn:fail? (lambda () (stepper-syntax-property #`13 'boozle)))
(check-exn exn:fail? (lambda () (stepper-syntax-property #`13 'boozle #t)))
(check-equal? (stepper-syntax-property #`13 'stepper-hint) #f)
(check-equal? (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes)
                                       'stepper-hint) 'yes)
(check-equal? 
 (stepper-syntax-property (stepper-syntax-property (stepper-syntax-property #`13 
                                                                            'stepper-hint
                                                                            'no)
                                                   'stepper-hint 'yes)
                          'stepper-hint)
 'yes)
(check-equal? (stepper-syntax-property (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes) 'stepper-black-box-expr 'arg) 'stepper-hint) 'yes)
(check-equal? (syntax->datum (stepper-syntax-property (stepper-syntax-property #`13 'stepper-hint 'yes) 'stepper-black-box-expr 'arg)) 13)
