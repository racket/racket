

(load-relative "loadtest.rktl")

(Section 'letrec)

;; =============================================================================

(define letrec-exn? exn:fail:contract:variable?)

(test 5 'const (letrec ([x 5]) 5))
(test 5 'var-ref (letrec ([x 5]) x))

(test #t 'lambda (procedure? (letrec ([x (lambda () 5)]) x)))
(test #t 'lambda-rest-args (procedure? (letrec ([x (lambda l l)]) x)))
(test 5 'lambda-called (letrec ([x (lambda () 5)]) (x)))
(test (list 5) 'lambda-rest-args-called (letrec ([x (lambda l l)] [y (x 5)]) y))

(err/rt-test (letrec ([x x]) x) letrec-exn?)
(err/rt-test (letrec ([x y] [y 5]) y) letrec-exn?)

(test #t 'begin
      (procedure?
       (letrec ([x (begin (lambda () y)
                          (lambda () z))]
                [y 5]
                [z 6])
         x)))

(err/rt-test 
 (letrec ([x ((lambda l z) 5)] [z 5]) x)
 letrec-exn?)

(test 5 'nested (letrec ([x (letrec ([y 6]) 5)]) x))
(test 5 'nested-more (letrec ([x (letrec ([y 5]) y)]) x))
(test 5 'nested-even-more (letrec ([x 5] [y (letrec ([z x]) z)]) y))

(err/rt-test
 (letrec ([q (lambda () s)] [r (q)] [s 5]) r)
 letrec-exn?)

(test 5 'reordered (letrec ([s 5] [q (lambda () s)] [r (q)]) r))

(test 5 'set! (letrec ([x 0]
                       [f (lambda (v) (set! x v))])
                (begin (f 5) x)))

(err/rt-test
 (letrec ([f (lambda () (letrec ([x x]) 5))]
          [g (f)])
   g)
 letrec-exn?)

(test #t 'checked-but-ok
      (procedure?
       (letrec ([y (lambda () (letrec ([x x]) x))])
         y)))

(err/rt-test
 (letrec ([a (letrec ([y (lambda () x)]) (lambda () (y)))] [x (a)]) x)
 letrec-exn?)

(err/rt-test
 (letrec ([a (letrec ([x (lambda () (y))] [y (lambda () a)]) (x))]) a)
 letrec-exn?)

(err/rt-test
 (letrec ([w (lambda () r)]
          [z (lambda () 5)]
          [x (set! z w)]
          [y (set! x (z))]
          [r 5])
   x)
 letrec-exn?)

(err/rt-test
 (letrec-values ([(x y) (values (lambda () z) (lambda () z))] [(z) (y)]) z)
 letrec-exn?)

(test #t 'checked-but-ok
      (procedure?
       (let ([proc (letrec ([x (letrec ([v (lambda () x)]) v)]) x)])
         (proc))))

(err/rt-test
 (letrec ([x (letrec ([v (lambda () y)]) v)]
          [y (x)])
   y)
 letrec-exn?)

(err/rt-test
 (letrec ([a 1]
          [b (set! a (lambda () c))]
          [c (a)])
   c)
 letrec-exn?)

(err/rt-test
 (letrec ([b (let ([d (lambda () c)])
               (d))]
          [c 1])
   b)
 letrec-exn?)

(err/rt-test
 (letrec ([b (let-values ([(a) 5]
                          [(e d) (values 1 (lambda () c))])
               (d))]
          [c 1])
   b)
 letrec-exn?)

(err/rt-test
 (letrec ([b (let-values ([(e d) (values 1 (lambda () c))]
                          [(a) 5])
               (d))]
          [c 1])
   b)
 letrec-exn?)

(err/rt-test
 (letrec ([b (let ([e (lambda ()
                        (let ([d (lambda () c)])
                          (d)))])
               (e))]
          [c 1])
   b)
 letrec-exn?)

(err/rt-test
 (letrec ([b (with-continuation-mark
                 'x
                 (lambda () c)
               ((continuation-mark-set-first
                 (current-continuation-marks)
                 'x)))]
          [c 1])
   c)
 letrec-exn?)

(err/rt-test
 (letrec ([b (with-continuation-mark
                 'x
                 (lambda () c)
               (+ (random)
                  ((continuation-mark-set-first
                    (current-continuation-marks)
                    'x))))]
          [c 1])
   c)
 letrec-exn?)

(test '(1)
      'complex-forcing-path
      (let-values (((_tri)
                    (letrec-values (((all-types) 1))
                      (lambda (x) all-types))))
        (letrec-values (((quad-super-type) _tri)
                        ((offsets) (map quad-super-type (list 1))))
          offsets)))

(err/rt-test
 (let ()
   (define (wrap x) x)
   (letrec ([f (wrap (lambda () (g)))]
            [g (let ([g2 (lambda () (letrec ([x x]) x))])
                 g2)])
     (f)))
 letrec-exn?)

;; Make sure a useless `set!` isn't discarded early
(err/rt-test
 (letrec ((B (begin (set! B B) 1))) 1)
 letrec-exn?)

(err/rt-test
 (list (begin
         (letrec ((x (set! x x))) 0)
         0))
 letrec-exn?)

(err/rt-test
 (letrec ((x (set! x (/ 0)))) 'ok)
 exn:fail:contract:divide-by-zero?)

(err/rt-test
 (letrec ((x (set! x (values 1 0)))) 'ok)
 exn:fail:contract:arity?)

(err/rt-test
 (let ([indirect (lambda (f) (f))])
   (letrec ((x (indirect (lambda () (set! x (+ 1 0)) x))))
     'ok))
 letrec-exn?)

(test 8
      'ok
      (let ([save #f])
        (let ([indirect (lambda (f) (set! save f))]
              [also-indirect (lambda () 8)])
          (letrec ((x (indirect (lambda () (set! x (also-indirect)) x))))
            'ok)
          (save))))

(report-errs)
