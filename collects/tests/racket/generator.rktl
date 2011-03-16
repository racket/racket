
(load-relative "loadtest.rktl")

(Section 'generator)

(require racket/generator)

(test #f generator? 5)
(test #f generator? void)
(test #f generator? error)

(let ([g (generator () 
                    (test 'next 'yield (yield 0))
                    (yield 1)
                    (test 'q 'yield (yield 2 3))
                    3)])
  (test #t generator? g)
  (test 'fresh generator-state g)
  (test 0 g)
  (test 1 g 'next)
  (test 'suspended generator-state g)
  (test-values (list 2 3) (lambda () (g 'x 'y 'z)))
  (test 'suspended generator-state g)
  (test 3 g 'q)
  (test 'done generator-state g)
  (test 3 g)
  (test 3 g)
  (err/rt-test (g 1)))

(let ([g (infinite-generator (yield 11))])
  (test 11 g)
  (test 11 g)
  (test 11 g)
  (test 11 g))

(for ([gi (in-generator (yield 1)  (yield 2)  (yield 3))]
      [i (in-naturals)])
  (test i sub1 gi))

(let ([g (sequence->generator (in-range 3))])
  (test 0 g)
  (test 1 g)
  (test 2 g)
  (test (void) g)
  (test (void) g))

(let ([g (sequence->repeated-generator (in-range 3))])
  (test 0 g)
  (test 1 g)
  (test 2 g)
  (test 0 g)
  (test 1 g)
  (test 2 g)
  (test 0 g))

(let ([g (generator () (values 1 2))])
  (test-values '(1 2) g)
  (test-values '(1 2) g))

(report-errs)

