(load-relative "loadtest.rktl")
(Section 'places)
(require "benchmarks/places/place-utils.rkt")

(place-wait (place/base (p1 ch)
  (printf "Hello from place\n")))

(let ([p (place/base (p1 ch)
          (printf "Hello form place 2\n"))])
  (test #f place? 1)
  (test #f place? void)
  (test #t place? p)

  (err/rt-test (place-wait 1))
  (err/rt-test (place-wait void))
  (test 0 place-wait p)
)

(arity-test place 2 2)
(arity-test place-wait 1 1)
(arity-test place-channel 0 0)
(arity-test place-channel-put 2 2)
(arity-test place-channel-get 1 1)
(arity-test place-channel? 1 1)
(arity-test place? 1 1)
(arity-test place-channel-put/get 2 2)
(arity-test processor-count 0 0)

(err/rt-test (dynamic-place "foo.rkt"))
(err/rt-test (dynamic-place null 10))
(err/rt-test (dynamic-place "foo.rkt" 10))
        
(let ([p (place/base (p1 ch)
          (printf "Hello form place 2\n")
          (sync never-evt))])
  (place-kill p))

(report-errs)
