
(load-relative "loadtest.rktl")

(Section 'generator)

(require racket/generator
         "for-util.rkt")

(test #f generator? 5)
(test #f generator? void)
(test #f generator? error)

(test-sequence [(0 1 2)] (in-generator (yield 0) (yield 1) (yield 2)))
(let ([g (lambda () (in-generator (yield 0) (yield 1) (yield 2)))])
  (test-sequence [(0 1 2)] (g)))
(test '((1 0) (2 1) (3 2)) 'indexed-generator
      (for/list ([(x i) (in-indexed (in-generator (yield 1) (yield 2) (yield 3)))])
        (list x i)))

;; test multiple values for in-generator
(test '[(1 2) (3 4)] 'for*-generator
      (for*/list ([(n after)
              (in-generator
                (yield 1 2)
                (yield 3 4))])
            (list n after)))

;; test 0-ary yields
(test '(0 1 2) 'no-bind-in-generator
   (for/list ([() (in-generator (yield) (yield) (yield))]
              [i (in-naturals)])
     i))

(let ([helper (lambda (i)
                (yield (add1 i)))])
  (test '(1 2 3) 'parameterized-yield
        (for/list ([x (in-generator (helper 0) (helper 1) (helper 2))])
                  x)))

(let ([g (lambda () (generator () (yield 1) (yield 2) (yield 3)))])
  (let ([g (g)]) (test '(1 2 3) list (g) (g) (g)))
  (let ([g (g)]) (test '(1 2 3 10 10) list (g) (g) (g) (g 10) (g)))
  (let ([g (generator () (yield (yield (yield 1))))])
    (test '(1 2 3 4 4 4) list (g) (g 2) (g 3) (g 4) (g) (g)))
  (let ([g (g)])
    (test '(fresh 1 suspended 2 suspended 3 suspended last done)
          list (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g 'last)
               (generator-state g)))
  (letrec ([g (generator () (yield (generator-state g))
                            (yield (generator-state g)))])
    (test '(fresh running suspended running suspended last done)
          list (generator-state g) (g)
               (generator-state g) (g)
               (generator-state g) (g 'last)
               (generator-state g))))

(let* ([helper (lambda (pred num)
                 (for ([i (in-range 0 3)]) (yield (pred (+ i num)))))]
       [g1 (generator () (helper odd? 1) (yield 'odd))]
       [g2 (generator () (helper even? 1) (yield 'even))])
  (test '(#t #f #f #t #t #f odd even) 'yield-helper
        (list (g1) (g2) (g1) (g2) (g1) (g2) (g1) (g2))))

(test '(1 2 3)
      'sequence->generator-1
      (let ([maker (sequence->generator '(1 2 3))])
        (list (maker) (maker) (maker))))

(test '(1 2 3)
      'sequence->generator-2
      (let ([maker (sequence->generator (in-list '(1 2 3)))])
        (list (maker) (maker) (maker))))

(test '(0 1 2 3 4)
      'sequence->generator-3
      (let ([maker (sequence->generator (in-range 0 5))])
        (list (maker) (maker) (maker) (maker) (maker))))

(test '(0 1 2 3 4)
      'sequence->generator-4
      (let ([maker (sequence->generator (in-naturals))])
        (list (maker) (maker) (maker) (maker) (maker))))

(test '(1 2 3 1 2 3)
      'sequence->repeated-generator
      (let ([maker (sequence->repeated-generator '(1 2 3))])
        (list (maker) (maker) (maker)
              (maker) (maker) (maker))))

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

(let ()
  ;; Yield 1 value, using `in-generator' returned from a function:
  (define (in-gen-1)
    (in-generator #:arity 1
                  (for ([i (in-range 4)])
                    (yield i))))
  (test '(0 1 2 3)
        'gen-1
        (for/list ([x (in-gen-1)])
          x)))

(let ()
  ;; Yield 2 values, using `in-generator' returned from a function:
  (define (in-gen-2)
    (in-generator #:arity 2
                  (for ([i (in-range 4)])
                    (yield i 0))))
  (test '((0 . 0) (1 . 0) (2 . 0) (3 . 0))
        'gen-2
        (for/list ([(x y) (in-gen-2)])
          (cons x y))))

;; Make sure that `for/list` doesn't tigger quadtradic-time behavior
;; from `in-producer`, based on test constructed by @kalbr
(let ()
  (define (make-real-generator N)
    (generator
     ()
     (for ([i (in-range N)])
       (yield i))))

  (define (time-it N)
    (let ([start (current-process-milliseconds)])
      (let ([len (length (for/list ([x (in-producer (make-real-generator N) void?)])
                           x))])
        (if (zero? len)
            (error "that's not right")
            (- (current-process-milliseconds) start)))))

  (let loop ([tries 3])
    (when ((time-it 40000) . > . (* 3 (time-it 20000)))
      (if (zero? tries)
          (error "doubling an `in-producer` sequence seems to take more than twice as long")
        (loop (sub1 tries))))))
  
(report-errs)
