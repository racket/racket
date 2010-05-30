#lang racket

(require rackunit rackunit/text-ui unstable/class "helpers.rkt")

(run-tests
 (test-suite "class.ss"

   (test-suite "Predicates and Contracts"

     (test-suite "class-or-interface/c"
       (test (check-ok (with/c class-or-interface/c object%)))
       (test (check-ok (with/c class-or-interface/c (interface ()))))
       (test (check-bad (with/c class-or-interface/c (new object%)))))

     (test-suite "object-provides/c"
       (test-ok (with/c (object-provides/c) (new object%)))
       (test-ok (define c% (class object% (super-new)))
                (with/c (object-provides/c c%) (new c%)))
       (test-ok (define i<%> (interface ()))
                (define c% (class* object% (i<%>) (super-new)))
                (with/c (object-provides/c i<%>) (new c%)))
       (test-bad (define c% (class object% (super-new)))
                 (with/c (object-provides/c c%) (new object%)))
       (test-bad (define i<%> (interface ()))
                 (with/c (object-provides/c i<%>) (new object%)))
       (test-bad (with/c (object-provides/c) object%)))

     (test-suite "class-provides/c"
       (test-ok (with/c (class-provides/c) object%))
       (test-ok (define c% (class object% (super-new)))
                (with/c (class-provides/c c%) c%))
       (test-ok (define c% (class object% (super-new)))
                (with/c (class-provides/c object%) c%))
       (test-ok (define i<%> (interface ()))
                (define c% (class* object% (i<%>) (super-new)))
                (with/c (class-provides/c i<%>) c%))
       (test-bad (define c% (class object% (super-new)))
                 (with/c (class-provides/c c%) object%))
       (test-bad (define i<%> (interface ()))
                 (with/c (class-provides/c i<%>) object%)))

     (test-suite "mixin-provides/c"
       (test-ok ((with/c (mixin-provides/c [] []) values) object%))
       (test-bad (define i<%> (interface ()))
                 ((with/c (mixin-provides/c [i<%>] []) values) object%))
       (test-bad (define i<%> (interface ()))
                 ((with/c (mixin-provides/c [i<%>] []) values) object%))))

   (test-suite "Mixins"

     (test-suite "ensure-interface"
       (test-case "implementation unchanged"
         (let* ([i<%> (interface ())]
                [c% (class* object% (i<%>) (super-new))]
                [mx (lambda (parent%) (class* parent% (i<%>) (super-new)))])
           (check-eq? (ensure-interface i<%> mx c%) c%)))
       (test-case "non-implementation subclassed"
         (let* ([i<%> (interface ())]
                [c% (class object% (super-new))]
                [mx (lambda (parent%) (class* parent% (i<%>) (super-new)))]
                [result (ensure-interface i<%> mx c%)])
           (check-pred class? result)
           (check subclass? result c%)
           (check implementation? result i<%>)))))

   (test-suite "Messages"

     (test-suite "send+"
       (test-case "no messages"
         (let* ([o (new object%)])
           (check-eq? (send+ o) o)))
       (test-case "multiple messages"
         (let* ([c% (class object%
                      (super-new)
                      (init-field count)
                      (define/public (add n) (set! count (+ count n)))
                      (define/public (get) count))]
                [o (new c% [count 0])])
           (check-eq? (send+ o [add 1] [add 2]) o)
           (check = (send o get) 3))))

     (test-suite "send-each"
       (test-case "counter"
         (let* ([c% (class object%
                      (super-new)
                      (init-field count)
                      (define/public (add n) (set! count (+ count n)))
                      (define/public (get) count))]
                [o1 (new c% [count 1])]
                [o2 (new c% [count 2])]
                [o3 (new c% [count 3])])
           (send-each (list o1 o2 o3) add 3)
           (check-equal? (list (send o1 get) (send o2 get) (send o3 get))
                         (list 4 5 6))))))))
