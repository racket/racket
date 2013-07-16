#lang at-exp racket
(require rackunit)

#|

This file has tests for the mutator transformation to make
sure that the right things are in the result of get-root-set.

It works by setting up a collector (one that doesn't actually collect)
that prints out all of the flat values in the root set at the point
when a cons happens. 

Then it sets up various little expressions (in the calls to 'run-one')
that check the root set contents and the arguments to cons.

The roots are printed only if they are flat values and the values 
themselves are printed, sorted with duplicates removed. (Also the code 
crashes if there are non-real-number flat values
because the root values are sorted before printing.) So this means
that the test cases have to be set up somewhat carefully.

|#

(define ns (make-base-namespace))
(parameterize ([current-namespace ns]
               [current-module-declare-name (make-resolved-module-path 'gc)])
  (eval
   (parameterize ([read-accept-reader #t])
     (read-syntax
      'stuff
      (open-input-string
       @string-append{#lang plai/gc2/collector
      (define heap-ptr 'uninitialized-heap-ptr)
      (define (init-allocator) (set! heap-ptr 0))

      (define (gc:closure code vs)
        (define len (length vs))
        (when (> (+ heap-ptr len) (heap-size))
          (error "out of memory"))
        (heap-set! heap-ptr 'closure)
        (heap-set! (+ 1 heap-ptr) code)
        (for ([v (in-list vs)]
              [i (in-naturals 1)])
          (heap-set! (+ 1 i heap-ptr) (read-root v)))
        (set! heap-ptr (+ len 2 heap-ptr))
        ;; return the location of this flat data
        (- heap-ptr len 2))

      (define (gc:closure-code-ptr a) (heap-ref (+ a 1)))
      (define (gc:closure-env-ref a i) (heap-ref (+ a 1 1 i)))
      (define (gc:closure? a) (eq? (heap-ref a) 'closure))
      
      (define (gc:alloc-flat p)
        (begin 
          (when (> (+ heap-ptr 2) (heap-size))
            (error "out of memory"))
          (heap-set! heap-ptr 'prim)
          (heap-set! (+ 1 heap-ptr) p)
          (set! heap-ptr (+ 2 heap-ptr))
          ; return the location of this flat data
          (- heap-ptr 2)))

      (define (gc:cons f r)
        (begin
          (when (> (+ heap-ptr 3) (heap-size))
            (error "out of memory"))
          (define (get-prim x) (heap-ref (+ (read-root x) 1)))
          (define prim-roots
            (for/list ([x (in-list (get-root-set))]
                       #:when (eq? 'prim (heap-ref (read-root x))))
              (get-prim x)))
          (printf "~s\n" (append (cons 'roots (remove-duplicates (sort prim-roots <)))
                                 (list 'hd (get-prim f) 'tl (get-prim r))))
          (heap-set! heap-ptr 'cons)
          (heap-set! (+ 1 heap-ptr) (read-root f))
          (heap-set! (+ 2 heap-ptr) (read-root r))
          (set! heap-ptr (+ 3 heap-ptr))
          (- heap-ptr 3)))

      (define (gc:deref a) (heap-ref (+ 1 a)))
      (define (gc:cons? a) (eq? (heap-ref a) 'cons))
      (define (gc:first a) (heap-ref (+ 1 a)))
      (define (gc:rest a) (heap-ref (+ 2 a)))
      (define (gc:set-first! a f) 
        (if (gc:cons? a)
            (heap-set! (+ 1 a) f)
            (error 'set-first! "expects address of cons")))
      
      (define (gc:set-rest! a r) (heap-set! (+ 2 a) r))
      (define (gc:flat? a) (eq? 'prim (heap-ref a)))})))))

;; each call to 'run-one' must have a unique name;
;; these names are used as module names. 
;; the result is a list with one element for each
;; call to cons, listing the roots (as discussed above)
(define (run-one name . strings)
  (parameterize ([current-namespace ns]
                 [current-module-declare-name 
                  (make-resolved-module-path name)])
    (eval
     (parameterize ([read-accept-reader #t])
       (read-syntax
        'stuff
        (open-input-string (apply string-append strings)))))
    (define sp (open-output-string))
    (parameterize ([current-output-port sp])
      (namespace-require `',name))
    (define ip (open-input-string (get-output-string sp)))
    (let loop ()
      (define l (read-line (peeking-input-port ip)))
      (cond
        [(eof-object? l) '()]
        [(regexp-match #rx"^[(]roots" l)
         (cons (read ip) (loop))]
        [else
         ;; skip over lines that don't look like the roots printouts
         (read-line ip)
         (loop)]))))

(check-equal?
 @run-one['non-tail-cons]{#lang plai/gc2/mutator
                          (allocator-setup 'gc 200)
                          (first (cons 1 2))}
 '((roots hd 1 tl 2)))

(check-equal?
 @run-one['tail-cons]{#lang plai/gc2/mutator
                      (allocator-setup 'gc 200)
                      (define (f x) (cons 1 2))
                      (f 3)}
 '((roots 3 hd 1 tl 2)))

(check-equal?
 @run-one['tail-cons-with-unused-var]{#lang plai/gc2/mutator
                                      (allocator-setup 'gc 200)
                                      (define (f x) (let ([y 2]) (cons 3 4)))
                                      (f 1)}
 '((roots 1 hd 3 tl 4)))

(check-equal?
 @run-one['cons-with-used-var]{#lang plai/gc2/mutator
                               (allocator-setup 'gc 200)
                               (define (f x) (let ([y 2]) 
                                               (let ([z (cons 3 4)])
                                                 y)))
                               (f 1)}
 '((roots 1 2 hd 3 tl 4)))


(check-equal?
 @run-one['cons-with-unused-var]{#lang plai/gc2/mutator
                                 (allocator-setup 'gc 200)
                                 (define (f x) (let ([y 2]) 
                                                 (let ([z (cons 3 4)])
                                                   x)))
                               (f 1)}
 '((roots 1 hd 3 tl 4)))


(check-equal?
 @run-one['let-values]{#lang plai/gc2/mutator
                                 (allocator-setup 'gc 200)
                                 (define (f x) (let-values ([(y) 2]
                                                            [(z) (cons 3 4)])
                                                   x))
                                 (f 1)}
 '((roots 1 hd 3 tl 4)))

(check-equal?
 @run-one['let-values2]{#lang plai/gc2/mutator
                        (allocator-setup 'gc 200)
                        (define (f x) (let-values ([(y) 2]
                                                   [(z) (cons 3 4)])
                                        y))
                        (f 1)}
 '((roots 1 2 hd 3 tl 4)))

(check-equal?
 @run-one['fn-args]{#lang plai/gc2/mutator
                    (allocator-setup 'gc 200)
                    (define (f x) (let ([z (cons 1 2)]) x))
                    (define (g y) (f 3))
                    (g 4)}
 '((roots 3 4 hd 1 tl 2)))

(check-equal?
 @run-one['fn-args2]{#lang plai/gc2/mutator
                    (allocator-setup 'gc 200)
                    (define (f x) (let ([z (cons 1 2)]) z))
                    (define (g y) (f 3))
                    (g 4)}
 '((roots 4 hd 1 tl 2)))

(check-equal?
 @run-one['fn-args3]{#lang plai/gc2/mutator
                    (allocator-setup 'gc 200)
                    (define (f x) (cons 1 2))
                    (define (g y) (f 3))
                    (g 4)}
 '((roots 4 hd 1 tl 2)))
