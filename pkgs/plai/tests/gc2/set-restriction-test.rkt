#lang racket
(require racket/runtime-path)
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ exp reg)
     (with-syntax ([line (syntax-line stx)])
       #'(test/proc line 'exp reg))]))

(define-runtime-path here ".")

(define ns (make-base-namespace))
(define tests 0)
(define failed 0)
(define (test/proc line exp reg)
  (set! tests (+ tests 1))
  (define err
    (with-handlers (((λ (x)
                       (and (exn:fail:syntax? x)
                            reg
                            (let ([m (exn-message x)])
                              (regexp-match? reg m))))
                     exn-message))
      (parameterize ([current-namespace ns]
                     [current-directory here])
        (expand 
         `(,#'module m plai/gc2/mutator
                     (allocator-setup "good-collectors/trivial-collector.rkt" 200)
                     (define x '(1))
                     ,exp))
        #f)))
  (unless (or (and reg err)
              (and (not reg) (not err)))
    (set! failed (+ failed 1))
    (eprintf "test on line ~a failed:\n  ~s\n  expected ~a, got ~a\n"
             line
             exp
             (if reg (format "a syntax error matching ~a" reg) "no error")
             (or err "no error"))))
  
  
(test (+ (set! x 2) 1) #rx"set!")
(test (cons (set! x 3) empty) #rx"set!")
(test (set! x 2) #f)
(test (set! x (set! x 2)) #rx"set!")
(test (begin (begin (set! x 2) 1) 2) #f)
(test (λ () (set! x 1)) #rx"set!")
(test (λ () (set! x 1) 2) #f)
(test (let ([y (begin 1 (set! x 2))]) 1) #rx"set!")
(test (let ([y 2]) (begin 1 (set! x 2))) #f)
(test (+ (set-first! x 2) 3) #rx"set-first!")
(test (begin (set-first! x 2) 3) #f)
(test (+ (set-rest! x 2) 3) #rx"set-rest!")
(test (begin (set-rest! x 2) 3) #f)
(test set-first! #rx"set-first!")
(test set-rest! #rx"set-rest!")
(test (if (set! x 1) 2 3) #rx"set!")
(test (if 1 (set! x 2) (set! x 3)) #f)

(cond
  [(zero? failed)
   (printf "passed ~a tests\n" tests)]
  [else
   (eprintf "failed ~a test~a\n" failed (if (= 1 failed) "" "s"))])
