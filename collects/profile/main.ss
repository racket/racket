#lang scheme/base

(require "sampler.ss" (except-in "analyzer.ss" profile)
         (prefix-in text: "render-text.ss")
         (for-syntax scheme/base))

(define (profile-thunk thunk
                       #:delay   [delay 0.05]
                       #:repeat  [rpt 1]
                       #:threads [threads? #f]
                       #:render  [renderer text:render])
  (define cust (and threads? (make-custodian (current-custodian))))
  (define sampler (create-sampler (if threads?
                                    (list cust (current-thread))
                                    (current-thread))
                                  delay))
  (define (run) (for ([i (in-range rpt)]) (thunk)))
  (with-handlers ([void (lambda (e)
                          (fprintf (current-error-port)
                                   "profiled thunk error: ~a\n"
                                   (if (exn? e)
                                     (exn-message e)
                                     (format "~e" e))))])
    (if threads?
      (parameterize ([current-custodian cust]) (run))
      (run)))
  (sampler 'stop)
  (renderer (analyze-samples (sampler 'get-snapshots))))

(define-syntax (profile stx)
  (syntax-case stx ()
    [(_ x ...)
     (let loop ([expr #f] [kwds '()] [xs (syntax->list #'(x ...))])
       (cond
         [(null? xs)
          (if expr
            (with-syntax ([expr expr] [kwds (reverse kwds)])
              #'(profile-thunk (lambda () expr) . kwds))
            (raise-syntax-error 'profile "missing expression" stx))]
         [(keyword? (syntax-e (car xs)))
          (if (pair? (cdr xs))
            (loop expr (list* (cadr xs) (car xs) kwds) (cddr xs))
            ;; let #%app throw the error
            (loop expr (cons (car xs) kwds) (cdr xs)))]
         [expr (raise-syntax-error 'profile "redundant expresion given"
                                   stx (car xs))]
         [else (loop (car xs) kwds (cdr xs))]))]))


(define (fib1 n) (if (<= n 1) n (+ (fib1 (- n 1)) (fib1 (- n 2)))))
(define (fib22 n) (if (<= n 2) 1 (+ (fib22 (- n 1)) (fib22 (- n 2)))))
(define (fib3 n)
  (for ([i (in-range 100000000)]) (* i 3))
  (if (<= n 2) 1 (+ (fib22 (- n 1)) (fib22 (- n 2)))))
(define (fibs n) (+ (fib1 n) (fib22 n) (fib3 n)))

(define (foo n)
  (define ch (make-channel))
  (define (bg-fib) (channel-put ch (fib1 n)))
  (thread bg-fib)
  (list (fib22 n) (channel-get ch)))

(require "render-graphviz.ss")

(profile (fibs 35)
         ;(dynamic-require '(lib "scribblings/reference/reference.scrbl") #f)
         ;(foo 35)
         ;#:render render
         #:threads #t)
