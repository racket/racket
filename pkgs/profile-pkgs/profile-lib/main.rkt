#lang racket/base

(provide profile-thunk profile)

(require "sampler.rkt" (except-in "analyzer.rkt" profile)
         (prefix-in text: "render-text.rkt")
         (for-syntax racket/base))

(define (profile-thunk thunk
                       #:delay   [delay 0.05]
                       #:repeat  [rpt 1]
                       #:threads [threads? #f]
                       #:render  [renderer text:render]
                       #:periodic-renderer [periodic-renderer #f]
                       #:use-errortrace? [et? #f])
  (define cust (and threads? (make-custodian (current-custodian))))
  (define sampler (create-sampler (if threads?
                                    (list cust (current-thread))
                                    (current-thread))
                                  delay
                                  #:use-errortrace? et?))
  (define periodic-thread
    (and periodic-renderer
         (let ([delay (car periodic-renderer)]
               [renderer (cadr periodic-renderer)])
           (define (loop)
             (sleep delay)
             (renderer (analyze-samples (sampler 'get-snapshots)))
             (loop))
           (thread loop))))
  (define (run) (for ([i (in-range rpt)]) (thunk)))
  (with-handlers ([void (λ (e) (eprintf "profiled thunk error: ~a\n"
                                        (if (exn? e)
                                          (exn-message e)
                                          (format "~e" e))))])
    (if threads?
      (parameterize ([current-custodian cust]) (run))
      (run)))
  (when periodic-thread (kill-thread periodic-thread))
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
              #'(profile-thunk (λ () expr) . kwds))
            (raise-syntax-error 'profile "missing expression" stx))]
         [(keyword? (syntax-e (car xs)))
          (if (pair? (cdr xs))
            (loop expr (list* (cadr xs) (car xs) kwds) (cddr xs))
            ;; let #%app throw the error
            (loop expr (cons (car xs) kwds) (cdr xs)))]
         [expr (raise-syntax-error 'profile "redundant expresion given"
                                   stx (car xs))]
         [else (loop (car xs) kwds (cdr xs))]))]))

#|

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
  (list (fibs n) (channel-get ch)))

(require "render-graphviz.rkt")

(profile ;(fibs 40)
         ;(dynamic-require '(lib "scribblings/reference/reference.scrbl") #f)
         (foo 40)
         ;#:render render
         #:threads #t
         #:periodic-renderer
         (list 0.5 text:render)
         )
|#
