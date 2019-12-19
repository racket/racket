#lang racket/base

(provide (all-defined-out))

(define cm-logger (make-logger 'compiler/cm (current-logger)))
(define (default-manager-trace-handler str)
  (when (log-level? cm-logger 'debug)
    (log-message cm-logger 'debug str (current-inexact-milliseconds))))

(struct compile-event (timestamp path action) #:prefab)
(define (log-compile-event path action)
  (when (log-level? cm-logger 'info 'compiler/cm)
    (log-message cm-logger 'info (format "~a~a: ~a" (get-indent-string) action path)
                 (compile-event (current-inexact-milliseconds) path action))))

(define manager-compile-notify-handler (make-parameter void))
(define manager-trace-handler (make-parameter default-manager-trace-handler))
(define indent (make-parameter 0))

(define managed-compiled-context-key (gensym))
(define (make-compilation-context-error-display-handler orig)
  (lambda (str exn)
    (define l (continuation-mark-set->list
               (exn-continuation-marks exn)
               managed-compiled-context-key))
    (orig (if (null? l)
              str
              (apply
               string-append
               str
               "\n  compilation context...:"
               (for/list ([i (in-list l)])
                 (format "\n   ~a" i))))
          exn)))

(define (trace-printf fmt . args)
  (let ([t (manager-trace-handler)])
    (unless (or (eq? t void)
                (and (equal? t default-manager-trace-handler)
                     (not (log-level? cm-logger 'debug))))
      (t (string-append (get-indent-string)
                        (apply format fmt args))))))

(define (get-indent-string)
  (build-string (indent)
                (λ (x)
                  (if (and (= 2 (modulo x 3))
                           (not (= x (- (indent) 1))))
                      #\|
                      #\space))))
