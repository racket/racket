#lang racket/base
(require racket/pretty)
(provide write-debug-file
         load-debug-file)

(define (write-debug-file file exn events)
  (with-output-to-file file
    (lambda ()
      (pretty-print
       `(list ,@(map (lambda (e) (serialize-datum e)) events)))
      (newline)
      (write (exn-message exn))
      (newline)
      (pretty-print
       (map serialize-context-frame
            (continuation-mark-set->context
             (exn-continuation-marks exn)))))
    #:exists 'replace))

(define (serialize-datum d)
  (cond [(number? d) `(quote ,d)]
        [(boolean? d) `(quote ,d)]
        [(symbol? d) `(quote ,d)]
        [(string? d) `(quote ,d)]
        [(pair? d) `(cons ,(serialize-datum (car d)) ,(serialize-datum (cdr d)))]
        [(null? d) ''()]
        [(exn? d) `(make-exn ,(exn-message d) (current-continuation-marks))]
        [(syntax? d) `(datum->syntax #f ',(syntax->datum d))]
        #;[(syntax? d) `(eval (quote ,(compile `(,#'quote-syntax ,d))))]
        [else (error 'serialize-datum "got ~s" d)]))

(define (serialize-context-frame frame)
  (cons (car frame)
        (if (cdr frame)
            (serialize-srcloc (cdr frame))
            null)))

(define (serialize-srcloc s)
  (list (let ([src (srcloc-source s)])
          (cond [(path? src) (path->string src)]
                [(string? src) src]
                [else '?]))
        (srcloc-line s)
        (srcloc-column s)))

(define (load-debug-file file)
  (parameterize ((read-accept-compiled #t))
    (with-input-from-file file
      (lambda ()
        (let* ([events-expr (read)]
               [exnmsg (read)]
               [ctx (read)])
          (let ([events (eval events-expr)])
            (values events exnmsg ctx)))))))
