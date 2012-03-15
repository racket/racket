#lang racket/base
(require racket/pretty)
(provide write-debug-file
         load-debug-file
         serialize-datum)

(define (write-debug-file file exn events)
  (with-output-to-file file
    (lambda ()
      (pretty-write (serialize-datum events))
      (newline)
      (write (exn-message exn))
      (newline)
      (pretty-print
       (map serialize-context-frame
            (continuation-mark-set->context
             (exn-continuation-marks exn)))))
    #:exists 'replace))

(define (quoted? x) (and (pair? x) (eq? (car x) 'quote)))

(define (serialize-datum d)
  (list 'quasiquote (serialize-datum* d)))

(define (serialize-datum* d)
  (define (UNQUOTE x) (list 'unquote x))
  (cond [(number? d) d]
        [(boolean? d) d]
        [(symbol? d)
         (case d
           ((unquote) (UNQUOTE '(quote unquote)))
           ((unquote-splicing) (UNQUOTE '(quote unquote-splicing)))
           (else d))]
        [(string? d) d]
        [(bytes? d) d]
        [(null? d) d]
        [(pair? d)
         (cons (serialize-datum* (car d)) (serialize-datum* (cdr d)))]
        [(exn? d) (UNQUOTE `(make-exn ,(exn-message d) (current-continuation-marks)))]
        [(syntax? d) (UNQUOTE `(datum->syntax #f ',(syntax->datum d)))]
        [(module-path-index? d)
         (define-values (path rel)
           (module-path-index-split d))
         (UNQUOTE `(module-path-index-join
                    ,(serialize-datum path)
                    ,(serialize-datum rel)))]
        [(resolved-module-path? d)
         (UNQUOTE `(make-resolved-module-path
                    ,(serialize-datum
                      (resolved-module-path-name d))))]
        [(path? d)
         (UNQUOTE `(bytes->path
                    ,(serialize-datum (path->bytes d))
                    ,(serialize-datum (path-convention-type d))))]
        [else
         (eprintf "unserializable value: ~e" d)
         `(UNSERIALIZABLE ,(format "~s" d))]))

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
