;; Poor man's stack-trace-on-exceptions/profiler.
;; See docs for information.

(module calltrace-lib mzscheme
  (require "stacktrace.rkt"
           mzlib/list
           mzlib/etc
           mzlib/unit)


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Eval handler, exception handler

  (define instrumenting-enabled (make-parameter #t))

  (define output-port (current-error-port))
  
  ;; (union symbol #f) syntax-object (list-of value) boolean int -> void
  ;; effect: prints out the context surrounding the exception
  (define (print-call-trace inferred-name original? src args improper? depth)
    (build-list depth (lambda (n) (fprintf output-port " ")))
    (fprintf output-port "~v\n" (cons (or inferred-name src) 
                                      (if improper? 
                                          (list->improper-list args)
                                          args))))


  (define calltrace-eval-handler    
    (let ([orig (current-eval)]
          [ns (current-namespace)])
      (lambda (e)
        (if (and (eq? ns (current-namespace))
		 (not (compiled-expression? (if (syntax? e)
						(syntax-e e)
						e))))
            ;; Loop to flatten top-level `begin's:
            (let loop ([e (if (syntax? e)
			      e
			      (namespace-syntax-introduce
			       (datum->syntax-object #f e)))])
              (let ([top-e (expand-syntax-to-top-form e)])
                (syntax-case top-e (begin)
                  [(begin expr ...)
                   ;; Found a `begin', so expand/eval each contained
                   ;; expression one at a time
                   (foldl (lambda (e old-val)
                            (loop e))
                          (void)
                          (syntax->list #'(expr ...)))]
                  [_else
                   ;; Not `begin', so proceed with normal expand and eval
                   (let* ([ex (expand-syntax top-e)]
                          [a (if (not (instrumenting-enabled))
                                 ex
                                 (annotate ex))])
                     (orig a))])))
            (orig e)))))

  (define (list->improper-list a)
    (cond [(null? a) (error 'list->improper-list "list->improper-list called with null argument: ~e" a)]
          [(and (cons? a) (null? (cdr a))) (car a)]
          [else (cons (car a) (list->improper-list (cdr a)))]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Stacktrace instrumenter
  
  (define calltrace-key #`(quote #,(gensym 'key)))
  
  (define-values/invoke-unit stacktrace@
    (import stacktrace-imports^) (export stacktrace^))
  
  (provide calltrace-eval-handler
           instrumenting-enabled
           annotate))
