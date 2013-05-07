(module toplevel racket/base
  (require "kerncase.rkt"
           racket/undefined)
  
  (provide eval-compile-time-part-of-top-level
           eval-compile-time-part-of-top-level/compile
           expand-top-level-with-compile-time-evals
           expand-syntax-top-level-with-compile-time-evals
           expand-syntax-top-level-with-compile-time-evals/flatten)

  ;; eval-compile-time-part-of-top-level/compile : syntax -> (listof compiled-expression)
  (define (eval-compile-time-part-of-top-level/compile expr)
    (map (lambda (e) (compile-and-eval-compile-time-part e #t))
         (flatten-out-begins expr)))
  
  (define (eval-compile-time-part-of-top-level stx)
    (for-each (lambda (e) (compile-and-eval-compile-time-part e #f))
              (flatten-out-begins stx)))
  
  (define (expand-top-level-with-compile-time-evals stx)
    (expand-syntax-top-level-with-compile-time-evals 
     (namespace-syntax-introduce stx)))

  ;; expand-syntax-top-level-with-compile-time-evals/flatten : syntax -> (listof syntax)
  (define (expand-syntax-top-level-with-compile-time-evals/flatten stx)
    (let loop ([stx stx])
      (let ([e (expand-syntax-to-top-form stx)])
        (syntax-case e (begin)
          [(begin expr ...)
           (apply append (map loop (syntax->list (syntax (expr ...)))))]
          [else 
           (let ([e (expand-syntax e)])
             (compile-and-eval-compile-time-part e #f)
             (list e))]))))
  
  (define (expand-syntax-top-level-with-compile-time-evals stx)
    (let ([e (expand-syntax-to-top-form stx)])
      (syntax-case e (begin)
        [(begin expr ...)
         (with-syntax ([(expr ...) 
                        ;;left-to-right part of this map is important:
                        (map expand-syntax-top-level-with-compile-time-evals
                             (syntax->list (syntax (expr ...))))]
                       [(beg . _) e])
           (datum->syntax e (syntax-e (syntax (beg expr ...))) e e))]
        [else 
         (let ([e (expand-syntax e)])
           (compile-and-eval-compile-time-part e #f)
           e)])))
  
  ;; compile-and-eval-compile-time-part : syntax boolean -> (union syntax compiled-expression)
  ;; compiles the syntax it receives as an argument and evaluates the compile-time part of it.
  ;; result depends on second argument. If #t, returns compiled expressions
  ;; if #f, returns void (and doesn't do any extra compilation)
  ;; pre: there are no top-level begins in stx.
  (define (compile-and-eval-compile-time-part stx compile?)
    (let ([eval/compile (lambda (stx) 
                          (let ([compiled (compile-syntax stx)])
                            (eval compiled)
                            (when compile?
                              compiled)))])
      (kernel-syntax-case stx #f
        [(#%require req ...)
	 (begin0
	  (when compile? (compile-syntax stx))
	  (for-each (lambda (req) (namespace-require/expansion-time (syntax->datum req)))
		    (syntax->list (syntax (req ...)))))]
        [(module . _)
         (eval/compile stx)]
        [(define-syntaxes . _)
         (eval/compile stx)]
        [(begin-for-syntax . _)
         (eval/compile stx)]
        [(define-values (id ...) . _)
	 (begin0
	  (when compile? (compile-syntax stx))
	  (for-each (lambda (id)
		      (with-syntax ([id id])
			(eval-syntax (syntax (define-values (id) undefined)))))
		    (syntax->list (syntax (id ...)))))]
        [_else 
         (when compile? (compile-syntax stx))])))
  
  ;; flatten-out-begins : syntax -> (listof syntax)
  ;; flattens out the begins in a top-level expression,
  ;; into multiple expressions
  (define (flatten-out-begins expr)
    (let loop ([expr expr])
      (let ([expr (expand-syntax-to-top-form expr)])
	(syntax-case expr (begin)
	  [(begin expr ...)
	   (apply append (map loop (syntax->list (syntax (expr ...)))))]
	  [else 
	   (list expr)])))))
