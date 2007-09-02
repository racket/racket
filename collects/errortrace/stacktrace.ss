(module stacktrace mzscheme
  (require (lib "unit.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax"))

  (provide stacktrace@ stacktrace^ stacktrace-imports^)

  (define-signature stacktrace-imports^
    (with-mark

     test-coverage-enabled
     test-covered
     initialize-test-coverage-point

     profile-key
     profiling-enabled
     initialize-profile-point
     register-profile-start
     register-profile-done))

  (define-signature stacktrace^
    (annotate-top
     annotate
     make-st-mark
     st-mark-source
     st-mark-bindings))

  (define-unit stacktrace@
    (import stacktrace-imports^)
    (export stacktrace^)
      
      (define (short-version v depth)
        (cond
         [(identifier? v) (syntax-e v)]
         [(null? v) null]
         [(vector? v) (if (zero? depth)
                          #(....)
                          (list->vector
                           (short-version (vector->list v) (sub1 depth))))]
         [(box? v) (if (zero? depth)
                       #&(....)
                       (box (short-version (unbox v) (sub1 depth))))]
         [(pair? v)
          (cond
            [(zero? depth) '(....)]
            [(memq (syntax-e (car v)) '(#%datum #%app #%top))
             (short-version (cdr v) depth)]
            [else
             (cons (short-version (car v) (sub1 depth))
                   (short-version (cdr v) (sub1 depth)))])]
         [(syntax? v) (short-version (syntax-e v) depth)]
         [else v]))

      (define (make-st-mark stx)
        (unless (syntax? stx)
          (error 'make-st-mark
                 "expected syntax object as argument, got ~e" stx))
        #`(quote (#,(short-version stx 10)
                    #,(let ([s (let ([source (syntax-source stx)])
                                 (cond
                                   [(string? source) source]
                                   [(path? source) (path->string source)]
                                   [(not source) #f]
                                   [else (format "~a" source)]))])
                        (and s (string->symbol s)))
                    #,(syntax-line stx)
                    #,(syntax-column stx)
                    #,(syntax-position stx)
                    #,(syntax-span stx))))
      (define (st-mark-source src)
        (datum->syntax-object #f (car src) (cdr src) #f))

      (define (st-mark-bindings x) null)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Test case coverage instrumenter

      ;; The next procedure is called by `annotate' and `annotate-top' to wrap
      ;; expressions with test suite coverage information.  Returning the
      ;; first argument means no tests coverage information is collected.

      ;; test-coverage-point : syntax syntax -> syntax
      ;; sets a test coverage point for a single expression
      (define (test-coverage-point body expr)
        (if (test-coverage-enabled)
            (let ([key (gensym 'test-coverage-point)])
              (initialize-test-coverage-point key expr)
              (with-syntax ([key (datum->syntax-object
                                  #f key (quote-syntax here))]
                            [body body]
                            [test-covered test-covered])
                #'(begin (test-covered 'key) body)))
            body))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; Profiling instrumenter

      ;; profile-point :
      ;;   (syntax[list of exprs] symbol-or-#f syntax boolean
      ;;    -> syntax[list of exprs])

      ;; This procedure is called by `annotate' and `annotate-top' to wrap
      ;; expressions with profile collecting information.  Returning the
      ;; first argument means no profiling information is collected.

      ;; The second argument is the point's inferred name, if any, the third
      ;; argument is the source expression, and the fourth argument is #t for
      ;; a transformer expression and #f for a normal expression.

      (define (profile-point bodies name expr trans?)
        (let ([key (gensym 'profile-point)])
          (initialize-profile-point key name expr)
          (with-syntax ([key (datum->syntax-object #f key (quote-syntax here))]
                        [start (datum->syntax-object
                                #f (gensym) (quote-syntax here))]
                        [profile-key (datum->syntax-object
                                      #f profile-key (quote-syntax here))]
                        [register-profile-start register-profile-start]
                        [register-profile-done register-profile-done])
            (with-syntax ([rest
                           (insert-at-tail*
                            (syntax (register-profile-done 'key start))
                            bodies
                            trans?)])
              (syntax
               (let ([start (register-profile-start 'key)])
                 (with-continuation-mark 'profile-key 'key
                   (begin . rest))))))))

      (define (insert-at-tail* e exprs trans?)
        (let ([new
               (rebuild exprs
                        (let loop ([exprs exprs])
                          (if (stx-null? (stx-cdr exprs))
                              (list (cons (stx-car exprs)
                                          (insert-at-tail
                                           e (stx-car exprs) trans?)))
                              (loop (stx-cdr exprs)))))])
          (if (syntax? exprs)
              (certify exprs new)
              new)))

      (define (insert-at-tail se sexpr trans?)
        (with-syntax ([expr sexpr]
                      [e se])
          (kernel-syntax-case sexpr trans?
            ;; negligible time to eval
            [id
             (identifier? sexpr)
             (syntax (begin e expr))]
            [(quote _) (syntax (begin e expr))]
            [(quote-syntax _) (syntax (begin e expr))]
            [(#%datum . d) (syntax (begin e expr))]
            [(#%top . d) (syntax (begin e expr))]
            [(#%variable-reference . d) (syntax (begin e expr))]

            ;; No tail effect, and we want to account for the time
            [(lambda . _) (syntax (begin0 expr e))]
            [(case-lambda . _) (syntax (begin0 expr e))]
            [(set! . _) (syntax (begin0 expr e))]

            [(let-values bindings . body)
             (insert-at-tail* se sexpr trans?)]
            [(letrec-values bindings . body)
             (insert-at-tail* se sexpr trans?)]

            [(begin . _)
             (insert-at-tail* se sexpr trans?)]
            [(with-continuation-mark . _)
             (insert-at-tail* se sexpr trans?)]

            [(begin0 body ...)
             (certify sexpr (syntax (begin0 body ... e)))]

            [(if test then)
             (certify
              sexpr
              (append-rebuild
               (rebuild sexpr (list (cons #'then (insert-at-tail
                                                  se (syntax then) trans?))))
               #'(begin e (void))))]
            [(if test then else)
             ;; WARNING: e inserted twice!
             (certify
              sexpr
              (rebuild
               sexpr
               (list
                (cons #'then (insert-at-tail se (syntax then) trans?))
                (cons #'else (insert-at-tail se (syntax else) trans?)))))]

            [(#%app . rest)
             (if (stx-null? (syntax rest))
                 ;; null constant
                 (syntax (begin e expr))
                 ;; application; exploit guaranteed left-to-right evaluation
                 (insert-at-tail* se sexpr trans?))]

            [_else
             (error 'errortrace
                    "unrecognized (non-top-level) expression form: ~e"
                    (syntax-object->datum sexpr))])))

      (define (profile-annotate-lambda name expr clause bodys-stx trans?)
        (let* ([bodys (stx->list bodys-stx)]
               [bodyl (map (lambda (e) (annotate e trans?))
                           bodys)])
          (rebuild clause
                   (if (profiling-enabled)
                       (let ([prof-expr
                              (profile-point bodyl name expr trans?)])
                         ;; Tell rebuild to replace first expressions with
                         ;; (void), and replace the last expression with
                         ;; prof-expr:
                         (let loop ([bodys bodys])
                           (if (null? (cdr bodys))
                               (list (cons (car bodys) prof-expr))
                               (cons (cons (car bodys) #'(void))
                                     (loop (cdr bodys))))))
                       ;; Map 1-to-1:
                       (map cons bodys bodyl)))))

      (define (keep-lambda-properties orig new)
        (let ([p (syntax-property orig 'method-arity-error)]
              [p2 (syntax-property orig 'inferred-name)])
          (let ([new (if p
                         (syntax-property new 'method-arity-error p)
                         new)])
            (if p2
                (syntax-property new 'inferred-name p2)
                new))))

      (define (annotate-let expr trans? varss-stx rhss-stx bodys-stx)
        (let ([varss (syntax->list varss-stx)]
              [rhss (syntax->list rhss-stx)]
              [bodys (syntax->list bodys-stx)])
          (let ([rhsl (map
                       (lambda (vars rhs)
                            (annotate-named
                             (syntax-case vars ()
                               [(id)
                                (syntax id)]
                               [_else #f])
                             rhs
                             trans?))
                          varss
                          rhss)]
                [bodyl (map
                        (lambda (body)
                          (annotate body trans?))
                        bodys)])
            (rebuild expr (append (map cons bodys bodyl)
                                  (map cons rhss rhsl))))))

      (define (annotate-seq expr bodys-stx annotate trans?)
        (let* ([bodys (syntax->list bodys-stx)]
               [bodyl (map (lambda (b)
                             (annotate b trans?))
                           bodys)])
          (rebuild expr (map cons bodys bodyl))))

      (define orig-inspector (current-code-inspector))

      (define (certify orig new)
        (syntax-recertify new orig orig-inspector #f))

      (define (rebuild expr replacements)
        (let loop ([expr expr]
                   [same-k (lambda () expr)]
                   [diff-k (lambda (x) x)])
          (let ([a (assq expr replacements)])
            (if a
                (diff-k (cdr a))
                (cond
                 [(pair? expr) (loop (car expr)
                                     (lambda ()
                                       (loop (cdr expr)
                                             same-k
                                             (lambda (y)
                                               (diff-k (cons (car expr) y)))))
                                     (lambda (x)
                                       (loop (cdr expr)
                                             (lambda ()
                                               (diff-k (cons x (cdr expr))))
                                             (lambda (y)
                                               (diff-k (cons x y))))))]
                 [(vector? expr)
                  (loop (vector->list expr)
                        same-k
                        (lambda (x) (diff-k (list->vector x))))]
                 [(box? expr) (loop (unbox expr)
                                    same-k
                                    (lambda (x)
                                      (diff-k (box x))))]
                 [(syntax? expr) (if (identifier? expr)
                                     (same-k)
                                     (loop (syntax-e expr)
                                           same-k
                                           (lambda (x)
                                             (diff-k
                                              (datum->syntax-object
                                               expr
                                               x
                                               expr)))))]
                 [else (same-k)])))))

      (define (append-rebuild expr end)
        (cond
         [(syntax? expr)
          (datum->syntax-object expr
                                (append-rebuild (syntax-e expr) end)
                                expr)]
         [(pair? expr)
          (cons (car expr) (append-rebuild (cdr expr) end))]
         [(null? expr)
          (list end)]
         [else
          (error 'append-rebuild "shouldn't get here")]))

      (define (one-name names-stx)
        (let ([l (syntax->list names-stx)])
          (and (pair? l)
               (null? (cdr l))
               (car l))))

      (define (make-annotate top? name)
        (lambda (expr trans?)
          (test-coverage-point
           (kernel-syntax-case expr trans?
             [_
              (identifier? expr)
              (let ([b ((if trans?
                          identifier-binding
                          identifier-transformer-binding)
                        expr)])
                (cond
                 [(eq? 'lexical b)
                  ;; lexical variable - no error possile
                  expr]
                 [(and (pair? b) (eq? '#%kernel (car b)))
                  ;; built-in - no error possible
                  expr]
                 [else
                  ;; might be undefined/uninitialized
                  (with-mark expr expr)]))]

             [(#%top . id)
              ;; might be undefined/uninitialized
              (with-mark expr expr)]
             [(#%datum . _)
              ;; no error possible
              expr]
	     [(#%variable-reference . _)
              ;; no error possible
              expr]

             ;; Can't put annotation on the outside
             [(define-values names rhs)
              top?
              (let ([marked (with-mark expr
                                       (annotate-named
                                        (one-name #'names)
                                        (syntax rhs)
                                        trans?))])
                (certify
                 expr
                 (rebuild expr (list (cons #'rhs marked)))))]
             [(begin . exprs)
              top?
              (certify
               expr
               (annotate-seq expr
                             (syntax exprs)
                             annotate-top trans?))]
             [(define-syntaxes (name ...) rhs)
              top?
              (let ([marked (with-mark expr
                                       (annotate-named
                                        (one-name #'(name ...))
                                        (syntax rhs)
                                        #t))])
                (certify
                 expr
                 (rebuild expr (list (cons #'rhs marked)))))]

             [(define-values-for-syntax (name ...) rhs)
              top?
              (let ([marked (with-mark expr
                                       (annotate-named
                                        (one-name (syntax (name ...)))
                                        (syntax rhs)
                                        #t))])
                (certify
                 expr
                 (rebuild expr (list (cons #'rhs marked)))))]

             ;; Just wrap body expressions
             [(module name init-import (#%plain-module-begin body ...))
              top?
              (let ([bodys (syntax->list (syntax (body ...)))]
		    [mb (list-ref (syntax->list expr) 3)])
                (let ([bodyl (map (lambda (b)
                                    (annotate-top b trans?))
                                  bodys)])
		  (certify
		   expr
		   (rebuild
		    expr
		    (list (cons
			   mb
			   (certify
			    mb
			    (rebuild mb (map cons bodys bodyl)))))))))]

             [(#%expression e)
              top?
              (certify expr #`(#%expression #,(annotate (syntax e) trans?)))]
		   
             ;; No way to wrap
             [(require i ...) expr]
             [(require-for-syntax i ...) expr]
             [(require-for-template i ...) expr]
             [(require-for-label i ...) expr]
             ;; No error possible (and no way to wrap)
             [(provide i ...) expr]
             [(provide-for-syntax i ...) expr]
             [(provide-for-label i ...) expr]

             ;; No error possible
             [(quote _)
              expr]
             [(quote-syntax _)
              expr]

             ;; Wrap body, also a profile point
             [(lambda args . body)
              (certify
               expr
               (keep-lambda-properties
                expr
                (profile-annotate-lambda name expr expr (syntax body)
                                         trans?)))]
             [(case-lambda clause ...)
              (with-syntax ([([args . body] ...)
                             (syntax (clause ...))])
                (let* ([clauses (syntax->list (syntax (clause ...)))]
                       [clausel (map
                                 (lambda (body clause)
                                   (profile-annotate-lambda
                                    name expr clause body trans?))
                                 (syntax->list (syntax (body ...)))
                                 clauses)])
                  (certify
                   expr
                   (keep-lambda-properties
                    expr
                    (rebuild expr (map cons clauses clausel))))))]

             ;; Wrap RHSs and body
             [(let-values ([vars rhs] ...) . body)
              (with-mark expr
                         (certify
                          expr
                          (annotate-let expr trans?
                                        (syntax (vars ...))
                                        (syntax (rhs ...))
                                        (syntax body))))]
             [(letrec-values ([vars rhs] ...) . body)
              (with-mark expr
                         (certify
                          expr
                          (annotate-let expr trans?
                                        (syntax (vars ...))
                                        (syntax (rhs ...))
                                        (syntax body))))]

             ;; Wrap RHS
             [(set! var rhs)
              (let ([new-rhs (annotate-named
                              (syntax var)
                              (syntax rhs)
                              trans?)])
                ;; set! might fail on undefined variable, or too many values:
                (with-mark expr
                           (certify
                            expr
                            (rebuild expr (list (cons #'rhs new-rhs))))))]

             ;; Wrap subexpressions only
	     [(begin e)
	      ;; Single expression: no mark
	      (certify
	       expr
	       #`(begin #,(annotate (syntax e) trans?)))]
             [(begin . body)
              (with-mark expr
                         (certify
                          expr
                          (annotate-seq expr #'body annotate trans?)))]
             [(begin0 . body)
              (with-mark expr
                         (certify
                          expr
                          (annotate-seq expr #'body annotate trans?)))]
             [(if tst thn els)
              (let ([w-tst (annotate (syntax tst) trans?)]
                    [w-thn (annotate (syntax thn) trans?)]
                    [w-els (annotate (syntax els) trans?)])
                (with-mark expr
                           (certify
                            expr
                            (rebuild expr (list (cons #'tst w-tst)
                                                (cons #'thn w-thn)
                                                (cons #'els w-els))))))]
             [(if tst thn)
              (let ([w-tst (annotate (syntax tst) trans?)]
                    [w-thn (annotate (syntax thn) trans?)])
                (with-mark expr
                           (certify
                            expr
                            (rebuild expr (list (cons #'tst w-tst)
                                                (cons #'thn w-thn))))))]
             [(with-continuation-mark . body)
              (with-mark expr
                         (certify
                          expr
                          (annotate-seq expr (syntax body)
                                        annotate trans?)))]

             ;; Wrap whole application, plus subexpressions
             [(#%app . body)
              (cond
               [(stx-null? (syntax body))
                ;; It's a null:
                expr]
               [(syntax-case* expr (#%app void)
                              (if trans?
                                module-transformer-identifier=?
                                module-identifier=?)
                  [(#%app void) #t]
                  [_else #f])
                ;; It's (void):
                expr]
               [else
                (with-mark expr (certify
                                 expr
                                 (annotate-seq expr (syntax body)
                                               annotate trans?)))])]

             [_else
              (error 'errortrace "unrecognized expression form~a: ~e"
                     (if top? " at top-level" "")
                     (syntax-object->datum expr))])
           expr)))

      (define annotate (make-annotate #f #f))
      (define annotate-top (make-annotate #t #f))
      (define (annotate-named name expr trans?)
        ((make-annotate #t name) expr trans?))))
