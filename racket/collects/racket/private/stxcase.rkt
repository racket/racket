;;----------------------------------------------------------------------
;; syntax-case and syntax

(module stxcase '#%kernel
  (#%require "stx.rkt" "small-scheme.rkt" '#%paramz '#%unsafe
             "ellipses.rkt"
             (for-syntax "stx.rkt" "small-scheme.rkt"
                         "member.rkt" "sc.rkt" '#%kernel))

  (-define (datum->syntax/shape orig datum)
     (if (syntax? datum)
	 datum
	 (let ([stx (datum->syntax orig datum orig #f orig)])
	   (let ([shape (syntax-property orig 'paren-shape)])
	     (if shape
		 (syntax-property stx 'paren-shape shape)
		 stx)))))

  (-define (catch-ellipsis-error thunk sexp sloc)
      ((let/ec esc
	 (with-continuation-mark
	     exception-handler-key
             (lambda (exn)
               (esc
                (lambda ()
                  (if (exn:break? exn)
                      (raise exn)
                      (raise-syntax-error
                       'syntax
                       "incompatible ellipsis match counts for template"
                       sexp
                       sloc)))))
	   (let ([v (thunk)])
	     (lambda () v))))))

  (-define substitute-stop 'dummy)

  ;; pattern-substitute optimizes a pattern substitution by
  ;;  merging variables that look up the same simple mapping
  (-define-syntax pattern-substitute
    (lambda (stx)
      (let ([pat (stx-car (stx-cdr stx))]
	    [subs (stx->list (stx-cdr (stx-cdr stx)))])
	(let ([ht-common (make-hash)]
	      [ht-map (make-hasheq)])
	  ;; Determine merges:
	  (let loop ([subs subs])
	    (unless (null? subs)
	      (let ([id (syntax-e (car subs))]
		    [expr (cadr subs)])
		(when (or (identifier? expr)
			  (and (stx-pair? expr)
			       (memq (syntax-e (stx-car expr))
				     '(car cadr caddr cadddr
					   cdr cddr cdddr cddddr
					   list-ref list-tail))
			       (stx-pair? (stx-cdr expr))
			       (identifier? (stx-car (stx-cdr expr)))))
		  (let ([s-expr (syntax->datum expr)])
		    (let ([new-id (hash-ref ht-common s-expr #f)])
		      (if new-id
			  (hash-set! ht-map id new-id)
			  (hash-set! ht-common s-expr id))))))
	      (loop (cddr subs))))
	  ;; Merge:
	  (let ([new-pattern (if (zero? (hash-count ht-map))
				 pat
				 (let loop ([stx pat])
				   (cond
				    [(pair? stx)
				     (let ([a (loop (car stx))]
					   [b (loop (cdr stx))])
				       (if (and (eq? a (car stx))
						(eq? b (cdr stx)))
					   stx
					   (cons a b)))]
				    [(symbol? stx)
				     (let ([new-id (hash-ref ht-map stx #f)])
				       (or new-id stx))]
				    [(syntax? stx) 
				     (let ([new-e (loop (syntax-e stx))])
				       (if (eq? (syntax-e stx) new-e)
					   stx
					   (datum->syntax stx new-e stx stx)))]
				    [(vector? stx)
				     (list->vector (map loop (vector->list stx)))]
				    [(box? stx) (box (loop (unbox stx)))]
				    [else stx])))])
	    (datum->syntax (quote-syntax here)
				  `(apply-pattern-substitute
				    ,new-pattern
				    (quote ,(let loop ([subs subs])
					      (cond
					       [(null? subs) null]
					       [(hash-ref ht-map (syntax-e (car subs)) #f)
						;; Drop mapped id
						(loop (cddr subs))]
					       [else
						(cons (car subs) (loop (cddr subs)))])))
				    . ,(let loop ([subs subs])
					 (cond
					  [(null? subs) null]
					  [(hash-ref ht-map (syntax-e (car subs)) #f)
					   ;; Drop mapped id
					   (loop (cddr subs))]
					  [else
					   (cons (cadr subs) (loop (cddr subs)))])))
				  stx))))))

  (-define apply-pattern-substitute
     (lambda (stx sub-ids . sub-vals)
       (let loop ([stx stx])
	 (cond
	  [(pair? stx) (let ([a (loop (car stx))]
			     [b (loop (cdr stx))])
			 (if (and (eq? a (car stx))
				  (eq? b (cdr stx)))
			     stx
			     (cons a b)))]
	  [(symbol? stx)
	   (let sloop ([sub-ids sub-ids][sub-vals sub-vals])
	     (cond
	      [(null? sub-ids) stx]
	      [(eq? stx (car sub-ids)) (car sub-vals)]
	      [else (sloop (cdr sub-ids) (cdr sub-vals))]))]
	  [(syntax? stx) 
	   (let ([new-e (loop (syntax-e stx))])
	     (if (eq? (syntax-e stx) new-e)
		 stx
                 (datum->syntax/shape stx new-e)))]
	  [(vector? stx)
	   (list->vector (map loop (vector->list stx)))]
	  [(box? stx) (box (loop (unbox stx)))]
	  [else stx]))))

  (-define interp-match
     (lambda (pat e literals immediate=?)
       (interp-gen-match pat e literals immediate=? #f)))

  (-define interp-s-match
     (lambda (pat e literals immediate=?)
       (interp-gen-match pat e literals immediate=? #t)))

  (-define interp-gen-match
     (lambda (pat e literals immediate=? s-exp?)
       (let loop ([pat pat][e e][cap e])
         (cond
          [(null? pat) 
           (if s-exp?
               (null? e)
               (stx-null? e))]
          [(number? pat)
           (and (if s-exp? (symbol? e) (identifier? e))
                (immediate=? e (vector-ref (if s-exp? literals (syntax-e literals)) pat)))]
          [(not pat)
           #t]
          [else
           (let ([i (vector-ref pat 0)])
             (cond
              [(eq? i 'bind)
               (let ([e (if s-exp?
                            e
                            (if (vector-ref pat 2)
                                (datum->syntax cap e cap)
                                e))])
                 (if (vector-ref pat 1)
                     e
                     (list e)))]
              [(eq? i 'pair)
               (let ([match-head (vector-ref pat 1)]
                     [match-tail (vector-ref pat 2)]
                     [mh-did-var? (vector-ref pat 3)]
                     [mt-did-var? (vector-ref pat 4)])
                 (let ([cap (if (syntax? e) e cap)])
                   (and (stx-pair? e)
                        (let ([h (loop match-head (stx-car e) cap)])
                          (and h
                               (let ([t (loop match-tail (stx-cdr e) cap)])
                                 (and t
                                      (if mh-did-var?
                                          (if mt-did-var?
                                              (append h t)
                                              h)
                                          t))))))))]
              [(eq? i 'quote)
               (if s-exp?
                   (and (equal? (vector-ref pat 1) e)
                        null)
                   (and (syntax? e)
                        (equal? (vector-ref pat 1) (syntax-e e))
                        null))]
              [(eq? i 'ellipses)
               (let ([match-head (vector-ref pat 1)]
                     [nest-cnt (vector-ref pat 2)]
                     [last? (vector-ref pat 3)])
                 (and (if s-exp?
                          (list? e)
                          (stx-list? e))
                      (if (zero? nest-cnt)
                          (andmap (lambda (e) (loop match-head e cap)) 
                                  (if s-exp? e (stx->list e)))
                          (let/ec esc
                            (let ([l (map (lambda (e)
                                            (let ([m (loop match-head e cap)])
                                              (if m
                                                  m
                                                  (esc #f))))
                                          (if s-exp? e (stx->list e)))])
                              (if (null? l)
                                  (let loop ([cnt nest-cnt])
                                    (cond
                                     [(= 1 cnt) (if last? '() '(()))]
                                     [else (cons '() (loop (sub1 cnt)))]))
                                  ((if last? stx-rotate* stx-rotate) l)))))))]
              [(eq? i 'mid-ellipses)
               (let ([match-head (vector-ref pat 1)]
                     [match-tail (vector-ref pat 2)]
                     [tail-cnt (vector-ref pat 3)]
                     [prop? (vector-ref pat 4)]
                     [mh-did-var? (vector-ref pat 5)]
                     [mt-did-var? (vector-ref pat 6)])
                 (let-values ([(pre-items post-items ok?) 
                               (split-stx-list e tail-cnt prop?)]
                              [(cap) (if (syntax? e) e cap)])
                   (and ok?
                        (let ([h (loop match-head pre-items cap)])
                          (and h
                               (let ([t (loop match-tail post-items cap)])
                                 (and t
                                      (if mt-did-var?
                                          (if mh-did-var?
                                              (append h t)
                                              t)
                                          h))))))))]
              [(eq? i 'veclist)
               (and (if s-exp?
                        (vector? e)
                        (stx-vector? e #f))
                    (loop (vector-ref pat 1) (vector->list (if s-exp? e (syntax-e e))) cap))]
              [(eq? i 'vector)
               (and (if s-exp?
                        (and (vector? e) (= (vector-length e) (vector-ref pat 1)))
                        (stx-vector? e (vector-ref pat 1)))
                    (let vloop ([p (vector-ref pat 2)][pos 0])
                      (cond
                       [(null? p) null]
                       [else 
                        (let ([clause (car p)])
                          (let ([match-elem (car clause)]
                                [elem-did-var? (cdr clause)])
                            (let ([m (loop match-elem (if s-exp? (vector-ref e pos) (stx-vector-ref e pos)) cap)])
                              (and m
                                   (let ([body (vloop (cdr p) (add1 pos))])
                                     (and body
                                          (if elem-did-var?
                                              (if (null? body)
                                                  m
                                                  (append m body))
                                              body)))))))])))]
              [(eq? i 'box)
               (let ([match-content (vector-ref pat 1)])
                 (and (if s-exp?
                          (box? e)
                          (stx-box? e))
                      (loop match-content (unbox (if s-exp? e (syntax-e e))) cap)))]
              [(eq? i 'prefab)
               (and (if s-exp?
                        (equal? (vector-ref pat 1) (prefab-struct-key e))
                        (stx-prefab? (vector-ref pat 1) e))
                    (loop (vector-ref pat 2) (cdr (vector->list (struct->vector (if s-exp? e (syntax-e e))))) cap))]
              [else (error "yikes!" pat)]))]))))

  (-define-syntax syntax-case**
    (lambda (x)
      (-define l (and (stx-list? x) (cdr (stx->list x))))
      (unless (and (stx-list? x)
		   (> (length l) 3))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (let ([who (car l)]
	    [arg-is-stx? (cadr l)]
	    [expr (caddr l)]
	    [kws (cadddr l)]
	    [lit-comp (cadddr (cdr l))]
            [s-exp? (syntax-e (cadddr (cddr l)))]
	    [clauses (cddddr (cddr l))])
	(unless (stx-list? kws)
	  (raise-syntax-error
	   (syntax-e who)
	   "expected a parenthesized sequence of literal identifiers"
	   kws))
	(for-each
	 (lambda (lit)
	   (unless (identifier? lit)
	     (raise-syntax-error
	      (syntax-e who)
	      "literal is not an identifier"
	      lit)))
	 (stx->list kws))
	(for-each
	 (lambda (clause)
	   (unless (and (stx-list? clause)
			(<= 2 (length (stx->list clause)) 3))
	     (raise-syntax-error
	      (syntax-e who)
	      "expected a clause containing a pattern, an optional guard expression, and an expression"
	      clause)))
	 clauses)
	(let ([patterns (map stx-car clauses)]
	      [fenders (map (lambda (clause)
			      (and (stx-pair? (stx-cdr (stx-cdr clause)))
				   (stx-car (stx-cdr clause))))
			    clauses)]
	      [answers (map (lambda (clause)
			      (let ([r (stx-cdr (stx-cdr clause))])
				(if (stx-pair? r) 
				    (stx-car r)
				    (stx-car (stx-cdr clause)))))
			    clauses)])
	  (let* ([arg (quote-syntax arg)]
		 [rslt (quote-syntax rslt)]
		 [pattern-varss (map
				 (lambda (pattern)
				   (get-match-vars who pattern pattern (stx->list kws)))
				 (stx->list patterns))]
		 [lit-comp-is-mod? (and (identifier? lit-comp)
					(free-identifier=? 
					 lit-comp
					 (quote-syntax free-identifier=?)))])
            (syntax-arm
             (datum->syntax
              (quote-syntax here)
              (list (quote-syntax let) (list (list arg (if (or s-exp? (syntax-e arg-is-stx?))
                                                           expr
                                                           (list (quote-syntax datum->syntax)
                                                                 (list
                                                                  (quote-syntax quote-syntax)
                                                                  (datum->syntax
                                                                   expr
                                                                   'here))
                                                                 expr))))
                    (let loop ([patterns patterns]
                               [fenders fenders]
                               [unflat-pattern-varss pattern-varss]
                               [answers answers])
                      (cond
                       [(null? patterns)
                        (list
                         (quote-syntax raise-syntax-error)
                         #f
                         "bad syntax"
                         arg)]
                       [else
                        (let ([rest (loop (cdr patterns) (cdr fenders)
                                          (cdr unflat-pattern-varss) (cdr answers))])
                          (let ([pattern (car patterns)]
                                [fender (car fenders)]
                                [unflat-pattern-vars (car unflat-pattern-varss)]
                                [answer (car answers)])
                            (-define pattern-vars
                                     (map (lambda (var)
                                            (let loop ([var var])
                                              (if (syntax? var)
                                                  var
                                                  (loop (car var)))))
                                          unflat-pattern-vars))
                            (-define temp-vars
                                     (map
                                      (lambda (p) (gen-temp-id 'sc))
                                      pattern-vars))
                            (-define tail-pattern-var (sub1 (length pattern-vars)))
                            ;; Here's the result expression for one match:
                            (let* ([do-try-next (if (car fenders)
                                                    (list (quote-syntax try-next))
                                                    rest)]
                                   [mtch (make-match&env
                                          who
                                          pattern
                                          pattern
                                          (stx->list kws)
                                          (not lit-comp-is-mod?)
                                          s-exp?)]
                                   [cant-fail? (if lit-comp-is-mod?
                                                   (equal? mtch '(lambda (e) e))
                                                   (equal? mtch '(lambda (e free-identifier=?) e)))]
                                   ;; Avoid generating gigantic matching expressions.
                                   ;; If it's too big, interpret at run time, instead
                                   [interp? (and (not cant-fail?)
                                                 (zero?
                                                  (let sz ([mtch mtch][fuel 100])
                                                    (cond
                                                     [(zero? fuel) 0]
                                                     [(pair? mtch) (sz (cdr mtch)
                                                                       (sz (car mtch)
                                                                           fuel))]
                                                     [(syntax? mtch) (sz (syntax-e mtch) (sub1 fuel))]
                                                     [else (sub1 fuel)]))))]
                                   [mtch (if interp?
                                             (let ([interp-box (box null)])
                                               (let ([pat (make-interp-match pattern (syntax->list kws) interp-box s-exp?)])
                                                 (list 'lambda
                                                       '(e)
                                                       (list (if s-exp? 'interp-s-match 'interp-match)
                                                             (list 'quote pat)
                                                             'e
                                                             (if (null? (unbox interp-box))
                                                                 #f
                                                                 (list (if s-exp? 'quote 'quote-syntax)
                                                                       (list->vector (reverse (unbox interp-box)))))
                                                             lit-comp))))
                                             mtch)]
                                   [m
                                    ;; Do match, bind result to rslt:
                                    (list (quote-syntax let)
                                          (list 
                                           (list rslt
                                                 (if cant-fail?
                                                     arg
                                                     (list* (datum->syntax
                                                             (quote-syntax here)
                                                             mtch
                                                             pattern)
                                                            arg
                                                            (if (or interp? lit-comp-is-mod?)
                                                                null
                                                                (list lit-comp))))))
                                          ;; If match succeeded...
                                          (list 
                                           (quote-syntax if)
                                           (if cant-fail?
                                               #t
                                               rslt)
                                           ;; Extract each name binding into a temp variable:
                                           (list
                                            (quote-syntax let) 
                                            (map (lambda (pattern-var temp-var)
                                                   (list
                                                    temp-var
                                                    (let ([pos (stx-memq-pos pattern-var pattern-vars)])
                                                      (let ([accessor (cond
                                                                       [(= tail-pattern-var pos)
                                                                        (cond
                                                                         [(eq? pos 0) 'tail]
                                                                         [(eq? pos 1) (quote-syntax unsafe-cdr)]
                                                                         [else 'tail])]
                                                                       [(eq? pos 0) (quote-syntax unsafe-car)]
                                                                       [else #f])])
                                                        (cond
                                                         [(eq? accessor 'tail)
                                                          (if (zero? pos)
                                                              rslt
                                                              (list
                                                               (quote-syntax unsafe-list-tail)
                                                               rslt
                                                               pos))]
                                                         [accessor (list
                                                                    accessor
                                                                    rslt)]
                                                         [else (list
                                                                (quote-syntax unsafe-list-ref)
                                                                rslt
                                                                pos)])))))
                                                 pattern-vars temp-vars)
                                            ;; Tell nested `syntax' forms about the
                                            ;;  pattern-bound variables:
                                            (list
                                             (quote-syntax letrec-syntaxes+values) 
                                             (map (lambda (pattern-var unflat-pattern-var temp-var)
                                                    (list (list pattern-var)
                                                          (list
                                                           (if s-exp?
                                                               (quote-syntax make-s-exp-mapping)
                                                               (quote-syntax make-syntax-mapping))
                                                           ;; Tell it the shape of the variable:
                                                           (let loop ([var unflat-pattern-var][d 0])
                                                             (if (syntax? var)
                                                                 d
                                                                 (loop (car var) (add1 d))))
                                                           ;; Tell it the variable name:
                                                           (list
                                                            (quote-syntax quote-syntax)
                                                            temp-var))))
                                                  pattern-vars unflat-pattern-vars
                                                  temp-vars)
                                             null
                                             (if fender
                                                 (list (quote-syntax if) fender
                                                       answer
                                                       do-try-next)
                                                 answer)))
                                           do-try-next))])
                              (if fender
                                  (list
                                   (quote-syntax let)
                                   ;; Bind try-next to try next case
                                   (list (list (quote try-next)
                                               (list (quote-syntax lambda)
                                                     (list)
                                                     rest)))
                                   ;; Try one match
                                   m)
                                  ;; Match try already embed the rest case
                                  m))))])))
              x)))))))

  (begin-for-syntax
   (define-values (gen-template)
    (lambda (x s-exp?)
      (-define here-stx (quote-syntax here))
      (unless (and (stx-pair? x)
		   (let ([rest (stx-cdr x)])
		     (and (stx-pair? rest)
			  (stx-null? (stx-cdr rest)))))
	(raise-syntax-error
	 #f
	 "bad form"
	 x))
      (syntax-arm
       (datum->syntax
        here-stx
        (let ([pattern (stx-car (stx-cdr x))])
          (let-values ([(unique-vars all-varss) (make-pexpand pattern #f null #f s-exp?)])
            (let ([var-bindings
                   (map
                    (lambda (var)
                      (and (let ([v (syntax-local-value var (lambda () #f))])
                             (and (if s-exp?
                                      (s-exp-pattern-variable? v)
                                      (syntax-pattern-variable? v))
                                  v))))
                    unique-vars)])
              (if (and (or (null? var-bindings)
                           (not (ormap (lambda (x) x) var-bindings)))
                       (no-ellipses? pattern))
                  ;; Constant template:
                  (list (if s-exp?
                            (quote-syntax quote) 
                            (quote-syntax quote-syntax))
                        pattern)
                  ;; Non-constant:
                  (let ([proto-r (let loop ([vars unique-vars][bindings var-bindings])
                                   (if (null? bindings)
                                       null
                                       (let ([rest (loop (cdr vars)
                                                         (cdr bindings))])
                                         (if (car bindings)
                                             (cons (let loop ([v (car vars)]
                                                              [d (if s-exp?
                                                                     (s-exp-mapping-depth (car bindings))
                                                                     (syntax-mapping-depth (car bindings)))])
                                                     (if (zero? d)
                                                         v
                                                         (loop (list v) (sub1 d))))
                                                   rest)
                                             rest))))]
                        [non-pattern-vars (let loop ([vars unique-vars][bindings var-bindings])
                                            (if (null? bindings)
                                                null
                                                (let ([rest (loop (cdr vars)
                                                                  (cdr bindings))])
                                                  (if (car bindings)
                                                      rest
                                                      (cons (car vars) rest)))))])
                    (let ([build-from-template
                           ;; Even if we don't use the builder, we need to check
                           ;; for a well-formed pattern:
                           (make-pexpand pattern proto-r non-pattern-vars pattern s-exp?)]
                          [r (let loop ([vars unique-vars][bindings var-bindings][all-varss all-varss])
                               (cond
                                [(null? bindings) null]
                                [(car bindings)
                                 (cons
                                  (syntax-property 
                                   (let ([id (if s-exp?
                                                 (s-exp-mapping-valvar (car bindings))
                                                 (syntax-mapping-valvar (car bindings)))])
                                     (datum->syntax
                                      id
                                      (syntax-e id)
                                      x))
                                   'disappeared-use
                                   (map syntax-local-introduce (car all-varss)))
                                  (loop (cdr vars) (cdr bindings) (cdr all-varss)))]
                                [else  (loop (cdr vars) (cdr bindings) (cdr all-varss))]))])
                      (if (identifier? pattern)
                          ;; Simple syntax-id lookup:
                          (car r)
                          ;; General case:
                          (list (datum->syntax
                                 here-stx
                                 build-from-template
                                 pattern)
                                (let ([len (length r)])
                                  (cond
                                   [(zero? len) (quote-syntax ())]
                                   [(= len 1) (car r)]
                                   [else
                                    (cons (quote-syntax list*) r)]))))))))))
        x)))))

  (-define-syntax syntax (lambda (stx) (gen-template stx #f)))
  (-define-syntax datum (lambda (stx) (gen-template stx #t)))

  (#%provide (all-from "ellipses.rkt") syntax-case** syntax datum
             (for-syntax syntax-pattern-variable?)))
