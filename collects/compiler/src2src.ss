
;; Implements a source-to-source optimizer

;; The src-to-src transformation currently drops
;;  properties, which is bad. The 'mzc-cffi,
;;  'method-arity-error, and 'inferred-name properties are
;;  specially preserved for `lambda' expressions.

(module src2src mzscheme
  (require (lib "class.ss")
	   (lib "kerncase.ss" "syntax")
	   (lib "primitives.ss" "syntax")
	   (lib "etc.ss")
	   (lib "list.ss"))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Optimizer
  ;; classes representing syntax with methods for optimization steps
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; Maximum number of times to inline while processing a call site
  (define max-fuel 0)
  (define fuel (make-parameter max-fuel))

  (define foldable-prims '(void
			   + - * / arithmetic-shift
			   < <= = > >=
			   number? positive? negative? zero?
			   real? complex?
			   string-ref))

  (define effectless-prims '(list list* cons vector))

  ;; The following primitives either invoke functions, or
  ;;  install functions that can be used later.
  (define (non-valueable-prims) (procedure-calling-prims))

  (define recert-insp (current-code-inspector))

  (define (keep-mzc-property stx-out stx)
    (let ([v (syntax-property stx 'mzc-cffi)]
	  [v2 (syntax-property stx 'method-arity-error)]
	  [v3 (syntax-property stx 'inferred-name)])
      (let ([stx-out2 (if v
			  (syntax-property stx-out 'mzc-cffi v)
			  stx-out)])
	(let ([stx-out3 (if v2
			    (syntax-property stx-out2 'method-arity-error v2)
			    stx-out2)])
	  (if v3
	      (syntax-property stx-out3 'inferred-name v3)
	      stx-out3)))))
  
  (define-struct context (need indef))
  ;; need = #f => don't need  the value
  ;; need = 'bool => need bool only
  ;; need = 'all => need exact result

  ;; indef = list of binding%s

  (define (need-all ctx)
    (if (eq? 'all (context-need ctx))
	ctx
	(make-context 'all (context-indef ctx))))
  (define (need-none ctx)
    (if (eq? 'none (context-need ctx))
	ctx
	(make-context 'none (context-indef ctx))))
  (define (need-bool ctx)
    (make-context 'bool (context-indef ctx)))

  (define-struct accessor (make-struct-type-expr position))
  (define-struct mutator (make-struct-type-expr position))
  (define-struct ctor (make-struct-type-expr))
  
  (define exp%
    (class object%
      
      (init-field src-stx)
      (if (not (syntax? src-stx))
          (begin
            (printf "~a~n" src-stx)
            (error 'stx)))
      (init-field [cert-stxes (list src-stx)])
      (field (known-value #f))
      
      ;; resets known-value computation, use counts, etc.
      (define/public (reset-varflags)
        (set! known-value #f)
        (for-each (lambda (e) (send e reset-varflags)) (sub-exprs)))
      
      ;; accumulates known-value mappings, use counts on bindings, etc.;
      ;; assumes varflags are reset
      (define/public (set-known-values)
        (for-each (lambda (e) (send e set-known-values)) (nonbind-sub-exprs)))

      ;; sets `mutable?' flags; set-known-values does that, too,
      ;; but this one only sets mutable flags
      (define/public (set-mutability)
        (for-each (lambda (e) (send e set-mutability)) (nonbind-sub-exprs)))

      ;; for each reference of a binding in the exp, drop one use
      (define/public (drop-uses)
        (for-each (lambda (e) (send e drop-uses)) (nonbind-sub-exprs)))

      ;; any side-effects might be in this expression?
      ;; (return #t if unsure)
      (define/public (no-side-effect?)
        (andmap (lambda (e) (send e no-side-effect?))
                (nonbind-sub-exprs)))

      ;; arity is a number or 'unknown
      (define/public (get-result-arity) 'unknown)

      ;; gets all subexpressions, including binding%s for lambda, etc.
      (define/public (sub-exprs) (append (bind-sub-exprs) (nonbind-sub-exprs)))
      ;; just the binding%s
      (define/public (bind-sub-exprs) null)
      ;; all subexpressions that aren't binding%s
      (define/public (nonbind-sub-exprs) null)

      ;; some default implementations map over nonbind-sub-exprs, 
      ;; the install the results with this method
      (define/public (set-nonbind-sub-exprs x) (void))

      ;; valueable means that evaluating the expression can't access
      ;;  a variable before it is initialized or mutate a
      ;;  variable. It's used, for example, on the RHSs of a letrec
      ;;  to determine known bindings.
      (define/public (valueable?)
        (andmap (lambda (x) (send x valueable?)) (nonbind-sub-exprs)))
      
      ;; ok to duplicate or move the expression?
      ;; (return #f if unsure)
      (define/public (can-dup/move?) #f)
      
      ;; known value is an exp%; usually only binding% objects
      ;; get known-value settings
      (define/public (set-known-value x) (set! known-value x))

      ;; finds the most-specific exp% whose value is the
      ;; same this this expression's value
      (define/public (get-value) (or known-value this))

      ;; helper:
      (define/private (subexp-map! f)
        (set-nonbind-sub-exprs (map f (nonbind-sub-exprs)))
        this)

      ;; main optimization method:
      (define/public (simplify ctx)
        (subexp-map! (lambda (x) 
                       (send x simplify (need-all ctx)))))
	
      (define/public (escape)
        (subexp-map! (lambda (x) (send x escape))))

      (define/public (stack-allocate)
        (subexp-map! (lambda (x) (send x stack-allocate))))
      
      ;; not an optimizations, but exposes info (epsecially to mzc)
      (define/public (reorganize)
        (subexp-map! (lambda (x) (send x reorganize))))
      ;; reverses reorganize
      (define/public (deorganize)
        (subexp-map! (lambda (x)
                       (send x deorganize))))

      ;; substitution of lexical refs for global variables
      (define/public (global->local env)
        (subexp-map! (lambda (x)
                       (send x global->local env))))
      
      ;; substitution of lexical refs for either lex or global vars
      (define/public (substitute env)
        (subexp-map! (lambda (x)
                       (send x substitute env))))

      ;; creates a copy, used for inling; don't try to preserve
      ;;  analysis, because we'll just re-compute it
      (define/public (clone env)
        (error 'clone "unimplemented: ~a" this))

      ;; gets stx object, usually for src info
      (define/public (get-stx) src-stx)

      ;; convert back to a syntax object
      (define/public (sexpr) 
	(recertify src-stx))

      ;; adds certifications back to result of sexp, when needed
      (define/public (recertify stx)
	(let loop ([cert-stxes cert-stxes][stx stx])
	  (cond
	   [(null? cert-stxes) stx]
	   [else (loop (cdr cert-stxes)
		       (syntax-recertify stx 
					 (car cert-stxes)
					 recert-insp 
					 #f))])))

      ;; returns cert stxes
      (define/public (get-cert-stxes)
	cert-stxes)
      
      ;; merges cert info from another expression
      (define/public (merge-certs exp)
	(set! cert-stxes 
	      (append (filter (lambda (i) (not (memq i cert-stxes)))
			      (send exp get-cert-stxes))
		      cert-stxes)))
      
      ;; list of body exprs (avoids redundant `begin', just for
      ;; readability)
      (define/public (body-sexpr) (list (sexpr)))

      (super-instantiate ())))

  (define (get-sexpr o) (send o sexpr))
  (define (get-body-sexpr o) (send o body-sexpr))

  (define-struct bucket (mutated? inited-before-use?))

  (define (global-bucket table stx)
    (let ([l (hash-table-get table (syntax-e stx) (lambda () null))])
      (let ([s (ormap (lambda (b)
			(and (module-identifier=? stx (car b))
			     (cdr b)))
		      l)])
	(if s
	    s
	    (let ([s (make-bucket #f #f)])
	      (hash-table-put! table (syntax-e stx) (cons (cons stx s) l))
	      s)))))

  (define-struct tables (global-ht et-global-ht))

  (define global%
    (class exp% 
      (init-field trans? tables needs-top?)
      (super-instantiate ())
      (inherit-field src-stx) ;; The identifier
      (inherit-field cert-stxes) ;; The identifier
      
      (define mbind #f)
      (define bucket (global-bucket ((if trans? tables-et-global-ht tables-global-ht) tables) src-stx))
      (define/private (get-mbind!)
        (unless mbind
          (set! mbind ((if trans?
                           identifier-transformer-binding 
                           identifier-binding)
                       src-stx))))
      (define/public (orig-name)
        (get-mbind!)
        (if (pair? mbind)
            (cadr mbind)
            (syntax-e src-stx)))

      (define/public (is-kernel?)
        (get-mbind!)
        (and (pair? mbind)
             (eq? (car mbind) '#%kernel)))

      (define/public (is-trans?) trans?)

      (define/public (is-mutated?) (bucket-mutated? bucket))

      (define/override (no-side-effect?)
        ;; If not built in, could raise exn
	(is-kernel?))

      (define/override (get-result-arity) 1)

      (define/override (valueable?) (or (bucket-inited-before-use? bucket)
                                        (is-kernel?)))

      (define/override (can-dup/move?) (valueable?))

      (define/override (clone env) (make-object global% trans? tables needs-top? src-stx cert-stxes))

      (define/override (global->local env)
        (or (ormap (lambda (e)
                     (and (module-identifier=? (car e) src-stx)
                          (make-object ref% (cdr e) src-stx cert-stxes)))
                   env)
            this))

      (inherit recertify)
      (define/override (sexpr)
	(recertify 
	 (if needs-top?
	     (with-syntax ([stx src-stx])
	       (syntax (#%top . stx)))
	     src-stx)))
      
      (define/public (set-mutated) (set-bucket-mutated?! bucket #t))
      (define/public (set-inited) (set-bucket-inited-before-use?! bucket #t))))

  (define binding% 
    (class exp% 
      (init-field always-inited?)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      (define value #f)
      (define used 0)
      (define mutated? #f)
      (define inited? always-inited?)
      (define escape #f)
      
      (define/public (is-used?) (positive? used))
      (define/public (is-mutated?) mutated?)
      (define/public (is-inited?) inited?)
      (define/public (get-use-count) used)

      (define/public (set-mutated) (set! mutated? #t))
      (define/public (set-inited) (set! inited? #t))
      (define/public (set-value v) (set! value v))

      (define/public (escapes?) escape)
      (define/public (set-escapes x) (set! escape #t))
      
      (define/public (clone-binder env) 
        (make-object binding% 
          always-inited?
          (datum->syntax-object
           #f
           (gensym (syntax-e src-stx))
           src-stx
	   cert-stxes)))


      (define/override (reset-varflags)
        (set! used 0)
        (set! mutated? #f)
        (set! inited? always-inited?))
      (define/override (set-known-values)
        (set! used (add1 used))
        (unless inited?
          (set! mutated? #t)))

      (define/override (valueable?) (and inited? (not mutated?)))

      (define/override (drop-uses) (set! used (sub1 used)))

      (define/override (get-value) 
        (and (not mutated?)
             value
             (send value get-value)))
      
      (inherit recertify)
      (define/override (sexpr)
        ;; `(==lexical== ,name ,used ,mutated? ,inited? ,(get-value))
        (recertify src-stx)
        )
      (define/public (orig-name)
        (syntax-e src-stx))))

  (define ref% 
    (class exp% 
      (init-field binding)
      (super-instantiate ())
      (inherit-field src-stx) ;; The identifier
      
      
      (define/public (is-used?) (send binding is-used?))
      (define/public (is-mutated?) (send binding is-mutated?))
      (define/public (is-inited?) (send binding is-inited?))
      
      (define/public (get-use-count) (send binding get-use-count))

      (define/public (set-mutated) (send binding set-mutated))
      (define/public (set-inited) (send binding set-inited))
      (define/public (set-value v) (send binding set-value v))

      (define/override (set-known-values) (send binding set-known-values))

      (define/override (valueable?) (send binding valueable?))
      (define/override (can-dup/move?) (valueable?))

      (define/override (drop-uses) (send binding drop-uses))

      (define/override (get-result-arity) 1)

      (define/override (get-value) (send binding get-value))

      (define/override (escape) 
        (send binding set-escape))
      
      (define/override (simplify ctx)
        (if (context-need ctx)
            (let ([v (get-value)])
              (if (and v (send v can-dup/move?))
                  (begin
                    (drop-uses)
                    (send v simplify ctx))
                  this))
            (begin
              (drop-uses)
              (make-object void% src-stx))))

      (define/override (clone env) (lookup-clone binding this env))
      (define/override (substitute env) (lookup-clone binding this env))

      (inherit recertify)
      (define/override (sexpr) 
        (let ([x (send binding sexpr)])
	  (recertify (datum->syntax-object
		      x
		      (syntax-e x)
		      src-stx))))

      (define/public (get-binding) binding)
      (define/public (orig-name) (send binding orig-name))))
      

  (define begin% 
    (class exp%
      (init-field subs)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      (inherit merge-certs)
      
      (define/override (nonbind-sub-exprs) subs)
      (define/override (set-nonbind-sub-exprs s) (set! subs s))

      (define/override (get-result-arity)
        (if (null? subs)
            'unknown
            (let loop ([subs subs])
              (if (null? (cdr subs))
                  (send (car subs) get-result-arity)
                  (loop (cdr subs))))))
      
      (define/override (simplify ctx)
        (set! subs
              (let loop ([subs subs])
                (cond
                  [(null? subs) null]
                  [(null? (cdr subs))
                   (list (send (car subs) simplify ctx))]
                  [else
                   (let ([r (send (car subs) simplify (need-none ctx))]
                         [rest (loop (cdr subs))])
                     (cond
                       [(send r no-side-effect?)
                        (send r drop-uses)
                        rest]
                       [(is-a? r begin%)
			(merge-certs r)
                        (append (send r nonbind-sub-exprs)
                                rest)]
                       [else (cons r rest)]))])))
        (if (and (pair? subs)
                 (null? (cdr subs)))
            (let ([v (car subs)])
	      (send v merge-certs this)
	      v)
            this))

      (define/override (clone env)
        (make-object begin% 
          (map (lambda (x) (send x clone env)) 
               subs)
          src-stx
	  cert-stxes))

      (inherit recertify)
      (define/override (sexpr)
	(with-syntax ([(body ...) (body-sexpr)])
	  (syntax/loc src-stx (begin body ...))))

      (define/override (body-sexpr)
	(map (lambda (e) (recertify (get-sexpr e))) subs))))
  
  (define top-def% 
    (class exp% 
      (init-field formname varnames expr tables)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      (define globals #f)
      
      (define/override (nonbind-sub-exprs) (list expr))
      (define/override (set-nonbind-sub-exprs s) (set! expr (car s)))

      (define/override (get-result-arity) 1)

      (define/override (no-side-effect?) #f)
      (define/override (valueable?) #f)
      
      (define/override (clone env) (make-object top-def% 
                                     formname
                                     varnames 
                                     (send expr clone env)
                                     tables
                                     src-stx
				     cert-stxes))

      (inherit recertify)
      (define/override (sexpr)
        (with-syntax ([formname formname]
                      [(varname ...) varnames]
                      [rhs (get-sexpr expr)])
	  (recertify
	   (syntax/loc src-stx (formname (varname ...) rhs)))))

      (define/public (get-vars) varnames)
      (define/public (get-rhs) expr)

      ;; Like get-vars, but return global% objects, instead.
      ;; Useful because the global% object has the global variable bucket info.
      (define/public (get-globals)
        (unless globals
          (set! globals
                (map (lambda (v)
                       (make-object global% #f tables #f v))
                     varnames)))
        globals)))

  (define variable-def% 
    (class top-def% 
      (init varnames expr tables stx)
      
      (super-instantiate ((quote-syntax define-values) varnames expr tables stx))))

  (define syntax-def% 
    (class top-def% 
      (init varnames expr tables stx)
      (super-instantiate ((quote-syntax define-syntaxes) varnames expr tables stx))))

  (define for-syntax-def% 
    (class top-def% 
      (init varnames expr tables stx)
      (super-instantiate ((quote-syntax define-values-for-syntax) varnames expr tables stx))))

  (define (install-values vars expr)
    (when (= 1 (length vars))
      (send (car vars) set-value expr)))

  (define constant% 
    (class exp% 
      (init-field val)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      
      (define/public (get-const-val) val)

      (define/override (get-value) this)
      
      (define/override (valueable?) #t)

      (define/override (can-dup/move?)
        (or (number? val)
            (boolean? val)
            (char? val)
            (symbol? val)
            (void? val)))

      (define/override (get-result-arity) 1)

      (define/override (simplify ctx)
        (cond
          [(eq? 'bool (context-need ctx))
           (if (boolean? val)
               this
               (make-object constant% #t src-stx))]
          [(context-need ctx)
           (cond
             [(eq? val (void))
              (make-object void% src-stx)]
             [else this])]
          [else (make-object void% src-stx)]))
      
      (define/override (clone env) (make-object constant% val src-stx cert-stxes))
      
      (inherit recertify)
      (define/override (sexpr)
	(let ([vstx (datum->syntax-object (quote-syntax here) val src-stx)])
	  (cond
	   [(or (number? val)
		(string? val)
		(boolean? val)
		(char? val))
	    vstx]
	   [(syntax? val)
	    (with-syntax ([vstx vstx])
	      (recertify
	       (syntax (quote-syntax vstx))))]
	    [else
	     (with-syntax ([vstx vstx])
	       (syntax (quote vstx)))])))))

  (define void%
    (class constant% 
      (init stx)
      (super-instantiate ((void) stx))
      (inherit-field src-stx cert-stxes)

      (define/override (sexpr) (quote-syntax (void)))

      (define/override (simplify ctx)
        (if (eq? 'bool (context-need ctx))
            (make-object constant% #t src-stx)
            this))

      (define/override (clone env) (make-object void% src-stx cert-stxes))))


  (define app%
    (class exp% 
      (init-field rator rands tables)
      (super-instantiate ())
      (inherit-field src-stx
		     cert-stxes)
      (inherit merge-certs)

      (define known-single-result? #f)

      (inherit set-known-value)
      
      (define/private (known-single-result v)
        (set! known-single-result? #t)
        (set-known-value v)
        v)

        
      (define/override (nonbind-sub-exprs) (cons rator rands))
      (define/override (set-nonbind-sub-exprs s) 
        (set! rator (car s))
        (set! rands (cdr s)))

      (define/override (no-side-effect?)
        ;; Note: get-result-arity assumes #t result => single value
        ;;
        ;; Some prims are known to be side-effect-free (including no errors)
        ;; get-result-arity assumes 1 when this returns #t
        (or known-single-result?
            (and (rator . is-a? . global%)
                 (send rator is-kernel?)
                 (memq (send rator orig-name) effectless-prims)
                 (andmap (lambda (rand) (send rand no-side-effect?))
                         rands))))

      (define/override (valueable?)
        (and (rator . is-a? . global%)
             (send rator is-kernel?)
             (not (memq (send rator orig-name)
                        (non-valueable-prims)))
             (super valueable?)))

      (define/override (get-result-arity)
        (if (or known-single-result? (no-side-effect?))
            1
            'unknown))

      (define/override (escape)
        (send rator escape)
        (cond
          ((bound-identifier=? #'vector-ref (send rator get-stx)) (void))
          ((and (bound-identifier=? #'vector-set! (send rator get-stx))
                (not (null? rands)))
           (map (lambda (x) (send x escape)) (cdr rands)))
          (else
           (map (lambda (x) (send x escape)) rands))))

      (define/override (simplify ctx)
        (super simplify ctx)
        (cond
          ;; ((lambda (a ...) ...) v ...) => (let ([a v] ...) ...)
          [(and (is-a? rator lambda%)
                (send rator can-inline?))
	   (if (send rator arg-body-exists? (length rands))
	       (begin
		 (send rator drop-other-uses (length rands))
		 (let-values ([(vars body) (send rator arg-vars-and-body (length rands))])
		   (for-each (lambda (var rand)
			       (install-values (list var) rand))
			     vars rands)
		   (let ([let-form (make-object let%
						(map list vars)
						rands
						body
						src-stx
						cert-stxes)])
		     (send let-form merge-certs this)
		     (send let-form merge-certs rator)
		     (send let-form simplify ctx))))
	       (begin
		 (unless (send rator arg-count-ok? (length rands))
		   (warning "immediate procedure called with wrong number of arguments"
			    this))
		 this))]
          
          ;; constant folding
          [(and (is-a? rator global%)
                (memq (send rator orig-name) foldable-prims)
                (send rator is-kernel?)
                (andmap (lambda (x) (is-a? x constant%)) rands))
           (if (eq? (send rator orig-name) 'void)
               (make-object void% src-stx)
               (let ([vals (map (lambda (x) (send x get-const-val)) rands)]
                     [f (dynamic-require 'mzscheme (send rator orig-name))])
                 (with-handlers ([exn:fail? (lambda (x) 
					      (fprintf (current-error-port)
						       "constant calculation error: ~a~n"
						       (exn-message x))
					      this)])
                   (known-single-result
                    (send (make-object constant% (apply f vals) src-stx)
                          simplify ctx)))))]
          
          ;; (+ x 1) => (add1 x)
          [(and (is-a? rator global%)
                (send rator is-kernel?)
                (eq? (send rator orig-name) '+)
                (= 2 (length rands))
                (or (and (is-a? (car rands) constant%)
                         (eq? 1 (send (car rands) get-const-val)))
                    (and (is-a? (cadr rands) constant%)
                         (eq? 1 (send (cadr rands) get-const-val)))))
           (make-object app% 
             (make-object global% (send rator is-trans?) tables #f (quote-syntax add1))
             (list
              (if (and (is-a? (car rands) constant%)
                       (eq? 1 (send (car rands) get-const-val)))
                  (cadr rands)
                  (car rands)))
             tables
             src-stx
	     cert-stxes)]
          ;; (- x 1) => (sub1 x)
          [(and (is-a? rator global%)
                (send rator is-kernel?)
                (eq? (send rator orig-name) '-)
                (= 2 (length rands))
                (and (is-a? (cadr rands) constant%)
                     (eq? 1 (send (cadr rands) get-const-val))))
           (make-object app% 
             (make-object global%  (send rator is-trans?) tables #f (quote-syntax sub1))
             (list (car rands))
             tables
             src-stx
	     cert-stxes)]
          
          ;; (car x) where x is known to be a list construction
          [(and (is-a? rator global%)
                (send rator is-kernel?)
                (let-values ([(pos len) (case (send rator orig-name) 
                                          [(car) (values 0 1)]
                                          [(cadr) (values 1 1)]
                                          [(caddr) (values 2 1)]
                                          [(cadddr) (values 3 1)]
                                          [(list-ref) (values (and (= 2 (length rands))
                                                                   (let ([v (send (cadr rands) get-value)])
                                                                     (and (v . is-a? . constant%)
                                                                          (send v get-const-val))))
                                                              2)]
                                          [else (values #f #f)])])
                  (and (number? pos)
                       (= len (length rands))
                       (and ((car rands) . is-a? . ref%)
                            (let ([val (send (car rands) get-value)])
                              (and (val . is-a? . app%)
                                   (send val get-list-ref pos)))))))
           =>
           (lambda (val)
             (send (car rands) drop-uses)
             (known-single-result val))]
          
          ;; (memv x '(c ...)) in a boolean context => (if (eq[v]? x 'c) ...)
          ;; relevant to the output of `case'
          [(and (eq? (context-need ctx) 'bool) 
                (is-a? rator global%)
                (send rator is-kernel?)
                (eq? (send rator orig-name) 'memv)
                (= 2 (length rands))
                (is-a? (car rands) ref%)
                (is-a? (cadr rands) constant%)
                (list? (send (cadr rands) get-const-val)))
           (let ([xformed
                  (let ([l (send (cadr rands) get-const-val)]
                        [l-stx (send (cadr rands) get-stx)]
                        [false (make-object constant% #f (datum->syntax-object #f #f))]
                        [true (make-object constant% #t (datum->syntax-object #f #t))])
                    (if (null? l)
                        false
                        (let loop ([l l])
                          (let ([test
                                 (make-object app%
                                   (make-object global%
                                     (send rator is-trans?)
                                     tables
                                     #f
                                     (let ([a (car l)])
                                       (if (or (symbol? a)
                                               (and (number? a)
                                                    (exact? a)
                                                    (integer? a)
                                                    ;; fixnums:
                                                    (<= (- (expt 2 29))
                                                        a
                                                        (expt 2 29))))
                                           (quote-syntax eq?)
                                           (quote-syntax eqv?))))
                                   (list
                                    (car rands)
                                    (make-object constant% 
                                      (car l)
                                      l-stx))
                                   tables
                                   src-stx
				   cert-stxes)])
                            (cond
                              [(null? (cdr l)) test]
                              [else (let ([rest (loop (cdr l))])
                                      ;; increment use count:
                                      (send (car rands) set-known-values)
                                      (make-object if%
                                        test
                                        true
                                        rest
                                        src-stx
					cert-stxes))])))))])
	     (send xformed merge-certs this)
             (send xformed simplify ctx))]
          
          ;; (values e) where e has result arity 1
          [(and (is-a? rator global%)
                (send rator is-kernel?)
                (eq? 'values (send rator orig-name))
                (= 1 (length rands))
                (equal? 1 (send (car rands) get-result-arity)))
	   (send (car rands) merge-certs this)
           (known-single-result (car rands))]
          
          ;; Check arity of other calls to primitives
          [(and (is-a? rator global%)
                (send rator is-kernel?))
	   (let ([f (dynamic-require 'mzscheme (send rator orig-name))])
	     (cond
	      [(not (procedure? f))
	       (warning "call of non-procedure" this)]
	      [(not (procedure-arity-includes? f (length rands)))
	       (warning "primitive called with wrong number of arguments" this)]))
           this]

          ;; inlining
          [(and #f ;; disabled!
		(> (fuel) 0)
                (or (is-a? rator ref%) (is-a? rator global%))
                (is-a? (send rator get-value) lambda%)
                (not (send (send rator get-value) get-simplifying-body)))
           (let ([f (send (send rator get-value) clone null)])
             (send rator drop-uses)
             (set! rator f)
             (send f set-known-values)
             ;; Now we have ((lambda ...) ...). Go again.
             (fuel (sub1 (fuel)))
             (if (= (fuel) (sub1 max-fuel))
                 (begin0
                   (simplify ctx)
                   (fuel max-fuel))
                 (simplify ctx)))]

          ;; Check arity of a call to a known (non-primitive) function
          [(and (or (is-a? rator ref%) (is-a? rator global%))
                (is-a? (send rator get-value) lambda%))
           (let ([f (send rator get-value)])
	     (unless (send f arg-count-ok? (length rands))
	       (warning "procedure called with wrong number of arguments"
			this))
	     this)]
          
          [else this]))
      
      (define/override (clone env) (make-object app%
                                     (send rator clone env)
                                     (map (lambda (rand)
                                            (send rand clone env))
                                          rands)
                                     tables
                                     src-stx
				     cert-stxes))

      (inherit recertify)
      (define/override (sexpr)
	(recertify
	 (keep-mzc-property
	  (with-syntax ([rator (get-sexpr rator)]
			[(rand ...) (map get-sexpr rands)])
	    (syntax/loc src-stx (rator rand ...)))
	  src-stx)))

      ;; Checks whether the expression is an app of `values'
      ;; to a particular set of bindings.
      (define/public (is-values-of? args)
        (and (rator . is-a? . global%)
             (send rator is-kernel?)
             (eq? (send rator orig-name) 'values)
             (= (length rands) (length args))
             (andmap
              (lambda (rand arg)
                (and (rand . is-a? . ref%)
                     (eq? arg (send rand get-binding))))
              rands args)))

      ;; If app constructs a list and the nth element can be
      ;;  safely extracted, then extract it.
      (define/public (get-list-ref n)
        (and (rator . is-a? . global%)
             (send rator is-kernel?)
             (eq? 'list (send rator orig-name))
             ((length rands) . > . n)
             (let ([i (list-ref rands n)])
               (if (send i can-dup/move?)
                   i
                   #f))))))

  (define lambda% 
    (class exp% 
      (init-field varss normal?s bodys)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      (define simplifying-body #f)
      
      
      (inherit drop-uses)
      
      
      (define/private (multarity-ize l)
        (if (null? (cdr l))
            (car l)
            (cons (car l)
                  (multarity-ize (cdr l)))))

      (define/public (get-simplifying-body) simplifying-body)
      
      (define/public (multi?) (or (null? bodys)
                                  (pair? (cdr bodys))))
      
      (define/public (arg-body-exists? n)
        (ormap (lambda (vs n?) (and n? (= n (length vs))))
               varss normal?s))
      (define/public (arg-count-ok? n)
        (ormap (lambda (vs n?) (or (and n? (= n (length vs)))
				   (and (not n?) (n . >= . (sub1 (length vs))))))
               varss normal?s))
      (define/public (arg-vars-and-body n)
        (let loop ([varss varss][normal?s normal?s][bodys bodys])
          (if (and (car normal?s)
                   (= (length (car varss)) n))
              (values (car varss) (car bodys))
              (loop (cdr varss) (cdr normal?s) (cdr bodys)))))
      
      (define/public (drop-other-uses n)
        (let loop ([n n][varss varss][normal?s normal?s][bodys bodys])
          (unless (null? varss)
            (let ([n (if (and (car normal?s)
                              (= (length (car varss)) n))
                         -1
                         (begin
                           (send (car bodys) drop-uses)
                           n))])
              (loop n (cdr varss) (cdr normal?s) (cdr bodys))))))
      
      (define/public (can-inline?)
        (not (syntax-property src-stx 'mzc-cffi)))
      
      (define/override (bind-sub-exprs) (apply append varss))
      (define/override (nonbind-sub-exprs) bodys)
      (define/override (set-nonbind-sub-exprs s) (set! bodys s))
      
      (define/override (no-side-effect?) #t)
      (define/override (get-result-arity) 1)
      
      (define/override (valueable?) #t)
	
      (define/override (simplify ctx)
        (if (eq? 'bool (context-need ctx))
            (begin
              (drop-uses)
              (make-object constant% #t src-stx))
            (begin
              (set! simplifying-body #t)
              (begin0
                (super simplify ctx)
                (set! simplifying-body #f)))))
      
      (define/override (clone env)
        (let ([varss+bodys
               (let loop ([varss varss][bodys bodys])
                 (if (null? varss)
                     null
                     (let* ([vars (car varss)]
                            [new-vars (map (lambda (v) (send v clone-binder env))
                                           vars)])
                       (cons
                        (cons new-vars
                              (send (car bodys)
                                    clone (append (map cons vars new-vars)
                                                  env)))
                        (loop (cdr varss) (cdr bodys))))))])
          (make-object lambda%
            (map car varss+bodys)
            normal?s
            (map cdr varss+bodys)
            src-stx
	    cert-stxes)))

      (inherit recertify)
      (define/override (sexpr)
        (with-syntax ([(vars ...)
                       (map (lambda (vars normal?)
                              (let ([vs (map get-sexpr vars)])
                                (if normal?
                                    vs
                                    (multarity-ize vs))))
                            varss normal?s)]
                      [(body ...)
                       (map (lambda (body)
                              (get-body-sexpr body))
                            bodys)])
	  (recertify
	   (keep-mzc-property
	    (if (multi?)
		(syntax/loc src-stx
		  (case-lambda
		   [vars . body] ...))
		(with-syntax ([body (car (syntax->list (syntax (body ...))))])
		  (syntax/loc src-stx
		    (lambda vars ... . body))))
	    src-stx))))))

  (define local% 
    (class exp% 
      (init-field form varss rhss body)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      (inherit merge-certs)
      
      (define/public (get-rhss) rhss)
      (define/public (get-varss) varss)
      (define/public (get-body) body)

      (define/override (bind-sub-exprs) (apply append varss))
      (define/override (nonbind-sub-exprs) (cons body rhss))
      (define/override (set-nonbind-sub-exprs s) 
        (set! body (car s))
        (set! rhss (cdr s)))

      (define/override (get-result-arity) (send body get-result-arity))
      
      (define/override (simplify ctx)
        (set! rhss (map (lambda (rhs vars) 
                          (send rhs simplify 
                                (make-context 'all
                                              (append vars (context-indef ctx)))))
                        rhss varss))
        (set! body (send body simplify ctx))
        
        ;; Drop unused constant bindings
        (set!-values (varss rhss)
                     (let loop ([varss varss][rhss rhss])
                       (cond
                         [(null? varss) (values null null)]
                         [else (let-values ([(rest-vss rest-rhss)
                                             (loop (cdr varss) (cdr rhss))])
                                 (if (and (andmap (lambda (var) (not (send var is-used?)))
                                                  (car varss))
                                          (equal? (send (car rhss) get-result-arity)
                                                  (length (car varss)))
                                          (send (car rhss) no-side-effect?))
                                     (begin
                                       (send (car rhss) drop-uses)
                                       (values rest-vss rest-rhss))
                                     (values (cons (car varss) rest-vss)
                                             (cons (car rhss) rest-rhss))))])))
        
        (cond
          ;; (let-values ([(x) e]) (if e ... ...))
          ;;  is a pattern created by `or'
          [(and (is-a? body if%)
                (let ([t (send body get-if-test)])
                  (and (is-a? t binding%)
                       (= 1 (length varss))
                       (= 1 (length (car varss)))
                       (eq? (caar varss) t)
                       (= 1 (send t get-use-count)))))
           (make-object if%
             (car rhss)
             (send body get-if-then)
             (send body get-if-else)
             src-stx
	     cert-stxes)]
          [(null? varss)
	   (send body merge-certs this)
           (send body simplify ctx)]
          ;; (let-values [(x) y] ...) whether y is inited, and
          ;;  neither x nor y is mutated => replace x by y
          [(and (andmap (lambda (vars) (= 1 (length vars))) varss)
                (send (caar varss) valueable?)
                (andmap (lambda (rhs) (and (or (rhs . is-a? . ref%)
                                               (rhs . is-a? . global%))
                                           (send rhs valueable?)))
                        rhss))
	   (send body merge-certs this)
           (send body substitute
                 (map (lambda (vars rhs) (cons (car vars) 
                                               (if (rhs . is-a? . ref%)
                                                   (send rhs get-binding)
                                                   rhs)))
                      varss rhss))]
          
          [else
           this]))

      (define/override (clone env)
        (let* ([new-varss
                (map (lambda (vs)
                       (map (lambda (v) (send v clone-binder env))
                            vs))
                     varss)]
               [body-env (append
                          (map cons
                               (apply append varss)
                               (apply append new-varss))
                          env)]
               [letrec? (eq? form 'letrec-values)])
          (make-object (if letrec? letrec% let%)
            new-varss
            (map (lambda (rhs) 
                   (send rhs clone (if letrec? body-env env)))
                 rhss)
            (send body clone body-env)
            src-stx
	    cert-stxes)))

      (define/override (get-value) (send body get-value))
	
      (inherit recertify)
      (define/override (sexpr)
        (with-syntax ([form form]
                      [(vars ...)
                       (map (lambda (vars)
                              (map get-sexpr vars))
                            varss)]
                      [(rhs ...)
                       (map get-sexpr rhss)]
                      [(body ...) (get-body-sexpr body)])
	  (recertify
	   (syntax/loc src-stx
	     (form ([vars rhs] ...) 
		   body ...)))))))
    
  (define let%
    (class local% 
      (init -varss -rhss -body -stx -cert-stxes)
      (inherit get-varss get-rhss get-body)
      
      (define/override (set-known-values)
        (for-each (lambda (vars rhs) (install-values vars rhs))
                  (get-varss) (get-rhss))
        (super set-known-values))
      
      (super-instantiate ((quote-syntax let-values) -varss -rhss -body -stx -cert-stxes))))

  (define letrec%
    (class local% 
      (init -varss -rhss -body -stx -cert-stxes) 
      (inherit get-varss get-rhss)
      
      (define/override (set-known-values)
        (let loop ([varss (get-varss)][rhss (get-rhss)])
          (unless (null? varss)
            (when (send (car rhss) valueable?)
              (for-each (lambda (var) (send var set-inited))
                        (car varss))
              (loop (cdr varss) (cdr rhss)))))
        (for-each install-values (get-varss) (get-rhss))
        (super set-known-values))

      (super-instantiate ((quote-syntax letrec-values) -varss -rhss -body -stx -cert-stxes))))

  (define set!%
    (class exp% 
      (init-field var val)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      
      (define/override (nonbind-sub-exprs) (list var val))
      (define/override (set-nonbind-sub-exprs s) 
        (set! var (car s))
        (set! val (cadr s)))

      (define/override (no-side-effect?) #f)
      (define/override (valueable?) #f)
      (define/override (get-result-arity) 1)
	
      (define/override (set-known-values)
        (send var set-mutated)
        (send var set-known-values) ; increments use
        (send val set-known-values))
	
      (define/override (set-mutability)
        (send var set-mutated)
	(super set-mutability))
	
      (define/override (clone env)
        (make-object set!% 
          (send var clone env)
          (send val clone env)
          src-stx
	  cert-stxes))

      (inherit recertify)
      (define/override (sexpr)
	(with-syntax ([var (get-sexpr var)]
		      [val (get-sexpr val)])
	  (recertify
	   (syntax/loc src-stx
	     (set! var val)))))))

  (define if%
    (class exp% 
      (init-field test then else)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      
      (define/public (get-if-test) test)
      (define/public (get-if-then) then)
      (define/public (get-if-else) else)

      (define/override (nonbind-sub-exprs) (list test then else))
      (define/override (set-nonbind-sub-exprs s)
        (set! test (car s))
        (set! then (cadr s))
        (set! else (caddr s)))
	
      (define/override (get-result-arity)
        (let ([t (send then get-result-arity)]
              [e (send else get-result-arity)])
          (if (equal? t e)
              t
              'unknown)))
      
      (define/override (simplify ctx)
        (set! test (send test simplify (need-bool ctx)))
        (set! then (send then simplify ctx))
        (set! else (send else simplify ctx))

        ;; (if xvar xvar y) when need bool
        ;;   => (if xvar #t y)
        (when (and (eq? 'bool (context-need ctx))
                   (is-a? test binding%)
                   (eq? test then))
          (send then drop-uses)
          (set! then (make-object constant% #t src-stx)))
        (when (and (eq? 'bool (context-need ctx))
                   (eq? test else)
                   (is-a? test binding%))
          (send else drop-uses)
          (set! else (make-object constant% #f src-stx)))
        
        
        (cond
          ;; Constant switch
          [(is-a? test constant%)
           (if (eq? (send test get-const-val) #f)
               (begin
                 (send test drop-uses)
                 (send then drop-uses)
                 else)
               (begin
                 (send test drop-uses)
                 (send else drop-uses)
                 then))]
          
          ;; (if (if x y #f) a (void))
          ;;    => (if x (if y a (void)) (void))
          [(and (is-a? test if%)
                (is-a? else void%)
                (let ([c (send test get-if-else)])
                  (and (is-a? c constant%)
                       (eq? #f (send c get-const-val)))))
           (send
            (make-object if%
              (send test get-if-test)
              (make-object if%
                (send test get-if-then)
                then
                (make-object void% src-stx)
                src-stx
		cert-stxes)
              (make-object void% src-stx)
              src-stx
	      cert-stxes)
            simplify ctx)]
          
          [else this]))

      (define/override (clone env) 
        (make-object if%
          (send test clone env)
          (send then clone env)
          (send else clone env)
          src-stx
	  cert-stxes))
      
      (inherit recertify)
      (define/override (sexpr)
        (with-syntax ([test (get-sexpr test)]
                      [then (get-sexpr then)])
	  (recertify
	   (if (else . is-a? . void%)
	       (syntax/loc src-stx
		 (if test then))
	       (with-syntax ([else (get-sexpr else)])
		 (syntax/loc src-stx
		   (if test then else)))))))))

  (define begin0%
    (class exp% 
      (init-field first rest)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)
      
      (define/override (nonbind-sub-exprs) (list first rest))
      (define/override (set-nonbind-sub-exprs s)
        (set! first (car s))
        (set! rest (cadr s)))
	
      (define/override (get-result-arity) (send first get-result-arity))

      (define/override (simplify ctx)
        (set! first (send first simplify ctx))
        (set! rest (send rest simplify (need-none ctx)))
        (if (send rest no-side-effect?)
            (begin
              (send rest drop-uses)
	      (send first merge-certs this)
              first)
            this))
	
      (define/override (clone env) 
        (make-object begin0%
          (send first clone env)
          (send rest clone env)
	  src-stx
	  cert-stxes))
	
      (inherit recertify)
      (define/override (sexpr)
	(with-syntax ([first (get-sexpr first)]
		      [(rest ...) (get-body-sexpr rest)])
	  (recertify
	   (syntax/loc src-stx
	     (begin0 first rest ...)))))))

  (define wcm%
    (class exp% 
      (init-field key val body)
      (super-instantiate ())
      (inherit-field src-stx cert-stxes)

      (define/override (nonbind-sub-exprs) (list key val body))
      (define/override (set-nonbind-sub-exprs s)
        (set! key (car s))
        (set! val (cadr s))
        (set! body (caddr s)))

      (define/override (get-result-arity) (send body get-result-arity))
	
      (define/override (clone env) 
        (make-object wcm%
          (send key clone env)
          (send val clone env)
          (send body clone env)
	  src-stx
	  cert-stxes))

      (inherit recertify)
      (define/override (sexpr)
        (with-syntax ([key (get-sexpr key)]
                      [val (get-sexpr val)]
                      [body (get-sexpr body)])
	  (recertify
	   (syntax/loc src-stx
	     (with-continuation-mark key val body)))))))

  (define module%
    (class exp% 
      (init-field body et-body name init-req req-prov tables src-module-begin-stx)
      (super-instantiate ())
      (inherit-field src-stx)
      
      (define/override (reset-varflags)
        (for-each (lambda (e) (send e reset-varflags)) body)
        (for-each (lambda (e) (send e reset-varflags)) et-body))
      (define/override (set-known-values)
        ;; Assumes varflags are reset
        (for-each (lambda (e) (send e set-known-values)) (nonbind-sub-exprs)))

      (define/override (drop-uses)
        ;; Assumes varflags are reset
        (for-each (lambda (e) (send e drop-uses)) (nonbind-sub-exprs)))
	
      (define/override (no-side-effect?) #f)
      (define/override (valueable?) #f)

      (define/override (get-result-arity) 'unknown)

      (define/override (sub-exprs) (append (append et-body body)))
      (define/override (bind-sub-exprs) null)
      (define/override (nonbind-sub-exprs) (sub-exprs))
      (define/override (set-nonbind-sub-exprs l)
        (let-values ([(etb b)
                      (let loop ([l l][etb et-body][accum null])
                        (cond
                          [(null? etb)
                           (values (reverse! accum) l)]
                          [else (loop (cdr l) (cdr etb) (cons (car l)
                                                              accum))]))])
          (set! body body)
          (set! et-body etb)))
      
      ;; expose known bindings by converting a sequence of top-level
      ;;  expressions into a letrec:
      ;;   (define-values (a ...) body) ...
      ;;   => (define-values (a ... ...) 
      ;;          (letrec-values ([(a ...) body] ...) (values a ... ...)))
      (define/override (reorganize)
        (let ([-body (map (lambda (x) (send x reorganize)) body)]
              [-et-body (map (lambda (x) (send x reorganize)) et-body)])
          (let loop ([l -body][defs null])
            (cond
              [(and (pair? l) 
                    ((car l) . is-a? . variable-def%)
                    (not (ormap (lambda (v) (send v is-mutated?))
                                (send (car l) get-globals)))
                    (send (send (car l) get-rhs) valueable?))
               (for-each (lambda (g) (send g set-inited))
                         (send (car l) get-globals))
               (loop (cdr l)
                     (cons (car l) defs))]
              [else
               (if (null? defs)
                   (void) ; no reorganization
                   (let* ([defs (reverse defs)]
                          [varss
                           (map (lambda (def) (send def get-vars)) defs)]
                          [rhss
                           (map (lambda (def) (send def get-rhs)) defs)]
                          [lex-varss (map (lambda (vars)
                                            (map (lambda (var)
                                                   (make-object binding%
                                                     #t
                                                     (datum->syntax-object
                                                      #f
                                                      (syntax-e var)
                                                      var)))
                                                 vars))
                                          varss)]
                          [vars (apply append varss)]
                          [lex-vars (apply append lex-varss)]
                          [env (map cons vars lex-vars)])
                     (set! -body
                           (cons
                            (make-object variable-def%
                              vars
                              (make-object letrec%
                                lex-varss
                                (map (lambda (rhs)
                                       (send rhs global->local env))
                                     rhss)
                                (make-object app%
                                  (make-object global%
                                    #f
                                    tables
                                    #f
                                    (quote-syntax values))
                                  (map (lambda (var lex-var)
                                         (make-object ref% lex-var var))
                                       vars
                                       lex-vars)
                                  tables 
                                  (send (car defs) get-stx))
                                (send (car defs) get-stx)
				(send (car defs) get-cert-stxes))
                              tables
                              (send (car defs) get-stx))
                            l))))])
            (set! body -body)
            (set! et-body -et-body)))
        this)

      (define/override (deorganize)
        ;; Check for
        ;;   (define-values (a ... ...) 
        ;;       (letrec-values ([(a ...) body] ...) (values a ... ...)))
        ;;   => (define-values (a ...) body) ...
        (when (and (pair? body)
                   (let ([first (car body)])
                     (and (first . is-a? . variable-def%)
                          (let ([rhs (send first get-rhs)])
                            (and (rhs . is-a? . letrec%)
                                 (let ([lbody (send rhs get-body)]
                                       [lvarss (send rhs get-varss)])
                                   (and (lbody . is-a? . app%)
                                        (send lbody is-values-of? 
                                              (apply append lvarss)))))))))
          (let ([vars (send (car body) get-vars)]
                [bindingss (send (send (car body) get-rhs) get-varss)]
                [bodys (send (send (car body) get-rhs) get-rhss)])
            ;; split vars into varss:
            (let ([varss (let loop ([bindingss bindingss][vars vars])
                           (if (null? bindingss)
                               null
                               (let loop2 ([bindings (car bindingss)][vars vars][accum null])
                                 (if (null? bindings)
                                     (cons (reverse! accum)
                                           (loop (cdr bindingss) vars))
                                     (loop2 (cdr bindings) (cdr vars) (cons (car vars) accum))))))]
                  [bindings (apply append bindingss)])
              (let ([env (map cons bindings 
                              (map (lambda (var) 
                                     (make-object global% #f tables #f var)) 
                                   vars))])
                (set! body
                      (append
                       (map (lambda (vars body)
                              (make-object variable-def%
                                vars
                                (send body substitute env)
                                tables
                                src-stx))
                            varss bodys)
                       (cdr body)))))))
        (super deorganize))

      (inherit recertify)
      (define/override (sexpr)
	(with-syntax ([name name]
		      [init-req init-req]
		      [(body ...) (map get-sexpr body)]
		      [(et-body ...) (map get-sexpr et-body)]
		      [(req-prov ...) (map get-sexpr req-prov)])
          (with-syntax ([body 
                         (syntax-recertify #'(#%plain-module-begin
                                              req-prov ...
                                              body ...
                                              et-body ...)
                                           src-module-begin-stx
                                           recert-insp
                                           #f)])
            (recertify
             (syntax/loc src-stx
               (module name init-req body))))))
      (define/override (body-sexpr)
	(list (sexpr)))))

  ;; requires and provides should really be ignored:
  (define require/provide%
    (class exp% 

      (define/override (valueable?) #f)
      (define/override (no-side-effect?) #f)
      (super-instantiate ())))
  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Warning reporting
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (print-warning msg exp)
    (let ([stx (send exp get-stx)])
      (when (syntax-source stx)
	(fprintf (current-output-port) "~a:" (syntax-source stx))
	(cond
	 [(syntax-column stx)
	  (fprintf (current-output-port) "~a:~a:" 
		   (syntax-line stx)
		   (syntax-column stx))]
	 [(syntax-position stx)
	  (fprintf (current-output-port) ":~a:" 
		   (syntax-position stx))])
	(fprintf (current-output-port) " "))
      (fprintf (current-output-port) 
	       "~a: ~e~n"
	       msg
	       (syntax-object->datum (send exp sexpr)))))

  (define (warning msg exp)
    ; (print-warning msg exp)
    (void))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Parser
  ;; converts a syntax object to an exp%
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (parse-args env args)
    (let-values ([(norm? ids)
		  (syntax-case args ()
		    [id
		     (identifier? (syntax id))
		     (values #f (list (syntax id)))]
		    [(id ...)
		     (values #t (syntax->list args))]
		    [_else (values #f
				   (let loop ([args args])
				     (syntax-case args ()
				       [id (identifier? args) (list args)]
				       [(id . rest)
					(cons (syntax id) (loop (syntax rest)))])))])])
      (let ([bindings (map (lambda (id) (make-object binding% #t id)) ids)])
	(values
	 (append (map cons ids bindings) env)
	 bindings
	 norm?))))

  (define (parse-let % rec? stx env loop)
    (syntax-case stx ()
      [(_ ([vars rhs] ...) . body)
       (let* ([varses (syntax->list (syntax (vars ...)))]
	      [rhses (syntax->list (syntax (rhs ...)))]
	      [var-objses (map (lambda (vars)
				 (map (lambda (var)
					(make-object binding% (not rec?) var))
				      (syntax->list vars)))
			       varses)]
	      [body-env (append
			 (apply
			  append
			  (map (lambda (var-objs vars)
				 (map cons
				      (syntax->list vars)
				      var-objs))
			       var-objses
			       varses))
			 env)])
	 (make-object %
           var-objses
           (map (lambda (rhs)
                  (loop rhs (if rec? body-env env)))
                rhses)
           (loop (syntax (begin . body)) body-env)
           stx
	   (list stx)))]))

  (define (stx-bound-assq ssym l)
    (ormap (lambda (p)
	     (and (bound-identifier=? ssym (car p))
		  p))
	   l))

  (define (lookup-clone binding var env)
    (let ([s (assq binding env)])
      (if s
	  (let ([b (cdr s)])
	    (if (b . is-a? . binding%)
		(make-object ref% b (send var get-stx) (send var get-cert-stxes))
		;; it's a global%:
		b))
	  var)))

  (define dummy 'dummy) ; for #%variable-reference

  (define (make-parse top?)
    (lambda (stx env trans? in-module? tables)
      (kernel-syntax-case stx trans?
	[id
	 (identifier? stx)
	 (let ([a (stx-bound-assq stx env)])
	   (if a
	       (make-object ref% (cdr a) stx)
	       (make-object global% trans? tables #f stx)))]

	[(#%top . id)
	 (make-object global% trans? tables #t (syntax id))]
	
	[(#%datum . val)
	 (make-object constant% (syntax-object->datum (syntax val)) stx)]

	[(#%variable-reference . val)
	 (make-object constant% (#%variable-reference dummy) stx)]

	[(define-values names rhs)
	 (make-object variable-def% 
           (syntax->list (syntax names))
           (parse (syntax rhs) env #f in-module? tables)
           tables
           stx)]
	
	[(define-syntaxes names rhs)
	 (make-object syntax-def% 
           (syntax->list (syntax names))
           (parse (syntax rhs) env #t in-module? tables)
           tables
           stx)]
	
	[(define-values-for-syntax names rhs)
	 (make-object for-syntax-def% 
           (syntax->list (syntax names))
           (parse (syntax rhs) env #t in-module? tables)
           tables
           stx)]
	
	[(begin . exprs)
	 (make-object begin%
           (map (lambda (e) ((if top? parse-top parse) e env trans? in-module? tables))
                (syntax->list (syntax exprs)))
           stx)]

	[(begin0 expr . exprs)
	 (make-object begin0%
           (parse (syntax expr) env trans? in-module? tables)
           (parse (syntax (begin . exprs)) env trans? in-module? tables)
           stx)]

	[(quote expr)
	 (make-object constant% (syntax-object->datum (syntax expr)) stx)]

	[(quote-syntax expr)
	 (make-object constant% (syntax expr) stx)]

	[(lambda args . body)
	 (let-values ([(env args norm?) (parse-args env (syntax args))])
	   (make-object lambda%
             (list args)
             (list norm?)
             (list (parse (syntax (begin . body)) env trans? in-module? tables))
             stx))]

	[(case-lambda [args . body] ...)
	 (let-values ([(envs argses norm?s)
		       (let ([es+as+n?s
			      (map
			       (lambda (args)
				 (let-values ([(env args norm?) (parse-args env args)])
				   (cons env (cons args norm?))))
			       (syntax->list (syntax (args ...))))])
			 (values
			  (map car es+as+n?s)
			  (map cadr es+as+n?s)
			  (map cddr es+as+n?s)))])
	   (make-object lambda%
             argses
             norm?s
             (map (lambda (env body)
                    (with-syntax ([body body])
                      (parse (syntax (begin . body)) env trans? in-module? tables)))
                  envs
                  (syntax->list (syntax (body ...))))
             stx))]
	
	[(let-values . _)
	 (parse-let let% #f stx env
		    (lambda (b env) (parse b env trans? in-module? tables)))]
	[(letrec-values . _)
	 (parse-let letrec% #t stx env
		    (lambda (b env) (parse b env trans? in-module? tables)))]

	[(set! var rhs)
	 (make-object set!% 
           (parse (syntax var) env trans? in-module? tables)
           (parse (syntax rhs) env trans? in-module? tables)
           stx)]

	[(if test then . else)
	 (make-object if%
           (parse (syntax test) env trans? in-module? tables)
           (parse (syntax then) env trans? in-module? tables)
           (if (null? (syntax-e (syntax else)))
               (parse (quote-syntax (#%app void)) env trans? in-module? tables)
               (parse (car (syntax-e (syntax else))) env trans? in-module? tables))
           stx)]
	
	[(with-continuation-mark k v body)
	 (make-object wcm% 
           (parse (syntax k) env trans? in-module? tables)
           (parse (syntax v) env trans? in-module? tables)
           (parse (syntax body) env trans? in-module? tables)
           stx)]
	
	[(#%app)
	 (make-object constant% null stx)]
	
	[(#%app func . args)
	 (make-object app% 
           (parse (syntax func) env trans? in-module? tables)
           (map (lambda (v) (parse v env trans? in-module? tables)) (syntax->list (syntax args)))
           tables
           stx)]

	[(module name init-require (#%plain-module-begin . body))
	 (let* ([body (map (lambda (x)
			     (parse x env #f #t tables))
			   (syntax->list (syntax body)))]
		[et-body
		 (filter (lambda (x) (or (x . is-a? . syntax-def%)
					 (x . is-a? . for-syntax-def%)))
			 body)]
		[rt-body
		 (filter (lambda (x) (not (or (x . is-a? . syntax-def%)
					      (x . is-a? . for-syntax-def%)
					      (x . is-a? . require/provide%))))
			 body)]
		[req-prov
		 (filter (lambda (x) (x . is-a? . require/provide%))
			 body)])
	   (make-object module%
             rt-body
             et-body
             (syntax name)
             (syntax init-require)
             req-prov
             tables 
             (syntax-case stx ()
               [(m n ir mb) #'mb])
             stx))]
        
	[(require . i) (make-object require/provide% stx)]
	[(require-for-syntax . i) (make-object require/provide% stx)]
	[(require-for-template . i) (make-object require/provide% stx)]
	[(provide i ...) (make-object require/provide% stx)]

	[else (error 'parse "unknown expression: ~a" (syntax-object->datum stx))])))

  (define parse (make-parse #f))
  (define parse-top (make-parse #t))

  (define (create-tables)
    (make-tables (make-hash-table) (make-hash-table)))

  
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Optimizer
  ;; the driver function
  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define optimize 
    (opt-lambda (e [for-mzc? #f])
      (let ([p (parse-top e null #f #f (create-tables))])
	(send p set-mutability)
        (send p reorganize)
	(send p set-known-values)
	(let ([p (send p simplify (make-context 'all null))])
	  (let ([v (get-sexpr (if for-mzc?
				  p
				  (send p deorganize)))])
	    v)))))
	
  (provide optimize))
