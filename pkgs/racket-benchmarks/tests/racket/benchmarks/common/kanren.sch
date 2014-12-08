;; smashed into benchmark form by Matthew

(define errorf error)

; like cout << arguments << args
; where argument can be any Scheme object. If it's a procedure
; (without args) it's executed rather than printed (like newline)

(define (cout . args)
  (for-each (lambda (x)
              (if (procedure? x) (x) (display x)))
            args))

(define cerr cout)

(define pntall (lambda v (write v) (newline)))
(define (_pretty-print v) (write v) (newline))

(define nl (string #\newline))

;; =========================================================================
;;  term.scm
;; =========================================================================

;		Terms, variables, substitutions, unification
;
; The appropriate prelude (e.g., chez-specific.scm) is assumed.

; Some terminology related to variables and substitutions
;
; A substitution subst is a finite map { xi -> ti ... }
; where xi is a logic variable.
; ti is a term ::= variable | Scheme-atom | (cons term term)
; We will sometimes call one `component' xi -> ti of a substitution
; a commitment, or a binding, of a variable xi to a term ti.
;
; A variable x is free in the substitution subst if x \not\in Dom(subst)
;
; Given a term t and a substitution subst, a weak reduction
;   t -->w  t'
; is defined as
;   x -->w subst[x]  if x is a var and x \in Dom(subst)
;   t -->w t otherwise
;
; A strong reduction
;   t -->s t' 
; is defined as
;   x -->s subst[x]  if x is a var and x \in Dom(subst)
;   (cons t1 t2) -->s (cons t1' t2')
;          where t1 -->s t1'  t2 -->s t2'
;   t -->s t otherwise
;
; The notion of reduction can be extended to substitutions themselves:
;   { xi -> ti ...} -->w { xi -> ti' } where ti -> ti'
; ditto for -->s.
; Let -->w* be a reflexive transitive closure of -->w, and 
; let -->w! be a fixpoint of -->w. Ditto for -->s* and -->s!
; For acyclic substitutions, the fixpoints exist.
;
; The confluence of the reduction is guaranteed by the particular form
; of the substitution produced by the unifier (the unifier always
; deals with the weak normal forms of submitted terms).
;
; The similarity of the weak normalization with call-by-value and
; the strong normalization with the applicative-order reduction should
; be apparent.
;
; Variable x is called ultimately free if
; x -->w! x' and x' is free in the subtutution in question.
;
; Two ultimately free variables x and y belong to the same equivalence class
; if x -->w! u and y -->w! u
; The (free) variable u is the natural class representative.
; For the purpose of presentation, one may wish for a better representative.
; Given a set of equivalent variables xi -->w! u,
; a pretty representative is a member z of that set such that the
; string name of 'z' is lexicographically smaller than the string names 
; of the other variables in that set.
;
; If a variable x is ultimately free in subst and x ->w! u, 
; then there is a binding
; v1 -> v2 where both v1 and v2 are variables and v2 ->w! u. Furthermore,
; the set of all such v1 union {u} is the whole equivalence class of x.
; That property is guaranteed by the unifier. That property lets us
; build an inverse index to find the equivalence class of x.
;
; $Id: term.scm,v 4.50 2005/02/12 00:05:27 oleg Exp $


;----------------------------------------
; A few preliminaries
; LET*-AND: a simplified and streamlined AND-LET*.
; The latter is defined in SRFI-2 <http://srfi.schemers.org/srfi-2/>

(define-syntax let*-and
  (syntax-rules ()
    ((_ false-exp () body0 body1 ...) (begin body0 body1 ...))
    ((_ false-exp ((var0 exp0) (var1 exp1) ...) body0 body1 ...)
     (let ((var0 exp0))
       (if var0
         (let*-and false-exp ((var1 exp1) ...) body0 body1 ...)
         false-exp)))))

; Regression testing framework
; test-check TITLE TESTED-EXPRESSION EXPECTED-RESULT 
; where TITLE is something printable (e.g., a symbol or a string)
; EXPECTED-RESULT and TESTED-EXPRESSION are both expressions.
; The expressions are evaluated and their results are cmpared
; by equal?
; If the results compare, we just print the TITLE.
; Otherwise, we print the TITLE, the TESTED-EXPRESSION, and
; the both results.
(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
       (begin
         (cout "Testing " title nl)
         (let* ((expected expected-result)
                (produced tested-expression))
           (or (equal? expected produced)
               (errorf 'test-check
                       "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                       'tested-expression expected produced)))
         #f))))

(define symbol-append
  (lambda symbs
    (string->symbol
      (apply string-append
        (map symbol->string symbs)))))

;----------------------------------------


;; use SRFI-9 records
(define (make-logical-variable name)
  (vector 'lv name))
(define (logical-variable? x)
  (and (vector? x) (eq? 'lv (vector-ref x 0))))
(define (logical-variable-id x)
  (vector-ref x 1))

(define logical-variable make-logical-variable)
(define var? logical-variable?)

; Introduction of a logical variable
(define-syntax let-lv
  (syntax-rules ()
    ((_ (id ...) body)
      (let ((id (logical-variable 'id)) ...) body))))

; The anonymous variable
(define __ (let-lv (_) _))

; Another way to introduce logical variables: via distinguished pairs
; (define logical-var-tag (list '*logical-var-tag*)) ; unique for eq?
; (define native-pair? pair?)
; (define logical-variable
;   (lambda (id)
;     (cons logical-var-tag id)))
; (define var?
;   (lambda (x)
;     (and (native-pair? x) (eq? (car x) logical-var-tag))))
; (define logical-variable-id
;   (lambda (x)
;     (if (var? x) (cdr x) 
;       (errorf 'logical-variable-id "Invalid Logic Variable: ~s" x))))
; (define pair?
;   (lambda (x)
;     (and (native-pair? x) (not (eq? (car x) logical-var-tag)))))


; Eigen-variables -- unique symbols that represent universally-quantified
; variables in a term
; For identification, we prefix the name of the eigen-variable with
; the exclamation mark. The mark makes sure the symbol stands out when
; printed.

(define counter 0)
(define (jensym s)
  (set! counter (+ counter 1))
  (string->symbol (string-append "!$gen$!" s (number->string counter))))

(define eigen-variable
  (lambda (id)
    (symbol-append '! id '_ (jensym "x"))))

(define eigen-var?
  (lambda (x)
    (and (symbol? x)
      (let ((str (symbol->string x)))
	(> (string-length str) 2)
	(char=? (string-ref str 0) #\!)))))


; (eigen (id ...) body) -- evaluate body in the environment
; extended with the bindings of id ... to the corresponding
; eigen-variables
(define-syntax eigen
  (syntax-rules ()
    ((_ (id ...) body)
      (let ((id (eigen-variable 'id)) ...) body))))

(define (eigen-test)
(test-check 'eigen
  (and
    (eigen () #t)
    (eigen (x) (eigen-var? x))
    (eigen (x y)
      (begin (display "eigens: ") (display (list x y))
	(newline) #t)))
  #t))

;;; ------------------------------------------------------

(define commitment cons)
(define commitment->term cdr)
(define commitment->var car)

(define empty-subst '())
(define empty-subst? null?)

(define extend-subst
  (lambda (v t subst)
    (cons (commitment v t) subst)))

; get the free vars of a term (a list without duplicates)
(define vars-of
  (lambda (term)
    (let loop ((term term) (fv '()))
      (cond
        ((var? term) (if (memq term fv) fv (cons term fv)))
        ((pair? term) (loop (cdr term) (loop (car term) fv)))
        (else fv)))))

; Check to see if a var occurs in a term
(define occurs?
  (lambda (var term)
    (cond
      ((var? term) (eq? term var))
      ((pair? term) (or (occurs? var (car term)) (occurs? var (cdr term))))
      (else #f))))

; A ground term contains no variables
(define ground?
  (lambda (t)
    (cond
      ((var? t) #f)
      ((pair? t) (and (ground? (car t)) (ground? (cdr t))))
      (else #t))))

; Given a term v and a subst s, return v', the weak normal form of v:
; v -->w! v' with respect to s
(define subst-in-weak
  (lambda (v s)
    (cond
      ((var? v) 
       (cond
         ((assq v s) =>
          (lambda (b) (subst-in-weak (commitment->term b) s)))
         (else v)))
      (else v))))

; Given a term v and a subst s, return v', the strong normal form of v:
; v -->s! v' with respect to s
(define subst-in
  (lambda (t subst)
    (cond
      ((var? t)
       (let ((c (assq t subst)))
         (if c (subst-in (commitment->term c) subst) t)))
      ((pair? t)
       (cons
         (subst-in (car t) subst)
         (subst-in (cdr t) subst)))
      (else t))))


; ; Given a term v and a subst s, return v', the strong normal form of v:
; ; v -->s! v' with respect to s
; (define subst-vars-recursively
;   (lambda (t subst)
;     (cond
;       ((var? t)
;        (cond
;          ((assq t subst) =>
;           (lambda (c)
;             (subst-vars-recursively
;               (commitment->term c) (remq c subst))))
;          (else t)))
;       ((pair? t)
;        (cons
;          (subst-vars-recursively (car t) subst)
;          (subst-vars-recursively (cdr t) subst)))
;       (else t))))

; (define normalize-subst
;   (lambda (subst)
;     (map (lambda (c)
;            (commitment (commitment->var c)
;              (subst-vars-recursively (commitment->term c) subst)))
;       subst)))


; Sooner or later, we will need to print out a term or do something
; else with it. We have to decide what to do with free variables that
; may be in that term.
; The long experience with Kanren and miniKanren and long discussions
; convinced us that it's best to `display' free variables as
; _.n where n is a number. BTW, we can't just display
; logical-variable-id, because distinct logical variables may have the same
; logical-variable-id.

; reify:: term -> reified-term
; where reified-term is identical to term if it is ground.
; Otherwise, we replace all free variables in term with _.n symbols
; The 'reverse' in (reverse (vars-of t))
; just makes the output look as it used to look before. Consider it
(define reify
  (lambda (term)
    (let ((fv (reverse (vars-of term))))
      (if (null? fv) term		; the term is ground
	(let ((renaming			; build the renaming substitution
		(let loop ((counter 0) (fv fv))
		  (if (null? fv) empty-subst
		    (extend-subst 
		      (car fv)
		      (string->symbol
			(string-append "_." (number->string counter)))
		      (loop (+ 1 counter) (cdr fv)))))))
	  (subst-in term renaming))))))


; we will also need to print the substitution, either in whole or in part
; reify-subst:: list-of-vars subst -> reified-subst
; where list-of-vars is a list of variables to reify, or the empty
; list. In the latter case, all variables from subst are reified.
; reified-subst has a form ((var-name reified-term) ...)
; where var-name, for historical reasons, has the form id.0
; where `id' is logical-variable-id.

(define reify-subst
  (lambda (vars subst)
    (let* ((vars (if (null? vars) (map commitment->var subst) vars))
	   (terms (reify (subst-in vars subst))))
      (map (lambda (x y)
	     (list (string->symbol 
		     (string-append (symbol->string (logical-variable-id x))
		       ".0"))
	       y))
	     vars terms))))

      
  

; (define compose-subst/own-survivors
;   (lambda (base refining survivors)
;     (let refine ((b* base))
;       (if (null? b*) survivors
;           (cons-if-real-commitment
;             (commitment->var (car b*))
;             (subst-in (commitment->term (car b*)) refining)
;             (refine (cdr b*)))))))
;
; (define compose-subst
;   (lambda (base refining)
;     (cond
;       ((null? base) refining)
;       ((null? refining) base)
;       (else
;         (compose-subst/own-survivors base refining
;           (let survive ((r* refining))
;             (cond
;               ((null? r*) '())
;               ((assq (commitment->var (car r*)) base) (survive (cdr r*)))
;               (else (cons (car r*) (survive (cdr r*)))))))))))

; Replace a logical variable with the corresponding eigen-variable
; Note: to be really right, universalize should be a scoping predicate,
; something like exists:
; (universalize (term) goal)
; to prove 'goal' in the env where term is universalized.
; In that case, the introduced eigen-variables do not escape.
; Also, perhaps universalize should take a subst and first
; do (subst-in term subst) and then universalize the remaining
; logical variables -- which by that time would surely be free.
(define universalize
  (lambda (term)
    (let ((fv (vars-of term)))
      (let ((subst
              (map
                (lambda (v)
                  (commitment v (eigen-variable (logical-variable-id v))))
                fv)))
        (subst-in term subst)))))


; copy-term TERM -> TERM
; return a TERM that is identical to the input term modulo the replacement
; of variables in TERM with fresh logical variables. 
; If a logical variable occurs several times in TERM, the result
; will have the same number of occurrences of the replacement fresh
; variable.
; This is a sort-of dual to universalize, to be used on the other side
; of the implication. It replaces the existential quantification
; (implicit in free logical variables of a term) with the universal
; quantification.
(define copy-term
  (lambda (t)
    (let* ((fv (vars-of t))
	   (subst
	     (map (lambda (old-var)
		    (commitment old-var
		      (logical-variable (logical-variable-id old-var))))
	       fv)))
      (subst-in t subst))))


; Similar to universalize: makes nicer symbols for variables that look
; nicer when printed. The 'reverse' in (reverse (vars-of t))
; just makes the output look as it used to look before. Consider it
; a historical accident.
; (define concretize
;   (lambda (t)
;     (subst-in t
;       (let loop ((fv (reverse (vars-of t))) (env '()))
; 	(cond
; 	  ((null? fv) empty-subst)
; 	  (else (let ((id (logical-variable-id (car fv))))
; 		  (let ((num (let*-and 0 ((pr (assq id env))) (+ (cdr pr) 1))))
; 		    (cons (commitment (car fv) (artificial-id id num))
; 		      (loop (cdr fv) (cons (cons id num) env)))))))))))
;  (define artificial-id
;   (lambda (t-id num)
;     (string->symbol
;       (string-append
;         (symbol->string t-id) "." (number->string num)))))





;-------------------------------------------------------
;;;; This is Oleg's unifier

; Either t or u may be:
; __
; free-var
; bound-var
; pair
; other-value
; So, we have in general 25 possibilities to consider.
; actually, a pair or components of a pair can be variable-free
; or not. In the latter case, we have got to traverse them.
; Also, if a term to unify has come from subst, it has special properties,
; which we can exploit. See below.
;
; "Measurements of the dynamic behavior of unification on four real
; programs show that one or both of the arguments are variables about
; 85% of the time [63]. A subroutine call is made only if both arguments
; are nonvariables." (Peter Van Roy, The Wonder Years ...)
;
; Just like in the union-find unification algorithm, we produce
; substitutions in the "triangular form" (see Baader, Snyder, Unification
; Theory). Circularity is detected only at the end (when we do subst-in).

(define unify
  (lambda (t u subst)
    (cond
      ((eq? t u) subst)			; quick tests first
      ((eq? t __) subst)
      ((eq? u __) subst)
      ((var? t)
       (let*-and (unify-free/any t u subst) ((ct (assq t subst)))
	 (if (var? u)			; ct is a bound var, u is a var
	   (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
	     (unify-bound/bound ct cu subst))
	   (unify-bound/nonvar ct u subst))))
      ((var? u)				; t is not a variable...
       (let*-and
         (cond
           ((pair? t) (unify-free/list u t subst))
           ; t is not a var and is not a pair: it's atomic
           (else (extend-subst u t subst)))
         ((cu (assq u subst)))
         (unify-bound/nonvar cu t subst)))
      ((and (pair? t) (pair? u))
       (let*-and #f ((subst (unify (car t) (car u) subst)))
         (unify (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst)))))

; ct is a commitment to a bound variable, u is a atomic or a composite
; value -- but not a variable
(define unify-bound/nonvar
  (lambda (ct u subst)
    (let ((t (commitment->term ct)))
      (cond				; search for the end of ct -> chain
	((eq? t u) subst)
	((var? t)
	  (let*-and 
	    (cond
	      ((pair? u) (unify-free/list t u subst))
              ; u is not a var and is not a pair: it's atomic
	      (else (extend-subst t u subst)))
	    ((ct (assq t subst)))
	    (unify-bound/nonvar ct u subst)))
	; t is some simple or composite value. So is u.
      ((and (pair? t) (pair? u))
	(let*-and #f ((subst (unify-internal/any (car t) (car u) subst)))
	  (unify-internal/any (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst))))))


; Just like unify. However, the first term, t, comes from
; an internalized term. We know it can't be __ and can't contain __

(define unify-internal/any
  (lambda (t u subst)
    (cond
      ((eq? t u) subst)			; quick tests first
      ((eq? u __) subst)
      ((var? t)
       (let*-and (unify-free/any t u subst) ((ct (assq t subst)))
	 (if (var? u)			; ct is a bound var, u is a var
	   (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
	     (unify-bound/bound ct cu subst))
	   (unify-bound/nonvar ct u subst))))
      ((var? u)				; t is not a variable...
       (let*-and			; It's a part of an internal term
	 (extend-subst u t subst)	; no further checks needed
         ((cu (assq u subst)))
         (unify-internals (commitment->term cu) t subst)))
      ((and (pair? t) (pair? u))
       (let*-and #f ((subst (unify-internal/any (car t) (car u) subst)))
         (unify-internal/any (cdr t) (cdr u) subst)))
      (else (and (equal? t u) subst)))))


; Unify two already bound variables represented by their commitments
; ct and cu.
; We single out this case because in the future we may wish
; to unify the classes of these variables, by making a redundant
; binding of (commitment->var ct) to (commitment->term cu) or
; the other way around.
; Aside from the above, this function can take advantage of the following
; facts about (commitment->term cx) (where cx is an existing commitment):
;   - it is never __
;   - it never contains __
; Most importantly, if, for example, (commitment->term ct) is a free variable,
; we enter its binding to (commitment->term cu) with fewer checks.
; in particular, we never need to call unify-free/list nor
; unify-free/any as we do need to rebuild any terms.

(define unify-internals
  (lambda (t u subst)
      (cond
	((eq? t u) subst)               ; quick tests first
	((var? t)
         (let*-and (cond                ; t is a free variable
                     ((var? u)
                      (let*-and (extend-subst t u subst) ((cu (assq u subst)))
                        (unify-free/bound t cu subst)))
                     (else              ; t is free, u is not a var: done
                       (extend-subst t u subst)))
           ((ct (assq t subst)))
           (cond			; t is a bound variable
             ((var? u) 
              (let*-and (unify-free/bound u ct subst) ((cu (assq u subst)))
                (unify-bound/bound ct cu subst)))
             (else                      ; unify bound and a value
               (unify-internals (commitment->term ct) u subst)))))
	((var? u)                       ; t is not a variable...
         (let*-and (extend-subst u t subst) ((cu (assq u subst)))
           (unify-internals (commitment->term cu) t subst)))
        ((and (pair? t) (pair? u))
         (let*-and #f ((subst (unify-internals (car t) (car u) subst)))
           (unify-internals (cdr t) (cdr u) subst)))
        (else (and (equal? t u) subst)))))

(define unify-bound/bound
  (lambda (ct cu subst)
    (unify-internals (commitment->term ct) (commitment->term cu) subst)))


; t-var is a free variable, u can be anything
; This is analogous to get_variable instruction of Warren Abstract Machine
; (WAM).
; This function is not recursive and always succeeds, 
; because unify-free/bound and unify-free/list always succeed.
(define unify-free/any
  (lambda (t-var u subst)
    (cond
      ((eq? u __) subst)
      ((var? u)
       (let*-and (extend-subst t-var u subst) ((cu (assq u subst)))
         (unify-free/bound t-var cu subst)))
      ((pair? u) (unify-free/list t-var u subst))
      (else ; u is not a var and is not a pair: it's atomic
	(extend-subst t-var u subst)))))

; On entrance: t-var is free.
; we are trying to unify it with a bound variable (commitment->var cu)
; Chase the binding chain, see below for comments
; This also works somewhat like union-find...
; This function always succeeds. The resulting substitution is either
; identical to the input one, or differs only in the binding to t-var.
;
; Unlike the previous version of the unifier,
; The following code does not introduce the temp variables *a and *d
; It makes substitutions more complex. Therefore, pruning them
; will take a while, and will break up the sharing. Therefore, we
; don't do any pruning.

(define unify-free/bound
  (lambda (t-var cu s)
    (let loop ((cm cu))
      (let ((u-term (commitment->term cm)))
	(cond
	  ((eq? u-term t-var) s)
	  ((var? u-term)
	    (cond
	      ((assq u-term s) => loop)
	      (else (extend-subst t-var u-term s)))) ; u-term is free here
	  (else (extend-subst t-var u-term s)))))))

; ((and (pattern-var? tree2) (assq tree2 env)) => ; tree2 is a bound var
;        ; binding a free variable to a bound. Search for a substantial binding
;        ; or a loop. If we find a loop tree1->tree2->...->tree1
;        ; then we do not enter the binding to tree1, because tree1 is not
;        ; actually constrained.
;       (lambda (tree2-binding)
; 	(let loop ((binding tree2-binding))
; 	  (cond
; 	    ((eq? tree1 (cdr binding)) env)  ; loop: no binding needed
; 	    ((and (pattern-var? (cdr binding)) (assq (cdr binding) env))
; 	      => loop)
; 	    (else (cons (cons tree1 (cdr binding)) env))))))

; t-var is a free variable, u-value is a proper or improper
; list, which may be either fully or partially grounded (or not at all).
; We scan the u-value for __, and if, found, replace them with fresh
; variables. We then bind t-var to the term.
; This function is not recursive and always succeeds.
;
; We assume that more often than not u-value does not contain __.
; Therefore, to avoid the wasteful rebuilding of u-value, we 
; first scan it for the occurrence of __. If the scan returns negative,
; we can use u-value as it is.

      ; Rebuild lst replacing all anonymous variables with some
      ; fresh logical variables
      ; If lst contains no anonymous variables, return #f
      ; Note that lst itself may be #f -- and yet no contradiction arises.
(define ufl-rebuild-without-anons
  (lambda (lst)
    (cond
      ((eq? lst __) (logical-variable '*anon))
      ((not (pair? lst)) #f)
      ((null? (cdr lst))
	(let ((new-car (ufl-rebuild-without-anons (car lst))))
	  (and new-car (cons new-car '()))))
      (else
	(let ((new-car (ufl-rebuild-without-anons (car lst)))
              (new-cdr (ufl-rebuild-without-anons (cdr lst))))
	  (if new-car
	    (cons new-car (or new-cdr (cdr lst)))
	    (and new-cdr (cons (car lst) new-cdr))))))))

(define unify-free/list
  (lambda (t-var u-value subst)
    (extend-subst t-var
      (or (ufl-rebuild-without-anons u-value) u-value)
      subst)))

;------------------------------------------------------------------------
; Tests

(define (term-tests)
  
  ; (cout nl "Compositions of substitutions" nl)
  ; (let-lv (x y)
  ;   (test-check 'test-compose-subst-0
  ;     (append (unit-subst x y) (unit-subst y 52))
  ;     `(,(commitment x y) ,(commitment y 52))))
  
  
  ; (test-check 'test-compose-subst-1
  ;   (let-lv (x y)
  ;     (equal?
  ;       (compose-subst (unit-subst x y) (unit-subst y 52))
  ;       `(,(commitment x 52) ,(commitment y 52))))
  ;   #t)
  
  ; (test-check 'test-compose-subst-2
  ;   (let-lv (w x y)
  ;     (equal?
  ;       (let ((s (compose-subst (unit-subst y w) (unit-subst w 52))))
  ; 	(compose-subst (unit-subst x y) s))
  ;       `(,(commitment x 52) ,(commitment y 52) ,(commitment w 52))))
  ;   #t)
  
  ; (test-check 'test-compose-subst-3
  ;   (let-lv (w x y)
  ;     (equal?
  ;       (let ((s (compose-subst (unit-subst w 52) (unit-subst y w))))
  ; 	(compose-subst (unit-subst x y) s))
  ;       `(,(commitment x w) ,(commitment w 52) ,(commitment y w))))
  ;   #t)
  
  ; (test-check 'test-compose-subst-4
  ;   (let-lv (x y z)
  ;     (equal?
  ;       (let ((s (compose-subst (unit-subst y z) (unit-subst x y)))
  ; 	    (r (compose-subst
  ; 		 (compose-subst (unit-subst x 'a) (unit-subst y 'b))
  ; 		 (unit-subst z y))))
  ; 	(compose-subst s r))
  ;       `(,(commitment x 'b) ,(commitment z y))))
  ;   #t)
  
  ; (test-check 'test-compose-subst-5
  ;   (concretize-subst
  ;     (compose-subst
  ;       (let-lv (x) (unit-subst x 3))
  ;       (let-lv (x) (unit-subst x 4))))
  ;   '((x.0 . 3) (x.1 . 4)))
  
  
  ; (test-check 'test-compose-subst-5
  ;   (let-lv (x y z)
  ;     (equal?
  ;       (let ((term `(p ,x ,y (g ,z))))
  ; 	(let ((s (compose-subst (unit-subst y z) (unit-subst x `(f ,y))))
  ; 	      (r (compose-subst (unit-subst x 'a) (unit-subst z 'b))))
  ; 	  (let ((term1 (subst-in term s)))
  ; 	    (write term1)
  ; 	    (newline)
  ; 	    (let ((term2 (subst-in term1 r)))
  ; 	      (write term2)
  ; 	      (newline)
  ; 	      (let ((sr (compose-subst s r)))
  ; 		(write sr)
  ; 		(newline)
  ; 		(subst-in term sr))))))
  ;       (begin
  ; 	`(p (f ,y) ,z (g ,z))
  ; 	`(p (f ,y) b (g b))
  ; 	`(,(commitment y 'b) ,(commitment x `(f ,y)) ,(commitment z 'b))
  ; 	`(p (f ,y) b (g b)))))
  ;   #t)
  
  
  (test-check 'test-unify/pairs-oleg1
    (let-lv (x y)
      (unify `(,x ,4) `(3 ,x) empty-subst))
    #f)
  
  (test-check 'test-unify/pairs-oleg2
    (let-lv (x y)
      (unify `(,x ,x) '(3 4) empty-subst))
    #f)
  
  (let-lv (x y)
    (test-check 'test-unify/pairs-oleg3
      (reify-subst '() (unify `(,x ,y) '(3 4) empty-subst))
      '((y.0 4) (x.0 3))))
  
  (let-lv (x y)
    (test-check 'test-unify/pairs-oleg4
      (reify-subst '() (unify `(,x 4) `(3 ,y) empty-subst))
      `((y.0 4) (x.0 3))))
  
  (let-lv (x y w z)
    (test-check 'test-unify/pairs-oleg5
      (reify-subst (list w y x)
        (unify `(,x 4 3 ,w) `(3 ,y ,x ,z) empty-subst))
      '((w.0 _.0) (y.0 4) (x.0 3))))
  
  (let-lv (x y w z)
    (test-check 'test-unify/pairs-oleg6
      (reify-subst (list y x)
        (unify `(,x 4) `(,y ,y) empty-subst))
      '((y.0 4) (x.0 4))))
  
  (test-check 'test-unify/pairs-oleg7
    (let-lv (x y)
      (unify `(,x 4 3) `(,y ,y ,x) empty-subst))
      #f)
  
  (let-lv (x y w z u)
    (test-check 'test-unify/pairs-oleg8
      (reify-subst (list u z y x)
        (unify
  	`(,w (,x (,y ,z) 8))
  	`(,w (,u (abc ,u) ,z))
  	empty-subst))
      '((u.0 8) (z.0 8) (y.0 abc) (x.0 8))))
  
  (let-lv (x y w z u)
    (test-check 'test-unify/pairs-oleg8
      (reify-subst (list y x)
        (unify `(p (f a) (g ,x)) `(p ,x ,y) empty-subst))
      '((y.0 (g (f a))) (x.0 (f a)))))
  
  (let-lv (x y w z u)
    (test-check 'test-unify/pairs-oleg10
      (reify-subst (list x y)
        (unify `(p (g ,x) (f a)) `(p ,y ,x) empty-subst))
      '((x.0 (f a)) (y.0 (g (f a))))))
  
  (let-lv (x y w z u)
    (test-check 'test-unify/pairs-oleg11
      (reify-subst (list y x z)
        (unify
  	`(p a ,x (h (g ,z)))
  	`(p ,z (h ,y) (h ,y))
  	empty-subst))
      '((y.0 (g a)) (x.0 (h (g a))) (z.0 a))))
  
  ; The following loops...
  ; (concretize-subst
  ;   (let-lv (x y)
  ;     (let* ((s (unify x `(1 ,x) '()))
  ; 	   (s (unify y `(1 ,y) s))
  ; 	   (s (unify x y s))) s)))
  
  
  ; (let-lv (x y w z u)
  ;   (test-check 'test-unify/pairs-oleg12
  ;     (concretize-subst ;;; was #f
  ;       (let ((s (unify `(p ,x ,x) `(p ,y (f ,y)) empty-subst)))
  ; 	(let ((var (map commitment->var s)))
  ; 	  (map commitment
  ; 	    var
  ; 	    (subst-vars-recursively var s)))))
  ;     `(;,(commitment '*d.0 '())
  ;        ,(commitment '*a.0 '(f *a.0))
  ;        ;,(commitment '*d.1 '((f . *d.1)))
  ;        ,(commitment '*d.0 '((f . *d.0)))
  ;        ;,(commitment '*a.1 'f)
  ;        ;,(commitment 'y.0  '(f (f . *d.1)))
  ;        ,(commitment 'y.0  '(f (f . *d.0)))
  ;        ,(commitment 'x.0  '(f (f . *d.0))))))
  
  ; (let-lv (x y w z u)
  ;   (test-check 'test-unify/pairs-oleg13
  ;     (concretize-subst ;;; was #f
  ;       (let ((s (unify `(p ,x ,x) `(p ,y (f ,y)) empty-subst)))
  ; 	(let ((var (map commitment->var s)))
  ; 	  (map commitment
  ; 	    var
  ; 	    (subst-vars-recursively var s)))))
  ;     `(;,(commitment '*d.0 '())
  ;        ,(commitment '*a.0 '(f *a.0))
  ;        ;,(commitment '*d.1 '((f . *d.1)))
  ;        ,(commitment '*d.0 '((f . *d.0)))
  ;        ;,(commitment '*a.1 'f)
  ;        ;,(commitment 'y.0  '(f (f . *d.1)))
  ;        ,(commitment 'y.0  '(f (f . *d.0)))
  ;        ,(commitment 'x.0  '(f (f . *d.0))))))
  
  ;Baader & Snyder
  (test-check 'test-pathological
    (list
        (let-lv (x0 x1 y0 y1)
  	(begin
  	  (_pretty-print
  	    (reify-subst '()
  	      (unify
  		`(h ,x1 (f ,y0 ,y0) ,y1)
  		`(h (f ,x0 ,x0) ,y1 ,x1)
  		empty-subst)))
  	  (newline) #t))
  
        (let-lv (x0 x1 x2 y0 y1 y2)
  	(begin
  	  (_pretty-print
             (reify-subst '()
              (unify
                `(h ,x1 ,x2 (f ,y0 ,y0) (f ,y1 ,y1) ,y2)
                `(h (f ,x0 ,x0) (f ,x1 ,x1) ,y1 ,y2 ,x2)
                empty-subst)))
          (newline) #t))
  
        (let-lv (x0 x1 x2 x3 x4 y0 y1 y2 y3 y4)
         (begin
          (_pretty-print
            (reify-subst '()
              (unify
                `(h ,x1 ,x2 ,x3 ,x4 (f ,y0 ,y0) (f ,y1 ,y1) (f ,y2 ,y2) (f ,y3 ,y3) ,y4)
                `(h (f ,x0 ,x0) (f ,x1 ,x1) (f ,x2 ,x2) (f ,x3 ,x3) ,y1 ,y2 ,y3 ,y4 ,x4)
                empty-subst))) #t)))
    (list #t #t #t))
  
  
  (test-check 'length-of-subst
    (let-lv (x y z)
      (let* ((subst (unify x `(1 2 3 4 5 ,z) '()))
  	    (subst (unify x `(1 . ,y) subst))
  	    (subst (unify z 42 subst)))
        (reify-subst '() subst)))
    '((z.0 42) (y.0 (2 3 4 5 42)) (x.0 (1 2 3 4 5 42))))
    ;'((z.0 . 42) (y.0 2 3 4 5 a*.0) (a*.0 . z.0) (x.0 1 2 3 4 5 a*.0)))

  10
  )
  

;; =========================================================================
;;  kanren.scm
;; =========================================================================

;			The body of KANREN
;
; The appropriate prelude (e.g., chez-specific.scm) is assumed.
;
; $Id: kanren.ss,v 4.50 2005/02/12 00:05:05 oleg Exp $

(define-syntax lambda@
  (syntax-rules ()
    ((_ (formal) body0 body1 ...) (lambda (formal) body0 body1 ...))
    ((_ (formal0 formal1 formal2 ...) body0 body1 ...)
     (lambda (formal0)
       (lambda@ (formal1 formal2 ...) body0 body1 ...)))))

(define-syntax at@
  (syntax-rules ()
    ((_ rator rand) (rator rand))
    ((_ rator rand0 rand1 rand2 ...) (at@ (rator rand0) rand1 rand2 ...))))

;(test-check 'test-@-lambda@
;  (at@ (lambda@ (x y z) (+ x (+ y z))) 1 2 3)
;  6)

;'(test-check 'test-@-lambda@
;  (at@ (lambda@ (x y z) (+ x (+ y z))) 1 2 3)
;  42)

(define Y
  (lambda (f)
    ((lambda (u) (u (lambda (x) (lambda (n) ((f (u x)) n)))))
     (lambda (x) (x x)))))

; An attempt to do a limited beta-substitution at macro-expand time
; (define-syntax @    
;   (syntax-rules (syntax-rules)
;     ((_ (syntax-rules sdata ...) rand0 ...)
;       (let-syntax 
; 	((tempname (syntax-rules sdata ...)))
; 	(tempname rand0 ...)))
;     ((_ rator rand0 rand1 ...)
;      (@-simple rator rand0 rand1 ...))))


;  Fk    = () -> Ans
;  Ans   = Nil + [Subst,Fk] or just a conceptual stream of substitutions
;  Sk    = Subst -> Fk -> Ans  
;  Goal  = Subst -> SGoal
;  SGoal = Sk -> Fk -> Ans

;  initial-sk : Sk
;  initial-fk : Fk

(define initial-sk (lambda@ (subst fk)
		     (cons subst fk)))
(define initial-fk (lambda () '()))


; Trivial goals
(define succeed (lambda@ (s k) (at@ k s)))  ; eta-reduced
(define fail (lambda@ (s k f) (f)))
(define sfail (lambda@ (k f) (f)))	; Failed SGoal


;------------------------------------------------------------------------
; Making logical variables "scoped" and garbage-collected
;  -----> it was used, but no longer
;  -----> The code is still here, as we plan to come back to this...
;
; A framework to remove introduced variables when they leave their scope.
; To make removing variables easier, we consider the list of subst as a
; "stack". Before we add a new variable, we retain a pointer to the
; stack. Then, when we are about to remove the added variables after their
; scope is ended, we stop at the shared retained substitution, and we know
; that anything below the retained substitution can't possibly contain the
; reference to the variables we're about to remove.
;
; Pruning of substitutions is analogous to environment pruning (aka tail-call
; optimization) in WAM on _forward_ execution.

; LV-ELIM IN-SUBST SUBST ID ....
; remove the bindings of ID ... from SUBST (by composing with the
; rest of subst). IN-SUBST is the mark.
; If we locate IN-SUBST in SUBST, we know that everything below the
; mark can't possibly contain ID ...

; lv-elim-1 VAR IN-SUBST SUBST
; VAR is a logical variable, SUBST is a substitution, and IN-SUBST
; is a tail of SUBST (which may be '()).
; VAR is supposed to have non-complex binding in SUBST
; (see Definition 3 in the document "Properties of Substitutions").
; If VAR is bound in SUBST, the corresponding commitment 
; is supposed to occur in SUBST up to but not including IN-SUBST.
; According to Proposition 10, if VAR freely occurs in SUBST, all such
; terms are VAR itself.
; The result is a substitution with the commitment to VAR removed
; and the other commitments composed with the removed commitment.
; The order of commitments is preserved.

(define lv-elim-1
  (lambda (var in-subst subst)
    (if (eq? subst in-subst) subst
      ; if VAR is not bound, there is nothing to prune
      (let*-and subst ((var-binding (assq var subst)))
	(let ((tv (commitment->term var-binding)))
	  (let loop ((current subst))
	    (cond
	      ((null? current) current)
	      ((eq? current in-subst) current)
	      ((eq? (car current) var-binding)
	       (loop (cdr current)))
	      ((eq? (commitment->term (car current)) var)
	       (cons (commitment (commitment->var (car current)) tv)
		 (loop (cdr current))))
	      (else (cons (car current) (loop (cdr current)))))))))))

; The same but for multiple vars
; To prune multiple-vars, we can prune them one-by-one
; We can attempt to be more efficient and prune them in parallel.
; But we encounter a problem:
; If we have a substitution
;  ((x . y) (y . 1) (a . x))
; Then pruning 'x' first and 'y' second will give us ((a . 1))
; Pruning 'y' first and 'x' second will give us ((a . 1))
; But naively attempting to prune 'x' and 'y' in parallel
; disregarding dependency between them results in ((a . y))
; which is not correct.
; We should only be concerned about a direct dependency:
;  ((x . y) (y . (1 t)) (t . x) (a . x))
; pruning x and y in sequence or in parallel gives the same result:
;  ((t . (1 t)) (a . (1 t)))
; We should also note that the unifier will never return a substitution
; that contains a cycle ((x1 . x2) (x2 . x3) ... (xn . x1))

(define lv-elim
  (lambda (vars in-subst subst)
    (if (eq? subst in-subst)
      subst
      (let ((var-bindings ; the bindings of truly bound vars
	      (let loop ((vars vars))
		(if (null? vars) vars
		  (let ((binding (assq (car vars) subst)))
		    (if binding
		      (cons binding (loop (cdr vars)))
		      (loop (cdr vars))))))))
	(cond
	  ((null? var-bindings) subst) ; none of vars are bound
	  ((null? (cdr var-bindings))
	    ; only one variable to prune, use the faster version
	   (lv-elim-1 (commitment->var (car var-bindings))
	     in-subst subst))
	  ((let test ((vb var-bindings)) ; check multiple dependency
	     (and (pair? vb)
	       (or (let ((term (commitment->term (car vb))))
		     (and (var? term) (assq term var-bindings)))
		 (test (cdr vb)))))
	    ; do pruning sequentially
	   (let loop ((var-bindings var-bindings) (subst subst))
	     (if (null? var-bindings) subst
	       (loop (cdr var-bindings)
		 (lv-elim-1 (commitment->var (car var-bindings))
		   in-subst subst)))))
	  (else				; do it in parallel
	    (let loop ((current subst))
	      (cond
		((null? current) current)
		((eq? current in-subst) current)
		((memq (car current) var-bindings)
		 (loop (cdr current)))
		((assq (commitment->term (car current)) var-bindings) =>
		 (lambda (ct)
		   (cons (commitment (commitment->var (car current)) 
			   (commitment->term ct))
		     (loop (cdr current)))))
		(else (cons (car current) (loop (cdr current))))))))))))

; when the unifier is moved up, move lv-elim test from below up...

; That was the code for the unifier that introduced temp variables
; (define-syntax exists
;   (syntax-rules ()
;     ((_ () gl) gl)
;     ((_ (ex-id) gl)
;      (let-lv (ex-id)
;        (lambda@ (sk fk in-subst)
;          (at@ gl
;            (lambda@ (fk out-subst)
;              (at@ sk fk (lv-elim-1 ex-id in-subst out-subst)))
;            fk in-subst))))
;     ((_ (ex-id ...) gl)
;      (let-lv (ex-id ...) 
;        (lambda@ (sk fk in-subst)
;          (at@ gl
;            (lambda@ (fk out-subst)
;              (at@ sk fk (lv-elim (list ex-id ...) in-subst out-subst)))
;            fk in-subst))))))

; For the unifier that doesn't introduce temp variables,
; exists is essentially let-lv
; At present, we don't do any GC.
; Here's the reason we don't do any pruning now:
; Let's unify the variable x with a term `(1 2 3 4 5 ,z). The result
; will be the binding x -> `(1 2 3 4 5 ,z). Let's unify `(1 . ,y) with
; x. The result will be a binding y -> `(2 3 4 5 ,z). Note that the
; bindings of x and y share a tail. Let us now unify z with 42. The
; result will be a binding z->42. So far, so good. Suppose however that
; z now "goes out of scope" (the exists form that introduced z
; finishes). We now have to traverse all the terms in the substitution
; and replace z with its binding. The result will be a substitution
; 	x -> (1 2 3 4 5 42)
; 	y -> (2 3 4 5 42)
; Now, the bindings of x and y do not share anything at all! The pruning
; has broke sharing. If we want to unify x and `(1 . ,y) again, we have
; to fully traverse the corresponding terms again.
; So, to prune variables and preserve sharing, we have to topologically sort
; the bindings first!

(define-syntax _exists
  (syntax-rules ()
    ((_ () gl) gl)
    ((_ (ex-id ...) gl)
     (let-lv (ex-id ...) gl))
    ))

;-----------------------------------------------------------
; Sequencing of relations
; Goal is a multi-valued function (which takes
;   subst, sk, fk, and exits to either sk or fk).
; A relation is a parameterized goal.
;
; All sequencing operations are defined on goals.
; They can be "lifted" to relations (see below).
; 

; TRACE-GOAL-RAW TITLE GL -> GL
; Traces all invocations and re-invocations of a goal
; printing subst before and after, in their raw form
(define trace-goal-raw
  (lambda (title gl)
    (let ((print-it
	    (lambda (event subst)
	      (display title) (display " ")
	      (display event) (_pretty-print subst) (newline))))
      (lambda@ (subst sk fk)
	(print-it "CALL:" subst)
	(at@ gl subst
	  (lambda@ (subst fk)
	    (print-it "RETURN:" subst)
	    (at@ sk subst
	      (lambda ()
		(display title) (display " REDO") (newline)
		(fk))
	      ))
	  (lambda ()
	    (display title) (display " FAIL") (newline)
	    (fk))
	  )))))

; Conjunctions
; All conjunctions below satisfy properties
;    ans is an answer of (a-conjunction gl1 gl2 ...) ==>
;       forall i. ans is an answer of gl_i
;    (a-conjunction) ==> success


; (all gl1 gl2 ...)
; A regular Prolog conjunction. Non-deterministic (i.e., can have 0, 1,
; or more answers).
; Properties:
;  (all gl) ==> gl
;  (all gl1 ... gl_{n-1} gln) is a "join" of answerlists of
;        (all gl1 ... gl_{n-1}) and gln

(define-syntax all
  (syntax-rules ()
    ((_) succeed)
    ((_ gl) gl)
    ((_ gl0 gl1 ...)
     (lambda@ (subst sk) (splice-in-gls/all subst sk gl0 gl1 ...)))))

(define-syntax splice-in-gls/all
  (syntax-rules ()
    ((_ subst sk gl) (at@ gl subst sk))
    ((_ subst sk gl0 gl1 ...)
     (at@ gl0 subst (lambda (subst) (splice-in-gls/all subst sk gl1 ...))))))


; (promise-one-answer gl)
; Operationally, it is the identity.
; It is an optimization directive: if the user knows that an goal
; can produce at most one answer, he can tell the system about it.
; The behavior is undefined if the user has lied.

(define-syntax promise-one-answer
  (syntax-rules ()
    ((_ gl) gl)))

; (all! gl1 gl2 ...)
; A committed choice nondeterminism conjunction
; From the Mercury documentation:

;   In addition to the determinism annotations described earlier, there
;   are "committed choice" versions of multi and nondet, called cc_multi
;   and cc_nondet. These can be used instead of multi or nondet if all
;   calls to that mode of the predicate (or function) occur in a context
;   in which only one solution is needed.
;
; (all! gl) evaluates gl in a single-choice context. That is,
; if gl fails, (all! gl) fails. If gl has at least one answer,
; this answer is returned.
; (all! gl) has at most one answer regardless of the answers of gl.
;   ans is an answer of (all! gl) ==> ans is an answer of gl
; The converse is not true.
; Corollary: (all! gl) =/=> gl
; Corollary: gl is (semi-) deterministic: (all! gl) ==> gl
; (all! (promise-one-answer gl)) ==> gl
;
; By definition, (all! gl1 gl2 ...) ===> (all! (all gl1 gl2 ...))

(define-syntax all!
  (syntax-rules (promise-one-answer)
    ((_) (promise-one-answer (all)))
    ((_ (promise-one-answer gl)) (promise-one-answer gl)) ; keep the mark
    ((_ gl0 gl1 ...)
     (promise-one-answer
       (lambda@ (subst sk fk)
	 (at@
	   (splice-in-gls/all subst
	     (lambda@ (subst fk-ign) (at@ sk subst fk)) gl0 gl1 ...)
	   fk))))))

; (all!! gl1 gl2 ...)
; Even more committed choice nondeterministic conjunction
; It evaluates all elements of the conjunction in a single answer context
; (all!! gl) ==> (all! gl) =/=> gl
; (all!! gl1 gl2 ...) ==> (all (all! gl1) (all! gl2) ...)
;                       ==> (all! (all! gl1) (all! gl2) ...)
; (all!! gl1 ... gln (promise-one-answer gl)) ==>
;    (all (all!! gl1 ... gln) gl)

(define-syntax all!!
  (syntax-rules ()
    ((_) (all!))
    ((_ gl) (all! gl))
    ((_ gl0 gl1 ...)
     (promise-one-answer 
       (lambda@ (subst sk fk)
         (splice-in-gls/all!! subst sk fk gl0 gl1 ...))))))

(define-syntax splice-in-gls/all!!
  (syntax-rules (promise-one-answer)
    ((_ subst sk fk)
      (at@ sk subst fk))
    ((_ subst sk fk (promise-one-answer gl))
      (at@ gl subst sk fk))
    ((_ subst sk fk gl0 gl1 ...)
      (at@ gl0 subst
	(lambda@ (subst fk-ign) (splice-in-gls/all!! subst sk fk gl1 ...))
	fk))))

; (if-only COND THEN)
; (if-only COND THEN ELSE)
; Here COND, THEN, ELSE are goals.
; If COND succeeds at least once, the result is equivalent to
;      (all (all! COND) TNEN)
; If COND fails, the result is the same as ELSE.
; If ELSE is omitted, it is assumed fail. That is, (if-only COND THEN)
; fails if the condition fails.  "This  unusual semantics
; is part of the ISO and all de-facto Prolog standards."
; Thus, declaratively,
;   (if-only COND THEN ELSE) ==> (any (all (all! COND) THEN)
;                                     (all (fails COND) ELSE))
; Operationally, we try to generate a good code.

; "The majority of predicates written by human programmers are
; intended to give at most one solution, i.e., they are
; deterministic. These predicates are in effect case statements
; [sic!], yet they are too often compiled in an inefficient manner
; using the full generality of backtracking (which implies saving the
; machine state and repeated failure and state restoration)." (Peter
; Van Roy, 1983-1993: The Wonder Years of Sequential Prolog
; Implementation).


(define-syntax if-only
  (syntax-rules ()
    ((_ condition then)
     (lambda@ (subst sk fk)
       (at@ condition subst
         ; sk from cond
         (lambda@ (subst fk-ign) (at@ then subst sk fk))
         ; failure from cond
         fk)))
    ((_ condition then else)
     (lambda@ (subst sk fk)
       (at@ condition subst
         (lambda@ (subst fk-ign) (at@ then subst sk fk))
         (lambda () (at@ else subst sk fk))
         )))))

; (if-all! (COND1 ... CONDN) THEN)
; (if-all! (COND1 ... CONDN) THEN ELSE)
;
; (if-all! (COND1 ... CONDN) THEN ELSE) ==>
;   (if-only (all! COND1 ... CONDN) THEN ELSE)
; (if-all! (COND1) THEN ELSE) ==>
;   (if-only COND1 THEN ELSE)

; Eventually, it might be a recognized special case in if-only.

; (define-syntax if-all!
;   (syntax-rules ()
;     ((_ (condition) then) (if-only condition then))
;     ((_ (condition) then else) (if-only condition then else))
;     ((_ (condition1 condition2 ...) then)
;      (lambda@ (sk fk)
;        (@ (splice-in-gls/all
;             (lambda@ (fk-ign)
;               (@ then sk fk))
;             condition1 condition2 ...)
;           fk)))
;     ((_ (condition1 condition2 ...) then else)
;      (lambda@ (sk fk subst)
;        (@ (splice-in-gls/all
;             (lambda@ (fk-ign)
;               (@ then sk fk)) condition1 condition2 ...)
;           (lambda ()
;             (@ else sk fk subst))
; 	 subst)))))

; Disjunction of goals
; All disjunctions below satisfy properties
;  ans is an answer of (a-disjunction gl1 gl2 ...) ==>
;    exists i. ans is an answer of gl_i
; (a-disjunction) ==> fail

; Any disjunction. A regular Prolog disjunction (introduces
; a choicepoints, in Prolog terms)
; Note that 'any' is not a union! In particular, it is not
; idempotent.
; (any) ===> fail
; (any gl) ===> gl
; (any gl1 ... gln) ==> _concatenation_ of their answerlists

(define-syntax any
  (syntax-rules ()
    ((_) fail)
    ((_ gl) gl)
    ((_ gl ...)
      (lambda@ (subst sk fk)
	(splice-in-gls/any subst sk fk gl ...)))))

(define-syntax splice-in-gls/any
  (syntax-rules ()
    ((_ subst sk fk gl1) (at@ gl1 subst sk fk))
    ((_ subst sk fk gl1 gl2 ...)
     (at@ gl1 subst sk (lambda () (splice-in-gls/any subst sk fk gl2 ...))))))


; Negation
; (fails gl) succeeds iff gl has no solutions
; (fails gl) is a semi-deterministic predicate: it can have at most
; one solution
; (succeeds gl) succeeds iff gl has a solution
;
; (fails (fails gl)) <===> (succeeds gl)
; but (succeeds gl) =/=> gl
; Cf. (equal? (not (not x)) x) is #f in Scheme in general.
; Note, negation is only sound if some rules (Grounding Rules) are satisfied.

(define fails
  (lambda (gl)
    (lambda@ (subst sk fk)
      (at@ gl subst
        (lambda@ (subst current-fk) (fk))
        (lambda () (at@ sk subst fk))
        ))))

; Again, G-Rule must hold for this predicate to be logically sound
(define succeeds
  (lambda (gl)
    (lambda@ (subst sk fk)
      (at@ gl subst (lambda@ (subst-ign fk-ign) (at@ sk subst fk))
	fk))))

; partially-eval-sgl: Partially evaluate a semi-goal. A
; semi-goal is an expression that, when applied to two
; arguments, sk and fk, can produce zero, one, or more answers.  Any
; goal can be turned into a semi-goal if partially applied
; to subst.  The following higher-order semi-goal takes a
; goal and yields the first answer and another, residual
; goal. The latter, when evaluated, will give the rest of the
; answers of the original semi-goal.  partially-eval-sgl could
; be implemented with streams (lazy lists). The following is a purely
; combinational implementation.
;
; (at@ partially-eval-sgl sgl a b) =>
;   (b) if sgl has no answers
;   (a s residial-sgl) if sgl has a answer. That answer is delivered
;                       in s. 
; The residial semi-goal can be passed to partially-eval-sgl
; again, and so on, to obtain all answers from a goal one by one.

; The following definition is eta-reduced.

(define (partially-eval-sgl sgl)
  (at@ sgl
    (lambda@ (subst fk a b)
      (at@ a subst 
	(lambda@ (sk1 fk1)
	  (at@
	    (fk) 
	    ; new a
	    (lambda@ (sub11 x) (at@ sk1 sub11 (lambda () (at@ x sk1 fk1))))
	    ; new b
	    fk1))))
    (lambda () (lambda@ (a b) (b)))))

; An interleaving disjunction.
; Declaratively, any-interleave is the same as any.
; Operationally, any-interleave schedules each component goal
; in round-robin. So, any-interleave is fair: it won't let a goal
; that produces infinitely many answers (such as repeat) starve the others.
; any-interleave introduces a breadth-first-like traversal of the
; decision tree.
; I seem to have seen a theorem that says that a _fair_ scheduling
; (like that provided by any-interleave) entails a minimal or well-founded
; semantics of a Prolog program.

(define-syntax any-interleave
  (syntax-rules ()
    ((_) fail)
    ((_ gl) gl)
    ((_ gl ...)
     (lambda@ (subst sk fk)
       (interleave sk fk (list (gl subst) ...))))))

; we treat sgls as a sort of a circular list
(define interleave
  (lambda (sk fk sgls)
    (cond
      ((null? sgls) (fk))		; all of the sgls are finished
      ((null? (cdr sgls))
       ; only one of sgls left -- run it through the end
       (at@ (car sgls) sk fk))
      (else
        (let loop ((curr sgls) (residuals '()))
	  ; check if the current round is finished
	  (if (null? curr) (interleave sk fk (reverse residuals))
	    (at@
	      partially-eval-sgl (car curr)
	      ; (car curr) had an answer
	      (lambda@ (subst residual)
	        (at@ sk subst
	          ; re-entrance cont
		  (lambda () (loop (cdr curr) (cons residual residuals)))))
	    ; (car curr) is finished - drop it, and try next
	    (lambda () (loop (cdr curr) residuals)))))))))

; An interleaving disjunction removing duplicates: any-union
; This is a true union of the constituent goals: it is fair, and
; it removes overlap in the goals to union, if any. Therefore,
;    (any-union gl gl) ===> gl
; whereas (any gl gl) =/=> gl
; because the latter has twice as many answers as gl.
;
; Any-union (or interleave-non-overlap, to be precise) is quite similar
; to the function interleave above. But now, the order of goals
; matters. Given goals gl1 gl2 ... glk ... gln,
; at the k-th step we try to partially-eval glk. If it yields an answer,
; we check if gl_{k+1} ... gln can be satisfied with that answer.
; If any of them does, we disregard the current answer and ask glk for
; another one. We maintain the invariant that
;  ans is an answer of (any-union gl1 ... gln) 
;  ===> exists i. ans is an answer of gl_i
;       && forall j>i. ans is not an answer of gl_j
; The latter property guarantees the true union.
; Note the code below does not check if the answers of each individual
; goal are unique. It is trivial to modify the code so that
; any-union removes the duplicates not only among the goals but
; also within a goal. That change entails a run-time cost. More
; importantly, it breaks the property
; (any-union gl gl) ===> gl
; Only a weaker version, (any-union' gl gl) ===> (any-union' gl)
; would hold. Therefore, we do not make that change.

(define-syntax any-union
  (syntax-rules ()
    ((_) fail)
    ((_ gl) gl)
    ((_ gl ...)
     (lambda@ (subst sk fk)
       (interleave-non-overlap sk fk (list (cons (gl subst) gl) ...))))))

; we treat sagls as a sort of a circular list
; Each element of sagls is a pair (sgl . gl)
; where gl is the original goal (needed for the satisfiability testing)
; and sgl is the corresponding semi-goal or a 
; residual thereof.
(define interleave-non-overlap
  (lambda (sk fk sagls)
    (let outer ((sagls sagls))
      (cond
        ((null? sagls) (fk))  ; all of the sagls are finished
        ((null? (cdr sagls))  ; only one gl is left -- run it through the end
	 (at@ (caar sagls) sk fk))
        (else
	  (let loop ((curr sagls)
                     (residuals '()))
            ; check if the current round is finished
	    (if (null? curr) (outer (reverse residuals))
                (at@
                 partially-eval-sgl (caar curr)
                  ; (caar curr) had an answer
                 (lambda@ (subst residual)
                  ; let us see now if the answer, subst, satisfies any of the
                  ; gls down the curr.
                   (let check ((to-check (cdr curr)))
                     (if (null? to-check) ; OK, subst is unique,give it to user
                         (at@ sk subst
                           ; re-entrance cont
                           (lambda ()
                             (loop (cdr curr) 
                               (cons (cons residual (cdar curr)) residuals))))
                         (at@ (cdar to-check) subst
                            ; subst was the answer to some other gl:
			    ; check failed
                            (lambda@ (subst1 fk1)
                              (loop (cdr curr) 
                                (cons (cons residual (cdar curr)) residuals)))
                            ; subst was not the answer: continue check
                            (lambda () (check (cdr to-check)))))))
                 ; (car curr) is finished - drop it, and try next
                 (lambda () (loop (cdr curr) residuals))))))))))


; Another if-then-else
; (if-some COND THEN)
; (if-some COND THEN ELSE)
; Here COND, THEN, ELSE are goals.
; If COND succeeds at least once, the result is equivalent to
;      (all COND TNEN)
; If COND fails, the result is the same as ELSE.
; If ELSE is omitted, it is assumed fail. That is, (if-some COND THEN)
; fails if the condition fails.  "This  unusual semantics
; is part of the ISO and all de-facto Prolog standards."
; Thus, declaratively,
;   (if-some COND THEN ELSE) ==> (any (all COND THEN)
;                                     (all (fails COND) ELSE))
; from which follows
;   (if-some COND THEN)              ==> (all COND THEN)
;   (if-some COND THEN fail)         ==> (all COND THEN)
; but
;   (if-some COND succeed ELSE)     =/=> (any COND ELSE)
;
; Other corollary:
;   (if-some COND THEN ELSE) ==> (if-only (fails COND) ELSE (all COND THEN))
;
; Operationally, we try to generate a good code.
;
; In Prolog, if-some is called a soft-cut (aka *->). In Mercury,
; if-some is the regular IF-THEN-ELSE.
;
; We can implement if-some with partially-eval-sgl. Given a COND, we
; peel off one answer, if possible. If there is one, we then execute THEN
; passing it the answer and the fk from COND so that if THEN fails,
; it can obtain another answer. If COND has no answers, we execute
; ELSE. Again, we can do all that purely declaratively, without
; talking about introducing and destroying choice points.

(define-syntax if-some
  (syntax-rules ()
    ((_ condition then) (all condition then))
    ((_ condition then else)
     (lambda@ (subst sk fk)
       (at@ partially-eval-sgl (condition subst)
         (lambda@ (ans residual)
           (at@ then ans sk
             ; then failed. Check to see if condition has another answer
             (lambda () (at@ residual (lambda@ (subst) (at@ then subst sk)) fk))))
             ; condition failed
         (lambda () (at@ else subst sk fk)))))))


; An interleaving conjunction: all-interleave
;
; This conjunction is similar to the regular conjunction `all' but
; delivers the answers in the breadth-first rather than depth-first
; order.
;
; Motivation.
; Let us consider the conjunction (all gl1 gl2)
; where gl1 is (any gl11 gl12) and gl2 is an goal with the
; infinite number of answers (in the environment when either gl11 or
; gl12 succeed). It is easy to see (all gl1 gl2) will have the
; infinite number of answers too -- but only the proper subset of
; all the possible answers. Indeed, (all gl1 gl2) will essentially
; be equivalent to (all gl11 gl2). Because gl2 succeeds infinitely
; many times, the choice gl12 in gl1 will never be explored.
; We can see that formally:
; (all gl1 gl2) 
;   = (all (any gl11 gl12) gl2) 
;   = (any (all gl11 gl2) (all gl12 gl2))
; Because (all gl11 gl2) can succeed infinitely many times, it starves
; the other disjunction, (all gl12 gl2).
; But we know how to deal with that: we just replace any with any-interleave:
; (all gl1 gl2) --> (any-interleave (all gl11 gl2) (all gl12 gl2))
;
; It seems that the problem is solved? We just re-write our expressions
; into the disjunctive normal form, and then replace the top-level
; `any' with `any-interleave'. Alas, that means that to get the benefit
; of fair scheduling and get all the possible solutions of the conjunction
; (i.e., recursive enumerability), we need to re-write all the code.
; We have to explicitly re-write a conjunction of disjunctions into
; the disjunctive normal form. That is not that easy considering that gl2
; will most likely be a recursive goal re-invoking the original
; conjunction. That would be a lot of re-writing.
;
; The conjunction all-interleave effectively does the above `re-writing'
; That is, given the example above,
;	(all-interleave (any gl11 gl12) gl2)
; is observationally equivalent to
;	(any-interleave (all gl11 gl2) (all gl12 gl2))
;
; The advantage is that we do not need to re-write our conjunctions:
; we merely replace `all' with `all-interleave.'
; 
; How can we do that in the general case, (all gl1 gl2)
; where gl1 is not _explicitly_ a disjunction? We should remember the
; property of partially-eval-sgl: Any goal `gl' with at least one
; answer can be represented as (any gl-1 gl-rest)
; where gl-1 is a primitive goal holding the first answer of `gl',
; and gl-rest holding the rest of the answers. We then apply the
; all-any-distributive law and re-write
; (all-interleave gl1 gl2) 
; ==> (all-interleave (any gl1-1 gl1-rest) gl2) 
; ==> (any-interleave (all gl1 gl2) (all-interleave gl1-rest gl2))
;
; If gl1 has no answers, then (all-interleave gl1 gl2) fails, as
; a conjunction must.
; It is also easy to see that
; (all-interleave gl1 gl2 ...) is the same as
; (all-interleave gl1 (all-interleave gl2 ...))
;
; Although all-interleave was motivated by an example (all gl1 gl2)
; where gl1 is finitary and only gl2 is infinitary, the above
; equations (and the implementation below) show that all-interleave
; can do the right thing even if gl1 is infinitary as well. To be
; precise, given
;
;	(all-interleave gl1 gl2)
;
; with gl1 and gl2 infinitary, the i-th solution of gl1 will be
; observed in every 2^i-th solution to the whole conjunction. Granted,
; all-interleave isn't precisely very fair -- the later solutions of
; gl1 will appear progressively more rarely -- yet, they will all
; appear. The infinity of c0 is big enough. That is, given any
; solution to gl1, we will eventually, in finite time, find it in the
; solution of the whole conjunction (provided gl2 doesn't fail on
; that solution, of course).



(define-syntax all-interleave
  (syntax-rules ()
    ((_) (all))
    ((_ gl) gl)
    ((_ gl0 gl1 ...)
      (lambda@ (subst)
	(all-interleave-bin
	  (gl0 subst) (all-interleave gl1 ...))))))

(define all-interleave-bin
  (lambda (sgl1 gl2)
    (lambda@ (sk fk)
      (at@ partially-eval-sgl sgl1
	(lambda@ (ans residual)
	  (interleave sk fk
	    (list 
	      (at@ gl2 ans)
	      (all-interleave-bin residual gl2)
	    )))
	  ;gl1 failed
	  fk))))


; Relations...........................

; The current incremented unification of argument passing is quite similar to
; the compilation of argument unifications in WAM.

; relation (VAR ...) (to-show TERM ...) [GL]
; Defines a relation of arity (length '(TERM ...)) with an optional body
; GL. VAR ... are logical variables that are local to the relation, i.e.,
; appear in TERM or GL. It's better to list as VAR ... only logical
; variables that appear in TERM. Variables that appear only in GL should
; be introduced with exists. That makes their existential quantification
; clearer. Variables that appear in TERM are universally quantified.
;
; relation (head-let TERM ...) [GL]
; See relation-head-let below.
;
; relation (ANNOT-VAR ...) (to-show TERM ...) [GL]  (see remark below!)
; where ANNOT-VAR is either a simple VAR or (once VAR)
; where 'once' is a distingushed symbol. The latter form introduces
; a once-var, aka linear variable. A linear variable appears only once in
; TERM ... and only at the top level (that is, one and only one TERM
; in the to-show pattern contains ONCE-VAR, and that term is ONCE-VAR
; itself). In addition, ONCE-VAR must appear at most once in the body GL.
; (Of course, then ONCE-VAR could be _, instead.)
; If these conditions are satisfied, we can replace a logical variable
; ONCE-VAR with a regular Scheme variable.

; Alternative notation:
; (relation (a c) (to-show term1 (once c) term2) body)
; Makes it easier to deal with. But it is unsatisfactory:
; to-show becomes a binding form...
;
; When ``compiling'' a relation, we now look through the
; (to-show ...) pattern for a top-level occurrence of the logical variable
; introduced by the relation. For example:
;	(relation (x y) (to-show `(,x . ,y) x) body)
; we notice that the logical variable 'x' occurs at the top-level. Normally we
; compile the relation like that into the following
;    (lambda (g1 g2)
;      (_exists (x y)
;        (lambda@ (subst)
; 	 (let*-and (fail subst) ((subst (unify g1 `(,x . ,y)  subst))
; 			         (subst (unify g2 x subst)))
; 	     (at@ body subst)))))
;
; However, that we may permute the order of 'unify g...' clauses
; to read
;    (lambda (g1 g2)
;      (_exists (x y)
;        (lambda@ (subst)
; 	 (let*-and (fail subst) ((subst (unify x g2 subst))
; 			         (subst (unify g1 `(,x . ,y)  subst))
; 			         )
; 	     (at@ body subst)))))
;
; We may further note that according to the properties of the unifier
; (see below), (unify x g2 subst) must always succeed, 
; because x is a fresh variable.
; Furthermore, the result of (unify x g2 subst) is either subst itself,
; or subst with the binding of x. Therefore, we can check if
; the binding at the top of (unify x g2 subst) is the binding to x. If
; so, we can remove the binding and convert the variable x from being logical
; to being lexical. Thus, we compile the relation as
;
;    (lambda (g1 g2)
;      (_exists (x y)
;        (lambda@ (subst)
; 	 (let* ((subst (unify-free/any x g2 subst))
; 	        (fast-path? (and (pair? subst)
; 			         (eq? x (commitment->var (car subst)))))
; 	        (x (if fast-path? (commitment->term (car subst)) x))
; 	        (subst (if fast-path? (cdr subst) subst)))
; 	 (let*-and sfail ((subst (unify g1 `(,x . ,y)  subst))
; 			 )
; 	     (at@ body subst))))))
;
; The benefit of that approach is that we limit the growth of subst and avoid
; keeping commitments that had to be garbage-collected later.


(define-syntax relation
  (syntax-rules (to-show head-let once __)
    ((_ (head-let head-term ...) gl)
     (relation-head-let (head-term ...) gl))
    ((_ (head-let head-term ...))	; not particularly useful without body
     (relation-head-let (head-term ...)))
    ((_ () (to-show term ...) gl)	; pattern with no vars _is_ linear
     (relation-head-let (`,term ...) gl))
    ((_ () (to-show term ...))		; the same without body: not too useful
     (relation-head-let (`,term ...)))
    ((_ (ex-id ...) (to-show term ...) gl)  ; body present
     (relation "a" () () (ex-id ...) (term ...) gl))
    ((_ (ex-id ...) (to-show term ...))      ; no body
     (relation "a" () () (ex-id ...) (term ...)))
    ; process the list of variables and handle annotations
    ((_ "a" vars once-vars ((once id) . ids) terms . gl)
     (relation "a" vars (id . once-vars) ids terms . gl))
    ((_ "a" vars once-vars (id . ids) terms . gl)
     (relation "a" (id . vars) once-vars ids terms . gl))
    ((_ "a" vars once-vars () terms . gl)
     (relation "g" vars once-vars () () () (subst) terms . gl))
    ; generating temp names for each term in the head
    ; don't generate if the term is a variable that occurs in
    ; once-vars
    ; For _ variables in the pattern, generate unique names for the lambda
    ; parameters, and forget them
    ; also, note and keep track of the first occurrence of a term
    ; that is just a var (bare-var) 
    ((_ "g" vars once-vars (gs ...) gunis bvars bvar-cl (__ . terms) . gl)
     (relation "g" vars once-vars (gs ... anon) gunis
       bvars bvar-cl terms . gl))
    ((_ "g" vars once-vars (gs ...) gunis bvars (subst . cls)
           (term . terms) . gl)
     (id-memv?? term once-vars 
       ; success continuation: term is a once-var
       (relation "g" vars once-vars (gs ... term) gunis bvars (subst . cls)
	 terms . gl)
       ; failure continuation: term is not a once-var
       (id-memv?? term vars
	 ; term is a bare var
	 (id-memv?? term bvars
	   ; term is a bare var, but we have seen it already: general case
	   (relation "g" vars once-vars  (gs ... g) ((g . term) . gunis) 
	     bvars (subst . cls) terms . gl)
	   ; term is a bare var, and we have not seen it
	   (relation "g" vars once-vars (gs ... g) gunis
	     (term . bvars)
	     (subst
	       (subst (unify-free/any term g subst))
	       (fast-path? (and (pair? subst)
			      (eq? term (commitment->var (car subst)))))
	       (term (if fast-path? (commitment->term (car subst)) term))
	       (subst (if fast-path? (cdr subst) subst))
	       . cls)
	     terms . gl))
	 ; term is not a bare var
	 (relation "g" vars once-vars  (gs ... g) ((g . term) . gunis) 
	   bvars (subst . cls) terms . gl))))
    ((_ "g" vars once-vars gs gunis bvars bvar-cl () . gl)
     (relation "f" vars once-vars gs gunis bvar-cl . gl))

    ; Final: writing the code
    ((_ "f" vars () () () (subst) gl)   ; no arguments (no head-tests)
      (lambda ()
	(_exists vars gl)))
                                    ; no tests but pure binding
    ((_ "f" (ex-id ...) once-vars (g ...) () (subst) gl)
     (lambda (g ...)
       (_exists (ex-id ...) gl)))
				    ; the most general
    ((_ "f" (ex-id ...) once-vars (g ...) ((gv . term) ...) 
       (subst let*-clause ...) gl) 
     (lambda (g ...)
       (_exists (ex-id ...)
	 (lambda (subst)
	   (let* (let*-clause ...)
	     (let*-and sfail ((subst (unify gv term subst)) ...)
	       (at@ gl subst)))))))))

; A macro-expand-time memv function for identifiers
;	id-memv?? FORM (ID ...) KT KF
; FORM is an arbitrary form or datum, ID is an identifier.
; The macro expands into KT if FORM is an identifier that occurs
; in the list of identifiers supplied by the second argument.
; Otherwise, id-memv?? expands to KF.
; All the identifiers in (ID ...) must be unique.
; Two identifiers match if both refer to the same binding occurrence, or
; (both are undefined and have the same spelling).

(define-syntax id-memv??
  (syntax-rules ()
    ((id-memv?? form (id ...) kt kf)
      (let-syntax
	((test
	   (syntax-rules (id ...)
	     ((test id _kt _kf) _kt) ...
	     ((test otherwise _kt _kf) _kf))))
	(test form kt kf)))))

; Test cases
; (id-memv?? x (a b c) #t #f)
; (id-memv?? a (a b c) 'OK #f)
; (id-memv?? () (a b c) #t #f)
; (id-memv?? (x ...) (a b c) #t #f)
; (id-memv?? "abc" (a b c) #t #f)
; (id-memv?? x () #t #f)
; (let ((x 1))
;   (id-memv?? x (a b x) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (a x b) 'OK #f))
; (let ((x 1))
;   (id-memv?? x (x a b) 'OK #f))


; relation-head-let (head-term ...) gl 
; A simpler, and more efficient kind of relation. The simplicity comes
; from a simpler pattern at the head of the relation. The pattern must
; be linear and shallow with respect to introduced variables.  The gl
; is optional (although omitting it doesn't make much sense in
; practice) There are two kinds of head-terms.  One kind is an
; identifier. This identifier is taken to be a logical identifier, to
; be unified with the corresponding actual argument.  Each logical
; identifier must occur exactly once.  Another kind of a head-terms is
; anything else. That anything else may be a constant, a scheme
; variable, or a complex term that may even include logical variables
; such as _ -- but not logical variables defined in the same head-let
; pattern.  To make the task of distinguishing logical identifiers
; from anything else easier, we require that anything else of a sort
; of a manifest constant be explicitly quoted or quasiquoted. It would
; be OK to add `, to each 'anything else' term.
;
; Examples:
; (relation-head-let (x y z) (foo x y z))
; Here x y and z are logical variables.
; (relation-head-let (x y '7) (foo x y))
; Here we used a manifest constant that must be quoted
; (relation-head-let (x y `(1 2 . ,_)) (foo x y))
; We used a quasi-quoted constant with an anonymous variable.
; (let ((z `(1 2 . ,_))) (relation-head-let (x y `,z) (foo x y))
; The same as above, but using a lexical Scheme variable.
; The binding procedure is justified by Proposition 9 of
; the Properties of Substitutions.
;
; 'head-let' is an example of "compile-time" simplifications. 
; For example, we distinguish constants in the term head at
; "compile time" and so we re-arrange the argument-passing
; unifications to handle the constants first.
; The test for the anonymous variable (eq? gvv0 _) below
; is an example of a global simplification with a run-time
; test. A compiler could have inferred the result of the test -- but only
; upon the global analysis of all the clauses.
; Replacing a logical variable with an ordinary variable, which does 
; not have to be pruned, is equivalent to the use of temporary and
; unsafe variables in WAM.

(define-syntax relation-head-let
  (syntax-rules ()
    ((_ (head-term ...) . gls)
     (relation-head-let "g" () (head-term ...) (head-term ...) . gls))
    ; generate names of formal parameters
    ((_ "g" (genvar ...) ((head-term . tail-term) . ht-rest)
       head-terms . gls)
     (relation-head-let "g" (genvar ... g) ht-rest head-terms . gls))
    ((_ "g" (genvar ...) (head-term . ht-rest) head-terms . gls)
     (relation-head-let "g" (genvar ... head-term) ht-rest head-terms . gls))
    ((_ "g" genvars  () head-terms . gls)
     (relation-head-let "d" () () genvars head-terms genvars . gls))
    ; partition head-terms into vars and others
    ((_ "d" vars others (gv . gv-rest) ((hth . htt) . ht-rest) gvs . gls)
     (relation-head-let "d" vars ((gv (hth . htt)) . others)
       gv-rest ht-rest gvs . gls))
    ((_ "d" vars others (gv . gv-rest) (htv . ht-rest) gvs . gls)
     (relation-head-let "d" (htv . vars) others
       gv-rest ht-rest gvs . gls))
    ((_ "d" vars others () () gvs . gls)
     (relation-head-let "f" vars others gvs . gls))
 
    ; final generation
    ((_ "f" vars ((gv term) ...) gvs) ; no body
     (lambda gvs                                     ; don't bother bind vars
       (lambda@ (subst)
	 (let*-and sfail ((subst (unify gv term subst)) ...)
	   (at@ succeed subst)))))

    ((_ "f" (var0 ...) ((gvo term) ...) gvs gl)
     (lambda gvs
       (lambda@ (subst)			; first unify the constants
	 (let*-and sfail ((subst (unify gvo term subst)) ...)
           (let ((var0 (if (eq? var0 __) (logical-variable '?) var0)) ...)
             (at@ gl subst))))))))

; (define-syntax relation/cut
;   (syntax-rules (to-show)
;     ((_ cut-id (ex-id ...) (to-show x ...) gl ...)
;      (relation/cut cut-id (ex-id ...) () (x ...) (x ...) gl ...))
;     ((_ cut-id ex-ids (var ...) (x0 x1 ...) xs gl ...)
;      (relation/cut cut-id ex-ids (var ... g) (x1 ...) xs gl ...))
;     ((_ cut-id (ex-id ...) (g ...) () (x ...) gl ...)
;      (lambda (g ...)
;        (_exists (ex-id ...)
;          (all! (== g x) ...
;            (lambda@ (sk fk subst cutk)
;              (let ((cut-id (!! cutk)))
;                (at@ (all gl ...) sk fk subst cutk)))))))))

(define-syntax fact
  (syntax-rules ()
    ((_ (ex-id ...) term ...)
     (relation (ex-id ...) (to-show term ...) succeed))))

; Lifting from goals to relations
; (define-rel-lifted-comb rel-syntax gl-proc-or-syntax)
; Given (gl-proc-or-syntax gl ...)
; define 
; (rel-syntax (id ...) rel-exp ...)
; We should make rel-syntax behave as a CBV function, that is,
; evaluate rel-exp early.
; Otherwise, things like
; (define father (extend-relation father ...))
; loop.

; (define-syntax extend-relation
;   (syntax-rules ()
;     ((_ (id ...) rel-exp ...)
;      (extend-relation-aux (id ...) () rel-exp ...))))

; (define-syntax extend-relation-aux
;   (syntax-rules ()
;     ((_ (id ...) ((g rel-exp) ...))
;      (let ((g rel-exp) ...)
;        (lambda (id ...)
;          (any (g id ...) ...))))
;     ((_ (id ...) (let-pair ...) rel-exp0 rel-exp1 ...)
;      (extend-relation-aux (id ...)
;        (let-pair ... (g rel-exp0)) rel-exp1 ...))))

(define-syntax define-rel-lifted-comb
  (syntax-rules ()
    ((_ rel-syntax-name gl-proc-or-syntax)
     (define-syntax rel-syntax-name
       (syntax-rules ()
         ((_ ids . rel-exps)
          (lift-gl-to-rel-aux gl-proc-or-syntax ids () . rel-exps)))))))

(define-syntax lift-gl-to-rel-aux
  (syntax-rules ()
    ((_ gl-handler ids ((g rel-var) ...))
     (let ((g rel-var) ...)
       (lambda ids
         (gl-handler (g . ids) ...))))
    ((_ gl-handler ids (let-pair ...) rel-exp0 rel-exp1 ...)
     (lift-gl-to-rel-aux gl-handler ids 
       (let-pair ... (g rel-exp0)) rel-exp1 ...))))

(define-rel-lifted-comb extend-relation any)

; The following  goal-to-relations 
; transformers are roughly equivalent. I don't know which is better.
; see examples below.

; (lift-to-relations ids (gl-comb rel rel ...))
(define-syntax lift-to-relations
  (syntax-rules ()
    ((_ ids (gl-comb rel ...))
     (lift-gl-to-rel-aux gl-comb ids () rel ...))))

; (let-gls ids ((name rel) ...) body)
; NB: some macro systems do not like if 'ids' below is replaced by (id ...)
(define-syntax let-gls
  (syntax-rules ()
    ((_ ids ((gl-name rel-exp) ...) body)
     (lambda ids
       (let ((gl-name (rel-exp . ids)) ...)
         body)))))

; Unify lifted to be a binary relation
(define-syntax ==
  (syntax-rules (__)
    ((_ __ u) (lambda@ (subst sk) (at@ sk subst)))
    ((_ t __) (lambda@ (subst sk) (at@ sk subst)))
    ((_ t u)
     (lambda@ (subst)
       (let*-and sfail ((subst (unify t u subst)))
         (succeed subst))))))


;	query (redo-k subst id ...) A SE ... -> result or '()
; The macro 'query' runs the goal A in the empty
; initial substitution, and reifies the resulting
; answer: the substitution and the redo-continuation bound
; to fresh variables with the names supplied by the user.
; The substitution and the redo continuation can then be used
; by Scheme expressions SE ...
; Before running the goal, the macro creates logical variables
; id ... for use in A and SE ...
; If the goal fails, '() is returned and SE ... are not evaluated.
; Note the similarity with shift/reset-based programming
; where the immediate return signifies "failure" and the invocation
; of the continuation a "success"
; Returning '() on failure makes it easy to create the list of answers

(define-syntax query
  (syntax-rules ()
    ((_ (redo-k subst id ...) A SE ...)
      (let-lv (id ...)
	(at@ A empty-subst
	  (lambda@ (subst redo-k) SE ...)
	  (lambda () '()))))))

(define stream-prefix
  (lambda (n strm)
    (if (null? strm) '()
      (cons (car strm)
        (if (zero? n) '()
          (stream-prefix (- n 1) ((cdr strm))))))))

(define-syntax solve
  (syntax-rules ()
    ((_ n (var0 ...) gl)
      (if (<= n 0) '()
	(stream-prefix (- n 1)
	  (query (redo-k subst var0 ...)
	    gl
	    (cons (reify-subst (list var0 ...) subst) redo-k)))))))


(define-syntax solution
  (syntax-rules ()
    ((_ (var0 ...) x)
     (let ((ls (solve 1 (var0 ...) x)))
       (if (null? ls) #f (car ls))))))


(define-syntax project
  (syntax-rules ()
    ((_ (var ...) gl)
     (lambda@ (subst)
       (let ((var (nonvar! (subst-in var subst))) ...)
	 (at@ gl subst))))))

(define-syntax project/no-check
  (syntax-rules ()
    ((_ (var ...) gl)
     (lambda@ (subst)
       (let ((var (subst-in var subst)) ...)
	 (at@ gl subst))))))

(define-syntax predicate
  (syntax-rules ()
    ((_ scheme-expression)
     (lambda@ (subst)
       (if scheme-expression (succeed subst) (fail subst))))))

(define nonvar!
  (lambda (t)
    (if (var? t)
      (errorf 'nonvar! "Logic variable ~s found after substituting."
	(reify t))
      t)))

; TRACE-VARS TITLE (VAR ...)
; Is a deterministic goal that prints the current values of VARS
; TITLE is any displayable thing.

; (define-syntax trace-vars
;   (syntax-rules ()
;     ((trace-vars title (var0 ...))
;      (promise-one-answer
;        (predicate/no-check (var0 ...)
;          (begin (display title) (display " ")
;                 (display '(var0 ...)) (display " ") (display (list var0 ...))
;                 (newline)))))))

(define-syntax trace-vars
  (syntax-rules ()
    ((_ title (var0 ...))
     (promise-one-answer
       (project/no-check (var0 ...)
         (predicate
	   (for-each 
	     (lambda (name val)
	       (cout title " " name ": " val nl))
             '(var0 ...) (reify `(,var0 ...)))
	   ))))))

;equality predicate: X == Y in Prolog
;if X is a var, then X == Y holds only if Y
;is the same var
(define *equal?
  (lambda (x y)
    (cond
      ((and (var? x) (var? y)) (eq? x y))
      ((var? x) #f)                     ; y is not a var
      ((var? y) #f)                     ; x is not a var
      (else (equal? x y)))))

; extend-relation-with-recur-limit LIMIT VARS RELS -> REL
; This is a variation of 'extend-relation' that makes sure
; that the extended relation is not recursively entered more
; than LIMIT times. The form extend-relation-with-recur-limit
; can be used to cut a left-recursive relation, and to implement
; an iterative deepening strategy.
; extend-relation-with-recur-limit must be a special form
; because we need to define the depth-counter-var
; outside of relations' lambda (so we count the recursive invocations
; for all arguments).
(define-syntax extend-relation-with-recur-limit
  (syntax-rules ()
    ((_ limit ids rel ...)
      (let ((depth-counter-var (logical-variable '*depth-counter*)))
	(lambda ids
	  (let ((gl (any (rel . ids) ...)))
	    (lambda@ (subst)
	      (cond
		((assq depth-counter-var subst)
		  => (lambda (cmt)
		       (let ((counter (commitment->term cmt)))
			 (if (>= counter limit)
			   sfail
			   (let ((s (extend-subst depth-counter-var
				      (+ counter 1) subst)))
			     (at@ gl s))))))
		(else
		  (let ((s (extend-subst depth-counter-var 1 subst)))
		    (at@ gl s)))))))))
    ))

; ?- help(call_with_depth_limit/3).
; call_with_depth_limit(+Goal, +Limit, -Result)
;     If  Goal can be proven  without recursion deeper than Limit  levels,
;     call_with_depth_limit/3 succeeds,  binding  Result  to  the  deepest
;     recursion  level  used  during the  proof.    Otherwise,  Result  is
;     unified  with depth_limit_exceeded  if the limit was exceeded  during
;     the  proof,  or the  entire predicate  fails if  Goal fails  without
;     exceeding Limit.

;     The  depth-limit is  guarded by the  internal machinery.   This  may
;     differ  from the depth computed based  on a theoretical model.   For
;     example,  true/0  is  translated  into an  inlined  virtual  machine
;     instruction.   Also, repeat/0 is not implemented as below, but  as a
;     non-deterministic foreign predicate.

;     repeat.
;     repeat :-
;             repeat.

;     As  a  result, call_with_depth_limit/3 may  still loop  inifitly  on
;     programs  that should  theoretically finish  in finite time.    This
;     problem  can be cured by  using Prolog equivalents to such  built-in
;     predicates.

;     This   predicate  may  be   used  for  theorem-provers  to   realise
;     techniques  like iterrative  deepening.   It  was implemented  after
;     discussion with Steve Moyle smoyle@ermine.ox.ac.uk.

;------------------------------------------------------------------------
;;;;; Starts the real work of the system.

(define-rel-lifted-comb intersect-relation all)
  
(define (kanren-tests)
  (let* ((father  
  	(relation ()
  	  (to-show 'jon 'sam)))
        (child-of-male
  	(relation (child dad)
  	  (to-show child dad)
  	  (father dad child)))
         (child-of-male1
  	 (relation (child dad)
  	   (to-show child dad)
  	   (child-of-male dad child)))
         )
    (test-check 'test-father0
      (let ((result
  	    (at@ (father 'jon 'sam) empty-subst
  	      initial-sk initial-fk)))
        (and
  	(equal? (car result) '())
  	(equal? ((cdr result)) '())))
      #t)
  
    (test-check 'test-child-of-male-0
      (reify-subst '()
        (car (at@ (child-of-male 'sam 'jon) empty-subst
  	     initial-sk initial-fk)))
    ;`(,(commitment 'child.0 'sam) ,(commitment 'dad.0 'jon)))
    '())  ; variables shouldn't leak
  
  
    ; The mark should be found here...
    (test-check 'test-child-of-male-1
    (reify-subst '()
      (car (at@ (child-of-male 'sam 'jon) empty-subst
  	    initial-sk initial-fk)))
    ;`(,(commitment 'child.0 'sam) ,(commitment 'dad.0 'jon)))
    '())
  )
  
  (let* ((father  
  	(relation ()
  	  (to-show 'jon 'sam)))
         (rob/sal
  	 (relation ()
  	   (to-show 'rob 'sal)))
         (new-father
  	 (extend-relation (a1 a2) father rob/sal))
  	(rob/pat
  	  (relation ()
  	    (to-show 'rob 'pat)))
  	(newer-father
  	  (extend-relation (a1 a2) new-father rob/pat))
  
  	)
    (test-check 'test-father-1
      (let ((result
  	    (at@ (new-father 'rob 'sal) empty-subst
  	      initial-sk initial-fk)))
        (and
  	(equal? (car result) '())
  	(equal? ((cdr result)) '())))
      #t)
  
    (test-check 'test-father-2
      (query (redo-k subst x)
        (new-father 'rob x)
        (list (equal? (car subst) (commitment x 'sal)) (redo-k)))
      '(#t ()))
  
    (test-check 'test-father-3
      (query (_ subst x)
        (new-father 'rob x)
        (reify-subst (list x) subst))
      '((x.0 sal)))
  
    (test-check 'test-father-4
      (query (_ subst x y)
        (new-father x y)
        (reify-subst (list x y) subst))
      '((x.0 jon) (y.0 sam)))
  
    (test-check 'test-father-5
      (query (redok subst x)
        (newer-father 'rob x)
        (_pretty-print subst)
        (cons
  	(reify-subst (list x) subst)
  	(redok)))
      '(((x.0 sal)) ((x.0 pat))))
  
  )
  
  (let* ((father  
  	 (extend-relation (a1 a2)
  	   (relation () (to-show 'jon 'sam))
  	   (relation () (to-show 'rob 'sal))
  	   (relation () (to-show 'rob 'pat))
  	   (relation () (to-show 'sam 'rob)))
  	 ))
  
    (test-check 'test-father-6/solve
      (and
        (equal?
  	(solve 5 (x) (father 'rob x))
  	'(((x.0 sal)) ((x.0 pat))))
        (equal?
  	(solve 6 (x y) (father x y))
  	'(((x.0 jon) (y.0 sam))
  	   ((x.0 rob) (y.0 sal))
  	   ((x.0 rob) (y.0 pat))
  	   ((x.0 sam) (y.0 rob)))))
    #t)
  
    (test-check 'test-father-7/solution
      (solution (x) (father 'rob x))
      '((x.0 sal)))
  )
  
  
  
  ; (define-syntax intersect-relation
  ;   (syntax-rules ()
  ;     ((_ (id ...) rel-exp) rel-exp)
  ;     ((_ (id ...) rel-exp0 rel-exp1 rel-exp2 ...)
  ;      (binary-intersect-relation (id ...) rel-exp0
  ;        (intersect-relation (id ...) rel-exp1 rel-exp2 ...)))))
  
  (let*
    ((parents-of-scouts
       (extend-relation (a1 a2)
         (fact () 'sam 'rob)
         (fact () 'roz 'sue)
         (fact () 'rob 'sal)))
      (parents-of-athletes
        (extend-relation (a1 a2)
  	(fact () 'sam 'roz)
  	(fact () 'roz 'sue)
  	(fact () 'rob 'sal)))
  
      (busy-parents
        (intersect-relation (a1 a2) parents-of-scouts parents-of-athletes))
  
      (conscientious-parents
        (extend-relation (a1 a2) parents-of-scouts parents-of-athletes))
      )
  
    (test-check 'test-conscientious-parents
      (solve 7 (x y) (conscientious-parents x y))
      '(((x.0 sam) (y.0 rob))
         ((x.0 roz) (y.0 sue))
         ((x.0 rob) (y.0 sal))
         ((x.0 sam) (y.0 roz))
         ((x.0 roz) (y.0 sue))
         ((x.0 rob) (y.0 sal))))
  )
  
  (let* ((father  
  	 (extend-relation (a1 a2)
  	   (relation () (to-show 'jon 'sam))
  	   (relation () (to-show 'rob 'sal))
  	   (relation () (to-show 'rob 'pat))
  	   (relation () (to-show 'sam 'rob)))
  	 ))
  
    (let
      ((grandpa-sam
         (relation (grandchild)
  	 (to-show grandchild)
  	 (_exists (parent)
  	   (all (father 'sam parent)
  	        (father parent grandchild))))))
      (test-check 'test-grandpa-sam-1
        (solve 6 (y) (grandpa-sam y))
        '(((y.0 sal)) ((y.0 pat))))
     )
  
    (let
      ((grandpa-sam
         (relation ((once grandchild))
  	 (to-show grandchild)
  	 (_exists (parent)
  	   (all (father 'sam parent)
  	        (father parent grandchild))))))
      (test-check 'test-grandpa-sam-1
        (solve 6 (y) (grandpa-sam y))
        '(((y.0 sal)) ((y.0 pat))))
      )
  
    (let ((child
  	  (relation ((once child) (once dad))
  	    (to-show child dad)
  	    (father dad child))))
      (test-check 'test-child-1
        (solve 10 (x y) (child x y))
        '(((x.0 sam) (y.0 jon))
  	 ((x.0 sal) (y.0 rob))
  	 ((x.0 pat) (y.0 rob))
  	 ((x.0 rob) (y.0 sam))))
      )
  
    (let ((grandpa
  	  (relation ((once grandad) (once grandchild))
  	    (to-show grandad grandchild)
  	    (_exists (parent)
  	      (all
  		(father grandad parent)
  		(father parent grandchild))))))
      (test-check 'test-grandpa-1
        (solve 4 (x) (grandpa 'sam x))
        '(((x.0 sal)) ((x.0 pat)))))
  
    (let ((grandpa-maker
  	  (lambda (guide* grandad*)
  	    (relation (grandchild)
  	      (to-show grandchild)
  	      (_exists (parent)
  		(all
  		  (guide* grandad* parent)
  		  (guide* parent grandchild)))))))
      (test-check 'test-grandpa-maker-2
        (solve 4 (x) ((grandpa-maker father 'sam) x))
        '(((x.0 sal)) ((x.0 pat)))))
    
  )
  
  (let*
    ((father
       (extend-relation (a1 a2)
         (fact () 'jon 'sam)
         (extend-relation (a1 a2)
  	 (fact () 'sam 'rob)
  	 (extend-relation (a1 a2)
  	   (fact () 'sam 'roz)
  	   (extend-relation (a1 a2)
  	     (fact () 'rob 'sal)
  	     (fact () 'rob 'pat))))))
      (mother
        (extend-relation (a1 a2)
  	(fact () 'roz 'sue)
  	(fact () 'roz 'sid)))
      )
  
    (let*
      ((grandpa/father
         (relation (grandad grandchild)
  	 (to-show grandad grandchild)
  	 (_exists (parent)
  	   (all
  	     (father grandad parent)
  	     (father parent grandchild)))))
       (grandpa/mother
         (relation (grandad grandchild)
  	 (to-show grandad grandchild)
  	 (_exists (parent)
  	   (all
  	     (father grandad parent)
  	     (mother parent grandchild)))))
       (grandpa
         (extend-relation (a1 a2) grandpa/father grandpa/mother)))
  
      (test-check 'test-grandpa-5
        (solve 10 (y) (grandpa 'sam y))
        '(((y.0 sal)) ((y.0 pat)) ((y.0 sue)) ((y.0 sid))))
      )
  
    ; A relation is just a function
    (let
      ((grandpa-sam
         (let ((r (relation (child)
  		  (to-show child)
  		  (_exists (parent)
  		    (all
  		      (father 'sam parent)
  		      (father parent child))))))
  	 (relation (child)
  	   (to-show child)
  	   (r child)))))
  
      (test-check 'test-grandpa-55
        (solve 6 (y) (grandpa-sam y))
        '(((y.0 sal)) ((y.0 pat))))
      )
  
  ; The solution that used cuts
  ; (define grandpa/father
  ;   (relation/cut cut (grandad grandchild)
  ;     (to-show grandad grandchild)
  ;     (_exists (parent)
  ;       (all
  ;         (father grandad parent)
  ;         (father parent grandchild)
  ;         cut))))
  ;
  ; (define grandpa/mother
  ;   (relation (grandad grandchild)
  ;     (to-show grandad grandchild)
  ;     (_exists (parent)
  ;       (all
  ;         (father grandad parent)
  ;         (mother parent grandchild)))))
  
  
  ; Now we don't need it
    (let*
      ((grandpa/father
         (relation (grandad grandchild)
  	 (to-show grandad grandchild)
  	 (_exists (parent)
  	   (all!
  	     (father grandad parent)
  	     (father parent grandchild)))))
  
        (grandpa/mother
  	(relation (grandad grandchild)
  	  (to-show grandad grandchild)
  	  (_exists (parent)
  	    (all
  	      (father grandad parent)
  	      (mother parent grandchild)))))
  
        (grandpa
  	(lift-to-relations (a1 a2)
  	  (all!
  	    (extend-relation (a1 a2) grandpa/father grandpa/mother))))
        )
      (test-check 'test-grandpa-8
        (solve 10 (x y) (grandpa x y))
        '(((x.0 jon) (y.0 rob))))
      )
    
  ; The solution that used to require cuts
  ; (define grandpa/father
  ;   (relation/cut cut (grandad grandchild)
  ;     (to-show grandad grandchild)
  ;     (_exists (parent)
  ;       (all cut (father grandad parent) (father parent grandchild)))))
  
    (let
      ((grandpa/father
         (relation (grandad grandchild)
  	 (to-show grandad grandchild)
  	 (_exists (parent)
  	   (all
  	     (father grandad parent) (father parent grandchild)))))
  
        (grandpa/mother
  	(relation (grandad grandchild)
  	  (to-show grandad grandchild)
  	  (_exists (parent)
  	    (all
  	      (father grandad parent) (mother parent grandchild)))))
        )
  
  ; Properly, this requires soft cuts, aka *->, or Mercury's
  ; if-then-else. But we emulate it...
      (let
        ((grandpa
  	 (let-gls (a1 a2) ((grandpa/father grandpa/father)
  			   (grandpa/mother grandpa/mother))
  	   (if-only (succeeds grandpa/father) grandpa/father grandpa/mother)))
  	)
        (test-check 'test-grandpa-10
  	(solve 10 (x y) (grandpa x y))
  	'(((x.0 jon) (y.0 rob))
  	   ((x.0 jon) (y.0 roz))
  	   ((x.0 sam) (y.0 sal))
  	   ((x.0 sam) (y.0 pat))))
        (test-check 'test-grandpa-10-1
  	(solve 10 (x) (grandpa x 'sue))
  	'(((x.0 sam))))
        )
  
  ; The same as above, with if-all! -- just to test the latter.
      (let
        ((grandpa
  	 (let-gls (a1 a2) ((grandpa/father grandpa/father)
  			    (grandpa/mother grandpa/mother))
  	   (if-only (all! (succeeds grandpa/father) (succeeds grandpa/father))
  	     grandpa/father grandpa/mother))))
  
        (test-check 'test-grandpa-10
  	(solve 10 (x y) (grandpa x y))
  	'(((x.0 jon) (y.0 rob))
  	   ((x.0 jon) (y.0 roz))
  	   ((x.0 sam) (y.0 sal))
  	   ((x.0 sam) (y.0 pat))))
  
        (test-check 'test-grandpa-10-1
  	(solve 10 (x) (grandpa x 'sue))
  	'(((x.0 sam))))
        )
  
  
  ; Now do it with soft-cuts
      (let
        ((grandpa
  	 (let-gls (a1 a2) ((grandpa/father grandpa/father)
  			    (grandpa/mother grandpa/mother))
  	   (if-some grandpa/father succeed grandpa/mother)))
  	)
        (test-check 'test-grandpa-10-soft-cut
  	(solve 10 (x y) (grandpa x y))
  	'(((x.0 jon) (y.0 rob))
  	   ((x.0 jon) (y.0 roz))
  	   ((x.0 sam) (y.0 sal))
  	   ((x.0 sam) (y.0 pat))))
        )
  
      (let*
        ((a-grandma
  	 (relation (grandad grandchild)
  	   (to-show grandad grandchild)
  	   (_exists (parent)
  	     (all! (mother grandad parent)))))
  	(no-grandma-grandpa
  	  (let-gls (a1 a2) ((a-grandma a-grandma)
  			     (grandpa (lift-to-relations (a1 a2)
  					(all!
  					  (extend-relation (a1 a2) 
  					    grandpa/father grandpa/mother)))))
  	    (if-only a-grandma fail grandpa)))
  	)
        (test-check 'test-no-grandma-grandpa-1
  	(solve 10 (x) (no-grandma-grandpa 'roz x))
  	'()))
  ))
  
  (let
    ((parents-of-scouts
       (extend-relation (a1 a2)
         (fact () 'sam 'rob)
         (fact () 'roz 'sue)
         (fact () 'rob 'sal)))
      (fathers-of-cubscouts
        (extend-relation (a1 a2)
  	(fact () 'sam 'bob)
  	(fact () 'tom 'adam)
  	(fact () 'tad 'carl)))
      )
  
    (test-check 'test-partially-eval-sgl
     (let-lv (p1 p2)
      (let* ((parents-of-scouts-sgl
  	     ((parents-of-scouts p1 p2) empty-subst))
             (cons@ (lambda@ (x y) (cons x y)))
             (split1 (at@ 
                      partially-eval-sgl parents-of-scouts-sgl
                      cons@ (lambda () '())))
             (a1 (car split1))
             (split2 (at@ partially-eval-sgl (cdr split1) cons@
                       (lambda () '())))
             (a2 (car split2))
             (split3 (at@ partially-eval-sgl (cdr split2) cons@
                       (lambda () '())))
             (a3 (car split3)))
        (map (lambda (subst)
               (reify-subst (list p1 p2) subst))
  	(list a1 a2 a3))))
    '(((p1.0 sam) (p2.0 rob)) ((p1.0 roz) (p2.0 sue)) ((p1.0 rob) (p2.0 sal))))
  )
  
  
  (test-check 'test-pred1
    (let ((test1
  	  (lambda (x)
  	    (any (predicate (< 4 5))
  	      (== x (< 6 7))))))
      (solution (x) (test1 x)))
    '((x.0 _.0)))
  
  (test-check 'test-pred2
    (let ((test2
  	  (lambda (x)
  	    (any (predicate (< 5 4))
  	      (== x (< 6 7))))))
      (solution (x) (test2 x)))
    '((x.0 #t)))
  
  (test-check 'test-pred3
    (let ((test3
  	  (lambda (x y)
  	    (any
  	      (== x (< 5 4))
  	      (== y (< 6 7))))))
      (solution (x y) (test3 x y)))
    `((x.0 #f) (y.0 _.0)))
  
  (test-check 'test-Seres-Spivey
    (let ((father
  	  (lambda (dad child)
  	    (any
  	      (all (== dad 'jon) (== child 'sam))
  	      (all (== dad 'sam) (== child 'rob))
  	      (all (== dad 'sam) (== child 'roz))
  	      (all (== dad 'rob) (== child 'sal))
  	      (all (== dad 'rob) (== child 'pat))
  	      (all (== dad 'jon) (== child 'hal))
  	      (all (== dad 'hal) (== child 'ted))
  	      (all (== dad 'sam) (== child 'jay))))))
      (letrec
          ((ancestor
             (lambda (old young)
               (any
                 (father old young)
                 (_exists (not-so-old)
                   (all
                     (father old not-so-old)
                     (ancestor not-so-old young)))))))
        (solve 20 (x) (ancestor 'jon x))))
    '(((x.0 sam))
      ((x.0 hal))
      ((x.0 rob))
      ((x.0 roz))
      ((x.0 jay))
      ((x.0 sal))
      ((x.0 pat))
      ((x.0 ted))))
  
  (let ()
  (define towers-of-hanoi
    (letrec
        ((move
           (extend-relation (a1 a2 a3 a4)
             (fact () 0 __ __ __)
             (relation (n a b c)
               (to-show n a b c)
               (project (n)
                 (if-only (predicate (positive? n))
                   (let ((m (- n 1)))
                     (all 
                       (move m a c b)
                       (project (a b)
                         (begin
                           (cout "Move a disk from " a " to " b nl)
                           (move m c b a)))))))))))
      (relation (n)
        (to-show n)
        (move n 'left 'middle 'right))))
  
  (cout "test-towers-of-hanoi with 3 disks: "
    (solution () (towers-of-hanoi 3))
    nl nl
    ))
  
  
  (test-check 'test-fun-resubst
    (reify
      (let ((j (relation (x w z)
  	       (to-show z)
  	       (let ((x 4)
                       (w 3))
                   (== z (cons x w))))))
        (solve 4 (q) (j q))))
    '(((q.0 (4 . 3)))))
  
  (let ()
  (define towers-of-hanoi-path
    (let ((steps '()))
      (let ((push-step (lambda (x y) (set! steps (cons `(,x ,y) steps)))))
        (letrec
            ((move
               (extend-relation (a1 a2 a3 a4)
                 (fact () 0 __ __ __)
                 (relation (n a b c)
                   (to-show n a b c)
                   (project (n)
                     (if-only (predicate (positive? n))
                       (let ((m (- n 1)))
                         (all
                           (move m a c b)
                           (project (a b)
                             (begin
                               (push-step a b)
                               (move m c b a)))))))))))
          (relation (n path)
            (to-show n path)
            (begin
              (set! steps '())
              (any
                (fails (move n 'l 'm 'r))
                (== path (reverse steps)))))))))
  
  (test-check 'test-towers-of-hanoi-path
    (solution (path) (towers-of-hanoi-path 3 path))
    '((path.0 ((l m) (l r) (m r) (l m) (r l) (r m) (l m))))))
  
  ;------------------------------------------------------------------------
  
            
  (test-check 'unification-of-free-vars-1
    (solve 1 (x)
      (let-lv (y)
        (all!! (== x y) (== y 5))))
    '(((x.0 5))))
  
  (test-check 'unification-of-free-vars-2
    (solve 1 (x)
      (let-lv (y)
        (all!! (== y 5) (== x y))))
    '(((x.0 5))))
  
  (test-check 'unification-of-free-vars-3
    (solve 1 (x)
      (let-lv (y)
        (all!! (== y x) (== y 5))))
    '(((x.0 5))))
  
  (test-check 'unification-of-free-vars-3
    (solve 1 (x)
      (let-lv (y)
        (all!! (== x y) (== y 5) (== x y))))
    '(((x.0 5))))
  
  (test-check 'unification-of-free-vars-4
    (solve 1 (x)
      (_exists (y)
        (all! (== y x) (== y 5) (== x y))))
    '(((x.0 5))))
  
  
  (letrec
    ((concat
       (lambda (xs ys)
         (cond
  	 ((null? xs) ys)
  	 (else (cons (car xs) (concat (cdr xs) ys)))))))
  
    (test-check 'test-concat-as-function
      (concat '(a b c) '(u v))
      '(a b c u v))
  
    (test-check 'test-fun-concat
      (solve 1 (q)
        (== q (concat '(a b c) '(u v))))
      '(((q.0 (a b c u v)))))
  )
  
  ; Now the same with the relation
  (letrec
    ((concat
       (extend-relation (a1 a2 a3)
         (fact (xs) '() xs xs)
         (relation (x xs (once ys) zs)
  	 (to-show `(,x . ,xs) ys `(,x . ,zs))
  	 (concat xs ys zs)))))
   (test-check 'test-concat
    (values
      (and
        (equal?
          (solve 6 (q) (concat '(a b c) '(u v) q))
          '(((q.0 (a b c u v)))))
        (equal?
          (solve 6 (q) (concat '(a b c) q '(a b c u v)))
          '(((q.0 (u v)))))
        (equal?
          (solve 6 (q) (concat q '(u v) '(a b c u v)))
          '(((q.0 (a b c)))))
        (equal?
          (solve 6 (q r) (concat q r '(a b c u v)))
          '(((q.0 ()) (r.0 (a b c u v)))
            ((q.0 (a)) (r.0 (b c u v)))
            ((q.0 (a b)) (r.0 (c u v)))
            ((q.0 (a b c)) (r.0 (u v)))
            ((q.0 (a b c u)) (r.0 (v)))
            ((q.0 (a b c u v)) (r.0 ()))))
        (equal?
          (solve 6 (q r s) (concat q r s))
  	'(((q.0 ()) (r.0 _.0) (s.0 _.0))
  	  ((q.0 (_.0)) (r.0 _.1) (s.0 (_.0 . _.1)))
  	  ((q.0 (_.0 _.1)) (r.0 _.2) (s.0 (_.0 _.1 . _.2)))
  	  ((q.0 (_.0 _.1 _.2)) (r.0 _.3) (s.0 (_.0 _.1 _.2 . _.3)))
  	  ((q.0 (_.0 _.1 _.2 _.3)) (r.0 _.4) (s.0 (_.0 _.1 _.2 _.3 . _.4)))
  	  ((q.0 (_.0 _.1 _.2 _.3 _.4)) (r.0 _.5)
  	   (s.0 (_.0 _.1 _.2 _.3 _.4 . _.5))))
  	)
        '(equal?
          (solve 6 (q r) (concat q '(u v) `(a b c . ,r)))
          '(((q.0 (a b c)) (r.0 (u v)))
            ((q.0 (a b c _.0)) (r.0 (_.0 u v)))
            ((q.0 (a b c _.0 _.1)) (r.0 (_.0 _.1 u v)))
            ((q.0 (a b c _.0 _.1 _.2)) (r.0 (_.0 _.1 _.2 u v)))
            ((q.0 (a b c _.0 _.1 _.2 _.3)) (r.0 (_.0 _.1 _.2 _.3 u v)))
            ((q.0 (a b c _.0 _.1 _.2 _.3 _.4))
             (r.0 (_.0 _.1 _.2 _.3 _.4 u v)))))
        (equal?
          (solve 6 (q) (concat q '() q))
          '(((q.0 ()))
            ((q.0 (_.0)))
            ((q.0 (_.0 _.1)))
            ((q.0 (_.0 _.1 _.2)))
            ((q.0 (_.0 _.1 _.2 _.3)))
            ((q.0 (_.0 _.1 _.2 _.3 _.4)))))
        ))
    #t)
  )
  
  ; Using the properties of the unifier to do the proper garbage
  ; collection of logical vars
  
  ; (test-check 'lv-elim-1
  ;   (reify
  ;     (let-lv (x z dummy)
  ;       (at@ 
  ; 	(_exists (y)
  ; 	  (== `(,x ,z ,y) `(5 9 ,x)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((y.0 . 5) (z.0 . 9) (x.0 . 5) (dummy.0 . dummy)))
  ;   ;'((z.0 . 9) (x.0 . 5) (dummy.0 . dummy)))
  
  ; (test-check 'lv-elim-2
  ;   (reify
  ;     (let-lv (x dummy)
  ;       (at@ 
  ; 	(_exists (y)
  ; 	  (== `(,x ,y) `((5 ,y) ,7)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((y.0 . 7) (x.0 5 y.0) (dummy.0 . dummy)))
  ;   ;'((a*.0 . 7) (x.0 5 a*.0) (dummy.0 . dummy)))
  
  ; ; verifying corollary 2 of proposition 10
  ; (test-check 'lv-elim-3
  ;   (reify
  ;     (let-lv (x v dummy)
  ;       (at@ 
  ; 	(_exists (y)
  ; 	  (== x `(a b c ,v d)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((x.0 a b c v.0 d) (dummy.0 . dummy)))
  ;   ;'((a*.0 . v.0) (x.0 a b c a*.0 d) (dummy.0 . dummy)))
  
  ; ; pruning several variables sequentially and in parallel
  ; (test-check 'lv-elim-4-1
  ;   (reify
  ;     (let-lv (x v b dummy)
  ;       (at@ 
  ; 	(let-lv (y)
  ; 	  (== `(,b ,x ,y) `(,x ,y 1)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((y.0 . 1) (x.0 . y.0) (b.0 . x.0) (dummy.0 . dummy)))
  
  ; ; (test-check 'lv-elim-4-2
  ; ;   (concretize
  ; ;     (let-lv (v b dummy)
  ; ;       (at@ 
  ; ; 	(_exists (x)
  ; ; 	  (_exists (y)
  ; ; 	    (== `(,b ,x ,y) `(,x ,y 1))))
  ; ; 	  (lambda@ (fk subst) subst)
  ; ; 	  initial-fk
  ; ; 	  (unit-subst dummy 'dummy))))
  ; ;     '((b.0 . 1) (dummy.0 . dummy)))
  
  ; ; (test-check 'lv-elim-4-3
  ; ;   (concretize
  ; ;     (let-lv (v b dummy)
  ; ;       (at@ 
  ; ; 	(_exists (y)
  ; ; 	  (_exists (x)
  ; ; 	    (== `(,b ,x ,y) `(,x ,y 1))))
  ; ; 	  (lambda@ (fk subst) subst)
  ; ; 	  initial-fk
  ; ; 	  (unit-subst dummy 'dummy))))
  ; ;     '((b.0 . 1) (dummy.0 . dummy)))
  
  ; (test-check 'lv-elim-4-4
  ;   (reify
  ;     (let-lv (v b dummy)
  ;       (at@ 
  ; 	(_exists (x y)
  ; 	    (== `(,b ,x ,y) `(,x ,y 1)))
  ; 	  (lambda@ (fk subst) subst)
  ; 	  initial-fk
  ; 	  (unit-subst dummy 'dummy))))
  ;   '((y.0 . 1) (x.0 . y.0) (b.0 . x.0) (dummy.0 . dummy)))
  ;   ;'((b.0 . 1) (dummy.0 . dummy)))
  
  ; ; pruning several variables sequentially and in parallel
  ; ; for indirect (cyclic) dependency
  ; (test-check 'lv-elim-5-1
  ;   (reify
  ;     (let-lv (x v b dummy)
  ;       (at@ 
  ; 	(let-lv (y)
  ; 	  (== `(,b ,y ,x) `(,x (1 ,x) ,y)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((x.0 1 x.0) (y.0 1 x.0) (b.0 . x.0) (dummy.0 . dummy)))
  ;   ;'((x.0 1 a*.0) (a*.0 . x.0) (y.0 1 a*.0) (b.0 . x.0) (dummy.0 . dummy)))
  
  ; ; (test-check 'lv-elim-5-2
  ; ;   (concretize
  ; ;     (let-lv (v b dummy)
  ; ;       (at@ 
  ; ; 	(_exists (x)
  ; ; 	  (_exists (y)
  ; ; 	  (== `(,b ,y ,x) `(,x (1 ,x) ,y))))
  ; ; 	(lambda@ (fk subst) subst)
  ; ; 	initial-fk
  ; ; 	(unit-subst dummy 'dummy))))
  ; ;   '((a*.0 1 a*.0) (b.0 1 a*.0) (dummy.0 . dummy)))
  
  ; ; (test-check 'lv-elim-5-3
  ; ;   (concretize
  ; ;     (let-lv (v b dummy)
  ; ;       (at@ 
  ; ; 	(_exists (y)
  ; ; 	  (_exists (x)
  ; ; 	  (== `(,b ,y ,x) `(,x (1 ,x) ,y))))
  ; ; 	(lambda@ (fk subst) subst)
  ; ; 	initial-fk
  ; ; 	(unit-subst dummy 'dummy))))
  ; ;   '((a*.0 1 a*.0) (b.0 1 a*.0) (dummy.0 . dummy)))
  
  ; (test-check 'lv-elim-5-4
  ;   (reify
  ;     (let-lv (v b dummy)
  ;       (at@ 
  ; 	(_exists (x y)
  ; 	  (== `(,b ,y ,x) `(,x (1 ,x) ,y)))
  ; 	(lambda@ (fk subst) subst)
  ; 	initial-fk
  ; 	(unit-subst dummy 'dummy))))
  ;   '((x.0 1 x.0) (y.0 1 x.0) (b.0 . x.0) (dummy.0 . dummy)))
  ;  ;'((a*.0 1 a*.0) (b.0 1 a*.0) (dummy.0 . dummy)))
  
  ; ; We should only be concerned about a direct dependency:
  ; ;  ((x . y) (y . (1 t)) (t . x) (a . x))
  ; ; pruning x and y in sequence or in parallel gives the same result:
  ; ;  ((t . (1 t)) (a . (1 t)))
  
  
  ; Extending relations in truly mathematical sense.
  ; First, why do we need this.
  (let*
    ((fact1 (fact () 'x1 'y1))
     (fact2 (fact () 'x2 'y2))
     (fact3 (fact () 'x3 'y3))
     (fact4 (fact () 'x4 'y4))
  
     ; R1 and R2 are overlapping
     (R1 (extend-relation (a1 a2) fact1 fact2))
     (R2 (extend-relation (a1 a2) fact1 fact3))
    )
     ; Infinitary relation
     ; r(z,z).
     ; r(s(X),s(Y)) :- r(X,Y).
    (letrec
     ((Rinf
       (extend-relation (a1 a2)
         (fact () 'z 'z)
         (relation (x y t1 t2)
  	 (to-show t1 t2)
  	 (all
  	   (== t1 `(s ,x))
  	   (== t2 `(s ,y))
  	   (Rinf x y)))))
    )
  
    (cout nl "R1:" nl)
    (_pretty-print (solve 10 (x y) (R1 x y)))
    (cout nl "R2:" nl)
    (_pretty-print (solve 10 (x y) (R2 x y)))
    (cout nl "R1+R2:" nl)
    (_pretty-print 
      (solve 10 (x y)
        ((extend-relation (a1 a2) R1 R2) x y)))
  
    (cout nl "Rinf:" nl)
    (values (_pretty-print (solve 5 (x y) (Rinf x y))))
    (cout nl "Rinf+R1: Rinf starves R1:" nl)
    (values
      (_pretty-print 
        (solve 5 (x y)
  	((extend-relation (a1 a2) Rinf R1) x y))))
  
  ; Solving the starvation problem: extend R1 and R2 so that they
  ; are interleaved
  ; ((sf-extend R1 R2) sk fk)
  ; (R1 sk fk)
  ; If R1 fails, we try the rest of R2
  ; If R1 succeeds, it executes (sk fk)
  ; with fk to re-prove R1. Thus fk is the "rest" of R1
  ; So we pass sk (lambda () (run-rest-of-r2 interleave-with-rest-of-r1))
  ; There is a fixpoint in the following algorithm!
  ; Or a second-level shift/reset!
  
    (test-check "Rinf+R1"
      (values
        (solve 7 (x y)
  	(any-interleave (Rinf x y) (R1 x y))))
      '(((x.0 z) (y.0 z))
         ((x.0 x1) (y.0 y1))
         ((x.0 (s z)) (y.0 (s z)))
         ((x.0 x2) (y.0 y2))
         ((x.0 (s (s z))) (y.0 (s (s z))))
         ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
         ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z)))))))
      )
  
    (test-check "R1+Rinf"
      (values
        (solve 7 (x y)
  	(any-interleave (R1 x y) (Rinf x y))))
      '(((x.0 x1) (y.0 y1))
         ((x.0 z) (y.0 z))
         ((x.0 x2) (y.0 y2))
         ((x.0 (s z)) (y.0 (s z)))
         ((x.0 (s (s z))) (y.0 (s (s z))))
         ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
         ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z)))))))
      )
  
  
    (test-check "R2+R1"
      (solve 7 (x y)
        (any-interleave (R2 x y) (R1 x y)))
      '(((x.0 x1) (y.0 y1))
         ((x.0 x1) (y.0 y1))
         ((x.0 x3) (y.0 y3))
         ((x.0 x2) (y.0 y2)))
      )
  
    (test-check "R1+fact3"
      (solve 7 (x y)
        (any-interleave (R1 x y) (fact3 x y)))
      '(((x.0 x1) (y.0 y1)) ((x.0 x3) (y.0 y3)) ((x.0 x2) (y.0 y2)))
      )
  
    (test-check "fact3+R1"
      (solve 7 (x y)
        (any-interleave (fact3 x y) (R1 x y)))
      '(((x.0 x3) (y.0 y3)) ((x.0 x1) (y.0 y1)) ((x.0 x2) (y.0 y2)))
      )
  
  ; testing all-interleave
    (test-check 'all-interleave-1
      (solve 100 (x y z)
        (all-interleave
  	(any (== x 1) (== x 2))
  	(any (== y 3) (== y 4))
  	(any (== z 5) (== z 6) (== z 7))))
      '(((x.0 1) (y.0 3) (z.0 5))
         ((x.0 2) (y.0 3) (z.0 5))
         ((x.0 1) (y.0 4) (z.0 5))
         ((x.0 2) (y.0 4) (z.0 5))
         ((x.0 1) (y.0 3) (z.0 6))
         ((x.0 2) (y.0 3) (z.0 6))
         ((x.0 1) (y.0 4) (z.0 6))
         ((x.0 2) (y.0 4) (z.0 6))
         ((x.0 1) (y.0 3) (z.0 7))
         ((x.0 2) (y.0 3) (z.0 7))
         ((x.0 1) (y.0 4) (z.0 7))
         ((x.0 2) (y.0 4) (z.0 7)))
      )
  
    (test-check "R1 * Rinf: clearly starvation"
      (solve 5 (x y u v)
        (all (R1 x y) (Rinf u v)))
    ; indeed, only the first choice of R1 is apparent
      '(((x.0 x1) (y.0 y1) (u.0 z) (v.0 z))
         ((x.0 x1) (y.0 y1) (u.0 (s z)) (v.0 (s z)))
         ((x.0 x1) (y.0 y1) (u.0 (s (s z))) (v.0 (s (s z))))
         ((x.0 x1) (y.0 y1) (u.0 (s (s (s z)))) (v.0 (s (s (s z)))))
         ((x.0 x1) (y.0 y1) (u.0 (s (s (s (s z))))) (v.0 (s (s (s (s z)))))))
      )
  
    (test-check "R1 * Rinf: interleaving"
      (solve 5 (x y u v)
        (all-interleave (R1 x y) (Rinf u v)))
    ; both choices of R1 are apparent
      '(((x.0 x1) (y.0 y1) (u.0 z) (v.0 z))
         ((x.0 x2) (y.0 y2) (u.0 z) (v.0 z))
         ((x.0 x1) (y.0 y1) (u.0 (s z)) (v.0 (s z)))
         ((x.0 x2) (y.0 y2) (u.0 (s z)) (v.0 (s z)))
         ((x.0 x1) (y.0 y1) (u.0 (s (s z))) (v.0 (s (s z)))))
      )
  
    ;; Test for nonoverlapping.
  
    (cout nl "any-union" nl)
    (test-check "R1+R2"
      (solve 10 (x y)
        (any-union (R1 x y) (R2 x y)))
      '(((x.0 x1) (y.0 y1))
         ((x.0 x2) (y.0 y2))
         ((x.0 x3) (y.0 y3))))
  
    (test-check "R2+R1"
      (solve 10 (x y)
        (any-union (R2 x y) (R1 x y)))
      '(((x.0 x1) (y.0 y1))
         ((x.0 x3) (y.0 y3))
         ((x.0 x2) (y.0 y2))))
    
    (test-check "R1+R1"
      (solve 10 (x y)
        (any-union (R1 x y) (R1 x y)))
      '(((x.0 x1) (y.0 y1))
         ((x.0 x2) (y.0 y2))))
    
    (test-check "Rinf+R1"
      (solve 7 (x y)
        (any-union (Rinf x y) (R1 x y)))
      '(((x.0 z) (y.0 z))
       ((x.0 x1) (y.0 y1))
        ((x.0 (s z)) (y.0 (s z)))
        ((x.0 x2) (y.0 y2))
        ((x.0 (s (s z))) (y.0 (s (s z))))
        ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
        ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z))))))))
  
    (test-check "R1+RInf"
      (solve 7 (x y)
        (any-union (R1 x y) (Rinf x y)))
      '(((x.0 x1) (y.0 y1))
         ((x.0 z) (y.0 z))
         ((x.0 x2) (y.0 y2))
         ((x.0 (s z)) (y.0 (s z)))
         ((x.0 (s (s z))) (y.0 (s (s z))))
         ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
         ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z))))))))
  
  
  ; Infinitary relation Rinf2
  ; r(z,z).
  ; r(s(s(X)),s(s(Y))) :- r(X,Y).
  ; Rinf2 overlaps with Rinf in the infinite number of points
    (letrec
      ((Rinf2
         (extend-relation (a1 a2)
  	 (fact () 'z 'z)
  	 (relation (x y t1 t2)
  	   (to-show t1 t2)
  	   (all
  	     (== t1 `(s (s ,x)))
  	     (== t2 `(s (s ,y)))
  	     (Rinf2 x y)))))
        )
      (test-check "Rinf2"
        (solve 5 (x y) (Rinf2 x y))
        '(((x.0 z) (y.0 z))
  	 ((x.0 (s (s z))) (y.0 (s (s z))))
  	 ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z))))))
  	 ((x.0 (s (s (s (s (s (s z)))))))
  	   (y.0 (s (s (s (s (s (s z))))))))
  	 ((x.0 (s (s (s (s (s (s (s (s z)))))))))
  	   (y.0 (s (s (s (s (s (s (s (s z))))))))))))
  
      (test-check "Rinf+Rinf2"
        (solve 9 (x y)
  	(any-union (Rinf x y) (Rinf2 x y)))
        '(((x.0 z) (y.0 z))
  	 ((x.0 (s z)) (y.0 (s z)))
  	 ((x.0 (s (s z))) (y.0 (s (s z))))
  	 ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z))))))
  	 ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
  	 ((x.0 (s (s (s (s (s (s z)))))))
  	   (y.0 (s (s (s (s (s (s z))))))))
  	 ((x.0 (s (s (s (s (s (s (s (s z)))))))))
  	   (y.0 (s (s (s (s (s (s (s (s z))))))))))
  	 ((x.0 (s (s (s (s (s z)))))) (y.0 (s (s (s (s (s z)))))))
  	 ((x.0 (s (s (s (s (s (s (s (s (s (s z)))))))))))
  	   (y.0 (s (s (s (s (s (s (s (s (s (s z))))))))))))))
      
      (test-check "Rinf2+Rinf"
        (solve 9 (x y)
  	(any-union (Rinf2 x y) (Rinf x y)))
        '(((x.0 z) (y.0 z))
  	 ((x.0 (s z)) (y.0 (s z)))
  	 ((x.0 (s (s z))) (y.0 (s (s z))))
  	 ((x.0 (s (s (s z)))) (y.0 (s (s (s z)))))
  	 ((x.0 (s (s (s (s z))))) (y.0 (s (s (s (s z))))))
  	 ((x.0 (s (s (s (s (s z)))))) (y.0 (s (s (s (s (s z)))))))
  	 ((x.0 (s (s (s (s (s (s z)))))))
  	   (y.0 (s (s (s (s (s (s z))))))))
  	 ((x.0 (s (s (s (s (s (s (s z))))))))
  	   (y.0 (s (s (s (s (s (s (s z)))))))))
  	 ((x.0 (s (s (s (s (s (s (s (s z)))))))))
  	   (y.0 (s (s (s (s (s (s (s (s z))))))))))))
      )))
  
  
  (cout nl "Append with limited depth" nl)
  ; In Prolog, we normally write:
  ; append([],L,L).
  ; append([X|L1],L2,[X|L3]) :- append(L1,L2,L3).
  ;
  ; If we switch the clauses, we get non-termination.
  ; In our system, it doesn't matter!
  
  (letrec
    ((extend-clause-1
       (relation (l)
         (to-show '() l l)
         succeed))
     (extend-clause-2
       (relation (x l1 l2 l3)
         (to-show `(,x . ,l1) l2 `(,x . ,l3))
         (extend-rel l1 l2 l3)))
     (extend-rel
         (extend-relation-with-recur-limit 5 (a b c)
  	 extend-clause-1
  	 extend-clause-2))
      )
  
    ; Note (solve 100 ...)
    ; Here 100 is just a large number: we want to print all solutions
      (cout nl "Extend: clause1 first: " 
        (solve 100 (a b c) (extend-rel a b c))
        nl))
  
  (letrec
    ((extend-clause-1
       (relation (l)
         (to-show '() l l)
         succeed))
     (extend-clause-2
       (relation (x l1 l2 l3)
         (to-show `(,x . ,l1) l2 `(,x . ,l3))
         (extend-rel l1 l2 l3)))
     (extend-rel
       (extend-relation-with-recur-limit 3 (a b c)
         extend-clause-2
         extend-clause-1)))
  
    (cout nl "Extend: clause2 first. In Prolog, it would diverge!: " 
      (solve 100 (a b c) (extend-rel a b c)) nl))
  
  
  (letrec
    ((base-+-as-relation
       (fact (n) 'zero n n))
     (recursive-+-as-relation
       (relation (n1 n2 n3)
         (to-show `(succ ,n1) n2 `(succ ,n3))
         (plus-as-relation n1 n2 n3)))
     ; Needed eta-expansion here: otherwise, SCM correctly reports
     ; an error (but Petite doesn't, alas)
     ; This is a peculiarity of extend-relation as a macro
     ; Potentially, we need the same approach as in minikanren
     (plus-as-relation
       (extend-relation (a1 a2 a3)
         (lambda (a1 a2 a3) (base-+-as-relation a1 a2 a3))
         (lambda (a1 a2 a3) (recursive-+-as-relation a1 a2 a3))
         ))
      )
  
    (test-check "Addition"
      (solve 20 (x y)
        (plus-as-relation x y '(succ (succ (succ (succ (succ zero)))))))
      '(((x.0 zero) (y.0 (succ (succ (succ (succ (succ zero)))))))
         ((x.0 (succ zero)) (y.0 (succ (succ (succ (succ zero))))))
         ((x.0 (succ (succ zero))) (y.0 (succ (succ (succ zero)))))
         ((x.0 (succ (succ (succ zero)))) (y.0 (succ (succ zero))))
         ((x.0 (succ (succ (succ (succ zero))))) (y.0 (succ zero)))
         ((x.0 (succ (succ (succ (succ (succ zero)))))) (y.0 zero))))
  
    (newline)
  )
10)

;; ========================================================================
;; type-inference example
;; ========================================================================

; Type Inference
;
; We show two variations of Hindley-Milner type inference. Both
; variations support polymorphic, generalizing `let'. Both variations
; use Kanren's logical variables for type variables, and take advantage
; of Kanren's unifier to solve the equations that arise during the course
; of type inference. These features make the Kanren realization of the
; type inference algorithm concise and lucid.
;
; The variations differ in the syntax of the `source' language, and in
; the way type environments are implemented.  One variation realizes
; type environments as regular lists, of associations between symbolic
; variable names and their types. The other variation extends the type
; entailment relation (which is a first-class relation in Kanren). The
; latter approach is similar to that of inductive proofs (see files
; ./deduction.scm and ./mirror-equ.scm)
;
; $Id: type-inference.scm,v 4.50 2005/02/12 00:05:01 oleg Exp $

; (display "Type inference") (newline)

; Variation 1: use a subset of Scheme itself as the source language
; The following two functions translate between the source language
; and intermediate one.

(define parse
  (lambda (e)
    (cond
      ((symbol? e) `(var ,e))
      ((number? e) `(intc ,e))
      ((boolean? e) `(boolc ,e))
      (else (case (car e)
              ((zero?) `(zero? ,(parse (cadr e))))
              ((sub1) `(sub1 ,(parse (cadr e))))
              ((+) `(+ ,(parse (cadr e)) ,(parse (caddr e))))
              ((if) `(if ,(parse (cadr e)) ,(parse (caddr e)) ,(parse (cadddr e))))
              ((fix) `(fix ,(parse (cadr e))))
              ((lambda) `(lambda ,(cadr e) ,(parse (caddr e))))
              ((let) `(let ((,(car (car (cadr e))) ,(parse (cadr (car (cadr e))))))
                        ,(parse (caddr e))))
              (else `(app ,(parse (car e)) ,(parse (cadr e)))))))))

(define unparse
  (lambda (e)
    (case (car e)
      ((var) (cadr e))
      ((intc) (cadr e))
      ((boolc) (cadr e))
      ((zero?) `(zero? ,(unparse (cadr e))))
      ((sub1) `(sub1 ,(unparse (cadr e))))
      ((+) `(+ ,(unparse (cadr e)) ,(unparse (caddr e))))
      ((if) `(if ,(unparse (cadr e)) ,(unparse (caddr e)) ,(unparse (cadddr e))))
      ((fix) `(fix ,(unparse (cadr e))))
      ((lambda) `(lambda (,(car (cadr e))) ,(unparse (caddr e))))
      ((let) 
       `(let ((,(car (car (cadr e)))
               ,(unparse (cadr (car (cadr e))))))
          ,(unparse (caddr e))))
      ((app) `(,(unparse (cadr e)) ,(unparse (caddr e)))))))

; Type environments
;
; A type environment (often denoted as \Gamma, or g in this code)
; is an association between the names of variables of source language
; terms and the types of those variables.
; As a side condition, each variable may occur in the list
; exactly once.
; Hmm, to model lexical scope better, we may relax that condition.
;
; Here we implement type environments as regular associative lists,
; lists of triples:
;    (<var-name> non-generic <type>)
;    (<var-name> generic <type-gen>)
;
; <var-name> is a symbolic name of a source term variable.
; <type> is a type term, e.g., int, bool, (--> int bool), etc.
; <type> may include logical variables, which are treated then as
; type variables.
;
; The association '(<var-name> generic <type-gen>)' asserts that
; <var-name> is given a _generic_ type. <type-gen> then is a
; predicate of arity 1. To be more precise, (<type-gen> <type>)
; is an goal that succeeds or fails depending on the fact if
; <type> is an instance of a generic type represented by <type-gen>.
; 
; This is precisely the logical meaning of generalization, as
; pointed out by Ken:
; <blockquote>
; A cleaner, but less efficient, formulation of HM type inference is to
; use the following let rule instead:
;
;     Gamma |- M : t    Gamma |- N[M/x] : t'
;     -------------------------------------- Let
;          Gamma |- let x = M in N : t'
;
; Look ma, no FV!  In words, this rule treats let as a construct for
; syntactic substitution.  This means storing either M, or a thunk
; returning (a logical variable associated with a fresh copy of) the type
; of M, under x in the environment.  This formulation avoids var? while
; taking advantage of built-in unification (to some extent).
; </blockquote>
;
; We must emphasize that in Kanren, relations are first-class, and may,
; therefore, be included as parts of a data structure: of an associative
; list in our case.

; Because type environments are regular lists, we can build them using
; regular cons. The empty type environemnt is the empty list.  The
; following is a Kanren relation that searches the associative
; list. We are interested in the first match.

; The following is a general-purpose function
; (membero v l) holds if v is a member of the list l. 
; 'v' must be sufficiently instantiated (at least, the search key
; must be instantiated, to justify our use of the committed choice
; non-determinism).
(define membero
  (relation (v lt lh)
    (to-show v `(,lh . ,lt))
    (if-some (== v lh) succeed
      (membero v lt))))

; The following is the type-environment-specific function.
; (env g v t) holds if the source term variable v has a type t
; in the environment g.
; We require that 'v' be instantiated, to justify our use
; of the committed choice non-determinism (e.g., membero).

(define env
  (relation (head-let g v t)
    (_exists (tq)
      (all!!
	(membero `(,v . ,tq) g)
	(any
	  (== tq `(non-generic ,t))
	  (_exists (type-gen)
	    (all!!
	      (== tq `(generic ,type-gen))
	      (project (type-gen)
		(type-gen t)))))))))

;;;; This starts the rules

(define int 'int)
(define bool 'bool)

(define var-rel
  (relation (g v t)
    (to-show g `(var ,v) t)
    (all! (env g v t))))

(define int-rel
  (fact (g x) g `(intc ,x) int))

(define bool-rel
  (fact (g x) g `(boolc ,x) bool))

(define zero?-rel
  (relation (g x)
    (to-show g `(zero? ,x) bool)
    (all! (!- g x int))))

(define sub1-rel
  (relation (g x)
    (to-show g `(sub1 ,x) int)
    (all! (!- g x int))))

(define plus-rel
  (relation (g x y)
    (to-show g `(+ ,x ,y) int)
    (all!! (!- g x int) (!- g y int))))

(define if-rel
  (relation (g t test conseq alt)
    (to-show g `(if ,test ,conseq ,alt) t)
    (all!! (!- g test bool) (!- g conseq t) (!- g alt t))))

(define lambda-rel
  (relation (g v t body type-v)
    (to-show g `(lambda (,v) ,body) `(a--> ,type-v ,t))
    (all! (!- `((,v non-generic ,type-v) . ,g) body t))))

(define app-rel
  (relation (g t rand rator)
    (to-show g `(app ,rator ,rand) t)
    (_exists (t-rand)
      (all!! (!- g rator `(a--> ,t-rand ,t)) (!- g rand t-rand)))))

(define fix-rel
  (relation (g rand t)
    (to-show g `(fix ,rand) t)
    (all! (!- g rand `(a--> ,t ,t)))))

; Type-checking polymorphic let: (let ([,v ,rand]) ,body)
; There is obviously an inefficiency, because we typecheck `rand'
; every time the variable `v' occurs in the body (and once more). 
; We can fix it, with copy term. But for now, we leave this optimization out.
; The reason to test `(!- g rand some-type)' at the very beginning is
; to make sure that `rand' itself is well-typed. As Ken pointed out,
; we must outlaw expressions such as (let ((x (z z))) y) where 'x'
; does not occur in the body. The variable 'x' still must have some
; type.

(define polylet-rel
  (relation (g v rand body t)
    (to-show g `(let ((,v ,rand)) ,body) t)
    (all!!
      (_exists (some-type) (!- g rand some-type))
      (!- `((,v generic ,(relation (head-let t-rand)
			   (all!!
			     (!- g rand t-rand)
			     (trace-vars 'poly-let (t-rand rand)))))
	     . ,g)
	body t))))


(define !-
  (extend-relation (a1 a2 a3)
    var-rel int-rel bool-rel zero?-rel sub1-rel plus-rel 
    if-rel lambda-rel app-rel fix-rel polylet-rel))

(define (ti-tests)
(test-check 'test-!-1
  (and
    (equal?
      (solution (?) (!- '() '(intc 17) int))
      '((?.0 _.0)))
    (equal?
      (solution (?) (!- '() '(intc 17) ?))
      '((?.0 int))))
  #t)

(test-check 'arithmetic-primitives
  (solution (?) (!- '() '(zero? (intc 24)) ?))
  '((?.0 bool)))

(test-check 'test-!-sub1
  (solution (?) (!- '() '(zero? (sub1 (intc 24))) ?))
  '((?.0 bool)))

(test-check 'test-!-+
  (solution (?)
      (!- '() '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
  '((?.0 bool)))

(test-check 'test-!-2
  (and
    (equal?
      (solution (?) (!- '() '(zero? (intc 24)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?) (!- '() '(zero? (+ (intc 24) (intc 50))) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '() '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
      '((?.0 bool))))
  #t)

(test-check 'test-!-3
  (solution (?) (!- '() '(if (zero? (intc 24)) (intc 3) (intc 4)) ?))
  '((?.0 int)))

(test-check 'if-expressions
  (solution (?)
    (!- '() '(if (zero? (intc 24)) (zero? (intc 3)) (zero? (intc 4))) ?))
  '((?.0 bool)))

(test-check 'variables
  (and
    (equal?
      (solution (?)
          (env '((b non-generic int) (a non-generic bool)) 'a ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((a non-generic int)) '(zero? (var a)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!- '((b non-generic bool) (a non-generic int))
            '(zero? (var a))
            ?))
      '((?.0 bool))))
  #t)

(test-check 'variables-4a
  (solution (?)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (intc 5)))
        ?))
  '((?.0 (a--> int int))))

(test-check 'variables-4b
  (solution (?)
    (!- '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (var a)))
        ?))
  '((?.0 (a--> int int))))

(test-check 'variables-4c
  (solution (?)
    (!- '() '(lambda (a) (lambda (x) (+ (var x) (var a)))) ?))
  '((?.0 (a--> int (a--> int int)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '() (parse
              '(lambda (f)
                 (lambda (x)
                   ((f x) x))))
        ?))
  '((?.0 (a-->
           (a--> _.0 (a--> _.0 _.1))
           (a--> _.0 _.1)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (if (zero? n)
                         0
                         (+ n (sum (sub1 n)))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (+ n (sum (sub1 n))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!- '()
        (parse '((lambda (f)
                   (if (f (zero? 5))
                       (+ (f 4) 8)
                       (+ (f 3) 7)))
                 (lambda (x) x)))
        ?))
  #f)

(test-check 'polymorphic-let
  (solution (?)
    (!- '()
        (parse
          '(let ((f (lambda (x) x)))
             (if (f (zero? 5))
                 (+ (f 4) 8)
                 (+ (f 3) 7))))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax
  (solution (?)
    (!- '()
        '(app
           (fix
             (lambda (sum)
               (lambda (n)
                 (if (if (zero? (var n)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (var n) (app (var sum) (sub1 (var n))))))))
           (intc 10))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax-but-long-jumps/poly-let
  (solution (?)
    (!- '()
        '(let ((f (lambda (x) (var x))))
           (if (app (var f) (zero? (intc 5)))
               (+ (app (var f) (intc 4)) (intc 8))
               (+ (app (var f) (intc 3)) (intc 7))))
        ?))
  '((?.0 int)))

(test-check 'type-habitation-1
  (solution (g ?)
    (!- g ? '(a--> int int)))
  '((g.0 ((_.0 non-generic (a--> int int)) . _.1)) (?.0 (var _.0))))

(test-check 'type-habitation-2
  (solution (g h r q z y t)
    (!- g `(,h ,r (,q ,z ,y)) t))
  '((g.0 ((_.0 non-generic int) . _.1))
    (h.0 +)
    (r.0 (var _.0))
    (q.0 +)
    (z.0 (var _.0))
    (y.0 (var _.0))
    (t.0 int))
)

(test-check 'type-habitation-3
  (and
    (equal?
      (solution (la f b)
	(!- '() `(,la (,f) ,b) '(a--> int int)))
      '((la.0 lambda) (f.0 _.0) (b.0 (var _.0))))
    (equal?
      (solution (h r q z y t u v)
	(!- '() `(,h ,r (,q ,z ,y)) `(,t ,u ,v)))
      '((h.0 lambda)
        (r.0 (_.0))
        (q.0 +)
        (z.0 (var _.0))
        (y.0 (var _.0))
        (t.0 a-->)
        (u.0 int)
        (v.0 int))))
  #t)

10)

;----------------------------------------------------------------------
; A different implementation of type environments
; We define a first-class (and recursive) relation !-
; so that (!- `(var ,v) t) holds iff the source term variable v has a type
; t. 
; This variant is close to the `natural deduction' scheme.
; It also has an OO flavor: we need open recursion.

; The following are the separate components of which the relation
; !- will be built. All these components nevertheless receive the full
; !- as the argument. Actually, they will receive the 'self'-like
; argument. We need to explicitly find the fixpoint.

; (cout nl "Natural-deduction-like type inference" nl nl)


(define pint-rel
  (lambda (s!-)
    (fact (x) `(intc ,x) int)))

(define pbool-rel
  (lambda (s!-)
    (fact (x) `(boolc ,x) bool)))

(define pzero?-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x)
	(to-show `(zero? ,x) bool)
	(all! (!- x int))))))

(define psub1-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x)
	(to-show `(sub1 ,x) int)
	(all! (!- x int))))))

(define p+-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (x y)
	(to-show `(+ ,x ,y) int)
	(all!! (!- x int) (!- y int))))))

(define pif-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (t test conseq alt)
	(to-show `(if ,test ,conseq ,alt) t)
	(all!! (!- test bool) (!- conseq t) (!- alt t))))))

; Here we extend !- with an additional assumption that v has the type
; type-v. This extension corresponds to a non-generic, regular type.
(define plambda-rel
  (lambda (s!-)
    (relation (v t body type-v)
      (to-show `(lambda (,v) ,body) `(a--> ,type-v ,t))
      (let* ((snew-!-
	       (lambda (self)
		 (extend-relation (v t)
		   (fact () `(var ,v) type-v) ; lexically-scoped relation
		   (s!- self))))
	      (!- (snew-!- snew-!-)))
	(all! (!- body t))))))


(define papp-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (t rand rator)
	(to-show `(app ,rator ,rand) t)
	(_exists (t-rand)
	  (all!! (!- rator `(a--> ,t-rand ,t)) (!- rand t-rand)))))))

(define pfix-rel
  (lambda (s!-)
     (let ((!- (s!- s!-)))
       (relation (rand t)
	 (to-show `(fix ,rand) t)
	 (all! (!- rand `(a--> ,t ,t)))))))

; Type-checking polymorphic let: (let ((,v ,rand)) ,body)
; There is obviously an inefficiency, because we typecheck `rand'
; every time the variable `v' occurs in the body (and once more). 
; We can fix it, with copy term. But for now, we leave this optimization out.
; The reason to test `(!- g rand some-type)' at the very beginning is
; to make sure that `rand' itself is well-typed. As Ken pointed out,
; we must outlaw expressions such as (let ((x (z z))) y) where 'x'
; does not occur in the body. The variable 'x' still must have some
; type.

(define ppolylet-rel
  (lambda (s!-)
    (let ((!- (s!- s!-)))
      (relation (v rand body t)
	(to-show `(let ((,v ,rand)) ,body) t)
	(all!! 
	  (_exists (some-type) (!- rand some-type))
	  (let* ((snew-!-
		   (lambda (self)
		     (extend-relation (v t)
		       (relation (head-let `(var ,v) t-rand)
			 (all!!
			   (!- rand t-rand)
			   (trace-vars 'poly-let (t-rand rand))))
		       (s!- self))))
		  (!- (snew-!- snew-!-)))
	    (!- body t)))))))

; Now we build the recursive !- relation, as a fixpoint

(define s!-
  (lambda (self)
    (lambda (v t)
      ((extend-relation (a1 a2)
	 (pint-rel self)
	 (pbool-rel self)  (pzero?-rel self)
	 (psub1-rel self)  (p+-rel self)
	 (pif-rel self)    (plambda-rel self)
	 (papp-rel self)   (pfix-rel self)
	 (ppolylet-rel self)) v t))))

(define !-/2 (s!- s!-))


; And we re-do all the tests

(define (ti-tests-2)

(test-check 'test-!-1
  (and
    (equal?
      (solution (?) (!-/2 '(intc 17) int))
      '((?.0 _.0)))
    (equal?
      (solution (?) (!-/2 '(intc 17) ?))
      '((?.0 int))))
  #t)

(test-check 'arithmetic-primitives
  (solution (?) (!-/2 '(zero? (intc 24)) ?))
  '((?.0 bool)))

(test-check 'test-!-sub1
  (solution (?) (!-/2 '(zero? (sub1 (intc 24))) ?))
  '((?.0 bool)))

(test-check 'test-!-+
  (solution (?)
      (!-/2 '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
  '((?.0 bool)))

(test-check 'test-!-2
  (and
    (equal?
      (solution (?) (!-/2 '(zero? (intc 24)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?) (!-/2 '(zero? (+ (intc 24) (intc 50))) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!-/2 '(zero? (sub1 (+ (intc 18) (+ (intc 24) (intc 50))))) ?))
      '((?.0 bool))))
  #t)

(test-check 'test-!-3
  (solution (?) (!-/2 '(if (zero? (intc 24)) (intc 3) (intc 4)) ?))
  '((?.0 int)))

(test-check 'if-expressions
  (solution (?)
    (!-/2 '(if (zero? (intc 24)) (zero? (intc 3)) (zero? (intc 4))) ?))
  '((?.0 bool)))

; Commented out: we need to extend !- if we wish to typecheck open terms
'(test-check 'variables
  (and
    (equal?
      (solution (?)
          (env '((b non-generic int) (a non-generic bool)) 'a ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!-/2 '((a non-generic int)) '(zero? (var a)) ?))
      '((?.0 bool)))
    (equal?
      (solution (?)
	(!-/2 '((b non-generic bool) (a non-generic int))
            '(zero? (var a))
            ?))
      '((?.0 bool))))
  #t)

(test-check 'variables-4a
  (solution (?)
    (!-/2 '(lambda (x) (+ (var x) (intc 5)))
        ?))
  '((?.0 (a--> int int))))

; Commented out: we need to extend !- if we wish to typecheck open terms
'(test-check 'variables-4b
  (solution (?)
    (!-/2 '((b non-generic bool) (a non-generic int))
        '(lambda (x) (+ (var x) (var a)))
        ?))
  '((?.0 (a--> int int))))

(test-check 'variables-4c
  (solution (?)
    (!-/2 '(lambda (a) (lambda (x) (+ (var x) (var a)))) ?))
  '((?.0 (a--> int (a--> int int)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!-/2 (parse
	  '(lambda (f)
	     (lambda (x)
	       ((f x) x))))
      ?))
  '((?.0 (a-->
           (a--> _.0 (a--> _.0 _.1))
           (a--> _.0 _.1)))))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!-/2 (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (if (zero? n)
                         0
                         (+ n (sum (sub1 n)))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!-/2 (parse
          '((fix (lambda (sum)
                   (lambda (n)
                     (+ n (sum (sub1 n))))))
            10))
        ?))
  '((?.0 int)))

(test-check 'everything-but-polymorphic-let
  (solution (?)
    (!-/2 (parse '((lambda (f)
                   (if (f (zero? 5))
                       (+ (f 4) 8)
                       (+ (f 3) 7)))
                 (lambda (x) x)))
        ?))
  #f)

(test-check 'polymorphic-let
  (solution (?)
    (!-/2 (parse
          '(let ((f (lambda (x) x)))
             (if (f (zero? 5))
                 (+ (f 4) 8)
                 (+ (f 3) 7))))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax
  (solution (?)
    (!-/2 '(app
           (fix
             (lambda (sum)
               (lambda (n)
                 (if (if (zero? (var n)) (boolc #t) (boolc #f))
                     (intc 0)
                     (+ (var n) (app (var sum) (sub1 (var n))))))))
           (intc 10))
        ?))
  '((?.0 int)))

(test-check 'with-robust-syntax-but-long-jumps/poly-let
  (solution (?)
    (!-/2 '(let ((f (lambda (x) (var x))))
           (if (app (var f) (zero? (intc 5)))
               (+ (app (var f) (intc 4)) (intc 8))
               (+ (app (var f) (intc 3)) (intc 7))))
        ?))
  '((?.0 int)))

; The latter doesn't work: but it wasn't too informative anyway
'(test-check 'type-habitation-1
  (solution (?)
    (!-/2 ? '(a--> int int)))
  '((g.0 ((v.0 non-generic (a--> int int)) . lt.0)) (?.0 (var v.0))))

(test-check 'type-habitation-2
  (solution (h r q z y t)
    (!-/2 `(,h ,r (,q ,z ,y)) t))
  '((h.0 +)
    (r.0 (intc _.0))
    (q.0 +)
    (z.0 (intc _.1))
    (y.0 (intc _.2))
    (t.0 int))
)

(test-check 'type-habitation-3
  (and
    (equal?
      (solution (la f b)
	(!-/2  `(,la (,f) ,b) '(a--> int int)))
      '((la.0 lambda) (f.0 _.0) (b.0 (var _.0))))
    (equal?
      (solution (h r q z y t u v)
	(!-/2  `(,h ,r (,q ,z ,y)) `(,t ,u ,v)))
      '((h.0 lambda)
        (r.0 (_.0))
        (q.0 +)
        (z.0 (var _.0))
        (y.0 (var _.0))
        (t.0 a-->)
        (u.0 int)
        (v.0 int))))
  #t)
10)


; The code below uses the low-level function var? Every use of var?
; entails a proof obligation that such use is safe. In our case here,
; invertible-binary-function->ternary-relation and
; invertible-unary-function->binary-relation are sound.

(define invertible-binary-function->ternary-relation
  (lambda (op inverted-op)
    (relation (head-let x y z)
      (project/no-check (z)
	(if-only (predicate (var? z))
          (project (x y) (== z (op x y))) ; z is free, x and y must not
	  (project/no-check (y)
	    (if-only (predicate (var? y)) ; y is free, z is not
	      (project (x)
		(== y (inverted-op z x)))
	      (project/no-check (x)
		(if-only (predicate (var? x)) ; x is free, y and z are not
		  (== x (inverted-op z y))
		  (== z (op x y)))))))))))


(define t++ (invertible-binary-function->ternary-relation + -))
(define t-- (invertible-binary-function->ternary-relation - +))
(define ** (invertible-binary-function->ternary-relation * /))
(define // (invertible-binary-function->ternary-relation / *))

(define symbol->lnum
  (lambda (sym)
    (map char->integer (string->list (symbol->string sym)))))

(define lnum->symbol
  (lambda (lnums)
    (string->symbol (list->string (map integer->char lnums)))))

(define invertible-unary-function->binary-relation
  (lambda (op inverted-op)
    (relation (head-let x y)
      (project/no-check (y)
	(if-only (predicate (var? y))
	  (project (x) (== y (op x)))	; y is free, x must not
	  (project/no-check (x)
	    (if-only (predicate (var? x))
	      (== x (inverted-op y))
	      (== y (op x)))))))))

(define name
  (invertible-unary-function->binary-relation symbol->lnum lnum->symbol))

(define (ti-tests-3)
(test-check 'test-instantiated-1
  (and
    (equal?
      (solution (x) (t++ x 16.0 8))
      '((x.0 -8.0)))
    (equal?
      (solution (x) (t++ 10 16.0 x))
      '((x.0 26.0)))
    (equal?
      (solution (x) (t-- 10 x 3))
      '((x.0 13))))
  #t)

(test-check 'test-instantiated-2
  (and
    (equal?
      (solution (x) (name 'sleep x))
      '((x.0 (115 108 101 101 112))))
    (equal?
      (solution (x) (name x '(115 108 101 101 112)))
      '((x.0 sleep))))
  #t)
10)

;; ========================================================================
;; typeclasses example
;; ========================================================================

;(newline)
;(display "Checking for dependency satisfaction in Haskell typeclasses")
;(newline)
; Suppose we have the following Haskell class and instance declarations
;      class C a b c | a b -> c 
;      instance C a b c => C a (x,y,b) c
;      instance C a (a,c,b) c
;
; They will be compiled into the following database of instances,
; which define the class membership.
(define typeclass-C-instance-1
  (relation (a b c x y)
    (to-show a `(,x ,y ,b) c)
    (typeclass-C a b c)))

(define typeclass-C-instance-2
  (relation (a b c)
    (to-show a `(,a ,c ,b) c)
    succeed))

(define typeclass-C
  (extend-relation (a b c) 
    typeclass-C-instance-2
    typeclass-C-instance-1))

; Run the checker for the dependency a b -> c
; Try to find the counter-example, that is, two members of (C a b c)
; such that a's and b's are the same but the c's are different.

  
(define typeclass-counter-example-query
  (lambda (a b c1 c2)
    (all 
      (typeclass-C a b c1)
      (typeclass-C a b c2)
      (fails (project/no-check (c1 c2) (predicate (*equal? c1 c2)))))))

; This does loop
;'(define typeclass-C
;   (extend-relation (a b c) 
;     typeclass-C-instance-1
;     typeclass-C-instance-2))

(define typeclass-C/x
  (extend-relation-with-recur-limit 2 (a b c)
    typeclass-C-instance-1
    typeclass-C-instance-2))

; (pntall "~%Test: checking dependency satisfaction: Another example.~%")
; Suppose we have the following Haskell class and instance declarations
;	class F a b | a->b
;	instance F a b => F [a] [b]
;	instance F [a] a
;

(define typeclass-F
  (extend-relation-with-recur-limit 10 (a b)
    (relation (a b)
      (to-show `(list ,a) `(list ,b))
      (typeclass-F a b))
    (fact (a) `(list ,a) a)))


; Run the checker for the dependency a -> b
; Try to find the counter-example, that is, two members of (F a b)
; such that as is the same but bs are different.
(define typeclass-F-counter-example-query
  (lambda (a b1 b2)
    (all 
      (typeclass-F a b1)
      (typeclass-F a b2)
      (fails (project/no-check (b1 b2) (predicate (*equal? b1 b2)))))))

; (pntall "~%Overloading resolution in Haskell.~%")
; Suppose we have the following Haskell class and instance declarations
;	class F a b | a->b where f :: a->b->Bool
;	instance F a b => F [a] [b]
;
; we need to typecheck
;   g x = f [x] x
; which says that f:: [a] -> a -> Bool
; In general, we need to figure out which instance to choose for f.
; In other words, we need to find out which subset of F to use.
; Here's only one instance. So we need to figure out if it applies.

(define typeclass-F-instance-1
  (relation (a b)
    (to-show `(list ,a) `(list ,b))
    (typeclass-F/x a b)))

; This is a closed-world assumption
(define typeclass-F/x
  (extend-relation-with-recur-limit 10 (a b)
    typeclass-F-instance-1))

; This is an open-world assumption
(define typeclass-F/x2
  (extend-relation-with-recur-limit 2 (a b)
    typeclass-F-instance-1
    (relation (a b1 b2)	; a relation under constraint a->b
      (to-show a b1)
      (fails
	(all!
	  (typeclass-F/x a b2)
	  (fails (project/no-check (b1 b2) (predicate (*equal? b1 b2)))))))
    ))

(define (tc-tests)
  (pntall "~%Counter-example: ~s~%"
          (solution (a b c1 c2)
                    (typeclass-counter-example-query a b c1 c2)))

  (pntall "~%Counter-example: ~s~%"
          (solution (a b c1 c2)
                    (typeclass-counter-example-query a b c1 c2)))

  (pntall "~%Counter-example: ~s~%"
          (solve 4 (a b c1 c2)
                 (typeclass-counter-example-query a b c1 c2)))
  
  (pntall "~%Counter-example: ~s~%" 
          (solve 4 (a b1 b2) (typeclass-F-counter-example-query a b1 b2)))
  
  
  (test-check "Typechecking (closed world)" 
              (solve 4 (a)
                     (typeclass-F-instance-1 `(list ,a) a))
              '())					; meaning: does not typecheck!
  
  
  (pntall "~%Typechecking (open world): ~s~%" 
          (solve 4 (a) (typeclass-F-instance-1 `(list ,a) a)))
  
  (test-check "Typechecking (open world) f [x] int" 
              (solve 4 (a) (typeclass-F-instance-1 `(list ,a) 'int))
              '())					; meaning: does not typecheck!

  10
  )

;; ========================================================================
;; zebra example
;; ========================================================================

; (display "Zebra") (newline)

;   1. There are five houses in a row, each of a different color
;       and inhabited by men of different nationalities,
;       with different pets, drinks, and cigarettes.
;   2. The Englishman lives in the red house.
;   3. The Spaniard owns a dog.
;   4. Coffee is drunk in the green house.
;   5. The Ukrainian drinks tea.
;   6. The green house is directly to the right of the ivory house.
;   7. The Old Gold smoker owns snails.
;   8. Kools are being smoked in the yellow house.
;   9. Milk is drunk in the middle house.
;  10. The Norwegian lives in the first house on the left.
;  11. The Chesterfield smoker lives next to the fox owner.
;  12. Kools are smoked in the house next to the house where the horse is kept.
;  13. The Lucky Strike smoker drinks orange juice.
;  14. The Japanese smokes Parliaments.
;  15. The Norwegian lives next to the blue house.

; (define memb 
;   (extend-relation (a1 a2)
;     (fact (item) item `(,item . ,_))
;     (relation (item rest) (to-show item `(,_ . ,rest)) (memb item rest))))

(define memb 
  (relation (head-let item lst) 
    (any (== lst `(,item . ,__))
      (_exists (rest)
	(if-only (== lst `(,__ . ,rest)) (memb item rest))))))


(define next-to
  (relation (head-let item1 item2 rest)
    (any (on-right item1 item2 rest) (on-right item2 item1 rest))))

(define on-right
  (extend-relation (a0 a1 a2)
    (fact (item1 item2) item1 item2 `(,item1 ,item2 . ,__))
    (relation ((once item1) (once item2) rest)
      (to-show item1 item2 `(,__ . ,rest))
      (on-right item1 item2 rest))))
        
(define zebra
  (relation (head-let h)
    (if-only
      (all!
        (== h `((norwegian ,__ ,__ ,__ ,__) ,__ (,__ ,__ milk ,__ ,__) ,__ ,__))
        (memb `(englishman ,__ ,__ ,__ red) h)
        (on-right `(,__ ,__ ,__ ,__ ivory) `(,__ ,__ ,__ ,__ green) h)
        (next-to `(norwegian ,__ ,__ ,__ ,__) `(,__ ,__ ,__ ,__ blue) h)
        (memb `(,__ kools ,__ ,__ yellow) h)
        (memb `(spaniard ,__ ,__ dog ,__) h)
        (memb `(,__ ,__ coffee ,__ green) h) 
        (memb `(ukrainian ,__ tea ,__ ,__) h)
        (memb `(,__ luckystrikes oj ,__ ,__) h)
        (memb `(japanese parliaments ,__ ,__ ,__) h)
        (memb `(,__ oldgolds ,__ snails ,__) h)
        (next-to `(,__ ,__ ,__ horse ,__) `(,__ kools ,__ ,__ ,__) h)
        (next-to `(,__ ,__ ,__ fox ,__) `(,__ chesterfields ,__ ,__ ,__) h)
        )
      (all (memb `(,__ ,__ water ,__ ,__) h)
	(memb `(,__ ,__ ,__ zebra ,__) h)))))

;'(_pretty-print
;  (time (let loop ((n 100000))
;              (cond
;                ((zero? n) 'done)
;                (else (solution (h) (zebra h))
;                  (loop (sub1 n)))))))

(define (zebra-test)
(test-check "Zebra"
  (values (solution (h) (zebra h)))
  '((h.0 ((norwegian kools water fox yellow)
          (ukrainian chesterfields tea horse blue)
          (englishman oldgolds milk snails red)
          (spaniard luckystrikes oj dog ivory)
          (japanese parliaments coffee zebra green)))))
10)

; Sample timing (Pentium IV, 2GHz, 1GB RAM)
; (time (solution (h) ...))
;     1 collection
;     22 ms elapsed cpu time, including 0 ms collecting
;     27 ms elapsed real time, including 0 ms collecting
;     981560 bytes allocated, including 1066208 bytes reclaimed

; For version 3.17 of kanren (with head-let ...)
; (time (solution (h) ...))
;     1 collection
;     19 ms elapsed cpu time, including 0 ms collecting
;     19 ms elapsed real time, including 0 ms collecting
;     788928 bytes allocated, including 1052312 bytes reclaimed
;
; For version of kanren 3.36 (with once annotations)
; This seems to be similar of SWI-Prolog, which gives 0.01 sec
; timing for the equivalent zebra code.
; (time (solution (h) ...))
;     no collections
;     11 ms elapsed cpu time
;     11 ms elapsed real time
;     532912 bytes allocated

; For version of kanren 4.0 (increased sharing during unification)
; (time (solution (h) ...))
;     no collections
;     7 ms elapsed cpu time
;     8 ms elapsed real time
;     443792 bytes allocated
; For version of kanren 4.1 (detection of bare variables, less garbage)
;     no collections
;     8 ms elapsed cpu time
;     9 ms elapsed real time
;     448920 bytes allocated
; For version of kanren 4.50 (subst sk fk order)
;     no collections
;     8 ms elapsed cpu time
;     8 ms elapsed real time
;     416864 bytes allocated

;; ========================================================================
;; Mirror example
;; ========================================================================

; First we need an extendible database of relations.
; We should be able to add to the database later on -- extend
; it with assumptions.
;
; One approach for the database is a finite map (hash table, assoc
; list) from the name of a relation to the procedure that is a relation
; in our system. Or, to make it even better, from a tuple
; (name arity) to the body of the relation.
; This is the approach of Prolog.
; Suppose we have a term (foo ?a ?b ?c) where ?a, ?b and ?c are arbitrary
; terms (logical variables, constants, expressions, etc). 
; We would like to check if this term is consistent with (i.e., can
; be proven by) a particular instance of the database.
; First, we need to look up a key (foo 3) in the database. If the
; lookup fails, so does our query. If the lookup succeeds, we get
; a procedure of three arguments. We apply this procedure to
; ?a, ?b, and ?c and obtain an goal, which we can 'solve'
; as usual.

; In the following, we chose a different approach. We represent the database
; of relations as a relation itself -- we will call it KB. That
; relation takes one argument -- the term to prove, and returns an goal
; that represents the answer (that goal may be 'fail').
; A database of one fact
;  foo(a,b,c).
; in Prolog notation will be represented in our approach as a relation
;   (relation _ () (to-show `(foo a b c)))
; If we want to add another relation, say
; bar(X,X).
; we need to _extend_ the above relation with
;  (relation _ (x) (to-show `(bar x x))).
;
; This approach is probably less efficient than the first one. It has
; however a redeeming value -- we do not need a separate procedure
; to look up names/arities of relations. We don't need separate procedures
; for extending our database. We can use the existing machinery of
; 'solving' relations for 'solving' the database of relations.
; This approach seems reminiscent of the Futamura projections:
; we use the same engine for meta-evaluations. Bootstrapping.

; First we define the inductive structure

; In Athena:
;  (structure (BTree S)
;     (leaf S)
;     (root (BTree S) (BTree S)))

; In Prolog
; btree(leaf(S)).
; btree(root(T1,T2)) :- btree(T1),btree(T2).

; Note, our trees here (as well as those in Prolog) are polytypic
; (polymorphic): leaves can have values of different sorts.

; When we attempt to translate
;	btree(root(T1,T2)) :- btree(T1),btree(T2).
; into our system, we encounter the first difficulty. To find out
; if a term btree(root(T1,T2)) is consistent with our database of relations,
; we need to check if terms  btree(T1) and  btree(T2) are consistent.
; Thus, to add btree(root(T1,T2)) to our database, we need to use
; the database itself to verify btree(T1) and btree(T2). Clearly,
; we need a fixpoint. The need for the fixpoint _exists no matter what is
; the representation of the database -- a finite map or a relation.
; Prolog solves the fixpoint problem by making the database global
; and using mutations (similar to the way letrec is implemented in Scheme).
; If we attempt to be purely functional, we must make the fixpoint explicit
; and employ Y.

; Note, the kb variable below represents the "current" database.
; In our approach, the database is a relation of one argument,
; which is a term to prove. A Second-order relation???

(define btree
  (lambda (kb)
    (extend-relation (t)
      (fact (val) `(btree (leaf ,val)))
      (relation (t1 t2)
	(to-show `(btree (root ,t1 ,t2)))
	(project (t1 t2)
          (all
	    (predicate (pntall "btree ~s ~s ~n" t1 t2))
	    (kb `(btree ,t1))
	    (kb `(btree ,t2))))))))

;%> (declare mirror ((S) -> ((BTree S)) (BTree S)))

; Introduce an equality predicate and the first axiom for mirror
; In Athena:
; (define mirror-axiom-1
;   (forall ?x
;     (= (mirror (leaf ?x)) (leaf ?x))))

; In Prolog
; myeq(leaf(X),mirror(leaf(X))).

(define mirror-axiom-eq-1
  (lambda (kb)
    (fact (val) `(myeq (leaf ,val) (mirror (leaf ,val))))))

; The second axiom
; In Athena:
; (define mirror-axiom-eq-2
;   (forall ?t1 ?t2
;     (= (mirror (root ?t1 ?t2))
;       (root (mirror ?t2) (mirror ?t1)))))

; In Prolog
; myeq(root(B,A),mirror(root(T1,T2))) :- myeq(A,mirror(T1)),myeq(B,mirror(T2)).

; implicitly the axiom in Prolog and the one below assume
; the transitivity of myeq. Indeed, one may think that the direct
; translation from Athena to Prolog would be
;
;   myeq(mirror(root(T1,T2)),root(mirror(T2),mirror(T1)))
; or
;   myeq(mirror(root(T1,T2)),root(B,A)) :- B = T2, A = T1.
; However, Athena actually assumes that B and T2 can be myeq rather
; than merely identical. We also switched the order of arguments
; in myeq, assuming symmetry of myeq.
; It really helped in Prolog. In our system, we could have used
; the same order as in Athena and add:
;    myeq(A,A).   % reflexivity: identity implies equality
;    myeq(A,B) :- myeq(B,A). % symmetry
; Clearly if we add these relations to Prolog code, it will diverge.
; In our system, we can use with-depth to keep divergence in check.
; Still, for simplicity and clarity we will simply model the Prolog solution
; in our code.

(define mirror-axiom-eq-2
  (lambda (kb)
    (relation  (a b t1 t2)
      (to-show `(myeq (root ,b ,a) (mirror (root ,t1 ,t2))))
      (all
	(kb `(myeq ,a (mirror ,t1)))
	(kb `(myeq ,b (mirror ,t2)))))))

; we could also add reflexivity and transitivity and symmetry axioms
; and with-depth to keep them from diverging.

; Define the goal
; In Athena:
;  (define (goal t)
;     (= (mirror (mirror t)) t))

; In Prolog
; Note, the goal is _equivalent_ to the conjunction of the
; predicates. That's why we couldn't use the standard Prolog
; notation goal(T) :- btree(T), ...
; because the latter would give us only the implication.
; goal(T,[btree(T),myeq(T,mirror(T1)),myeq(T1,mirror(T))]).

(define goal
  (lambda (t)
    (let-lv (t1)
      (list
	`(btree ,t)
	`(myeq ,t  (mirror ,t1))
	`(myeq ,t1 (mirror ,t))))))

; For clarity, the above predicate can be written as two (prolog) relations
; The forward relation:
; (goal t) is implied by (btree t), (myeq t (mirror t1)) and 
;                        (myeq t1 (mirror t))
; In the above, t is universally quantified and t1 is existentially
; quantified

(define goal-fwd
  (lambda (kb)
    (relation (t t1)
      (to-show `(goal ,t))
      (all
	(kb `(btree ,t))
	(kb `(myeq ,t  (mirror ,t1)))
	(kb `(myeq ,t1 (mirror ,t)))))))

; The reverse relation for the goal:
; (goal t) implies (btree t), (myeq t (mirror t1)) and 
;                             (myeq t1 (mirror t))
; In the above, t is universally quantified and t1 is existentially
; quantified
; Because t1 now appears on the left-hand side, it is represented
; as an eigenvariable (skolem function) rather than a logical variable

(define goal-rev
  (let* ((sk (eigen-variable 'sk))
	 (t1-sk (lambda (t) `(,sk ,t))))
    (lambda (kb)
      (extend-relation (t)
	(relation (t)			; (goal t) => (btree t)
	  (to-show `(btree ,t))
	  (kb `(goal ,t)))
	(relation (t)			; (goal t) => (myeq t  (mirror t1))
	  (to-show `(myeq ,t  (mirror ,(t1-sk t))))
	  (kb `(goal ,t)))
	(relation (t)			; (goal t) => (myeq t1 (mirror t))
	  (to-show `(myeq ,(t1-sk t) (mirror ,t)))
	  (kb `(goal ,t)))
	))))
      
; The initial assumptions: just the btree
(define init-kb (Y btree))

; Verification engine
;	verify-goal PREDS KB
; returns a nullary relation that is the conjunction of preds against the
; assumption base kb
(define verify-goal
  (lambda (preds kb)
    (cond
      ((null? (cdr preds)) (kb (car preds)))
      (else (all
              (kb (car preds))
              (verify-goal (cdr preds) kb))))))

; extend the kb with the list of assumptions
; this is just like 'any' only it's a procedure rather than a syntax
; Why we need universalize?
; Suppose, the list of facts includes
;	(fact (x) (foo x)) and (fact (x) (bar x))
; definitely, we do not want to imply that facts foo and bar _share_
; the same logical variable. The facts are independent and should
; not have any variables in common.
; Furthermore, we do not want to add
;	(fact (x) (foo x))
; because that would mean exist x. foo x
; We want our facts to be universally quantified. So, we add
;	(fact () (foo 'unique-symbol))
; See the distinction between sigma and pi in Lambda-Prolog.
; We use extend-kb to extend the database with assumptions, which most
; often are universally quantified.

(define extend-kb
  (lambda (facts kb)
    (let ((facts (universalize facts)))
      (pntall "Extending KB with ~s~%" facts)
      (let loop ((facts facts))
        (if (null? facts) kb
            (extend-relation (t)
              (fact () (car facts))
              (loop (cdr facts))))))))

; Here's Athena's induction proof.
;
; (by-induction-on ?t (goal ?t)
;   ((leaf x) (!pf (goal (leaf x)) [mirror-axiom-1]))
;   ((root t1 t2) 
;     (!pf (goal (root t1 t2)) [(goal t1) (goal t2)  mirror-axiom-2])))

; The first part of it, the base case, can be expressed in Prolog
; as follows.
; ?- goal(leaf(X),C),verify(C,[]).
; Here how it looks in our system:
(define (mirror-tests)
(test-check "First check the base case"
  (query (_ subst)
    (verify-goal (goal '(leaf x))
      (extend-relation (t) (mirror-axiom-eq-1 init-kb) init-kb))
    (reify-subst '() subst))
  '((val.0 x) (t1.0 (leaf x)) (val.0 x) (val.0 x)))

(test-check "Check the base case, using goal-fwd"
  (query (_ subst)
    (let ((kb0
	    (extend-relation (t) (mirror-axiom-eq-1 init-kb) init-kb)))
      (let ((kb1
	      (extend-relation (t) (goal-fwd kb0) kb0)))
	(kb1 '(goal (leaf x))))) ; note, x is an eigenvariable!
    (reify-subst '() subst))
  '((val.0 x) (t1.0 (leaf x)) (val.0 x) (val.0 x) (t.0 (leaf x))))

; that is, we obtain the list of subgoals to verify '(leaf x)
; by invoking the function 'goal'.
; we extend the initial database (which contains btree facts)
; with mirror-axiom-eq-1. Thus, mirror-axiom-eq-1 and btree form
; the assumptions. We then verify the subgoals against the assumptions.
; Note that we wrote
;    '(leaf x)
; rather than
;    (let-lv (x) `(leaf ,x))
; because we want to prove that (goal '(leaf x)) holds for _all_ x
; rather than for some particular x.
;
; non-empty result printed by the above expressions means success...


; The inductive case.
; Now, assume the goal holds for t1 and t2 and check if it holds
; for root(t1,t2)
;?- goal(t1,A1),goal(t2,A2), append(A1,A2,A), goal(root(t1,t2),C), verify(C,A).

(test-check "Some preliminary checks"
  (solution (foo)
    (verify-goal '((btree t2)) ; (goal t2) => (btree t2)
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) init-kb))))
	kb0)))
  '((foo.0 _.0)))

(test-check "Some preliminary checks, using goal-rev"
  (solution (foo)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (goal-rev kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2)))))))
      (kb '(btree t2))))
  '((foo.0 _.0)))

; the above two expressions should give the same result: a non-empty stream
; (with an empty substitution: no variables leak)

(test-check "Another check"
  (solution (foo)
	;(goal t1), (goal t2) => (btree (root t1 t2))
    (verify-goal '((btree t1) (btree t2)
		   (btree (root t1 t2)))
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) 
		  (fact () 'nothing)))))
	(Y
	  (lambda (kb)
	    (extend-relation (t)
	      kb0
	      (btree kb)
	      (mirror-axiom-eq-2 kb)))))))
  '((foo.0 _.0)))

(test-check "Another check, using goal-rev"
  (solution (foo)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (goal-rev kb)
		  (mirror-axiom-eq-2 kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2)))))))
      (kb '(btree (root t1 t2)))))
  '((foo.0 _.0)))

; now we really need Y because we rely on the clause
;	btree(root(T1,T2)) :- btree(T1),btree(T2).
; which is recursive.

(test-check "Check the inductive case"
  (query (_ subst)
    (verify-goal (goal '(root t1 t2))
      (let ((kb0
	      (extend-kb (goal 't1) 
		(extend-kb (goal 't2) 
		  (fact () 'initial)))))
	(Y
	  (lambda (kb)
	    (extend-relation (t)
	      kb0
	      (btree kb)
	      (mirror-axiom-eq-2 kb))))))
    (cout (reify-subst '() subst) nl) #t)
  #t)

(pntall "~%Check particulars of the inductive case, using goal-rev, goal-fwd ~s~%"
  (let ((kb
          (Y
            (lambda (kb)
              (extend-relation (t)
                (btree kb)
                (fact () '(goal t1))
                (fact () '(goal t2))
                (mirror-axiom-eq-2 kb)
                (goal-rev kb)
                )))))
    (list
      (solve 1 (x) (kb `(myeq (root t1 t2)  (mirror ,x))))
      (solve 1 (x) (kb `(myeq ,x (mirror (root t1 t2))))))))

(test-check "Check the inductive case, using goal-rev, goal-fwd"
  (query (_ subst)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (btree kb)
		  (fact () '(goal t1))
		  (fact () '(goal t2))
		  (mirror-axiom-eq-2 kb)
		  (goal-rev kb))))))
      (let ((kb1 (goal-fwd kb)))
	(kb1 '(goal (root t1 t2)))))
    (cout (reify-subst '() subst) nl) #t)
  #t)

10)


; Again, we use Y because btree and mirror-axiom-eq-2 are recursive.
; We need the database that is the fixpoint of all constituent
; relations.
; The output above is a non-empty list: meaning that the inductive
; phase of the proof checks.

;; ========================================================================
;; Mirror-equ example
;; ========================================================================

; See mirror.scm for preliminaries

(define btrii
  (lambda (kb)
    (extend-relation (t)
      (fact (val) `(btrii (leaf ,val)))
      (relation (t1 t2)
	(to-show `(btrii (root ,t1 ,t2)))
	(all
	  (trace-vars 'btrii (t1 t2))
	  (kb `(btrii ,t1))
	  (kb `(btrii ,t2)))))))

(define myeq-axioms
  (lambda (kb)
    (extend-relation (t)
      (fact (val) `(myeq ,val ,val)) ; reflexivity
      (relation (a b)
	(to-show `(myeq ,a ,b))		; symmetry
	(all
	  (trace-vars 'symmetry (a b))
	  (kb `(myeq ,b ,a))))
      (relation (a b)			; transitivity
	(to-show `(myeq ,a ,b))
	(_exists (c)
	  (all
	    (kb `(myeq ,a ,c))
	    (kb `(myeq ,c ,b)))))
      )))

(define myeq-axioms-trees		; equational theory of trees
  (lambda (kb)				; equality commutes with root
    (relation (a b c d)
      (to-show `(myeq (root ,a ,b) (root ,c ,d)))
      (all
	(trace-vars 'trees (a b))
	(kb `(myeq ,a ,c))
	(kb `(myeq ,b ,d))))))
  
; equality on leaves follows from the reflexivity of equality

(define myeq-axioms-mirror		; equational theory of mirror
  (lambda (kb)				; equality commutes with root
    (extend-relation (t)
      (relation (a b)
	(to-show `(myeq (mirror ,a) ,b))
	(all
	  (trace-vars 'mirror (a b))
	  (_exists (c)
	    (all (kb `(myeq ,b (mirror ,c)))
	         (kb `(myeq ,a ,c)))))))))

; Axioms of mirror
; In Prolog
; myeq(leaf(X),mirror(leaf(X))).

(define mirror-axiom-eq-1/x
  (lambda (kb)
    (fact (val) `(myeq (leaf ,val) (mirror (leaf ,val))))))


; The second axiom
; In Athena:
; (define mirror-axiom-eq-2/x
;   (forall ?t1 ?t2
;     (= (mirror (root ?t1 ?t2))
;       (root (mirror ?t2) (mirror ?t1)))))

(define mirror-axiom-eq-2/x
  (lambda (kb)
    (relation (t1 t2) 
      (to-show `(myeq (mirror (root ,t1 ,t2)) (root (mirror ,t2) (mirror ,t1))))
      (trace-vars 'mirror-ax2 (t1 t2)))))

; Define the goal
; In Athena:
;  (define (goal t)
;     (= (mirror (mirror t)) t))

(define goal/x
  (lambda (t)
    (list 
      `(btrii ,t)
      `(myeq (mirror (mirror ,t)) ,t))))

(define goal-fwd/x
  (lambda (kb)
    (relation (t)
      (to-show `(goal/x ,t))
      (all
	(kb `(btrii ,t))
	(kb `(myeq (mirror (mirror ,t)) ,t))))))

(define goal-rev/x
  (lambda (kb)
    (extend-relation (t)
      (relation (t)			; (goal t) => (btrii t)
	(to-show `(btrii ,t))
	(kb `(goal/x ,t)))
      (relation (t)		; (goal t) => (myeq (mirror (mirror t)) t)
	(to-show `(myeq (mirror (mirror ,t)) ,t))
	(kb `(goal/x ,t))))))

; (by-induction-on ?t (goal ?t)
;   ((leaf x) (!pf (goal (leaf x)) [mirror-axiom-1]))
;   ((root t1 t2) 
;     (!pf (goal (root t1 t2)) [(goal t1) (goal t2)  mirror-axiom-2])))



(define-syntax un@ ; uncurry 
  (syntax-rules ()
    ((_ proc arg1 ...)
      (lambda (arg1 ...) (at@ proc arg1 ...)))))

; The initial assumptions: just the btrii
;(define init-kb (Y btrii))
; Note that in order to be effective, 
; extend-relation-with-recur-limit should not be under lambda!
; We want to use the same recursion count for all
; entrances to init-kb-coll.
; Also note that the limit 5 is the number of axioms in init-kb-coll
; plus one. This count will guarantee that each axiom will be tried
; once, but not more than twice.
(define init-kb-coll
  (extend-relation-with-recur-limit 5 (kb t)
    (un@ btrii kb t)
    (un@ myeq-axioms kb t)
    (un@ myeq-axioms-mirror kb t)
    (un@ myeq-axioms-trees kb t)))

(define (mirror-equ-tests)
(test-check "First check the base case, using goal-fwd"
  (query (_ subst)
    (let ((kb0
	    (Y (lambda (kb)
		 (extend-relation (t) 
		   (mirror-axiom-eq-1/x kb)
		   (lambda (t) (init-kb-coll kb t)))))))
      (let ((kb1
	      (extend-relation (t) (goal-fwd/x kb0) kb0)))
	(kb1 '(goal/x (leaf x))))) ; note, x is an eigenvariable!
     ;(cout (reify-subst '() subst) nl)
    #t)
  #t)

; (goal t2) => (btrii t2)
(test-check "Some preliminary checks, using goal-rev"
  (query (_ subst)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (lambda (t) (init-kb-coll kb t))
		  (goal-rev/x kb)
		  (fact () '(goal/x t1))
		  (fact () '(goal/x t2)))))))
      (kb '(btrii t2)))
     ;(cout (reify-subst '() subst) nl)
    #t)
  #t)

(test-check "Another check, using goal-rev"
	;(goal t1), (goal t2) => (btrii (root t1 t2))
  (query (_ subst)
    (let ((kb
	    (Y
	      (lambda (kb)
		(extend-relation (t)
		  (lambda (t) (init-kb-coll kb t))
		  (goal-rev/x kb)
		  (mirror-axiom-eq-2/x kb)
		  (fact () '(goal/x t1))
		  (fact () '(goal/x t2)))))))
      (kb '(btrii (root t1 t2))))
    (cout (reify-subst '() subst) nl)
    #t)
  #t)

(pntall "~%Check particulars of the inductive case, using goal-rev, goal-fwd ~s~%"
  (let ((kb
          (Y
            (lambda (kb)
              (extend-relation (t)
                (lambda (t) (init-kb-coll kb t))
                (fact () '(goal/x t1))
                (fact () '(goal/x t2))
                (mirror-axiom-eq-2/x kb)
                (goal-rev/x kb)
                )))))
    (list
      ;(solve 1 (x) (kb `(myeq (root t1 t2)  (mirror ,x))))
      (solve 1 (x) (kb `(myeq ,x (mirror (root t1 t2)))))
      )))

10)

;; ========================================================================
;; pure bin arith example
;; ========================================================================

;	     Pure, declarative, and constructive binary arithmetics
;
; aka: Addition, Multiplication, Division with remainder 
; as sound and complete, pure and declarative relations that can be
; used in any mode whatsoever and that recursively enumerate their domains.
; The relations define arithmetics over base-2 non-negative numerals
; of *arbitrary* size.
;
; aka: division as relation.
; The function divo below is a KANREN relation between four binary numerals
; n, m, q, and r such that the following holds
;	_exists r. 0<=r<m, n = q*m + r
;
; The relation 'divo' encompasses all four operations of arithmetics:
; we can use (divo x y z zero) to multiply and divide and
; (divo x y one r) to add and subtract.
;
; See pure-arithm.scm in this directory for Peano arithmetics.

; The arithmetic relations possess interesting properties.
; For example, given the relation (divo N M Q R) which holds
; iff N = M*Q + R and 0<=R<M, we can try:
;   -- (divo 1 0 Q _). It fails and does not try to enumerate
;      all natural numbers.
;   -- (divo 5 M 1 _). It finds all such M that divide (perhaps unevenly)
;      5 with the quotient of 1. The answer is the set (5 4 3).
; Again, (divo 5 M 7 _) simply fails and does not loop forever.
; We can use the (**o X Y Z) relation either to multiply two numbers
; X and Y -- or to find all factorizations of Z. See the test below.
; Furthermore, we can try to evaluate (++o X 1 Y) and get the stream
; of answers, among which is ((0 _.0 . _.1) (1 _.0 . _.1))
; which essentially says that 2*x and 2*x +1 are successors, for all x>0!
;
; We give two implementations of addition and multiplication
; relations, `++o' and `**o'. Both versions have the properties of
; soundness and nealy refutational completeness. The first version of `++o'
; is faster, but it does not always recursively enumerate its domain
; if that domain is infinite.  This is the case when, e.g., (**o x y
; z) is invoked when all three x, y, and z are uninstantiated
; variables. The relation in that case has the infinite number of
; solutions, as expected. Alas, those solutions look as follows:
;	x = 2,  y = 3, z = 6
;	x = 4,  y = 3, z = 12
;	x = 8,  y = 3, z = 24
;	x = 16, y = 3, z = 48
; That is, (**o x y z) keeps generating solutions where x is a power of
; two. Therefore, when the answerset of the relation `**o' is infinite, it
; truly produces an infinite set of solutions -- but only the subset of
; all possible solutions. In other words, `**o' does not recursively
; enumerate the set of all numbers such that x*y=z if that set is infinite.
;
; Therefore, 
;   (all (== x '(1 1)) (== y '(1 1)) (**o x y z))
;   (all (**o x y z)   (== x '(1 1)) (== y '(1 1)))
; work differently. The former terminates and binds z to the representation
; of 9 (the product of 3 and 3). The latter fails to terminate.
; This is not generally surprising as `all', like 'commas' in Prolog, 
; is not truly a conjunction: they are not commutative. Still, 
; we would like our `++o' and `**o' to have the algebraic properties
; expected of addition and multiplication.
;
; The second version of `++o' and `**o' completely fixes the
; problem without losing any performance.  The addition and
; multiplication relations completely enumerate their domain, even if
; it is infinite. Furthermore, ++o and **o now generate the numbers
; _in sequence_, which is quite pleasing. We achieve the
; property of recursive enumerability without giving up neither
; completeness nor refutational completeness. As before, if 'z' is
; instantiated but 'x' and 'y' are not, (++o x y z) delivers *all*
; non-negative numbers that add to z and (**o x y z) computes *all*
; factorizations of z.
;
; Such relations are easy to implement in an impure system such as Prolog,
; with the help of a predicate 'var'. The latter can tell if its argument
; is an uninstantiated variable. However, 'var' is impure. The present
; file shows the implementation of arithmetic relations in a _pure_
; logic system.
;
; The present approach places the correct upper bounds on the
; generated numbers to make sure the search process will terminate.
; Therefore, our arithmetic relations are not only sound
; (e.g., if (**o X Y Z) holds then it is indeed X*Y=Z) but also
; complete (if X*Y=Z is true then (**o X Y Z) holds) and
; nearly refutationally complete (if X*Y=Z is false and X, Y, and Z
; are either fully instantiated, or not instantiated, then (**o X Y Z) fails,
; in finite time). The refutational completeness
; claim is limited to the case when all terms passed to arithmetical
; functions do not share variables, are either fully instantiated or not
; instantiated at all. Indeed, sharing of variables or partial
; instantiation essentially imposes the constraint: e.g.,
;   (solution (q) (**o `(1 . ,q) `(1 1) `(1 . ,q)))
; is tantamount to
; (solution (q) (exist (q1)
;         (all (**o `(1 . ,q) `(1 1) `(1 . ,q1)) (== q q1))))
; That conjunction will never succeed. See the corresponding Prolog
; code for justification and relation to the 10th Hilbert problem.
;
; The numerals are represented in the binary little-endian
; (least-significant bit first) notation. The higher-order bit must be 1.
; ()  represents 0
; (1) represents 1
; (0 1) represents 2
; (1 1) represents 3
; (0 0 1) represents 4
; etc.
;


; There is a Prolog version of this code, which has termination proofs.
;
; $Id: pure-bin-arithm.scm,v 4.50 2005/02/12 00:04:49 oleg Exp $

; Auxiliary functions to build and show binary numerals
;
(define (build n)
  (if (zero? n) '() (cons (if (even? n) 0 1) (build (quotient n 2)))))

(define (trans n)
  (if (null? n) 0 (+ (car n) (* 2 (trans (cdr n))))))


; (zeroo x) holds if x is zero numeral
(define zeroo
  (fact () '()))

; Not a zero
(define pos
  (fact () `(,__ . ,__)))

; At least two
(define gt1
  (fact () `(,__ ,__ . ,__)))

; compare the lengths of two numerals
; (<ol a b) 
; holds if a=0 and b>0, or if (floor (log2 a)) < (floor (log2 b))
; That is, we compare the length (logarithms) of two numerals
; For a positive numeral, its bitlength = (floor (log2 n)) + 1
; We also make sure that 'n' is a well-formed number.
(define <ol
  (extend-relation (n m)
    (fact () '() `(,__ . ,__))
    (fact () '(1) `(,__ ,__ . ,__))
    (relation (x y x1 y1) (to-show `(,__ ,x1 . ,x) `(,__ ,y1 . ,y))
      (<ol `(,x1 . ,x) `(,y1 . ,y)))))

; holds if both a and b have the same number of bits, i.e., they are zero
; or if (floor (log2 a)) = (floor (log2 b))
(define =ol
  (extend-relation (n m)
    (fact () '() '())
    (fact () '(1) '(1))
    (relation (x y x1 y1) (to-show `(,__ ,x1 . ,x) `(,__ ,y1 . ,y))
      (=ol `(,x1 . ,x) `(,y1 . ,y)))))

; (<ol3 p1 p n m) holds iff
; p1 = 0 and p > 0 or
; length(p1) < min(length(p), length(n) + length(m) + 1)
(define <ol3
  (relation (head-let p1 p n m)
    (any
      (all (== p1 '()) (pos p))
      (_exists (p1r pr)
	(all
	  (== p1 `(,__ . ,p1r))
	  (== p  `(,__ . ,pr))
	  (any-interleave
	    (_exists (mr)
	      (all (== n '()) (== m  `(,__ . ,mr)) 
		(<ol3 p1r pr n mr)))
	    (_exists (nr)
	      (all (== n  `(,__ . ,nr)) 
		(<ol3 p1r pr nr m)))
	    ))))))

; (<ol2 p n m) holds iff
; length(n) + length(m) -1 <= length(p) <= length(n) + length(m)
; This predicate has nice properties: see the corresponding Prolog
; code for proofs.
(define <ol2
  (relation (head-let p n m)
    (any-interleave
      (all (== p '()) (== n '()) (== m '()))
      (all (== p '()) (== n '()) (== m '(1)))
      (all (== p '()) (== n '(1)) (== m '()))
      (_exists (pr mr)
	(all
	  (== p `(,__ . ,pr)) (== n '()) (== m `(,__ . ,mr))
	  (<ol2 pr '() mr)))
      (_exists (pr nr)
	(all
	  (== p `(,__ . ,pr)) (== n `(,__ . ,nr))
	  (<ol2 pr nr m)))
      )))


; Half-adder: carry-in a b r carry-out
; The relation holds if
; carry-in + a + b = r + 2*carry-out
; where carry-in a b r carry-out are all either 0 or 1.

(define half-adder
  (extend-relation (carry-in a b r carry-out)
    (fact () 0 0 0 0 0)
    (fact () 0 1 0 1 0)
    (fact () 0 0 1 1 0)
    (fact () 0 1 1 0 1)

    (fact () 1 0 0 1 0)
    (fact () 1 1 0 0 1)
    (fact () 1 0 1 0 1)
    (fact () 1 1 1 1 1)
))

; full-adder: carry-in a b r
; holds if carry-in + a + b = r
; where a, b, and r are binary numerals and carry-in is either 0 or 1

; We do the addition bit-by-bit starting from the least-significant
; one. So, we have already two cases to consider per each number: The
; number has no bits, and the number has some bits.

; (define full-adder
;   (extend-relation (carry-in a b r)
;     (fact (a) 0 a '() a) 		; 0 + a + 0 = a
;     (relation (b)			; 0 + 0 + b = b
;       (to-show 0 '() b b)
;       (pos b))
;     (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
;       (full-adder 0 a '(1) r))
;     (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
;       (all (pos b)
; 	(full-adder 0 '(1) b r)))
; 
;     ; The following three relations are needed
;     ; to make all numbers well-formed by construction,
;     ; that is, to make sure the higher-order bit is one.
;     (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
;       (_exists (r1 r2)
; 	(all (== r `(,r1 ,r2))
; 	     (half-adder carry-in 1 1 r1 r2))))
; 
;     ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
;     (relation (carry-in bb br rb rr)
;       (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
;       (all
; 	(pos br) (pos rr)
; 	(_exists (carry-out)
; 	  (all
; 	    (half-adder carry-in 1 bb rb carry-out)
; 	    (full-adder carry-out '() br rr)))))
; 
;     ; symmetric case for the above
;     (relation (head-let carry-in a '(1) r)
;       (all
; 	(gt1 a) (gt1 r)
; 	(full-adder carry-in '(1) a r)))
; 
;     ; carry-in + (2*ar + ab) + (2*br + bb) 
;     ; = (carry-in + ab + bb) (mod 2)
;     ; + 2*(ar + br + (carry-in + ab + bb)/2)
;     ; The cases of ar= 0 or br = 0 have already been handled.
;     ; So, now we require ar >0 and br>0. That implies that rr>0.
;     (relation (carry-in ab ar bb br rb rr)
;       (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
;       (all
; 	(pos ar) (pos br) (pos rr)
; 	(_exists (carry-out)
; 	  (all
; 	    (half-adder carry-in ab bb rb carry-out)
; 	    (full-adder carry-out ar br rr))))
;     )))

; After we have checked that  both summands have some bits, and so we
; can decompose them the least-significant bit and the other ones, it appears
; we only need to consider the general case, the last relation in
; the code above.
; But that is not sufficient. Let's consider
;	(full-adder 0 (1 . ()) (1 0 . ()) (0 1 . ()))
; It would then hold. But it shouldn't, because (1 0 . ()) is a bad
; number (with the most-significant bit 0). One can say why we should
; care about user supplying bad numbers. But we do: we don't know which
; arguments of full-adder are definite numbers and which are
; uninstantiated variables. We don't know which are the input and which
; are the output. So, if we keep only the last relation for the
; case of positive summands, and try to
;	(_exists (x) (full-adder 0 (1 . ()) x (0 1 . ())))
; we will see x bound to (1 0) -- an invalid number. So, our adder, when
; asked to subtract numbers, gave a bad number. And it would give us
; a bad number in all the cases when we use it to subtract numbers and
; the result has fewer bits than the number to subtract from. 
;
; To guard against such a behavior (i.e., to transparently normalize
; the numbers when the full-adder is used in the ``subtraction'' mode)
; we have to specifically distinguish cases of 
; "bit0 + 2*bit_others" where bit_others>0, and the
; terminal case "1" (that is, the most significant bit 1 and no other
; bits).
; The various (pos ...) conditions in the code are to guarantee that all
; cases are disjoin. At any time, only one case can match. Incidentally,
; the lack of overlap guarantees the optimality of the code.


; The full-adder above is not recursively enumerating however.
; Indeed, (solve 10 (x y z) (full-adder '0 x y z))
; gives solutions with x = 1.
; We now convert the adder into a recursively enumerable form.
; We lose some performance however (but see below!)
;
; The general principles are:
; Convert the relation into a disjunctive normal form, that is
;  (any (all a b c) (all c d e) ...)
; and then replace the single, top-level any with any-interleave.
; The conversion may be too invasive. We, therefore, use an effective
; conversion: if we have a relation
; (all (any a b) (any c d))
; then rather than re-writing it into
; (any (all a c) (all a d) (all b c) (all b d))
; to push disjunctions out and conjunctions in, we do
; (all gen (all (any a b) (any c d)))
; where gen is a relation whose answer set is precisely such
; that each answer in gen makes (all (any a b) (any c d))
; semi-deterministic. That is, with the generator gen, we
; make all the further choices determined.
;
; In the code below we use a different kind of generator, whose full
; justification (with proofs) appears in the Prolog version of the code.
; Please see the predicate `enum' in that Prolog code.
;
; The price to pay is slow-down.
; Note, if we had all-interleave, then we would generally have
; breadth-first search and so the changes to the recursively enumerable
; version would be minimal and without loss of speed.

; The following full-adder* is almost the same as full-adder above.
; 
; (define full-adder*
;   (extend-relation (carry-in a b r)
; ;     (fact (a) 0 a '() a) 		; 0 + a + 0 = a
; ;     (relation (b)			; 0 + 0 + b = b
; ;       (to-show 0 '() b b)
; ;       (pos b))
; ;     (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
; ;       (full-adder 0 a '(1) r))
; ;     (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
; ;       (all (pos b)
; ; 	(full-adder 0 '(1) b r)))
; 
;     ; The following three relations are needed
;     ; to make all numbers well-formed by construction,
;     ; that is, to make sure the higher-order bit is one.
;     (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
;       (_exists (r1 r2)
; 	(all (== r `(,r1 ,r2))
; 	     (half-adder carry-in 1 1 r1 r2))))
; 
;     ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
;     (relation (carry-in bb br rb rr)
;       (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
;       (all
; 	(pos br) (pos rr)
; 	(_exists (carry-out)
; 	  (all
; 	    (half-adder carry-in 1 bb rb carry-out)
; 	    (full-adder carry-out '() br rr)))))
; 
;     ; symmetric case for the above
;     (relation (head-let carry-in a '(1) r)
;       (all
; 	(gt1 a) (gt1 r)
; 	(full-adder* carry-in '(1) a r)))
; 
;     ; carry-in + (2*ar + ab) + (2*br + bb) 
;     ; = (carry-in + ab + bb) (mod 2)
;     ; + 2*(ar + br + (carry-in + ab + bb)/2)
;     ; The cases of ar= 0 or br = 0 have already been handled.
;     ; So, now we require ar >0 and br>0. That implies that rr>0.
;     (relation (carry-in ab ar bb br rb rr)
;       (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
;       (all
; 	(pos ar) (pos br) (pos rr)
; 	(_exists (carry-out)
; 	  (all
; 	    (half-adder carry-in ab bb rb carry-out)
; 	    (full-adder* carry-out ar br rr))))
;     )))

; This driver handles the trivial cases and then invokes full-adder*
; coupled with the recursively enumerating generator.

; (define full-adder
;   (extend-relation (carry-in a b r)
;     (fact (a) 0 a '() a) 		; 0 + a + 0 = a
;     (relation (b)			; 0 + 0 + b = b
;       (to-show 0 '() b b)
;       (pos b))
;     (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
;       (full-adder 0 a '(1) r))
;     (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
;       (all (pos b)
; 	(full-adder 0 '(1) b r)))
;     (relation (head-let carry-in a b r)
;       (any-interleave
; 	; Note that we take advantage of the fact that if
; 	; a + b = r and length(b) <= length(a) then length(a) <= length(r)
; 	(all (<ol a `(,_ . ,r))		; or, length(a) < length(2*r)
; 	  (any (<ol b a) (=ol b a))
; 	  (full-adder* carry-in a b r))
; 	; commutative case, length(a) < length(b)
; 	(all (<ol b `(,_ . ,r))
; 	  (<ol a b)
; 	  (full-adder* carry-in a b r))
; 	))))

; There is the third way of doing the addition, using
; all-interleave and any-interleave.
; Note that the code below is almost identical to the very first,
; non-recursively enumerating full-adder, only
; extend-relation is replaced with extend-relation-interleave
; and all is replaced with all-interleave in two places.
; The results are amazing, as the tests below show.
; For example, the test "print a few numbers that are greater than 4"
; shows that the numbers are generated _in sequence_, despite
; our addition being binary (and so one would expect the numbers
; being generated in Gray code or so).
; Also, tests multiplication-all-3 and multiplication-all-4
; show that (**o (build 3) y z) and (**o y (build 3) z)
; generates the _same_ answerlist, and in that answerlist, 'y' appears
; in sequence: 0,1,2....


(define-rel-lifted-comb extend-relation-interleave any-interleave)

(define full-adder
  (extend-relation-interleave (carry-in a b r)
    (fact (a) 0 a '() a) 		; 0 + a + 0 = a
    (relation (b)			; 0 + 0 + b = b
      (to-show 0 '() b b)
      (pos b))
    (relation (head-let '1 a '() r)	; 1 + a + 0 = 0 + a + 1
      (full-adder 0 a '(1) r))
    (relation (head-let '1 '() b r)	; 1 + 0 + b = 0 + 1 + b
      (all (pos b)
	(full-adder 0 '(1) b r)))

    ; The following three relations are needed
    ; to make all numbers well-formed by construction,
    ; that is, to make sure the higher-order bit is one.
    (relation (head-let carry-in '(1) '(1) r)	; c + 1 + 1 >= 2
      (_exists (r1 r2)
	(all (== r `(,r1 ,r2))
	     (half-adder carry-in 1 1 r1 r2))))

    ; cin + 1 + (2*br + bb) = (2*rr + rb) where br > 0 and so is rr > 0
    (relation (carry-in bb br rb rr)
      (to-show carry-in '(1) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos br) (pos rr)
	(_exists (carry-out)
	  (all-interleave
	    (half-adder carry-in 1 bb rb carry-out)
	    (full-adder carry-out '() br rr)))))

    ; symmetric case for the above
    (relation (head-let carry-in a '(1) r)
      (all
	(gt1 a) (gt1 r)
	(full-adder carry-in '(1) a r)))

    ; carry-in + (2*ar + ab) + (2*br + bb) 
    ; = (carry-in + ab + bb) (mod 2)
    ; + 2*(ar + br + (carry-in + ab + bb)/2)
    ; The cases of ar= 0 or br = 0 have already been handled.
    ; So, now we require ar >0 and br>0. That implies that rr>0.
    (relation (carry-in ab ar bb br rb rr)
      (to-show carry-in `(,ab . ,ar) `(,bb . ,br) `(,rb . ,rr))
      (all
	(pos ar) (pos br) (pos rr)
	(_exists (carry-out)
	  (all-interleave
	    (half-adder carry-in ab bb rb carry-out)
	    (full-adder carry-out ar br rr))))
    )))

; a + b = c
(define a++o
  (relation (head-let a b c)
    (full-adder 0 a b c)))

; a - b = c
(define a--o
  (lambda (x y out)
    (a++o y out x)))


;(define <o  ; n < m iff _exists x >0 such that n + x = m
;  (relation (head-let n m)
;    (_exists (x) (all (pos x) (a++o n x m)))))

; The following is an optimization: it is easier to test for the
; length of two numbers. If one number has fewer bits than the other number,
; the former is clearly shorter (provided that the numbers are well-formed,
; that is, the higher-order bit is one). So we don't need to go through
; the trouble of subtracting them.
(define <o  ; n < m iff _exists x >0 such that n + x = m
  (relation (head-let n m)
    (any-interleave
      (<ol n m)
      (all (=ol n m)
	(_exists (x) (all (pos x) (a++o n x m)))))))


; n * m = p
(define **o
  (relation (head-let n m p)
    (any-interleave
      (all (zeroo n) (== p '()))		; 0 * m = 0
      (all (zeroo m) (pos n) (== p '()))	; n * 0 = 0
      (all (== n '(1)) (pos m) (== m p))        ; 1 * m = m
      (all (== m '(1)) (gt1 n) (== n p))        ; n * 1 = n, n>1

      ; (2*nr) * m = 2*(nr*m), m>0 (the case of m=0 is taken care of already)
      ; nr > 0, otherwise the number is ill-formed
      (_exists (nr pr)
	(all
	  (gt1 m)
	  (== n `(0 . ,nr))
	  (== p `(0 . ,pr))
	  (pos nr) (pos pr)
	  (**o nr m pr)))

      ; The symmetric case to the above: m is even, n is odd
      (_exists (mr pr)
	(all
	  (== n `(1 ,__ . ,__))		; n is odd and n > 1
	  (== m `(0 . ,mr))
	  (== p `(0 . ,pr))
	  (pos mr) (pos pr)
	  (**o n mr pr)))

      ; (2*nr+1) * m = 2*(n*m) + m
      ; m > 0; also nr>0 for well-formedness
      ; the result is certainly greater than 1.
      ; we note that m > 0 and so 2*(nr*m) < 2*(nr*m) + m
      ; and (floor (log2 (nr*m))) < (floor (log2 (2*(nr*m) + m)))
      (_exists (nr p1)
	(all
	  (== m `(1 ,__ . ,__))		; m is odd and n > 1
	  (== n `(1 . ,nr))
	  (pos nr) (gt1 p)
	  (<ol3 p1 p n m)
	  (**o nr m p1)
	  (a++o `(0 . ,p1) m p)))
)))

; n = q*m + r
; where 0<=r<m

; This is divo from pure-arithm.scm
; it still works -- but very slow for some operations
; because <o takes linear time...

;(define divo
;  (relation (head-let n m q r)
;    (any-interleave
;      (all (== q '()) (== r n) (<o n m))      ; if n < m, then q=0, n=r
;      (all (== n m) (== q '(1)) (== r '()))  ; n = 1*n + 0
;      (_exists (p)
;	(all (<o m n) (<o r m)  (a++o p r n) ;(trace-vars 1 (p r n))
;	  (**o q m p))))))

; A faster divo algorithm
; 
; (define divo
;   (relation (head-let n m q r)
;     (any-interleave
;       (all (== r n) (== q '()) (<ol n m) (<o n m)) ; m has more digits than n: q=0,n=r
;       (all
; 	(<ol m n)			; n has mode digits than m
; 					; q is not zero, n>0, so q*m <= n,
; 	(_exists (p)			; definitely q*m < 2*n
; 	  (all (<o r m) (<ol p `(0 . ,n))
; 	    (a++o p r n) ;(trace-vars 1 (p r n))
; 	    (**o q m p)))
; 	)
;       ; n has the same number of digits than m
;       (all (== q '(1)) (=ol n m) (a++o r m n) (<o r m))
;       (all (== q '()) (== r n) (=ol n m) (<o n m))  ; if n < m, then q=0, n=r
;       )))
; ; 	(any-interleave
; ; 	  (all (== m '(1)) (== r '()) (== n q)) ; n = n*1 + 0
; ; 	  ; For even divisors:
; ; 	  ; n = (2*m)*q + r => (n - r) is even and (n-r)/2 = m*q
; ; 	  (_exists (p m1)
; ; 	    (all (== m `(0 . ,m1))
; ; 	         (== m1 `(__, . ,__))
; ; 	         (**o m1 q p)
; ; 	         (a--o n r `(0 . ,p))))
; 

; A faster and more refutationally complete divo algorithm
; Again, divo n m q r 
; holds iff n = m*q + r
; Let l be the bit-length of r (if r=0, l=0).
; Let n = 2^(l+1) * n1 + n2
;     q = 2^(l+1) * q1 + q2
; Note that n1 or q1 may be zero.
; We obtain that
;    n = m*q + r
; is equivalent to the conjunction of the following two relations
;    q2*m + r - n2 is divisible by 2^(l+1)
;    n1 = q1*m + (q2*m + r - n2)/2^(l+1)
; We note that by construction (see the mentioning of (<ol m n) below)
; all numbers in 'q2*m + r - n2' are length-limited. Therefore, we can
; obtain the success or failure of 'q2*m + r - n2' in finite time.
; This fact let us fail the 'divo' relation, in finite time,
; when it obviously does not hold, as in
;	(divo `(0 . ,x) (build 2) q '(1))
; (because no even number can give the remainder 1 upon the 
; division by two).
;
; We should note that if n1=0, we obtain that q1 must be zero
; (because m>0) and q2*m + r = n2. The latter can be solved in finite
; time.
; We also note that (q2*m + r - n2)/2^(l+1) < m
; because r - n2 < (2^(l+1) - q2)* m
; because 2^(l+1) - q2 >=1 and m > r by construction. Therefore, to
; solve the relation n1 = q1*m + (q2*m + r - n2)/2^(l+1) we use
; divo itself: (divo n1 m q1 (q2*m + r - n2)/2^(l+1))
; Thus our division algorithm is recursive. On each stage we determine at
; least one bit of the quotient (if r=0, l=0 and q2 is either 0 or 1),
; in finite time.

(define divo
  (relation (head-let n m q r)
    (any-interleave
	; m has more digits than n: q=0,n=r
      (all (== r n) (== q '()) (<o n m)) ; if n < m, then q=0, n=r
      ; n is at least m and has the same number of digits than m
      (all (== q '(1)) (=ol n m) (a++o r m n) (<o r m))
      (all-interleave
	(<ol m n)			; n has more digits than m
					; Note that m is L-instantiated here
	(<o r m)			; r is L-instantiated
	(pos q)				; q must be positive then
	(_exists (n1 n2 q1 q2 q2m q2mr rr r1)
	  (all-interleave
	    (split n r n1 n2)
	    (split q r q1 q2)
	    (any
	      (all
		(== n1 '())
		(== q1 '())
		(a--o n2 r q2m)
		(**o q2 m q2m))			; provably terminates
	      (all-interleave (pos n1) 
		(**o q2 m q2m)
		(a++o q2m r q2mr)
		(a--o q2mr n2 rr)		; rr = q2*m + r - n2
		(split rr r r1 '())		; r1 = rr/2^(l+1), evenly
		(divo n1 m q1 r1)))))
	)
      )))

; split n r n1 n2
; holds if n = 2^(l+1)*n1 + n2 where l = bitlength(r)
; This relation makes sense to use only when 'r' is L-instantiated
; (see the Prolog code file for the definition of L-instantiated).
; In that case, the relation has only the finite number of answers, in
; all of which n2 is L-instatantiated.
; We take trouble to assure that we produce only well-formed numbers:
; the major bit must be one.

(define split
  (extend-relation-interleave (n r n1 n2)
    (fact () '() __ '() '())
    (fact (b n) `(0 ,b . ,n) '() `(,b . ,n) '())
    (fact (n) `(1 . ,n) '() n '(1))
    (relation (b n r n1)
      (to-show `(0 ,b . ,n) `(,__ . ,r) n1 '())
      (split `(,b . ,n) r n1 '()))
    (relation (n r n1)
      (to-show `(1 . ,n) `(,__ . ,r) n1 '(1))
      (split n r n1 '()))
    (relation (b n r n1 n2)
      (to-show `(,b . ,n) `(,__ . ,r) n1 `(,b . ,n2))
      (all (pos n2)
	(split n r n1 n2)))
))


; Exponentiation and discrete logarithm
; n = b^q + r, where 0 <= r and q is the largest such integer
;
; From the above condition we obtain the upper bound on r:
; n >= b^q, n < b^(q+1) = b^q * b = (n-r)* b 
; r*b < n*(b-1)
;
; We can also obtain the bounds on q:
; if |b| is the bitwidth of b and |n| is the bitwidth of n,
; we have, by the definition of the bitwidth:
;  (1) 2^(|b|-1) <= b < 2^|b|
;  (2) 2^(|n|-1) <= n < 2^|n|
; Raising (1) to the power of q:
;      2^((|b|-1)*q) <= b^q
; OTH, b^q <= n, and n < 2^|n|. So we obtain
;  (3)   (|b|-1)*q < |n|
; which defines the upper bound on |q|.
; OTH, raising (1) to the power of (q+1):
;    b^(q+1) < 2^(|b|*(q+1))
; But n < b^(q+1) by definition of exponentiation, and keeping in mind (1)
; (4) |n|-1 < |b|*(q+1)
; which is the lower bound on q.

; When b = 2, exponentiation and discrete logarithm are easier to obtain
; n = 2^q + r, 0<= 2*r < n
; Here, we just relate n and q.
;    exp2 n b q
; holds if: n = (|b|+1)^q + r, q is the largest such number, and
; (|b|+1) is a power of two.
; Side condition: (|b|+1) is a power of two and b is L-instantiated.
; To obtain the binary exp/log relation, invoke the relation as
;  (exp2 n '() q)
; Properties: if n is L-instantiated, one answer, q is fully instantiated.
; If q is fully instantiated: one answer, n is L-instantiated.
; In any event, q is always fully instantiated in any answer
; and n is L-instantiated.
; We depend on the properties of split.

(define exp2
  (letrec
    ((r-append				; relational append
       (extend-relation (a b c)
	 (fact (b) '() b b)
	 (relation (ah ar b cr) (to-show `(,ah . ,ar) b `(,ah . ,cr))
	   (r-append ar b cr)))))
  (relation (head-let n b q)
    (any-interleave
      (all (== n '(1)) (== q '()))  ; 1 = b^0
      (all (gt1 n) (== q '(1)) (split n b '(1) __))
      (_exists (q1 b2)			; n = (2^k)^(2*q) + r
	(all-interleave                 ;   = (2^(2*k))^q + r
	     (== q `(0 . ,q1))
	     (pos q1)
	     (<ol b n)
	     (r-append b `(1 . ,b) b2)
	     (exp2 n b2 q1)))
      (_exists (q1 n1 b2)		; n = (2^k)^(2*q+1) + r
	(all-interleave 		; n/(2^k) = (2^(2*k))^q + r'
	     (== q `(1 . ,q1))
	     (pos q1)
	     (pos n1)
	     (split n b n1 __)
	     (r-append b `(1 . ,b) b2)
	     (exp2 n1 b2 q1)))
      )))
)


; nq = n^q where n is L-instantiated and q is fully instantiated
(define repeated-mul
  (extend-relation (n q nq)
    (fact () `(,__ . ,__) '() '(1))
    (fact (n) n '(1) n)
    (relation (head-let n q nq)
      (all
	(gt1 q)
	(_exists (q1 nq1)
	  (all
	    (a++o q1 '(1) q)
	    (repeated-mul n q1 nq1)
	    (**o nq1 n nq)))))))

; We call this predicate logo rather than expo due to its close similarity
; to divo. As the tests at the end show, logo can be used for determining
; the exact discrete logarithm, logarithm with a residual, exponentiation,
; and even operations (such as determining the base) that are not
; commonly discussed in high school.
(define logo
  (relation (head-let n b q r)
    (any-interleave
      (all (== n '(1)) (pos b) (== q '()) (== r '())) ; 1 = b^0 + 0, b >0
      (all (== q '())  (<o n b)  (a++o r '(1) n)) ; n = b^0 + (n-1)
	; n = b + r, n and b the same sz
      (all (== q '(1)) (gt1 b) (=ol n b) (a++o r b n))
      (all (== b '(1)) (pos q) (a++o r '(1) n))  ; n = 1^q + (n-1), q>0
      (all (== b '()) (pos q) (== r n))         ; n = 0^q + n, q>0
      ; in the rest, n is longer than b
      (all (== b '(0 1))		; b = 2
	   (_exists (n1)
	     (all
	       (pos n1)
	       (== n `(,__ ,__ . ,n1))    ; n is at least 4
	       (exp2 n '() q)		; that will L-instantiate n and n1
	       (split n n1 __ r))))
      ; the general case
      (all
	(any (== b '(1 1)) (== b `(,__ ,__ ,__ . ,__))) ; b >= 3
	(<ol b n)			; b becomes L-instantiated
	                                ; If b was L-instantiated, the previous
					; goal had only *one* answer
	(_exists (bw nw nw1 bw1 ql1 ql qh qdh qd bql bqd bq bq1)
	 (all
	  (exp2 b '() bw1)
	  (a++o bw1 '(1) bw)
	  (<ol q n)			; A _very_ lose bound, but makes
					; sure q will be L-instatiated
					; Now, we can use b and q to bound n
					; |n|-1 < |b|*(q+1)
	  (_exists (q1 bwq1)
	    (all
	      (a++o q '(1) q1)
	      (**o bw q1 bwq1)		; |b|*(q+1)
	      (<o nw1 bwq1)))
	  (exp2 n '() nw1)		; n becomes L-instantiated
					; Now we have only finite number of ans
	  (a++o nw1 '(1) nw)
	  (divo nw bw ql1 __)		; low boundary on q:
	  (a++o ql '(1) ql1)		; |n| = |b|(ql+1) + c
	  (any (== q ql) (<ol ql q))	; Tighten the estimate for q
	  (repeated-mul b ql bql)	; bql = b^ql
	  (divo nw bw1 qh __)		; upper boundary on q-1
	  (a++o ql qdh qh)
	  (a++o ql qd q)
	  (any (== qd qdh) (<o qd qdh)) ; qd is bounded
	  (repeated-mul b qd bqd)	; b^qd
	  (**o bql bqd bq)		; b^q
	  (**o b   bq  bq1)		; b^(q+1)
	  (a++o bq r n)
	  (<o n bq1)			; check the r condition
	  ))))))

	


;------------------------------------------------------------------------
;				Tests


(define-syntax test
  (syntax-rules ()
    ((_ (x) gl)
      (query (redok subst x) gl
	(display (trans (subst-in x subst)))
	(newline)))))

(define (subset? l1 l2)
  (or (null? l1)
    (and (member (car l1) l2) (subset? (cdr l1) l2))))
(define (set-equal? l1 l2) (or (subset? l1 l2) (subset? l2 l1)))

(define (pure-bin-arith-tests)  
(cout nl "addition" nl)
(test (x) (a++o (build 29) (build 3) x))
(test (x) (a++o (build 3) x (build 29)))
(test (x) (a++o x (build 3) (build 29)))
(test-check "all numbers that sum to 4"
  (solve 10 (w)
    (_exists (y z)
      (all (a++o y z (build 4))
	(project (y z) (== `(,(trans y) ,(trans z)) w)))))
   '(((w.0 (4 0)))
     ((w.0 (0 4)))
     ((w.0 (1 3)))
     ((w.0 (3 1)))
     ((w.0 (2 2)))
     )
  )
(test-check "print a few numbers such as X + 1 = Y"
  (solve 5 (x y) (a++o x (build 1) y))
   '(((x.0 ()) (y.0 (1))) ; 0 + 1 = 1
     ((x.0 (1)) (y.0 (0 1))) ; 1 + 1 = 2
       ; 2*x and 2*x+1 are successors, for all x>0!
      ((x.0 (0 _.0 . _.1)) (y.0 (1 _.0 . _.1)))
      ((x.0 (1 1)) (y.0 (0 0 1)))
      ((x.0 (1 0 _.0 . _.1)) (y.0 (0 1 _.0 . _.1))))
)

; check that add(X,Y,Z) recursively enumerates all
; numbers such as X+Y=Z
;
(cout "Test recursive enumerability of addition" nl)
(let ((n 7))
  (do ((i 0 (+ 1 i))) ((> i n))
    (do ((j 0 (+ 1 j))) ((> j n))
      (let ((p (+ i j)))
	(test-check
	  (string-append "enumerability: " (number->string i)
	    "+" (number->string j) "=" (number->string p))
	  (solve 1 (x y z) 
	    (all (a++o x y z)
	      (== x (build i)) (== y (build j)) (== z (build p))))
	  `(((x.0 ,(build i)) (y.0 ,(build j))
	      (z.0 ,(build p)))))))))

(test-check "strong commutativity"
  (solve 5 (a b c)
    (all (a++o a b c)
    (_exists (x y z)
      (all!
	(a++o x y z)
	(== x b)
	(== y a)
	(== z c)
	))))
  '(((a.0 ()) (b.0 ()) (c.0 ()))
    ((a.0 ()) (b.0 (_.0 . _.1)) (c.0 (_.0 . _.1)))
    ((a.0 (1)) (b.0 (1)) (c.0 (0 1)))
    ((a.0 (1)) (b.0 (0 _.0 . _.1)) (c.0 (1 _.0 . _.1)))
    ((a.0 (0 _.0 . _.1)) (b.0 (1)) (c.0 (1 _.0 . _.1))))
)


(cout nl "subtraction" nl)
(test (x) (a--o (build 29) (build 3) x))
(test (x) (a--o (build 29) x (build 3)))
(test (x) (a--o x (build 3) (build 26)))
(test (x) (a--o (build 29) (build 29) x))
(test (x) (a--o (build 29) (build 30) x))
(test-check "print a few numbers such as Y - Z = 4"
  (solve 11 (y z) (a--o y z (build 4)))
  '(((y.0 (0 0 1)) (z.0 ()))    ; 4 - 0 = 4
    ((y.0 (1 0 1)) (z.0 (1)))   ; 5 - 1 = 4
    ((y.0 (0 1 1)) (z.0 (0 1))) ; 6 - 2 = 4
    ((y.0 (1 1 1)) (z.0 (1 1))) ; 7 - 3 = 4
    ((y.0 (0 0 0 1)) (z.0 (0 0 1))) ; 8 - 4 = 4
    ((y.0 (1 0 0 1)) (z.0 (1 0 1)))  ; 9 - 5 = 4
    ((y.0 (0 1 0 1)) (z.0 (0 1 1)))  ; 10 - 6 = 4
    ((y.0 (1 1 0 1)) (z.0 (1 1 1)))  ; 11 - 7 = 4
     ; 8*k + 4 - 8*k = 4 forall k> 0!!
    ((y.0 (0 0 1 _.0 . _.1)) (z.0 (0 0 0 _.0 . _.1)))
    ((y.0 (1 0 1 _.0 . _.1)) (z.0 (1 0 0 _.0 . _.1)))
    ((y.0 (0 1 1 _.0 . _.1)) (z.0 (0 1 0 _.0 . _.1))))
)

(test-check "print a few numbers such as X - Y = Z"
  (solve 5 (x y z) (a--o x y z))
  '(((x.0 _.0) (y.0 _.0) (z.0 ())) ; 0 - 0 = 0
    ((x.0 (_.0 . _.1)) (y.0 ()) (z.0 (_.0 . _.1))) ; a - 0 = a
    ((x.0 (0 1)) (y.0 (1)) (z.0 (1)))
    ((x.0 (1 _.0 . _.1)) (y.0 (1)) (z.0 (0 _.0 . _.1)))
    ((x.0 (1 _.0 . _.1)) (y.0 (0 _.0 . _.1)) (z.0 (1))))
)


(cout nl "comparisons" nl)
(test (x) (<o x (build 4)))
(test (x) (all (== x (build 3)) (<o x (build 4))))
(test (x) (all (== x (build 4)) (<o x (build 3))))
(test-check "print all numbers that are less than 6"
  (solve 10 (x) (<o x (build 6)))
  '(((x.0 ())) ((x.0 (1 0 1))) ((x.0 (1))) 
     ((x.0 (0 0 1))) ((x.0 (_.0 1))))
  )

(test-check "print *all* numbers that are greater than 4"
  (solve 10 (x) (<o (build 4) x))
  '(((x.0 (_.0 _.1 _.2 _.3 . _.4)))
    ((x.0 (1 0 1))) ((x.0 (0 1 1))) ((x.0 (1 1 1))))
)



(cout nl "multiplication" nl)
(test (x) (**o (build 2) (build 3) x))
(test (x) (**o (build 3) x (build 12)))
(test (x) (**o x (build 3) (build 12)))
(test (x) (**o x (build 5) (build 12)))
(test (x) (all (== x (build 2)) (**o x (build 2) (build 4))))
(test-check 'multiplication-fail-1
  (test (x) (all (== x (build 3)) (**o x (build 2) (build 4))))
  '())
(test-check 'multiplication-all-1
  (solve 7 (w) 
    (_exists (y z) (all (**o y z (build 6))
		    (project (y z) (== `(,(trans y) ,(trans z)) w)))))
  '(((w.0 (1 6))) ((w.0 (6 1))) ((w.0 (2 3)))  ((w.0 (3 2)))))

; Only one answer
(test-check 'multiplication-all-2
  (solve 7 (w) 
    (_exists (x)
      (all (**o (build 3) (build 2) x)
	(project (x) (== (trans x) w)))))
  '(((w.0 6))))

(test-check 'multiplication-all-3
  (solve 7 (y z) (**o (build 3) y z))
  '(((y.0 ()) (z.0 ()))  ; 0 * 3 = 0
    ((y.0 (1)) (z.0 (1 1))) ; 1 * 3 = 3
    ((y.0 (0 1)) (z.0 (0 1 1))) ; 2 * 3 = 6
    ((y.0 (1 1)) (z.0 (1 0 0 1))) ; 3 * 3 = 9
    ((y.0 (0 0 1)) (z.0 (0 0 1 1))) ; 4 * 3 = 12
    ((y.0 (1 0 1)) (z.0 (1 1 1 1))) ; 5 * 3 = 15
    ((y.0 (0 1 1)) (z.0 (0 1 0 0 1)))) ; 6 * 3 = 18
)

; Just as above
(test-check 'multiplication-all-4
  (solve 7 (y z) (**o y (build 3) z))
  '(((y.0 ()) (z.0 ()))
    ((y.0 (1)) (z.0 (1 1)))
    ((y.0 (0 1)) (z.0 (0 1 1)))
    ((y.0 (1 1)) (z.0 (1 0 0 1)))
    ((y.0 (0 0 1)) (z.0 (0 0 1 1)))
    ((y.0 (1 0 1)) (z.0 (1 1 1 1)))
    ((y.0 (0 1 1)) (z.0 (0 1 0 0 1))))
)

(test-check 'multiplication-all-5
  (solve 7 (x y z) (**o x y z))
  '(((x.0 ()) (y.0 _.0) (z.0 ())) ; 0 * y = 0 for any y whatsoever
    ((x.0 (_.0 . _.1)) (y.0 ()) (z.0 ())) ; x * 0 = 0 for x > 0
     ; 1 * y = y for y > 0
    ((x.0 (1)) (y.0 (_.0 . _.1)) (z.0 (_.0 . _.1)))
    ((x.0 (_.0 _.1 . _.2)) (y.0 (1)) 
      (z.0 (_.0 _.1 . _.2))) ;  x * 1 = x, x > 1
     ; 2 * y = even positive number, for y > 1
    ((x.0 (0 1)) (y.0 (_.0 _.1 . _.2)) 
      (z.0 (0 _.0 _.1 . _.2)))
     ; x * 2 = shifted-left x, for even x>1
    ((x.0 (1 _.0 . _.1)) (y.0 (0 1)) (z.0 (0 1 _.0 . _.1)))
     ; 3 * 3 = 9
    ((x.0 (1 1)) (y.0 (1 1)) (z.0 (1 0 0 1)))
    )
)

(test-check 'multiplication-even-1
  (solve 10 (y z) (**o (build 2) y z))
  '(((y.0 ()) (z.0 ()))
    ((y.0 (1)) (z.0 (0 1))) ; 2 * 1 = 2
     ; 2*y is an even number, for any y > 1!
    ((y.0 (_.0 _.1 . _.2)) (z.0 (0 _.0 _.1 . _.2)))
     )
)

(test-check 'multiplication-even-2
  ; multiplication by an even number cannot yield an odd number
  (solution (q x y u v) (**o '(1 1) `(0 0 1 ,x . ,y) `(1 0 0 ,u . ,v)))
  #f
)

(test-check 'multiplication-even-3
  ; multiplication by an even number cannot yield an odd number
  (solution (q x y z) (**o `(0 0 1 . ,y) `(1 . ,x) `(1 0 . ,z)))
  #f
)

; check that mul(X,Y,Z) recursively enumerates all
; numbers such as X*Y=Z
;
(cout "Test recursive enumerability of multiplication" nl)
(let ((n 7))
  (do ((i 0 (+ 1 i))) ((> i n))
    (do ((j 0 (+ 1 j))) ((> j n))
      (let ((p (* i j)))
	(test-check
	  (string-append "enumerability: " (number->string i)
	    "*" (number->string j) "=" (number->string p))
	  (solve 1 (x y z) 
	    (all (**o x y z)
	      (== x (build i)) (== y (build j)) (== z (build p))))
	  `(((x.0 ,(build i)) (y.0 ,(build j))
	      (z.0 ,(build p)))))))))

(cout nl "split" nl)

(test-check 'split-1
  (solve 5 (x y) (split (build 4) '() x y))
  '(((x.0 (0 1)) (y.0 ()))))
(test-check 'split-2
  (solve 5 (x y) (split (build 4) '(1) x y))
  '(((x.0 (1)) (y.0 ()))))
(test-check 'split-3
  (solve 5 (x y) (split (build 4) '(1 1) x y))
  '(((x.0 ()) (y.0 (0 0 1)))))
(test-check 'split-4
  (solve 5 (x y) (split (build 4) '(1 1 1) x y))
  '(((x.0 ()) (y.0 (0 0 1)))))
(test-check 'split-5
  (solve 5 (x y) (split (build 5) '(1) x y))
  '(((x.0 (1)) (y.0 (1)))))
(test-check 'split-6
  (solve 5 (n) (split n (build 5) '() '(1)))
  '(((n.0 (1)))))

(cout nl "division, general" nl)


(test-check 'divo-1
  (solution (x) (divo (build 4) (build 2) x __))
  '((x.0 (0 1))))
(test-check 'div-fail-1 (test (x) (divo (build 4) (build 0) x __)) '())
(test-check 'divo-2
  (solution (x) (divo (build 4) (build 3) x __))
  '((x.0 (1))))
(test-check 'divo-3
  (solution (x) (divo (build 4) (build 4) x __))
  '((x.0 (1))))
(test-check 'divo-4
  (solution (x y) (divo (build 4) (build 5) x y))
  '((x.0 ()) (y.0 (0 0 1))))


(test-check 'divo-33-1
  (solution (x) (divo (build 33) (build 3) x __))
  `((x.0 ,(build 11))))
(test-check 'divo-33-2
  (solution (x) (divo (build 33) x (build 11) __))
  `((x.0 ,(build 3))))
(test-check 'divo-33-3
  (solution (x) (divo x (build 3) (build 11) __))
  `((x.0 ,(build 33))))
(test-check 'divo-33-5
  (solution (x y) (divo (build 33) (build 5) x y))
  `((x.0 ,(build 6)) (y.0 ,(build 3))))


(test-check 'divo-5-4
  (solve 3 (x y) (divo x (build 5) y (build 4)))
  '(((x.0 (0 0 1)) (y.0 ()))
    ((x.0 (0 0 0 0 0 0 1)) (y.0 (0 0 1 1)))
    ((x.0 (1 0 0 0 1 1)) (y.0 (1 0 0 1))))
)
(test-check 'divo-5-5
  (solve 3 (x y) (divo x (build 5) y (build 5)))
  '())


(test (x) (divo x (build 5) __ (build 4)))
(test (x) (divo x (build 5) (build 3) (build 4)))
(test (x) (divo x __ (build 3) (build 4)))
(test-check 'div-fail-2 (test (x) (divo (build 5) x (build 7) __)) '())

(test-check "all numbers such as 5/Z = 1"
  (solve 7 (w) 
    (_exists (z) (all (divo (build 5) z (build 1) __)
		    (project (z) (== `(,(trans z)) w)))))
  '(((w.0 (5))) ((w.0 (3))) ((w.0 (4)))))

(test-check "all inexact factorizations of 12"
  (set-equal?
   (solve 100 (w) 
    (_exists (m q r n)
      (all 
	(== n (build 12))
	(<o m n)
	(divo n m q r)
	(project (m q r) (== `(,(trans m) ,(trans q) ,(trans r)) w)))))
  '(((w.0 (1 12 0))) ((w.0 (11 1 1)))
    ((w.0 (2 6 0)))  ((w.0 (10 1 2)))
    ((w.0 (4 3 0)))  ((w.0 (8 1 4)))
    ((w.0 (6 2 0)))  ((w.0 (3 4 0)))
    ((w.0 (9 1 3)))  ((w.0 (7 1 5)))
    ((w.0 (5 2 2)))))
  #t)


(test-check 'div-all-3
  (solve 4 (x y z r) (divo x y z r))
'(((x.0 ()) (y.0 (_.0 . _.1)) (z.0 ()) (r.0 ())) ; 0 = a*0 + 0, a>0
  ((x.0 (1)) (y.0 (1)) (z.0 (1)) (r.0 ())) ; 1 = 1*1 + 0
  ((x.0 (0 1)) (y.0 (1)) (z.0 (0 1)) (r.0 ())) ; 2 = 1*2 + 0
  ((x.0 (0 1)) (y.0 (1 1)) (z.0 ()) (r.0 (0 1))) ; 2 = 3*0 + 2
))

(test-check 'div-even
  (solve 3  (y z r) (divo `(0 . ,y) (build 2) z r))
  '(((y.0 (1)) (z.0 (1)) (r.0 ()))
    ((y.0 (0 1)) (z.0 (0 1)) (r.0 ()))
    ((y.0 (1 1)) (z.0 (1 1)) (r.0 ())))
)

(test-check 'div-even-fail
  (solve 3  (y z r) (divo `(0 . ,y) (build 2) z '(1)))
  '()
)

(test-check 'div-odd
  (solve 3  (y z) (divo `(1 0 . ,y) (build 2) z '(1)))
  '(((y.0 (0 1)) (z.0 (0 0 1))) ; 9 = 2*4 + 1
    ((y.0 (1)) (z.0 (0 1))) ; 5 = 2*2 + 1
    ((y.0 (0 0 1)) (z.0 (0 0 0 1)))) ; 17 = 8*2 + 1
)

(test-check 'div-odd-fail
  (solve 3  (y z r) (divo `(1 0 . ,y) (build 2) z '()))
  '()
)

(test-check 'div-enum-sample
  (solve 1 (n m q r)
    (all (divo n m q r)
      (== n (build 10)) (== m (build 2)) (== q (build 5))
      (== r '())))
  '(((n.0 (0 1 0 1)) (m.0 (0 1)) (q.0 (1 0 1)) (r.0 ())))
)

; the latter takes awfully long time
'(test-check 'div-enum-sample-1
  (solve 1 (n m q r)
    (all (divo n m q r)
      (== n (build 10)) (== m (build 3)) (== q (build 3))
      (== r '(1))))
  '(((n.0 (1 1 1)) (m.0 (0 1)) (q.0 (1 1)) (r.0 (1))))
)

; check that divo(N,M,Q,R) recursively enumerates all
; numbers such as N=M*Q+R, R<M
; It is a slow test...
(cout "Test recursive enumerability of division" nl)
'(let ((n 3))
  (do ((m 1 (+ 1 m))) ((> m n))
    (do ((q 0 (+ 1 q))) ((> q n))
      (do ((r 0 (+ 1 r))) ((>= r m))
	(let ((n (+ (* m q) r)))
	 (test-check
	  (string-append "enumerability: " (number->string n)
	    "=" (number->string m) "*" (number->string q)
	    "+" (number->string r))
	  (solve 1 (n1 m1 q1 r1) 
	    (all (divo n1 m1 q1 r1)
	      (== n1 (build n)) (== m1 (build m))
	      (== q1 (build q)) (== r1 (build r))
	      ))
	  `(((n1.0 ,(build n)) (m1.0 ,(build m))
	     (q1.0 ,(build q)) (r1.0 ,(build r))))))))))


; quite dubious tests. The problem is actually in =ol and <ol
; (solve 1 (q)
;     (poso q)
;     (divo `(0 . ,q) `(1 1 . ,q) '() `(0 . ,q))))
;
; (solve 1 (q)
;     (poso q)
;     (divo `(0 . ,q) `(1 . ,q) '() `(0 . ,q))))


(test-check 'exp2-1
  (solve 10 (q) (exp2 '(1 1 1 1) '() q))
  '(((q.0 (1 1)))))

(test-check 'exp2-2
  (solve 10 (q) (exp2 '(1 0 1 1 1) '()  q))
  '(((q.0 (0 0 1)))))

; These are all answers!
(test-check 'exp2-3
  (solve 100 (n) (exp2 n '() '(1 0 1)))
  '(((n.0 (0 0 0 0 0 1)))
    ((n.0 (1 0 0 0 0 1)))
    ((n.0 (0 1 0 0 0 1)))
    ((n.0 (1 1 0 0 0 1)))
    ((n.0 (0 _.0 1 0 0 1)))
    ((n.0 (1 _.0 1 0 0 1)))
    ((n.0 (0 _.0 _.1 1 0 1)))
    ((n.0 (1 _.0 _.1 1 0 1)))
    ((n.0 (0 _.0 _.1 _.2 1 1)))
    ((n.0 (1 _.0 _.1 _.2 1 1))))
)


(test-check 'exp2-4
  (solve 5 (n q) (exp2 n '() q))
  '(((n.0 (1)) (q.0 ()))
    ((n.0 (0 1)) (q.0 (1)))
    ((n.0 (0 0 1)) (q.0 (0 1)))
    ((n.0 (0 0 0 1)) (q.0 (1 1)))
    ((n.0 (1 1)) (q.0 (1)))))


(test-check 'logo-15-1
  (solve 10 (q r) (logo (build 15) (build 2) q r))
  '(((q.0 (1 1)) (r.0 (1 1 1)))))

(test-check 'logo-15-3
  (solve 10 (q r) (logo (build 15) (build 3) q r))
  '(((q.0 (0 1)) (r.0 (0 1 1)))))

(test-check 'logo-15-4
  (solve 10 (q r) (logo (build 15) (build 4) q r))
  '(((q.0 (1)) (r.0 (1 1 0 1)))))

(test-check 'logo-15-5
  (solve 10 (q r) (logo (build 15) (build 5) q r))
  '(((q.0 (1)) (r.0 (0 1 0 1)))))

(test-check 'logo-15-15
  (solve 10 (q r) (logo (build 15) (build 15) q r))
  '(((q.0 (1)) (r.0 ()))))

(test-check 'logo-15-16
  (solve 10 (q r) (logo (build 15) (build 16) q r))
  '(((q.0 ()) (r.0 (0 1 1 1)))))

(test-check 'logo-15--3
  (solve 10 (b r) (logo (build 15) b (build 3) r))
  '(((b.0 (1)) (r.0 (0 1 1 1)))  ; 15 = 1^3 + 14
    ((b.0 ()) (r.0 (1 1 1 1)))   ; 15 = 0^3 + 15
    ((b.0 (0 1)) (r.0 (1 1 1))))) ; 15 = 2^3 + 7

(test-check 'logo-15--3-1
  (solve 10 (q) (logo q '(1) (build 2) '()))
  '(((q.0 (1)))))

(test-check 'logo-32--4
  (solve 10 (b r) (logo (build 32) b (build 4) r))
  '(((b.0 (1)) (r.0 (1 1 1 1 1))) ((b.0 ()) (r.0 (0 0 0 0 0 1)))))

(test-check 'logo-33--5
  (solve 10 (b r) (logo (build 33) b (build 5) r))
  '(((b.0 (1)) (r.0 (0 0 0 0 0 1)))
    ((b.0 ()) (r.0 (1 0 0 0 0 1)))
    ((b.0 (0 1)) (r.0 (1)))))

(test-check 'logo-2-5
  (solve 10 (n) (logo n (build 2) (build 5) '(1)))
  '(((n.0 (1 0 0 0 0 1)))))

(test-check 'logo-3-2
  (solve 10 (n) (logo n (build 3) (build 2) '(1)))
  '(((n.0 (0 1 0 1)))))

(test-check 'logo-3-3
  (solve 10 (n) (logo n (build 3) (build 3) '(1)))
  '(((n.0 (0 0 1 1 1)))))

(test-check 'powers-of-3
  (solve 10 (n q r) (logo n (build 3) q r))
  '(((n.0 (1)) (q.0 ()) (r.0 ()))
    ((n.0 (0 1)) (q.0 ()) (r.0 (1)))
    ((n.0 (1 1)) (q.0 (1)) (r.0 ()))
    ((n.0 (0 0 1)) (q.0 (1)) (r.0 (1)))
    ((n.0 (1)) (q.0 ()) (r.0 ()))
    ((n.0 (0 0 0 1)) (q.0 (1)) (r.0 (1 0 1)))
    ((n.0 (1 0 1)) (q.0 (1)) (r.0 (0 1)))
    ((n.0 (1 1 1)) (q.0 (1)) (r.0 (0 0 1)))
    ((n.0 (0 1 1)) (q.0 (1)) (r.0 (1 1)))
    ((n.0 (0 0 0 0 1)) (q.0 (0 1)) (r.0 (1 1 1))))
)

(test-check 'powers-of-exp-3
  (solve 7 (n b r) (logo n b (build 3) r))
  '(((n.0 (1)) (b.0 (1)) (r.0 ()))
    ((n.0 _.0) (b.0 ()) (r.0 _.0))
    ((n.0 (0 0 0 1)) (b.0 (0 1)) (r.0 ()))
    ((n.0 (1 1 0 1 1)) (b.0 (1 1)) (r.0 ()))
    ((n.0 (0 1)) (b.0 (1)) (r.0 (1)))
    ((n.0 (1 0 0 1)) (b.0 (0 1)) (r.0 (1)))
    ((n.0 (0 0 1 1 1)) (b.0 (1 1)) (r.0 (1))))
)
10)

;; ========================================================================

(define (all-tests)
  (eigen-test)
  (term-tests)
  (kanren-tests)
  (ti-tests)
  (ti-tests-2)
  (ti-tests-3)
  (tc-tests)
  (zebra-test)
  (mirror-tests)
  (mirror-equ-tests)
  (pure-bin-arith-tests))

(time (all-tests))

