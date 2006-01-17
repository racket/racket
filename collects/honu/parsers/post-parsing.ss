(module post-parsing mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../ast.ss"
           "../parameters.ss"
           "../readerr.ss"
           "../tenv.ss"
           "../utils.ss")

  ;;;; Here are descriptions of what each of the three functions herein do:
  ;;;;
  ;;;; convert-static : converts bareword references to static slots/fields/methods <member>
  ;;;;                  into my.<member>
  ;;;; convert-slots  : converts init slots that are used in methods or exports into
  ;;;;                  init fields instead.  Since everything is exported from a struct,
  ;;;;                  all init slots are converted in those.
  ;;;; check-this     : checks to make sure that all uses of this that are not before a dot
  ;;;;                  are on the LHS of a cast or isa expression.  Else an error is thrown.
  ;;;;                  (also until we resolve the mixin safety issue, uses of this before a
  ;;;;                   dot are wrapped in a cast to the class's/mixin's selftype).
  ;;;;                  We also go ahead and implement the check that this is only used inside
  ;;;;                  a class or mixin form.
  ;;;; simplify-ast   : converts lets inside lets or seqs inside seqs into a single let or seq.
  ;;;;                  since the current parser generates a new let or seq for every binding
  ;;;;                  or expression inside of a block, this merges them.
  
  ;;;; convert-static MUST be run before convert-slots.

  ;;;; add-defns-to-tenv (from tenv-utils.ss) must be run before
  ;;;; post-parse-program.  This means that honu:struct and
  ;;;; honu:substruct structures will not appear in the defns,
  ;;;; and so we no longer need to cover them.
  
  (provide post-parse-program post-parse-interaction)
  (define (post-parse-program defns)
    (convert-slots (convert-static (check-this (simplify-ast defns)))))
  
  (define (post-parse-interaction ast)
    (cond
      [(honu:expr? ast)
       (convert-static-expression (check-this-expression (simplify-expression ast) #f) '())]
      [(honu:bind-top? ast)
       (convert-static-defn (check-this-defn (simplify-defn ast)))]))

;                                                                                                    
;                                                                                                    
;                                                                                         @          
;                                              @                    @             @                  
;    $@+@   $@$  @@:@@: @@@ @@@ -@@$   @@-$+  @@@@@         :@@+@  @@@@@   $@$:  @@@@@  -@@     $@+@ 
;   $+ -@  $- -$  @+ :@  $   $  $  -$   @$ :   @            @$ -@   @        -@   @       @    $+ -@ 
;   @      @   @  @   @  +: ++  @@@@@   @      @     @@@@@  :@@$-   @     -$@$@   @       @    @     
;   @      @   @  @   @   $ $   $       @      @               *@   @     $*  @   @       @    @     
;   $* -$  $- -$  @   @   $:+   +:      @      @: :$        @  :@   @: :$ @- *@   @: :$   @    $* -$ 
;    $@$-   $@$  @@@ @@@  :@     $@@+  @@@@@   :@@$-        $+@@:   :@@$- -$$-@@  :@@$- @@@@@   $@$- 
;                                                                                                    
;                                                                                                    
;                                                                                                    
;                                                                                                    

  (define (convert-static defns)
    (map convert-static-defn defns))

  (define (convert-static-defn defn)
    (match defn
      [(struct honu:iface (_ _ _ _))
       defn]
      [(struct honu:class (_ _ _ _ _ inits members _))
       (let-values ([(members _)
                     (convert-static-members members (map honu:formal-name inits))])
         (copy-struct honu:class defn
           [honu:class-members members]))]
      [(struct honu:mixin (_ _ _ arg-type _ _ inits _ super-new members-before members-after _))
       (let*-values ([(members-before env)
                      (convert-static-members members-before (map honu:formal-name inits))]
                     [(super-new)
                      (convert-static-super-new super-new env)]
                     [(env)
                      (extend-env-with-type-members env arg-type)]
                     [(members-after _)
                      (convert-static-members members-after env)])
         (copy-struct honu:mixin defn
           [honu:mixin-super-new super-new]
           [honu:mixin-members-before members-before]
           [honu:mixin-members-after members-after]))]
      [(struct honu:subclass (_ _ _ _))
       defn]
      [(struct honu:function (_ _ _ _ _))
       defn]
      [(struct honu:bind-top (_ _ _ _))
       defn]))

  (define (extend-env-with-type-members env type)
    (let ([type-entry (get-type-entry type)])
      (fold (lambda (m e)
              (cons (tenv:member-name m) e))
            env
            (tenv:type-members type-entry))))

  (define (convert-static-members members env)
    (let loop ([members members]
               [env     env]
               [results '()])
      (cond
        [(null? members) (values (reverse results) env)]
        [(honu:method? (car members))
         (let-values ([(methods remaining) (span honu:method? members)])
           (let ([env (append (map honu:member-defn-name methods) env)])
             (loop remaining
                   env
                   ;; reverse is here just to keep the order
                   (append (reverse (map (lambda (m)
                                           (convert-static-member m env))
                                         members))
                           results))))]
        [else
         (let ([name (honu:member-defn-name (car members))])
           (loop (cdr members)
                 (cons name env)
                 (cons (convert-static-member (car members) env) results)))])))
  
  (define (convert-static-member member env)
    (match member
      [(struct honu:init-field (_ name _ value))
       (if value
          (copy-struct honu:init-field member
            [honu:init-field-value (convert-static-expression value env)])
          member)]
      [(struct honu:field (_ name _ value))
       (copy-struct honu:field member
         [honu:field-value (convert-static-expression value env)])]
      [(struct honu:method (_ name _ args body))
       ;; remember to remove lexical bindings!
       (let ([env (fold (lambda (name env)
                          (delete name env bound-identifier=?))
                        env (map honu:formal-name args))])
         (copy-struct honu:method member
           [honu:method-body (convert-static-expression body env)]))]))

  (define (convert-static-super-new snew env)
    (match snew
      [(struct honu:super-new (_ args))
       (copy-struct honu:super-new snew
         [honu:super-new-args (map (lambda (a)
                                     (convert-static-name-arg a env))
                                   args)])]))
  
  (define (convert-static-name-arg arg env)
    (match arg
      [(struct honu:name-arg (_ value))
       (copy-struct honu:name-arg arg
         [honu:name-arg-value (convert-static-expression value env)])]))
  
  (define (convert-static-expression expr env)
    (match expr
      [(struct honu:this (_))
       expr]
      [(struct honu:select (_ _ arg))
       (copy-struct honu:select expr
         [honu:select-arg (convert-static-expression arg env)])]
      [(struct honu:var (stx name))
       (if (s:member name env bound-identifier=?)
           (make-honu:member stx 'my #f name #f)
           expr)]
      [(struct honu:assn (_ lhs rhs))
       (copy-struct honu:assn expr
         [honu:assn-lhs (convert-static-expression lhs env)]
         [honu:assn-rhs (convert-static-expression rhs env)])]
      [(struct honu:call (_ func arg))
       (copy-struct honu:call expr
         [honu:call-func (convert-static-expression func env)]
         [honu:call-arg  (convert-static-expression arg env)])]
      [(struct honu:lit (_ _ _))
       expr]
      [(struct honu:un-op (_ _ _ _ arg))
       (copy-struct honu:un-op expr
         [honu:un-op-arg (convert-static-expression arg env)])]
      [(struct honu:bin-op (_ _ _ _ larg rarg))
       (copy-struct honu:bin-op expr
         [honu:bin-op-larg (convert-static-expression larg env)]
         [honu:bin-op-rarg (convert-static-expression rarg env)])]
      ;; originally forgot to remove the identifiers bound by
      ;; the lambda from the environment
      [(struct honu:lambda (_ _ args body))
       (let ([env (fold (lambda (name env)
                          (delete name env bound-identifier=?))
                        env (map honu:formal-name args))])
         (copy-struct honu:lambda expr
           [honu:lambda-body (convert-static-expression body env)]))]
      [(struct honu:if (_ cond then else))
       (copy-struct honu:if expr
         [honu:if-cond (convert-static-expression cond env)]
         [honu:if-then (convert-static-expression then env)]
         [honu:if-else (if else (convert-static-expression else env) #f)])]
      [(struct honu:cast (_ obj _))
       (copy-struct honu:cast expr
         [honu:cast-obj (convert-static-expression obj env)])]
      [(struct honu:isa (_ obj _))
       (copy-struct honu:isa expr
         [honu:isa-obj (convert-static-expression obj env)])]
      [(struct honu:member (_ 'my _ _ _))
       expr]
      [(struct honu:member (_ obj _ _ _))
       (copy-struct honu:member expr
         [honu:member-obj (convert-static-expression obj env)])]
      [(struct honu:new (_ _ _ args))
       (copy-struct honu:new expr
         [honu:new-args (map (lambda (a)
                               (convert-static-name-arg a env))
                             args)])]
      [(struct honu:while (_ cond body))
       (copy-struct honu:while expr
         [honu:while-cond (convert-static-expression cond env)]
         [honu:while-body (convert-static-expression body env)])]
      [(struct honu:cond (_ clauses else))
       (copy-struct honu:cond expr
         [honu:cond-clauses (map (lambda (c)
                                   (convert-static-cond-clause c env))
                                 clauses)]
         [honu:cond-else    (if else (convert-static-expression else env) #f)])]
      [(struct honu:return (_ body))
       (copy-struct honu:return expr
         [honu:return-body (convert-static-expression body env)])]
      [(struct honu:tuple (_ vals))
       (copy-struct honu:tuple expr
         [honu:tuple-vals (map (lambda (e)
                                 (convert-static-expression e env))
                               vals)])]
      [(struct honu:let (_ bindings body))
       (let*-values ([(bindings env) (map-and-fold convert-static-binding env bindings)]
                     [(body)         (convert-static-expression body env)])
         (copy-struct honu:let expr
           [honu:let-bindings bindings]
           [honu:let-body     body]))]
      [(struct honu:seq (_ effects value))
       (let ([effects (map (lambda (e)
                             (convert-static-expression e env))
                           effects)]
             [value   (convert-static-expression value env)])
         (copy-struct honu:seq expr
           [honu:seq-effects effects]
           [honu:seq-value   value]))]))
  
  (define (convert-static-binding binding env)
    (match binding
      [(struct honu:binding (_ names _ value))
       (values
        (copy-struct honu:binding binding
          [honu:binding-value (convert-static-expression value env)])
        (fold (lambda (name env)
                (if name
                    (delete name env bound-identifier=?)
                    env))
              env names))]))
  
  (define (convert-static-cond-clause clause env)
    (match clause
      [(struct honu:cond-clause (_ pred rhs))
       (copy-struct honu:cond-clause clause
         [honu:cond-clause-pred (convert-static-expression pred env)]
         [honu:cond-clause-rhs  (convert-static-expression rhs env)])]))

  
;                                                                                             
;                                                                                             
;                                                                   @@                        
;                                              @                     @            @           
;    $@+@   $@$  @@:@@: @@@ @@@ -@@$   @@-$+  @@@@@         :@@+@    @     $@$   @@@@@  :@@+@ 
;   $+ -@  $- -$  @+ :@  $   $  $  -$   @$ :   @            @$ -@    @    $- -$   @     @$ -@ 
;   @      @   @  @   @  +: ++  @@@@@   @      @     @@@@@  :@@$-    @    @   @   @     :@@$- 
;   @      @   @  @   @   $ $   $       @      @               *@    @    @   @   @        *@ 
;   $* -$  $- -$  @   @   $:+   +:      @      @: :$        @  :@    @    $- -$   @: :$ @  :@ 
;    $@$-   $@$  @@@ @@@  :@     $@@+  @@@@@   :@@$-        $+@@:  @@@@@   $@$    :@@$- $+@@: 
;                                                                                             
;                                                                                             
;                                                                                             
;                                                                                             

  ;; CONVERT-SLOTS ASSUMES THAT CONVERT-STATIC HAS BEEN RUN FIRST, SO THAT WE DON'T HAVE TO WORRY
  ;; ABOUT SLOT NAMES BEING CAPTURED BY LEXICAL BINDINGS (AS ALL SLOT ACCESSES SHOULD HAVE BEEN
  ;; CONVERTED TO MY.<SLOTNAME>)
  
  (define (convert-slots defns)
    (map convert-slots-defn defns))

  (define (convert-slots-defn defn)
    (match defn
      [(struct honu:iface (_ _ _ _))
       defn]
      [(struct honu:class (_ _ _ _ _ inits members exports))
       (let* ([env                (map honu:formal-name inits)]
              [used-slots-members (apply append (map (lambda (m) (convert-slots-member m env)) members))]
              [used-slots-exports (apply append (map (lambda (e) (convert-slots-export e env)) exports))]
              [used-slots         (append used-slots-members used-slots-exports)])
         (let loop ([inits      inits]
                    [kept-inits '()]
                    [new-fields '()])
           (if (null? inits)
               (copy-struct honu:class defn
                 [honu:class-inits   (reverse kept-inits)]
                 [honu:class-members (append (reverse new-fields) members)])
               (if (s:member (honu:formal-name (car inits)) used-slots bound-identifier=?)
                   (loop (cdr inits)
                         kept-inits
                         (cons (make-honu:init-field (honu:ast-stx (car inits))
                                                     (honu:formal-name (car inits))
                                                     (honu:formal-type (car inits))
                                                     #f)
                               new-fields))
                   (loop (cdr inits)
                         (cons (car inits) kept-inits)
                         new-fields)))))]
      [(struct honu:mixin (_ _ _ _ _ _ inits _ _ members-before members-after exports))
       (let* ([env                (map honu:formal-name inits)]
              [used-slots-before  (apply append (map (lambda (m) (convert-slots-member m env)) members-before))]
              [used-slots-after   (apply append (map (lambda (m) (convert-slots-member m env)) members-after))]
              [used-slots-exports (apply append (map (lambda (e) (convert-slots-export e env)) exports))]
              [used-slots         (append used-slots-before used-slots-after used-slots-exports)])
         (let loop ([inits      inits]
                    [kept-inits '()]
                    [new-fields '()])
           (if (null? inits)
               (copy-struct honu:mixin defn
                 [honu:mixin-inits          (reverse kept-inits)]
                 [honu:mixin-members-before (append (reverse new-fields) members-before)])
               (if (s:member (honu:formal-name (car inits)) used-slots bound-identifier=?)
                   (loop (cdr inits)
                         kept-inits
                         (cons (make-honu:init-field (honu:ast-stx (car inits))
                                                     (honu:formal-name (car inits))
                                                     (honu:formal-type (car inits))
                                                     #f)
                               new-fields))
                   (loop (cdr inits)
                         (cons (car inits) kept-inits)
                         new-fields)))))]
      [(struct honu:subclass (_ _ _ _))
       defn]
      [(struct honu:function (_ _ _ _ _))
       defn]
      [(struct honu:bind-top (_ _ _ _))
       defn]))

  (define (convert-slots-member member env)
    (match member
      ;; init fields and fields do not necessitate converting init slots into init fields
      [(struct honu:init-field (_ name _ value))
       (list)]
      [(struct honu:field (_ name _ value))
       (list)]
      ;; methods do, though.
      [(struct honu:method (_ name _ _ body))
       (convert-slots-expression body env)]))

  (define (convert-slots-export export env)
    (match export
      [(struct honu:export (_ _ binds))
       (filter (lambda (old)
                 (s:member old env bound-identifier=?))
               (map honu:exp-bind-old binds))]))
  
  (define (convert-slots-name-arg arg env)
    (match arg
      [(struct honu:name-arg (_ value))
       (convert-slots-expression value env)]))
  
  (define (convert-slots-expression expr env)
    (match expr
      [(struct honu:this (_))
       (list)]
      [(struct honu:select (_ _ arg))
       (convert-slots-expression arg env)]
      [(struct honu:var (_ _))
       (list)]
      [(struct honu:assn (_ lhs rhs))
       (append (convert-slots-expression lhs env)
               (convert-slots-expression rhs env))]
      [(struct honu:call (_ func arg))
       (append (convert-slots-expression func env)
               (convert-slots-expression arg env))]
      [(struct honu:lit (_ _ _))
       (list)]
      [(struct honu:un-op (_ _ _ _ arg))
       (convert-slots-expression arg env)]
      [(struct honu:bin-op (_ _ _ _ larg rarg))
       (append (convert-slots-expression larg env)
               (convert-slots-expression rarg env))]
      [(struct honu:lambda (_ _ _ body))
       (convert-slots-expression body env)]
      [(struct honu:if (_ cond then else))
       (append (convert-slots-expression cond env)
               (convert-slots-expression then env)
               (if else (convert-slots-expression else env) (list)))]
      [(struct honu:cast (_ obj _))
       (convert-slots-expression obj env)]
      [(struct honu:isa (_ obj _))
       (convert-slots-expression obj env)]
      [(struct honu:member (_ 'my _ name _))
       (cond
         ;; if this is a (bare-referenced) init slot,
         ;; then we return a "set" of it, else we return
         ;; an empty "set"
         [(s:member name env bound-identifier=?)
          =>
          (lambda (l) (list (car l)))]
         [else (list)])]
      [(struct honu:member (_ obj _ _ _))
       (convert-slots-expression obj env)]
      [(struct honu:new (_ _ _ args))
       (apply append
              (map (lambda (a)
                     (convert-slots-name-arg a env))
                   args))]
      [(struct honu:while (_ cond body))
       (append (convert-slots-expression cond env)
               (convert-slots-expression body env))]
      [(struct honu:cond (_ clauses else))
       (apply append (cons (if else (convert-slots-expression else env) (list))
                           (map (lambda (c)
                                  (convert-slots-cond-clause c env))
                                clauses)))]
      [(struct honu:return (_ body))
       (convert-slots-expression body env)]
      [(struct honu:tuple (_ vals))
       (apply append
              (map (lambda (e)
                     (convert-slots-expression e env))
                   vals))]
      [(struct honu:let (_ bindings body))
       (let ([bindings (map (lambda (b)
                              (convert-slots-binding b env))
                            bindings)]
             [body     (convert-slots-expression body env)])
         (apply append (cons body bindings)))]
      [(struct honu:seq (_ effects value))
       (apply append (cons (convert-slots-expression value env)
                           (map (lambda (e)
                                  (convert-slots-expression e env))
                                effects)))]))
  
  (define (convert-slots-binding binding env)
    (match binding
      [(struct honu:binding (_ _ _ value))
       (convert-slots-expression value env)]))
  
  (define (convert-slots-cond-clause clause env)
    (match clause
      [(struct honu:cond-clause (_ pred rhs))
       (append (convert-slots-expression pred env)
               (convert-slots-expression rhs env))]))

  
;                                                                        
;                                                                        
;         @@                   @@                   @@        @          
;          @                    @              @     @                   
;    $@+@  @-@@:  -@@$    $@+@  @ @@@         @@@@@  @-@@:  -@@    :@@+@ 
;   $+ -@  @+ :@  $  -$  $+ -@  @ *$           @     @+ :@    @    @$ -@ 
;   @      @   @  @@@@@  @      @$$    @@@@@   @     @   @    @    :@@$- 
;   @      @   @  $      @      @$$            @     @   @    @       *@ 
;   $* -$  @   @  +:     $* -$  @ -$           @: :$ @   @    @    @  :@ 
;    $@$- @@@ @@@  $@@+   $@$- @@ @@@-         :@@$-@@@ @@@ @@@@@  $+@@: 
;                                                                        
;                                                                        
;                                                                        
;                                                                        

  (define (check-this defns)
    (map check-this-defn defns))

  (define (check-this-defn defn)
    (match defn
      [(struct honu:iface (_ _ _ _))
       defn]
      [(struct honu:class (_ _ type _ _ _ members _))
       (let ([members (map (lambda (m) (check-this-member m type)) members)])
         (copy-struct honu:class defn
           [honu:class-members members]))]
      [(struct honu:mixin (_ _ type _ _ _ _ _ super-new members-before members-after _))
       (let ([members-before (map (lambda (m) (check-this-member m type)) members-before)]
             [super-new      (check-this-super-new super-new type)]
             [members-after  (map (lambda (m) (check-this-member m type)) members-after)])
         (copy-struct honu:mixin defn
           [honu:mixin-super-new super-new]
           [honu:mixin-members-before members-before]
           [honu:mixin-members-after members-after]))]
      [(struct honu:subclass (_ _ _ _))
       defn]
      [(struct honu:function (_ _ _ _ body))
       ;; we only use check-this-expression here for side-effects (we should not get
       ;; a changed AST if this passes, only an exception if the this keyword occurs here).
       (begin
         (check-this-expression body #f)
         defn)]
      [(struct honu:bind-top (_ _ _ rhs))
       ;; same check as in honu:function.
       (begin
         (check-this-expression rhs #f)
         defn)]))

  (define (check-this-member member type)
    (match member
      [(struct honu:init-field (_ name _ value))
       (if value
           (copy-struct honu:init-field member
             [honu:init-field-value (check-this-expression value type)])
           member)]
      [(struct honu:field (_ name _ value))
       (copy-struct honu:field member
         [honu:field-value (check-this-expression value type)])]
      [(struct honu:method (_ name _ args body))
       (copy-struct honu:method member
         [honu:method-body (check-this-expression body type)])]))

  (define (check-this-super-new snew type)
    (match snew
      [(struct honu:super-new (_ args))
       (copy-struct honu:super-new snew
         [honu:super-new-args (map (lambda (a)
                                     (check-this-name-arg a type))
                                   args)])]))
  
  (define (check-this-name-arg arg type)
    (match arg
      [(struct honu:name-arg (_ value))
       (copy-struct honu:name-arg arg
         [honu:name-arg-value (check-this-expression value type)])]))
  
  (define (check-this-expression expr type)
    (match expr
      [(struct honu:this (stx))
       (if type
           (raise-read-error-with-stx
            "Unprotected use of this in a client context"
            stx)
           (raise-read-error-with-stx
            "Use of this keyword found outside of a class or mixin"
            stx))]
      [(struct honu:select (_ _ arg))
       (copy-struct honu:select expr
         [honu:select-arg (check-this-expression arg type)])]
      [(struct honu:var (_ _))
       expr]
      [(struct honu:assn (_ lhs rhs))
       (copy-struct honu:assn expr
         [honu:assn-lhs (check-this-expression lhs type)]
         [honu:assn-rhs (check-this-expression rhs type)])]
      [(struct honu:call (_ func arg))
       (copy-struct honu:call expr
         [honu:call-func (check-this-expression func type)]
         [honu:call-arg  (check-this-expression arg type)])]
      [(struct honu:lit (_ _ _))
       expr]
      [(struct honu:un-op (_ _ _ _ arg))
       (copy-struct honu:un-op expr
         [honu:un-op-arg (check-this-expression arg type)])]
      [(struct honu:bin-op (_ _ _ _ larg rarg))
       (copy-struct honu:bin-op expr
         [honu:bin-op-larg (check-this-expression larg type)]
         [honu:bin-op-rarg (check-this-expression rarg type)])]
      [(struct honu:lambda (_ _ _ body))
       (copy-struct honu:lambda expr
         [honu:lambda-body (check-this-expression body type)])]
      [(struct honu:if (_ cond then else))
       (copy-struct honu:if expr
         [honu:if-cond (check-this-expression cond type)]
         [honu:if-then (check-this-expression then type)]
         [honu:if-else (if else (check-this-expression else type) #f)])]
      [(struct honu:cast (_ obj _))
       (if (honu:this? obj)
           (if type
               expr
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (honu:ast-stx obj)))
           (copy-struct honu:cast expr
             [honu:cast-obj (check-this-expression obj type)]))]
      [(struct honu:isa (_ obj _))
       (if (honu:this? obj)
           (if type
               expr
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (honu:ast-stx obj)))
           (copy-struct honu:isa expr
             [honu:isa-obj (check-this-expression obj type)]))]
      [(struct honu:member (_ 'my _ _ _))
       expr]
      [(struct honu:member (_ obj _ _ _))
       (if (honu:this? obj)
           (if type
               ;; to deal with the fact that mixins can mess up the selftype
               ;; property, we hack by creating casts in this case.
               (copy-struct honu:member expr
                 [honu:member-obj (make-honu:cast (honu:ast-stx obj)
                                                  obj
                                                  type)])
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (honu:ast-stx obj)))               
           (copy-struct honu:member expr
             [honu:member-obj (check-this-expression obj type)]))]
      [(struct honu:new (_ _ _ args))
       (copy-struct honu:new expr
         [honu:new-args (map (lambda (a)
                               (check-this-name-arg a type))
                             args)])]
      [(struct honu:while (_ cond body))
       (copy-struct honu:while expr
         [honu:while-cond (check-this-expression cond type)]
         [honu:while-body (check-this-expression body type)])]
      [(struct honu:cond (_ clauses else))
       (copy-struct honu:cond expr
         [honu:cond-clauses (map (lambda (c)
                                   (check-this-cond-clause c type))
                                 clauses)]
         [honu:cond-else    (if else (check-this-expression else type) #f)])]
      [(struct honu:return (_ body))
       (copy-struct honu:return expr
         [honu:return-body (check-this-expression body type)])]
      [(struct honu:tuple (_ vals))
       (copy-struct honu:tuple expr
         [honu:tuple-vals (map (lambda (e)
                                 (check-this-expression e type))
                               vals)])]
      [(struct honu:let (_ bindings body))
       (let ([bindings (map (lambda (b)
                              (check-this-binding b type))
                            bindings)]
             [body     (check-this-expression body type)])
         (copy-struct honu:let expr
           [honu:let-bindings bindings]
           [honu:let-body     body]))]
      [(struct honu:seq (_ effects value))
       (let ([effects (map (lambda (e)
                             (check-this-expression e type))
                           effects)]
             [value   (check-this-expression value type)])
         (copy-struct honu:seq expr
           [honu:seq-effects effects]
           [honu:seq-value   value]))]))
  
  (define (check-this-binding binding type)
    (match binding
      [(struct honu:binding (_ names _ value))
       (copy-struct honu:binding binding
         [honu:binding-value (check-this-expression value type)])]))
  
  (define (check-this-cond-clause clause type)
    (match clause
      [(struct honu:cond-clause (_ pred rhs))
       (copy-struct honu:cond-clause clause
         [honu:cond-clause-pred (check-this-expression pred type)]
         [honu:cond-clause-rhs  (check-this-expression rhs type)])]))  
  
  
;                                                                                      
;                                                                                      
;            @                   @@      @     :@@$                                    
;                                 @            @:                                 @    
;   :@@+@  -@@   @@+-$: @@:@$-    @    -@@    @@@@@ @@@ @@@         $@$:  :@@+@  @@@@@ 
;   @$ -@    @    @+@$@  @: -$    @      @     @     $-  $-           -@  @$ -@   @    
;   :@@$-    @    @ @ @  @   @    @      @     @     -$  $  @@@@@  -$@$@  :@@$-   @    
;      *@    @    @ @ @  @   @    @      @     @      $*$:         $*  @     *@   @    
;   @  :@    @    @ @ @  @: -$    @      @     @       $$          @- *@  @  :@   @: :$
;   $+@@:  @@@@@ @@@@@@@ @-@$   @@@@@  @@@@@  @@@@@    $*          -$$-@@ $+@@:   :@@$-
;                        @                             $                               
;                       @@@                          @@@@                              
;                                                                                      
;                                                                                      
  
  (define (simplify-ast defns)
    (map simplify-defn defns))
  
  (define (simplify-defn defn)
    (match defn
      [(struct honu:iface (_ _ _ _))
       defn]
      [(struct honu:class (_ _ _ _ _ _ members _))
       (copy-struct honu:class defn
         [honu:class-members (map simplify-member members)])]
      [(struct honu:mixin (_ _ _ _ _ _ _ _ super-new members-before members-after _))
       (copy-struct honu:mixin defn
         [honu:mixin-super-new (simplify-super-new super-new)]
         [honu:mixin-members-before (map simplify-member members-before)]
         [honu:mixin-members-after (map simplify-member members-after)])]
      [(struct honu:subclass (_ _ _ _))
       defn]
      [(struct honu:function (_ _ _ _ body))
       (copy-struct honu:function defn
         [honu:function-body (simplify-expression body)])]
      [(struct honu:bind-top (_ _ _ value))
       (copy-struct honu:bind-top defn
         [honu:bind-top-value (simplify-expression value)])]))
  
  (define (simplify-member member)
    (match member
      [(struct honu:init-field (_ _ _ value))
       (if value
           (copy-struct honu:init-field member
             [honu:init-field-value (simplify-expression value)])
           member)]
      [(struct honu:field (_ _ _ value))
       (copy-struct honu:field member
         [honu:field-value (simplify-expression value)])]
      [(struct honu:method (_ _ _ _ body))
       (copy-struct honu:method member
         [honu:method-body (simplify-expression body)])]))
  
  (define (simplify-super-new snew)
    (match snew
      [(struct honu:super-new (_ args))
       (copy-struct honu:super-new snew
         [honu:super-new-args (map simplify-name-arg args)])]))
  
  (define (simplify-name-arg arg)
    (match arg
      [(struct honu:name-arg (_ value))
       (copy-struct honu:name-arg arg
         [honu:name-arg-value (simplify-expression value)])]))
  
  (define (simplify-expression expr)
    (match expr
      [(struct honu:this (_))
       expr]
      [(struct honu:select (_ _ arg))
       (copy-struct honu:select expr
         [honu:select-arg (simplify-expression arg)])]
      [(struct honu:var (_ _))
       expr]
      [(struct honu:assn (_ lhs rhs))
       (copy-struct honu:assn expr
         [honu:assn-lhs (simplify-expression lhs)]
         [honu:assn-rhs (simplify-expression rhs)])]
      [(struct honu:call (_ func arg))
       (copy-struct honu:call expr
         [honu:call-func (simplify-expression func)]
         [honu:call-arg  (simplify-expression arg)])]
      [(struct honu:lit (_ _ _))
       expr]
      [(struct honu:un-op (_ _ _ _ arg))
       (copy-struct honu:un-op expr
         [honu:un-op-arg (simplify-expression arg)])]
      [(struct honu:bin-op (_ _ _ _ larg rarg))
       (copy-struct honu:bin-op expr
         [honu:bin-op-larg (simplify-expression larg)]
         [honu:bin-op-rarg (simplify-expression rarg)])]
      [(struct honu:lambda (_ _ _ body))
       (copy-struct honu:lambda expr
         [honu:lambda-body (simplify-expression body)])]
      [(struct honu:if (_ cond then else))
       (copy-struct honu:if expr
         [honu:if-cond (simplify-expression cond)]
         [honu:if-then (simplify-expression then)]
         [honu:if-else (if else (simplify-expression else) #f)])]
      [(struct honu:cast (_ obj _))
       (copy-struct honu:cast expr
         [honu:cast-obj (simplify-expression obj)])]
      [(struct honu:isa (_ obj _))
       (copy-struct honu:isa expr
         [honu:isa-obj (simplify-expression obj)])]
      [(struct honu:member (_ 'my _ _ _))
       expr]
      [(struct honu:member (_ obj _ _ _))
       (copy-struct honu:member expr
         [honu:member-obj (simplify-expression obj)])]
      [(struct honu:new (_ _ _ args))
       (copy-struct honu:new expr
         [honu:new-args (map simplify-name-arg args)])]
      [(struct honu:cond (_ clauses else))
       (copy-struct honu:cond expr
         [honu:cond-clauses (map simplify-cond-clause clauses)]
         [honu:cond-else    (if else (simplify-expression else) #f)])]
      [(struct honu:while (_ cond body))
       (copy-struct honu:while expr
         [honu:while-cond (simplify-expression cond)]
         [honu:while-body (simplify-expression body)])]
      [(struct honu:return (_ body))
       (copy-struct honu:return expr
         [honu:return-body (simplify-expression body)])]
      [(struct honu:tuple (_ vals))
       (copy-struct honu:tuple expr
         [honu:tuple-vals (map simplify-expression vals)])]
      [(struct honu:let (stx bindings body))
       (let ([bindings (map simplify-binding bindings)]
             [body     (simplify-expression body)])
         (match body
           [(struct honu:let (_ sub-bindings sub-body))
            (make-honu:let stx (append bindings sub-bindings) sub-body)]
           [_
            (copy-struct honu:let expr
              [honu:let-bindings bindings]
              [honu:let-body     body])]))]
      [(struct honu:seq (stx effects value))
       (let ([effects (map simplify-expression effects)]
             [value   (simplify-expression value)])
         (match value
           [(struct honu:seq (_ sub-effects sub-value))
            (make-honu:seq stx (append effects sub-effects) sub-value)]
           [_
            (copy-struct honu:seq expr
              [honu:seq-effects effects]
              [honu:seq-value   value])]))]))
  
  (define (simplify-binding binding)
    (match binding
      [(struct honu:binding (_ _ _ value))
       (copy-struct honu:binding binding
         [honu:binding-value (simplify-expression value)])]))

  (define (simplify-cond-clause clause)
    (match clause
      [(struct honu:cond-clause (_ pred rhs))
       (copy-struct honu:cond-clause clause
         [honu:cond-clause-pred (simplify-expression pred)]
         [honu:cond-clause-rhs  (simplify-expression rhs)])]))
  )
