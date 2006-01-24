(module post-parsing mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../ast.ss"
           "../parameters.ss"
           "../readerr.ss"
           "../tenv.ss"
           "../private/tools/general.ss")

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
  ;;;; post-parse-program.  This means that ast:defn:structure and
  ;;;; ast:defn:substructure structures will not appear in the defns,
  ;;;; and so we no longer need to cover them.
  
  (provide post-parse-program post-parse-interaction)
  (define (post-parse-program defns)
    (convert-slots (convert-static (check-this (simplify-ast defns)))))
  
  (define (post-parse-interaction ast)
    (cond
      [(ast:expr? ast)
       (convert-static-expression (check-this-expression (simplify-expression ast) #f) '())]
      [(ast:defn:binding? ast)
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
      [(struct ast:defn:iface (_ _ _ _))
       defn]
      [(struct ast:defn:class (_ _ _ _ _ inits members _))
       (let-values ([(members _)
                     (convert-static-members members (map ast:formal-name inits))])
         (copy-struct ast:defn:class defn
           [ast:defn:class-members members]))]
      [(struct ast:defn:mixin (_ _ _ arg-type _ _ inits _ super-new members-before members-after _))
       (let*-values ([(members-before env)
                      (convert-static-members members-before (map ast:formal-name inits))]
                     [(super-new)
                      (convert-static-super-new super-new env)]
                     [(env)
                      (extend-env-with-type-members env arg-type)]
                     [(members-after _)
                      (convert-static-members members-after env)])
         (copy-struct ast:defn:mixin defn
           [ast:defn:mixin-super-new super-new]
           [ast:defn:mixin-pre-members members-before]
           [ast:defn:mixin-post-members members-after]))]
      [(struct ast:defn:subclass (_ _ _ _))
       defn]
      [(struct ast:defn:function (_ _ _ _ _))
       defn]
      [(struct ast:defn:binding (_ _ _ _))
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
        [(ast:class/member:method? (car members))
         (let-values ([(methods remaining) (span ast:class/member:method? members)])
           (let ([env (append (map ast:class/member-name methods) env)])
             (loop remaining
                   env
                   ;; reverse is here just to keep the order
                   (append (reverse (map (lambda (m)
                                           (convert-static-member m env))
                                         members))
                           results))))]
        [else
         (let ([name (ast:class/member-name (car members))])
           (loop (cdr members)
                 (cons name env)
                 (cons (convert-static-member (car members) env) results)))])))
  
  (define (convert-static-member member env)
    (match member
      [(struct ast:class/member:field/formal (_ name _ value))
       (if value
          (copy-struct ast:class/member:field/formal member
            [ast:class/member:field/formal-default (convert-static-expression value env)])
          member)]
      [(struct ast:class/member:field (_ name _ value))
       (copy-struct ast:class/member:field member
         [ast:class/member:field-default (convert-static-expression value env)])]
      [(struct ast:class/member:method (_ name _ args body))
       ;; remember to remove lexical bindings!
       (let ([env (fold (lambda (name env)
                          (delete name env bound-identifier=?))
                        env (map ast:formal-name args))])
         (copy-struct ast:class/member:method member
           [ast:class/member:method-body (convert-static-expression body env)]))]))

  (define (convert-static-super-new snew env)
    (match snew
      [(struct ast:super-new (_ args))
       (copy-struct ast:super-new snew
         [ast:super-new-args (map (lambda (a)
                                     (convert-static-name-arg a env))
                                   args)])]))
  
  (define (convert-static-name-arg arg env)
    (match arg
      [(struct ast:named/arg (_ _ value))
       (copy-struct ast:named/arg arg
         [ast:named/arg-actual (convert-static-expression value env)])]))
  
  (define (convert-static-expression expr env)
    (match expr
      [(struct ast:expr:self (_))
       expr]
      [(struct ast:expr:tuple/select (_ _ arg))
       (copy-struct ast:expr:tuple/select expr
         [ast:expr:tuple/select-arg (convert-static-expression arg env)])]
      [(struct ast:expr:var (stx name))
       (if (s:member name env bound-identifier=?)
           (make-ast:expr:member stx 'my #f name #f)
           expr)]
      [(struct ast:expr:assign (_ lhs rhs))
       (copy-struct ast:expr:assign expr
         [ast:expr:assign-lhs (convert-static-expression lhs env)]
         [ast:expr:assign-rhs (convert-static-expression rhs env)])]
      [(struct ast:expr:apply (_ func arg))
       (copy-struct ast:expr:apply expr
         [ast:expr:apply-func (convert-static-expression func env)]
         [ast:expr:apply-arg  (convert-static-expression arg env)])]
      [(struct ast:expr:literal (_ _ _))
       expr]
      [(struct ast:expr:unary/op (_ _ _ _ arg))
       (copy-struct ast:expr:unary/op expr
         [ast:expr:unary/op-arg (convert-static-expression arg env)])]
      [(struct ast:expr:binary/op (_ _ _ _ larg rarg))
       (copy-struct ast:expr:binary/op expr
         [ast:expr:binary/op-left (convert-static-expression larg env)]
         [ast:expr:binary/op-right (convert-static-expression rarg env)])]
      ;; originally forgot to remove the identifiers bound by
      ;; the lambda from the environment
      [(struct ast:expr:function (_ _ args body))
       (let ([env (fold (lambda (name env)
                          (delete name env bound-identifier=?))
                        env (map ast:formal-name args))])
         (copy-struct ast:expr:function expr
           [ast:expr:function-body (convert-static-expression body env)]))]
      [(struct ast:expr:if (_ cond then else))
       (copy-struct ast:expr:if expr
         [ast:expr:if-test (convert-static-expression cond env)]
         [ast:expr:if-then (convert-static-expression then env)]
         [ast:expr:if-else (if else (convert-static-expression else env) #f)])]
      [(struct ast:expr:cast (_ obj _))
       (copy-struct ast:expr:cast expr
         [ast:expr:cast-object (convert-static-expression obj env)])]
      [(struct ast:expr:isa (_ obj _))
       (copy-struct ast:expr:isa expr
         [ast:expr:isa-object (convert-static-expression obj env)])]
      [(struct ast:expr:member (_ 'my _ _ _))
       expr]
      [(struct ast:expr:member (_ obj _ _ _))
       (copy-struct ast:expr:member expr
         [ast:expr:member-object (convert-static-expression obj env)])]
      [(struct ast:expr:new (_ _ _ args))
       (copy-struct ast:expr:new expr
         [ast:expr:new-args (map (lambda (a)
                               (convert-static-name-arg a env))
                             args)])]
      [(struct ast:expr:while (_ cond body))
       (copy-struct ast:expr:while expr
         [ast:expr:while-test (convert-static-expression cond env)]
         [ast:expr:while-body (convert-static-expression body env)])]
      [(struct ast:expr:cond (_ clauses else))
       (copy-struct ast:expr:cond expr
         [ast:expr:cond-clauses (map (lambda (c)
                                   (convert-static-cond-clause c env))
                                 clauses)]
         [ast:expr:cond-else    (if else (convert-static-expression else env) #f)])]
      [(struct ast:expr:return (_ body))
       (copy-struct ast:expr:return expr
         [ast:expr:return-result (convert-static-expression body env)])]
      [(struct ast:expr:tuple (_ vals))
       (copy-struct ast:expr:tuple expr
         [ast:expr:tuple-elems (map (lambda (e)
                                 (convert-static-expression e env))
                               vals)])]
      [(struct ast:expr:let (_ bindings body))
       (let*-values ([(bindings env) (map-and-fold convert-static-binding env bindings)]
                     [(body)         (convert-static-expression body env)])
         (copy-struct ast:expr:let expr
           [ast:expr:let-bindings bindings]
           [ast:expr:let-body     body]))]
      [(struct ast:expr:sequence (_ effects value))
       (let ([effects (map (lambda (e)
                             (convert-static-expression e env))
                           effects)]
             [value   (convert-static-expression value env)])
         (copy-struct ast:expr:sequence expr
           [ast:expr:sequence-statements effects]
           [ast:expr:sequence-result   value]))]))
  
  (define (convert-static-binding binding env)
    (match binding
      [(struct ast:defn:binding (_ names _ value))
       (values
        (copy-struct ast:defn:binding binding
          [ast:defn:binding-init (convert-static-expression value env)])
        (fold (lambda (name env)
                (if name
                    (delete name env bound-identifier=?)
                    env))
              env names))]))
  
  (define (convert-static-cond-clause clause env)
    (match clause
      [(struct ast:cond/clause (_ pred rhs))
       (copy-struct ast:cond/clause clause
         [ast:cond/clause-test (convert-static-expression pred env)]
         [ast:cond/clause-result  (convert-static-expression rhs env)])]))

  
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
      [(struct ast:defn:iface (_ _ _ _))
       defn]
      [(struct ast:defn:class (_ _ _ _ _ inits members exports))
       (let* ([env                (map ast:formal-name inits)]
              [used-slots-members (apply append (map (lambda (m) (convert-slots-member m env)) members))]
              [used-slots-exports (apply append (map (lambda (e) (convert-slots-export e env)) exports))]
              [used-slots         (append used-slots-members used-slots-exports)])
         (let loop ([inits      inits]
                    [kept-inits '()]
                    [new-fields '()])
           (if (null? inits)
               (copy-struct ast:defn:class defn
                 [ast:defn:class-formals   (reverse kept-inits)]
                 [ast:defn:class-members (append (reverse new-fields) members)])
               (if (s:member (ast:formal-name (car inits)) used-slots bound-identifier=?)
                   (loop (cdr inits)
                         kept-inits
                         (cons (make-ast:class/member:field/formal (ast-syntax (car inits))
                                                     (ast:formal-name (car inits))
                                                     (ast:formal-type (car inits))
                                                     #f)
                               new-fields))
                   (loop (cdr inits)
                         (cons (car inits) kept-inits)
                         new-fields)))))]
      [(struct ast:defn:mixin (_ _ _ _ _ _ inits _ _ members-before members-after exports))
       (let* ([env                (map ast:formal-name inits)]
              [used-slots-before  (apply append (map (lambda (m) (convert-slots-member m env)) members-before))]
              [used-slots-after   (apply append (map (lambda (m) (convert-slots-member m env)) members-after))]
              [used-slots-exports (apply append (map (lambda (e) (convert-slots-export e env)) exports))]
              [used-slots         (append used-slots-before used-slots-after used-slots-exports)])
         (let loop ([inits      inits]
                    [kept-inits '()]
                    [new-fields '()])
           (if (null? inits)
               (copy-struct ast:defn:mixin defn
                 [ast:defn:mixin-formals          (reverse kept-inits)]
                 [ast:defn:mixin-pre-members (append (reverse new-fields) members-before)])
               (if (s:member (ast:formal-name (car inits)) used-slots bound-identifier=?)
                   (loop (cdr inits)
                         kept-inits
                         (cons (make-ast:class/member:field/formal (ast-syntax (car inits))
                                                     (ast:formal-name (car inits))
                                                     (ast:formal-type (car inits))
                                                     #f)
                               new-fields))
                   (loop (cdr inits)
                         (cons (car inits) kept-inits)
                         new-fields)))))]
      [(struct ast:defn:subclass (_ _ _ _))
       defn]
      [(struct ast:defn:function (_ _ _ _ _))
       defn]
      [(struct ast:defn:binding (_ _ _ _))
       defn]))

  (define (convert-slots-member member env)
    (match member
      ;; init fields and fields do not necessitate converting init slots into init fields
      [(struct ast:class/member:field/formal (_ name _ value))
       (list)]
      [(struct ast:class/member:field (_ name _ value))
       (list)]
      ;; methods do, though.
      [(struct ast:class/member:method (_ name _ _ body))
       (convert-slots-expression body env)]))

  (define (convert-slots-export export env)
    (match export
      [(struct ast:export (_ _ binds))
       (filter (lambda (old)
                 (s:member old env bound-identifier=?))
               (map ast:export/member-internal binds))]))
  
  (define (convert-slots-name-arg arg env)
    (match arg
      [(struct ast:named/arg (_ _ value))
       (convert-slots-expression value env)]))
  
  (define (convert-slots-expression expr env)
    (match expr
      [(struct ast:expr:self (_))
       (list)]
      [(struct ast:expr:tuple/select (_ _ arg))
       (convert-slots-expression arg env)]
      [(struct ast:expr:var (_ _))
       (list)]
      [(struct ast:expr:assign (_ lhs rhs))
       (append (convert-slots-expression lhs env)
               (convert-slots-expression rhs env))]
      [(struct ast:expr:apply (_ func arg))
       (append (convert-slots-expression func env)
               (convert-slots-expression arg env))]
      [(struct ast:expr:literal (_ _ _))
       (list)]
      [(struct ast:expr:unary/op (_ _ _ _ arg))
       (convert-slots-expression arg env)]
      [(struct ast:expr:binary/op (_ _ _ _ larg rarg))
       (append (convert-slots-expression larg env)
               (convert-slots-expression rarg env))]
      [(struct ast:expr:function (_ _ _ body))
       (convert-slots-expression body env)]
      [(struct ast:expr:if (_ cond then else))
       (append (convert-slots-expression cond env)
               (convert-slots-expression then env)
               (if else (convert-slots-expression else env) (list)))]
      [(struct ast:expr:cast (_ obj _))
       (convert-slots-expression obj env)]
      [(struct ast:expr:isa (_ obj _))
       (convert-slots-expression obj env)]
      [(struct ast:expr:member (_ 'my _ name _))
       (cond
         ;; if this is a (bare-referenced) init slot,
         ;; then we return a "set" of it, else we return
         ;; an empty "set"
         [(s:member name env bound-identifier=?)
          =>
          (lambda (l) (list (car l)))]
         [else (list)])]
      [(struct ast:expr:member (_ obj _ _ _))
       (convert-slots-expression obj env)]
      [(struct ast:expr:new (_ _ _ args))
       (apply append
              (map (lambda (a)
                     (convert-slots-name-arg a env))
                   args))]
      [(struct ast:expr:while (_ cond body))
       (append (convert-slots-expression cond env)
               (convert-slots-expression body env))]
      [(struct ast:expr:cond (_ clauses else))
       (apply append (cons (if else (convert-slots-expression else env) (list))
                           (map (lambda (c)
                                  (convert-slots-cond-clause c env))
                                clauses)))]
      [(struct ast:expr:return (_ body))
       (convert-slots-expression body env)]
      [(struct ast:expr:tuple (_ vals))
       (apply append
              (map (lambda (e)
                     (convert-slots-expression e env))
                   vals))]
      [(struct ast:expr:let (_ bindings body))
       (let ([bindings (map (lambda (b)
                              (convert-slots-binding b env))
                            bindings)]
             [body     (convert-slots-expression body env)])
         (apply append (cons body bindings)))]
      [(struct ast:expr:sequence (_ effects value))
       (apply append (cons (convert-slots-expression value env)
                           (map (lambda (e)
                                  (convert-slots-expression e env))
                                effects)))]))
  
  (define (convert-slots-binding binding env)
    (match binding
      [(struct ast:defn:binding (_ _ _ value))
       (convert-slots-expression value env)]))
  
  (define (convert-slots-cond-clause clause env)
    (match clause
      [(struct ast:cond/clause (_ pred rhs))
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
      [(struct ast:defn:iface (_ _ _ _))
       defn]
      [(struct ast:defn:class (_ _ type _ _ _ members _))
       (let ([members (map (lambda (m) (check-this-member m type)) members)])
         (copy-struct ast:defn:class defn
           [ast:defn:class-members members]))]
      [(struct ast:defn:mixin (_ _ type _ _ _ _ _ super-new members-before members-after _))
       (let ([members-before (map (lambda (m) (check-this-member m type)) members-before)]
             [super-new      (check-this-super-new super-new type)]
             [members-after  (map (lambda (m) (check-this-member m type)) members-after)])
         (copy-struct ast:defn:mixin defn
           [ast:defn:mixin-super-new super-new]
           [ast:defn:mixin-pre-members members-before]
           [ast:defn:mixin-post-members members-after]))]
      [(struct ast:defn:subclass (_ _ _ _))
       defn]
      [(struct ast:defn:function (_ _ _ _ body))
       ;; we only use check-this-expression here for side-effects (we should not get
       ;; a changed AST if this passes, only an exception if the this keyword occurs here).
       (begin
         (check-this-expression body #f)
         defn)]
      [(struct ast:defn:binding (_ _ _ rhs))
       ;; same check as in ast:defn:function.
       (begin
         (check-this-expression rhs #f)
         defn)]))

  (define (check-this-member member type)
    (match member
      [(struct ast:class/member:field/formal (_ name _ value))
       (if value
           (copy-struct ast:class/member:field/formal member
             [ast:class/member:field/formal-default (check-this-expression value type)])
           member)]
      [(struct ast:class/member:field (_ name _ value))
       (copy-struct ast:class/member:field member
         [ast:class/member:field-default (check-this-expression value type)])]
      [(struct ast:class/member:method (_ name _ args body))
       (copy-struct ast:class/member:method member
         [ast:class/member:method-body (check-this-expression body type)])]))

  (define (check-this-super-new snew type)
    (match snew
      [(struct ast:super-new (_ args))
       (copy-struct ast:super-new snew
         [ast:super-new-args (map (lambda (a)
                                     (check-this-name-arg a type))
                                   args)])]))
  
  (define (check-this-name-arg arg type)
    (match arg
      [(struct ast:named/arg (_ _ value))
       (copy-struct ast:named/arg arg
         [ast:named/arg-actual (check-this-expression value type)])]))
  
  (define (check-this-expression expr type)
    (match expr
      [(struct ast:expr:self (stx))
       (if type
           (raise-read-error-with-stx
            "Unprotected use of this in a client context"
            stx)
           (raise-read-error-with-stx
            "Use of this keyword found outside of a class or mixin"
            stx))]
      [(struct ast:expr:tuple/select (_ _ arg))
       (copy-struct ast:expr:tuple/select expr
         [ast:expr:tuple/select-arg (check-this-expression arg type)])]
      [(struct ast:expr:var (_ _))
       expr]
      [(struct ast:expr:assign (_ lhs rhs))
       (copy-struct ast:expr:assign expr
         [ast:expr:assign-lhs (check-this-expression lhs type)]
         [ast:expr:assign-rhs (check-this-expression rhs type)])]
      [(struct ast:expr:apply (_ func arg))
       (copy-struct ast:expr:apply expr
         [ast:expr:apply-func (check-this-expression func type)]
         [ast:expr:apply-arg  (check-this-expression arg type)])]
      [(struct ast:expr:literal (_ _ _))
       expr]
      [(struct ast:expr:unary/op (_ _ _ _ arg))
       (copy-struct ast:expr:unary/op expr
         [ast:expr:unary/op-arg (check-this-expression arg type)])]
      [(struct ast:expr:binary/op (_ _ _ _ larg rarg))
       (copy-struct ast:expr:binary/op expr
         [ast:expr:binary/op-left (check-this-expression larg type)]
         [ast:expr:binary/op-right (check-this-expression rarg type)])]
      [(struct ast:expr:function (_ _ _ body))
       (copy-struct ast:expr:function expr
         [ast:expr:function-body (check-this-expression body type)])]
      [(struct ast:expr:if (_ cond then else))
       (copy-struct ast:expr:if expr
         [ast:expr:if-test (check-this-expression cond type)]
         [ast:expr:if-then (check-this-expression then type)]
         [ast:expr:if-else (if else (check-this-expression else type) #f)])]
      [(struct ast:expr:cast (_ obj _))
       (if (ast:expr:self? obj)
           (if type
               expr
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (ast-syntax obj)))
           (copy-struct ast:expr:cast expr
             [ast:expr:cast-object (check-this-expression obj type)]))]
      [(struct ast:expr:isa (_ obj _))
       (if (ast:expr:self? obj)
           (if type
               expr
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (ast-syntax obj)))
           (copy-struct ast:expr:isa expr
             [ast:expr:isa-object (check-this-expression obj type)]))]
      [(struct ast:expr:member (_ 'my _ _ _))
       expr]
      [(struct ast:expr:member (_ obj _ _ _))
       (if (ast:expr:self? obj)
           (if type
               ;; to deal with the fact that mixins can mess up the selftype
               ;; property, we hack by creating casts in this case.
               (copy-struct ast:expr:member expr
                 [ast:expr:member-object (make-ast:expr:cast (ast-syntax obj)
                                                  obj
                                                  type)])
               (raise-read-error-with-stx
                "Use of this keyword found outside of a class or mixin"
                (ast-syntax obj)))               
           (copy-struct ast:expr:member expr
             [ast:expr:member-object (check-this-expression obj type)]))]
      [(struct ast:expr:new (_ _ _ args))
       (copy-struct ast:expr:new expr
         [ast:expr:new-args (map (lambda (a)
                               (check-this-name-arg a type))
                             args)])]
      [(struct ast:expr:while (_ cond body))
       (copy-struct ast:expr:while expr
         [ast:expr:while-test (check-this-expression cond type)]
         [ast:expr:while-body (check-this-expression body type)])]
      [(struct ast:expr:cond (_ clauses else))
       (copy-struct ast:expr:cond expr
         [ast:expr:cond-clauses (map (lambda (c)
                                   (check-this-cond-clause c type))
                                 clauses)]
         [ast:expr:cond-else    (if else (check-this-expression else type) #f)])]
      [(struct ast:expr:return (_ body))
       (copy-struct ast:expr:return expr
         [ast:expr:return-result (check-this-expression body type)])]
      [(struct ast:expr:tuple (_ vals))
       (copy-struct ast:expr:tuple expr
         [ast:expr:tuple-elems (map (lambda (e)
                                 (check-this-expression e type))
                               vals)])]
      [(struct ast:expr:let (_ bindings body))
       (let ([bindings (map (lambda (b)
                              (check-this-binding b type))
                            bindings)]
             [body     (check-this-expression body type)])
         (copy-struct ast:expr:let expr
           [ast:expr:let-bindings bindings]
           [ast:expr:let-body     body]))]
      [(struct ast:expr:sequence (_ effects value))
       (let ([effects (map (lambda (e)
                             (check-this-expression e type))
                           effects)]
             [value   (check-this-expression value type)])
         (copy-struct ast:expr:sequence expr
           [ast:expr:sequence-statements effects]
           [ast:expr:sequence-result   value]))]))
  
  (define (check-this-binding binding type)
    (match binding
      [(struct ast:defn:binding (_ names _ value))
       (copy-struct ast:defn:binding binding
         [ast:defn:binding-init (check-this-expression value type)])]))
  
  (define (check-this-cond-clause clause type)
    (match clause
      [(struct ast:cond/clause (_ pred rhs))
       (copy-struct ast:cond/clause clause
         [ast:cond/clause-test (check-this-expression pred type)]
         [ast:cond/clause-result  (check-this-expression rhs type)])]))  
  
  
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
      [(struct ast:defn:iface (_ _ _ _))
       defn]
      [(struct ast:defn:class (_ _ _ _ _ _ members _))
       (copy-struct ast:defn:class defn
         [ast:defn:class-members (map simplify-member members)])]
      [(struct ast:defn:mixin (_ _ _ _ _ _ _ _ super-new members-before members-after _))
       (copy-struct ast:defn:mixin defn
         [ast:defn:mixin-super-new (simplify-super-new super-new)]
         [ast:defn:mixin-pre-members (map simplify-member members-before)]
         [ast:defn:mixin-post-members (map simplify-member members-after)])]
      [(struct ast:defn:subclass (_ _ _ _))
       defn]
      [(struct ast:defn:function (_ _ _ _ body))
       (copy-struct ast:defn:function defn
         [ast:defn:function-body (simplify-expression body)])]
      [(struct ast:defn:binding (_ _ _ value))
       (copy-struct ast:defn:binding defn
         [ast:defn:binding-init (simplify-expression value)])]))
  
  (define (simplify-member member)
    (match member
      [(struct ast:class/member:field/formal (_ _ _ value))
       (if value
           (copy-struct ast:class/member:field/formal member
             [ast:class/member:field/formal-default (simplify-expression value)])
           member)]
      [(struct ast:class/member:field (_ _ _ value))
       (copy-struct ast:class/member:field member
         [ast:class/member:field-default (simplify-expression value)])]
      [(struct ast:class/member:method (_ _ _ _ body))
       (copy-struct ast:class/member:method member
         [ast:class/member:method-body (simplify-expression body)])]))
  
  (define (simplify-super-new snew)
    (match snew
      [(struct ast:super-new (_ args))
       (copy-struct ast:super-new snew
         [ast:super-new-args (map simplify-name-arg args)])]))
  
  (define (simplify-name-arg arg)
    (match arg
      [(struct ast:named/arg (_ _ value))
       (copy-struct ast:named/arg arg
         [ast:named/arg-actual (simplify-expression value)])]))
  
  (define (simplify-expression expr)
    (match expr
      [(struct ast:expr:self (_))
       expr]
      [(struct ast:expr:tuple/select (_ _ arg))
       (copy-struct ast:expr:tuple/select expr
         [ast:expr:tuple/select-arg (simplify-expression arg)])]
      [(struct ast:expr:var (_ _))
       expr]
      [(struct ast:expr:assign (_ lhs rhs))
       (copy-struct ast:expr:assign expr
         [ast:expr:assign-lhs (simplify-expression lhs)]
         [ast:expr:assign-rhs (simplify-expression rhs)])]
      [(struct ast:expr:apply (_ func arg))
       (copy-struct ast:expr:apply expr
         [ast:expr:apply-func (simplify-expression func)]
         [ast:expr:apply-arg  (simplify-expression arg)])]
      [(struct ast:expr:literal (_ _ _))
       expr]
      [(struct ast:expr:unary/op (_ _ _ _ arg))
       (copy-struct ast:expr:unary/op expr
         [ast:expr:unary/op-arg (simplify-expression arg)])]
      [(struct ast:expr:binary/op (_ _ _ _ larg rarg))
       (copy-struct ast:expr:binary/op expr
         [ast:expr:binary/op-left (simplify-expression larg)]
         [ast:expr:binary/op-right (simplify-expression rarg)])]
      [(struct ast:expr:function (_ _ _ body))
       (copy-struct ast:expr:function expr
         [ast:expr:function-body (simplify-expression body)])]
      [(struct ast:expr:if (_ cond then else))
       (copy-struct ast:expr:if expr
         [ast:expr:if-test (simplify-expression cond)]
         [ast:expr:if-then (simplify-expression then)]
         [ast:expr:if-else (if else (simplify-expression else) #f)])]
      [(struct ast:expr:cast (_ obj _))
       (copy-struct ast:expr:cast expr
         [ast:expr:cast-object (simplify-expression obj)])]
      [(struct ast:expr:isa (_ obj _))
       (copy-struct ast:expr:isa expr
         [ast:expr:isa-object (simplify-expression obj)])]
      [(struct ast:expr:member (_ 'my _ _ _))
       expr]
      [(struct ast:expr:member (_ obj _ _ _))
       (copy-struct ast:expr:member expr
         [ast:expr:member-object (simplify-expression obj)])]
      [(struct ast:expr:new (_ _ _ args))
       (copy-struct ast:expr:new expr
         [ast:expr:new-args (map simplify-name-arg args)])]
      [(struct ast:expr:cond (_ clauses else))
       (copy-struct ast:expr:cond expr
         [ast:expr:cond-clauses (map simplify-cond-clause clauses)]
         [ast:expr:cond-else    (if else (simplify-expression else) #f)])]
      [(struct ast:expr:while (_ cond body))
       (copy-struct ast:expr:while expr
         [ast:expr:while-test (simplify-expression cond)]
         [ast:expr:while-body (simplify-expression body)])]
      [(struct ast:expr:return (_ body))
       (copy-struct ast:expr:return expr
         [ast:expr:return-result (simplify-expression body)])]
      [(struct ast:expr:tuple (_ vals))
       (copy-struct ast:expr:tuple expr
         [ast:expr:tuple-elems (map simplify-expression vals)])]
      [(struct ast:expr:let (stx bindings body))
       (let ([bindings (map simplify-binding bindings)]
             [body     (simplify-expression body)])
         (match body
           [(struct ast:expr:let (_ sub-bindings sub-body))
            (make-ast:expr:let stx (append bindings sub-bindings) sub-body)]
           [_
            (copy-struct ast:expr:let expr
              [ast:expr:let-bindings bindings]
              [ast:expr:let-body     body])]))]
      [(struct ast:expr:sequence (stx effects value))
       (let ([effects (map simplify-expression effects)]
             [value   (simplify-expression value)])
         (match value
           [(struct ast:expr:sequence (_ sub-effects sub-value))
            (make-ast:expr:sequence stx (append effects sub-effects) sub-value)]
           [_
            (copy-struct ast:expr:sequence expr
              [ast:expr:sequence-statements effects]
              [ast:expr:sequence-result   value])]))]))
  
  (define (simplify-binding binding)
    (match binding
      [(struct ast:defn:binding (_ _ _ value))
       (copy-struct ast:defn:binding binding
         [ast:defn:binding-init (simplify-expression value)])]))

  (define (simplify-cond-clause clause)
    (match clause
      [(struct ast:cond/clause (_ pred rhs))
       (copy-struct ast:cond/clause clause
         [ast:cond/clause-test (simplify-expression pred)]
         [ast:cond/clause-result  (simplify-expression rhs)])]))
  )
