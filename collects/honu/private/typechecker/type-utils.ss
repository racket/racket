(module type-utils mzscheme
  (require (prefix srfi1: (lib "list.ss" "srfi" "1"))
           (lib "contract.ss")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss")
  
  (provide make-null-type make-any-type make-void-type make-error-type make-top-type make-bottom-type
           make-int-type make-string-type make-char-type make-bool-type make-float-type
           make-tuple-type make-func-type make-method-type make-iface-type)
  (define (make-null-type stx)
    (make-ast:type:object:null stx))
  (define (make-any-type stx)
    (make-ast:type:object:any stx))
  ;; if you want non-strict void context method bodies, change this to
  ;; (make-ast:type:top stx)
  ;; if you want to make sure that a value of "void" type is returned, do
  ;; (make-ast:type:tuple stx '())
  (define (make-void-type stx)
    (make-ast:type:tuple stx (list)))
  (define (make-error-type stx)
    (make-ast:type:bot stx))
  (define (make-top-type stx)
    (make-ast:type:top stx))
  (define (make-bottom-type stx)
    (make-ast:type:bot stx))
  (define (make-tuple-type stx args)
    (if (and (not (null? args))
             (null? (cdr args)))
        (car args) ;; just return the type if there's only one.
        (make-ast:type:tuple stx args)))
  (define (make-func-type stx arg ret)
    (make-ast:type:function stx arg ret))
  (define (make-method-type stx disp arg ret)
    (make-ast:type:method stx disp arg ret))
  (define (make-iface-type stx name)
    (make-ast:type:object:iface stx name))
  (define (make-int-type stx)
    (make-ast:type:primitive stx 'int))
  (define (make-string-type stx)
    (make-ast:type:primitive stx 'string))
  (define (make-char-type stx)
    (make-ast:type:primitive stx 'char))
  (define (make-bool-type stx)
    (make-ast:type:primitive stx 'bool))
  (define (make-float-type stx)
    (make-ast:type:primitive stx 'float))
  
  (provide printable-type)
  (define (printable-type t)
    (match t
      [(struct ast:type:top (_))
       "(no info)"]
      [(struct ast:type:bot (_))
       "'a"]
      [(struct ast:type:primitive (_ name))
       (symbol->string name)]
      [(struct ast:type:tuple (_ args))
       (if (null? args)
           "void"
           (string-append "<"
                          (srfi1:fold (lambda (t i)
                                  (string-append i ", " (printable-type t)))
                                (printable-type (car args))
                                (cdr args))
                          ">"))]
      [(struct ast:type:partial/tuple (_ slot type))
       (format "tuple of size >= ~a where the type in slot ~a is ~a"
               slot slot (printable-type type))]
      [(struct ast:type:function (_ arg ret))
       (if (ast:type:function? arg)
           (string-append "<" (printable-type arg) "> -> " (printable-type ret))
           (string-append (printable-type arg) " -> " (printable-type ret)))]
      [(struct ast:type:method (_ disp arg ret))
       (string-append "[" (printable-type disp) "] "
                      (printable-type arg) " -> " (printable-type ret))]
      [(struct ast:type:object:iface (_ name))
       (symbol->string (syntax-e name))]
      [(struct ast:type:object:any (_))
       "Any"]
      [(struct ast:type:object:null (_))
       "null"]))
  
  (provide type-valid?)
  (define (type-valid? t)
    (match t
      [(struct ast:type:object:any (_)) #t]
      [(struct ast:type:primitive (stx name))
       (case name
         [(int float char string bool) #t]
         [else (raise-read-error-with-stx
                (format "Unexpected primitive type ~a" name)
                stx)])]
      [(struct ast:type:object:iface (stx name))
       (let ([tentry (get-tenv-entry name)])
         (and tentry (tenv:type? tentry)))]
      [(struct ast:type:tuple (_ args))
       (andmap (lambda (t)
                 (type-valid? t))
               args)]
      [(struct ast:type:function (_ arg ret))
       (and (type-valid? arg)
            (type-valid? ret))]
      [(struct ast:type:method (_ disp arg ret))
       (and (type-valid? disp)
            (type-valid? arg)
            (type-valid? ret))]))
  
  (provide type-equal?)
  (define (type-equal? t1 t2)
    (cond
      ;; first all the easy ones
      [(and (ast:type:top? t1)
            (ast:type:top? t2))
       #t]
      [(and (ast:type:bot? t1)
            (ast:type:bot? t2))
       #t]
      [(and (ast:type:object:any? t1)
            (ast:type:object:any? t2))
       #t]
      [(and (ast:type:object:null? t1)
            (ast:type:object:null? t2))
       #t]
      ;; primitive types are equal if their names are equal.
      [(and (ast:type:primitive? t1)
            (ast:type:primitive? t2))
       (eqv? (ast:type:primitive-name t1)
             (ast:type:primitive-name t2))]
      ;; same for ifaces
      [(and (ast:type:object:iface? t1)
            (ast:type:object:iface? t2))
       (tenv-key=? (ast:type:object:iface-name t1)
                   (ast:type:object:iface-name t2))]
      ;; function, dispatch types are equal if their component types are.
      [(and (ast:type:function? t1)
            (ast:type:function? t2))
       (and (type-equal? (ast:type:function-input t1) (ast:type:function-input t2))
            (type-equal? (ast:type:function-output t1) (ast:type:function-output t2)))]
      [(and (ast:type:method? t1)
            (ast:type:method? t2))
       (and (type-equal? (ast:type:method-receiver t1) (ast:type:method-receiver t2))
            (type-equal? (ast:type:method-input  t1) (ast:type:method-input  t2))
            (type-equal? (ast:type:method-output  t1) (ast:type:method-output  t2)))]
      ;; tuple types are equal if they have the same number of components and
      ;; their components are pairwise equal
      [(and (ast:type:tuple? t1)
            (ast:type:tuple? t2))
       (let ([t1-args (ast:type:tuple-elems t1)]
             [t2-args (ast:type:tuple-elems t2)])
         (and (= (length t1-args) (length t2-args))
              (andmap (lambda (t1 t2)
                        (type-equal? t1 t2))
                      t1-args t2-args)))]
      ;; for select types, they must be the same type on the same slot
      ;; (should we even get here?)
      [(and (ast:type:partial/tuple? t1)
            (ast:type:partial/tuple? t2))
       (and (= (ast:type:partial/tuple-position t1) (ast:type:partial/tuple-position t2))
            (type-equal? (ast:type:partial/tuple-elem t1) (ast:type:partial/tuple-elem t2)))]
      [else #f]))
  
  ;; assumes either Any or some type identifier
  (define (get-type-name t)
    (cond 
      [(ast:type:object:iface? t) 
       (ast:type:object:iface-name t)]
      [(ast:type:object:any? t)
       #'Any]))
  
  ;; is t1 a _direct_ subtype of t2?
  (define (Subtype_P t1 t2)
    (let ([type-entry (get-type-entry t1)])
      (match type-entry
        [(struct tenv:type (_ supers _ _))
         (let ([super-names (map get-type-name supers)])
           (srfi1:s:member (get-type-name t2) super-names tenv-key=?))])))
  
  ;; is t1 a (ref-trans-closed) subtype of t2?
  (provide <:_P)
  (define (<:_P t1 t2)
    (cond
      ;; if t1 = t2, t1 <:_P t2
      [(type-equal? t1 t2)
       #t]
      ;; if t1 is the bottom of the type lattice, then it trivially holds
      [(ast:type:bot? t1)
       #t]
      ;; if t2 is the top of the type lattice, then it trivially holds
      [(ast:type:top? t2)
       #t]
      ;; if t1 =/= t2 and they're both primitives, then they cannot be equal.
      [(and (ast:type:primitive? t1)
            (ast:type:primitive? t2))
       #f]
      ;; for function types...
      [(and (ast:type:function? t1)
            (ast:type:function? t2))
       ;; the arg is contravariant and the ret is covariant
       (and (<:_P (ast:type:function-input t2) (ast:type:function-input t1))
            (<:_P (ast:type:function-output t1) (ast:type:function-output t2)))]
      ;; for dispatch types...
      [(and (ast:type:method? t1)
            (ast:type:method? t2))
       ;; dispatch args must be co-, regular args contra-, and ret co-
       (and (<:_P (ast:type:method-receiver t1) (ast:type:method-receiver t2))
            (<:_P (ast:type:method-input  t2) (ast:type:method-input  t1))
            (<:_P (ast:type:method-output  t1) (ast:type:method-output  t2)))]
      ;; for tuple types...
      [(and (ast:type:tuple? t1)
            (ast:type:tuple? t2))
       (let ([t1-args (ast:type:tuple-elems t1)]
             [t2-args (ast:type:tuple-elems t2)])
         ;; the lengths must be equal...
         (and (= (length t1-args) (length t2-args))
              ;; and each component must be a subtype (covariantly)
              (andmap (lambda (t1 t2)
                        (<:_P t1 t2))
                      t1-args t2-args)))]
      ;; for a select statement (s, t), we have that a tuple type (t_1 ... t_n) is <:_P t if
      ;; if t_s <:_P t.
      [(and (ast:type:tuple?  t1)
            (ast:type:partial/tuple? t2))
       (let ([t2-slot (ast:type:partial/tuple-position t2)]
             [t1-args (ast:type:tuple-elems t1)])
         (and (<= t2-slot (length t1-args))
              ;; we have to subtract one from t2-slot because list-ref is zero-based
              (<:_P (list-ref t1-args (- t2-slot 1)) (ast:type:partial/tuple-elem t2))))]
      ;; not sure if this is necessary.  Hmm.
      [(and (ast:type:partial/tuple? t1)
            (ast:type:partial/tuple? t2))
       (and (= (ast:type:partial/tuple-position t1) (ast:type:partial/tuple-position t2))
            (<:_P (ast:type:partial/tuple-elem t1) (ast:type:partial/tuple-elem t2)))]
      ;; the bottom of the iface lattice is <:_P either the iface-top or
      ;; any iface
      [(and (ast:type:object:null? t1)
            (or (ast:type:object:iface? t2)
                (ast:type:object:any? t2)))
       #t]
      ;; any iface type is <:_P the iface-top (iface-bot already caught above)
      [(and (ast:type:object:iface? t1)
            (ast:type:object:any? t2))
       #t]
      ;; if two (non-equal) iface types...
      [(and (ast:type:object:iface? t1)
            (ast:type:object:iface? t2))
       (if (Subtype_P t1 t2)
           ;; return true if it's a direct subtype relation
           #t
           (let ([type-entry (get-type-entry t1)])
             ;; if any of the direct supertypes of t1 is a subtype of t2,
             ;; then t1 is also
             (ormap (lambda (t)
                      (<:_P t t2))
                    (tenv:type-supers type-entry))))]
      [else #f]))

  (provide/contract [type-member-names (ast:type? . -> . (listof identifier?))])
  (define (type-member-names type)
    (let* ([entry (get-type-entry type)])
      (map tenv:member-name
           (append (tenv:type-members entry)
                   (tenv:type-inherited entry)))))
  
  (provide iface-name)
  (define (iface-name type)
    (match type
      [(struct ast:type:object:any (_))  #'Any]
      [(struct ast:type:object:iface (_ name)) name]))
    
  (provide raise-honu-type-error)
  (define (raise-honu-type-error stx expected received)
    (raise-read-error-with-stx
     (format "Expected type ~a, got type ~a"
             (printable-type expected)
             (printable-type received))
     stx))
  )
