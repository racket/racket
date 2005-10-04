(module type-utils mzscheme
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss")
  
  (provide make-null-type make-any-type make-void-type make-error-type make-top-type make-bottom-type
           make-int-type make-string-type make-char-type make-bool-type make-float-type
           make-tuple-type make-func-type make-method-type make-iface-type)
  (define (make-null-type stx)
    (make-honu:type-iface-bot stx))
  (define (make-any-type stx)
    (make-honu:type-iface-top stx))
  ;; if you want non-strict void context method bodies, change this to
  ;; (make-honu:type-top stx)
  ;; if you want to make sure that a value of "void" type is returned, do
  ;; (make-honu:type-tuple stx '())
  (define (make-void-type stx)
    (make-honu:type-tuple stx (list)))
  (define (make-error-type stx)
    (make-honu:type-bot stx))
  (define (make-top-type stx)
    (make-honu:type-top stx))
  (define (make-bottom-type stx)
    (make-honu:type-bot stx))
  (define (make-tuple-type stx args)
    (if (and (not (null? args))
             (null? (cdr args)))
        (car args) ;; just return the type if there's only one.
        (make-honu:type-tuple stx args)))
  (define (make-func-type stx arg ret)
    (make-honu:type-func stx arg ret))
  (define (make-method-type stx disp arg ret)
    (make-honu:type-disp stx disp arg ret))
  (define (make-iface-type stx name)
    (make-honu:type-iface stx name))
  (define (make-int-type stx)
    (make-honu:type-prim stx 'int))
  (define (make-string-type stx)
    (make-honu:type-prim stx 'string))
  (define (make-char-type stx)
    (make-honu:type-prim stx 'char))
  (define (make-bool-type stx)
    (make-honu:type-prim stx 'bool))
  (define (make-float-type stx)
    (make-honu:type-prim stx 'float))
  
  (provide printable-type)
  (define (printable-type t)
    (match t
      [(struct honu:type-top (_))
       "(no info)"]
      [(struct honu:type-bot (_))
       "'a"]
      [(struct honu:type-prim (_ name))
       (symbol->string name)]
      [(struct honu:type-tuple (_ args))
       (if (null? args)
           "void"
           (string-append "<"
                          (fold (lambda (t i)
                                  (string-append i ", " (printable-type t)))
                                (printable-type (car args))
                                (cdr args))
                          ">"))]
      [(struct honu:type-select (_ slot type))
       (format "tuple of size >= ~a where the type in slot ~a is ~a"
               slot slot (printable-type type))]
      [(struct honu:type-func (_ arg ret))
       (if (honu:type-func? arg)
           (string-append "<" (printable-type arg) "> -> " (printable-type ret))
           (string-append (printable-type arg) " -> " (printable-type ret)))]
      [(struct honu:type-disp (_ disp arg ret))
       (string-append "[" (printable-type disp) "] "
                      (printable-type arg) " -> " (printable-type ret))]
      [(struct honu:type-iface (_ name))
       (symbol->string (syntax-e name))]
      [(struct honu:type-iface-top (_))
       "Any"]
      [(struct honu:type-iface-bot (_))
       "null"]))
  
  (provide type-valid?)
  (define (type-valid? t)
    (match t
      [(struct honu:type-iface-top (_)) #t]
      [(struct honu:type-prim (stx name))
       (case name
         [(int float char string bool) #t]
         [else (raise-read-error-with-stx
                (format "Unexpected primitive type ~a" name)
                stx)])]
      [(struct honu:type-iface (stx name))
       (let ([tentry (get-tenv-entry name)])
         (and tentry (tenv:type? tentry)))]
      [(struct honu:type-tuple (_ args))
       (andmap (lambda (t)
                 (type-valid? t))
               args)]
      [(struct honu:type-func (_ arg ret))
       (and (type-valid? arg)
            (type-valid? ret))]
      [(struct honu:type-disp (_ disp arg ret))
       (and (type-valid? disp)
            (type-valid? arg)
            (type-valid? ret))]))
  
  (provide type-equal?)
  (define (type-equal? t1 t2)
    (cond
      ;; first all the easy ones
      [(and (honu:type-top? t1)
            (honu:type-top? t2))
       #t]
      [(and (honu:type-bot? t1)
            (honu:type-bot? t2))
       #t]
      [(and (honu:type-iface-top? t1)
            (honu:type-iface-top? t2))
       #t]
      [(and (honu:type-iface-bot? t1)
            (honu:type-iface-bot? t2))
       #t]
      ;; primitive types are equal if their names are equal.
      [(and (honu:type-prim? t1)
            (honu:type-prim? t2))
       (eqv? (honu:type-prim-name t1)
             (honu:type-prim-name t2))]
      ;; same for ifaces
      [(and (honu:type-iface? t1)
            (honu:type-iface? t2))
       (tenv-key=? (honu:type-iface-name t1)
                   (honu:type-iface-name t2))]
      ;; function, dispatch types are equal if their component types are.
      [(and (honu:type-func? t1)
            (honu:type-func? t2))
       (and (type-equal? (honu:type-func-arg t1) (honu:type-func-arg t2))
            (type-equal? (honu:type-func-ret t1) (honu:type-func-ret t2)))]
      [(and (honu:type-disp? t1)
            (honu:type-disp? t2))
       (and (type-equal? (honu:type-disp-disp t1) (honu:type-disp-disp t2))
            (type-equal? (honu:type-disp-arg  t1) (honu:type-disp-arg  t2))
            (type-equal? (honu:type-disp-ret  t1) (honu:type-disp-ret  t2)))]
      ;; tuple types are equal if they have the same number of components and
      ;; their components are pairwise equal
      [(and (honu:type-tuple? t1)
            (honu:type-tuple? t2))
       (let ([t1-args (honu:type-tuple-args t1)]
             [t2-args (honu:type-tuple-args t2)])
         (and (= (length t1-args) (length t2-args))
              (andmap (lambda (t1 t2)
                        (type-equal? t1 t2))
                      t1-args t2-args)))]
      ;; for select types, they must be the same type on the same slot
      ;; (should we even get here?)
      [(and (honu:type-select? t1)
            (honu:type-select? t2))
       (and (= (honu:type-select-slot t1) (honu:type-select-slot t2))
            (type-equal? (honu:type-select-type t1) (honu:type-select-type t2)))]
      [else #f]))
  
  ;; assumes either Any or some type identifier
  (define (get-type-name t)
    (cond 
      [(honu:type-iface? t) 
       (honu:type-iface-name t)]
      [(honu:type-iface-top? t)
       #'Any]))
  
  ;; is t1 a _direct_ subtype of t2?
  (define (Subtype_P t1 t2)
    (let ([type-entry (get-type-entry t1)])
      (match type-entry
        [(struct tenv:type (_ supers _ _))
         (let ([super-names (map get-type-name supers)])
           (s:member (get-type-name t2) super-names tenv-key=?))])))
  
  ;; is t1 a (ref-trans-closed) subtype of t2?
  (provide <:_P)
  (define (<:_P t1 t2)
    (cond
      ;; if t1 = t2, t1 <:_P t2
      [(type-equal? t1 t2)
       #t]
      ;; if t1 is the bottom of the type lattice, then it trivially holds
      [(honu:type-bot? t1)
       #t]
      ;; if t2 is the top of the type lattice, then it trivially holds
      [(honu:type-top? t2)
       #t]
      ;; if t1 =/= t2 and they're both primitives, then they cannot be equal.
      [(and (honu:type-prim? t1)
            (honu:type-prim? t2))
       #f]
      ;; for function types...
      [(and (honu:type-func? t1)
            (honu:type-func? t2))
       ;; the arg is contravariant and the ret is covariant
       (and (<:_P (honu:type-func-arg t2) (honu:type-func-arg t1))
            (<:_P (honu:type-func-ret t1) (honu:type-func-ret t2)))]
      ;; for dispatch types...
      [(and (honu:type-disp? t1)
            (honu:type-disp? t2))
       ;; dispatch args must be co-, regular args contra-, and ret co-
       (and (<:_P (honu:type-disp-disp t1) (honu:type-disp-disp t2))
            (<:_P (honu:type-disp-arg  t2) (honu:type-disp-arg  t1))
            (<:_P (honu:type-disp-ret  t1) (honu:type-disp-ret  t2)))]
      ;; for tuple types...
      [(and (honu:type-tuple? t1)
            (honu:type-tuple? t2))
       (let ([t1-args (honu:type-tuple-args t1)]
             [t2-args (honu:type-tuple-args t2)])
         ;; the lengths must be equal...
         (and (= (length t1-args) (length t2-args))
              ;; and each component must be a subtype (covariantly)
              (andmap (lambda (t1 t2)
                        (<:_P t1 t2))
                      t1-args t2-args)))]
      ;; for a select statement (s, t), we have that a tuple type (t_1 ... t_n) is <:_P t if
      ;; if t_s <:_P t.
      [(and (honu:type-tuple?  t1)
            (honu:type-select? t2))
       (let ([t2-slot (honu:type-select-slot t2)]
             [t1-args (honu:type-tuple-args t1)])
         (and (<= t2-slot (length t1-args))
              ;; we have to subtract one from t2-slot because list-ref is zero-based
              (<:_P (list-ref t1-args (- t2-slot 1)) (honu:type-select-type t2))))]
      ;; not sure if this is necessary.  Hmm.
      [(and (honu:type-select? t1)
            (honu:type-select? t2))
       (and (= (honu:type-select-slot t1) (honu:type-select-slot t2))
            (<:_P (honu:type-select-type t1) (honu:type-select-type t2)))]
      ;; the bottom of the iface lattice is <:_P either the iface-top or
      ;; any iface
      [(and (honu:type-iface-bot? t1)
            (or (honu:type-iface? t2)
                (honu:type-iface-top? t2)))
       #t]
      ;; any iface type is <:_P the iface-top (iface-bot already caught above)
      [(and (honu:type-iface? t1)
            (honu:type-iface-top? t2))
       #t]
      ;; if two (non-equal) iface types...
      [(and (honu:type-iface? t1)
            (honu:type-iface? t2))
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
  
  (provide iface-name)
  (define (iface-name type)
    (match type
      [(struct honu:type-iface-top (_))  #'Any]
      [(struct honu:type-iface (_ name)) name]))
    
  (provide raise-honu-type-error)
  (define (raise-honu-type-error stx expected received)
    (raise-read-error-with-stx
     (format "Expected type ~a, got type ~a"
             (printable-type expected)
             (printable-type received))
     stx))
  )
