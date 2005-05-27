(module honu-translate-expression mzscheme

  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "plt-match.ss"))

  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "honu-translate-utils.ss")
  (require "../../read-error-with-stx.ss")
  
  (define (get-builtin-translation name)
;    (case (printable-key name)
;      [(println) (at name '(lambda (s) (display s) (newline)))]
;      [(error) (at name '(lambda (s) (error s)))]))
    ;; since we can change the context of identifiers, just make
    ;; sure that the appropriate things are bound in honu-compile-context.
    (at-ctxt name))
  
  (define (field-in-defn? field defn)
    (or (find (lambda (n)
                (tenv-key=? n field))
              (cond
                [(honu-class? defn) (honu-class-init-names defn)]
                [(honu-mixin? defn) (honu-mixin-init-names defn)]))
        (find (match-lambda
                [(struct honu-field (_ name _ _))
                 (tenv-key=? name field)]
                [(struct honu-init-field (_ name _ _))
                 (tenv-key=? name field)]
                [_ #f])
              (cond
                [(honu-class? defn) (honu-class-defns defn)]
                [(honu-mixin? defn) (append (honu-mixin-defns-before defn)
                                            (honu-mixin-defns-after defn))]))))

  (provide/contract [honu-translate-expression
                     (tenv?
                      (union false/c
                             honu-defn?)
                      honu-exp?
                      . -> .
;                      (syntax/c any/c))])
                      any)])
  (define (honu-translate-expression tenv defn exp)
    (match exp
      [(struct honu-null (stx))
       (at stx 'null)]
      
      [(struct honu-int (stx val))
       (at stx val)]
      [(struct honu-float (stx val))
       (at stx val)]
      [(struct honu-char (stx val))
       (at stx val)]
      [(struct honu-str (stx val))
       (at stx val)]
      [(struct honu-bool (stx val))
       (at stx val)]
      
      [(struct honu-var (stx name builtin?))
       (if builtin?
           (get-builtin-translation name)
           name)]
      [(struct honu-this (stx))
       (at stx 'this)]
      
      [(struct honu-uprim (stx op op-stx op-type body))
       (let ((body-exp (honu-translate-expression tenv defn body)))
         (case op
           [(not)
            (at stx `(,(at op-stx 'not) ,body-exp))]
           [(minus)
            (at stx `(,(at op-stx '-) ,body-exp))]))]
      [(struct honu-prim (stx op op-stx op-type left right))
       (let ((left-exp (honu-translate-expression tenv defn left))
             (right-exp (honu-translate-expression tenv defn right)))
         (case op
           [(plus)  
            (if (eqv? 'str (honu-prim-type-name op-type))
                (at stx `(,(at op-stx 'string-append) ,left-exp ,right-exp))
                (at stx `(,(at op-stx '+) ,left-exp ,right-exp)))]
           [(minus)
            (at stx `(,(at op-stx '-) ,left-exp ,right-exp))]
           [(times)
            (at stx `(,(at op-stx '*) ,left-exp ,right-exp))]
           [(div)   
            (if (eqv? 'float (honu-prim-type-name op-type))
                (at stx `(,(at op-stx '/) ,left-exp ,right-exp))
                (at stx `(,(at op-stx 'quotient) ,left-exp ,right-exp)))]
           [(mod)
            (at stx `(,(at op-stx 'modulo) ,left-exp ,right-exp))]
           [(lt)
            (case (honu-prim-type-name op-type)
              [(int float) 
               (at stx `(,(at op-stx '<) ,left-exp ,right-exp))]
              [(string)
               (at stx `(,(at op-stx 'string<?) ,left-exp ,right-exp))]
              [(char)
               (at stx `(,(at op-stx 'char<?) ,left-exp ,right-exp))])]
           [(le)
            (case (honu-prim-type-name op-type)
              [(int float) 
               (at stx `(,(at op-stx '<=) ,left-exp ,right-exp))]
              [(string)
               (at stx `(,(at op-stx 'string<=?) ,left-exp ,right-exp))]
              [(char)
               (at stx `(,(at op-stx 'char<=?) ,left-exp ,right-exp))])]
           [(gt)
            (case (honu-prim-type-name op-type)
              [(int float) 
               (at stx `(,(at op-stx '>) ,left-exp ,right-exp))]
              [(string)
               (at stx `(,(at op-stx 'string>?) ,left-exp ,right-exp))]
              [(char)
               (at stx `(,(at op-stx 'char>?) ,left-exp ,right-exp))])]
           [(ge)
            (case (honu-prim-type-name op-type)
              [(int float) 
               (at stx `(,(at op-stx '>=) ,left-exp ,right-exp))]
              [(string)
               (at stx `(,(at op-stx 'string>=?) ,left-exp ,right-exp))]
              [(char)
               (at stx `(,(at op-stx 'char>=?) ,left-exp ,right-exp))])]
           [(and)   
            (at stx `(,(at op-stx 'and) ,left-exp ,right-exp))]
           [(or)
            (at stx `(,(at op-stx 'or) ,left-exp ,right-exp))]
           [(clseq)
            (at stx `(,(at op-stx 'equal?) ,left-exp ,right-exp))]
           [(equal) 
            (if (and (honu-prim-type? op-type)
                     (eqv? 'str (honu-prim-type-name op-type)))
                (at stx `(,(at op-stx 'string=?) ,left-exp ,right-exp))
                (at stx `(,(at op-stx 'eqv?) ,left-exp ,right-exp)))]
           [(neq) 
            (if (and (honu-prim-type? op-type)
                     (eqv? 'str (honu-prim-type-name op-type)))
                (at stx `(,(at op-stx 'not) (,(at op-stx 'string=?) ,left-exp ,right-exp)))
                (at stx `(,(at op-stx 'not) (,(at op-stx 'eqv?) ,left-exp ,right-exp))))]))]
       
      [(struct honu-lambda (stx arg-names _ body))
       (at stx `(lambda ,(map (lambda (n) (at-ctxt n)) arg-names)
                  ,(honu-translate-expression tenv defn body)))]
      
      [(struct honu-assn (stx name rhs))
       (at stx `(set! ,(at-ctxt name)
                      ,(honu-translate-expression tenv defn rhs)))]
      [(struct honu-call (stx name args builtin?))
       (let ([f (if builtin?
                    (get-builtin-translation name)
                    (at-ctxt name))])
         (at stx (cons f (map (lambda (e)
                                (honu-translate-expression tenv defn e))
                              args))))]
      
      [(struct honu-facc (stx obj elab field))
       (if (eqv? obj 'my)
           (if (field-in-defn? field defn)
               (at stx field)
               (at stx `(super ,(honu-translate-dynamic-field-getter tenv 
                                                                     field
                                                                     (honu-mixin-arg-type defn)))))
           (at stx `(send ,(honu-translate-expression tenv defn obj)
                          ,(honu-translate-dynamic-field-getter tenv field elab))))]
      [(struct honu-fassn (stx obj elab field rhs))
       (if (eqv? (honu-fassn-obj exp) 'my)
           (if (field-in-defn? field defn)
               (at stx `(set! ,(at-ctxt field)
                              ,(honu-translate-expression tenv defn rhs)))
               (at stx `(super ,(honu-translate-dynamic-field-setter tenv
                                                                     field
                                                                     (honu-mixin-arg-type defn))
                               ,(honu-translate-expression tenv defn rhs))))
           (at stx `(send ,(honu-translate-expression tenv defn obj)
                          ,(honu-translate-dynamic-field-setter tenv field elab)
                          ,(honu-translate-expression tenv defn rhs))))]
      [(struct honu-mcall (stx obj elab method args))
       (if (eqv? obj 'my)
           (if (find (match-lambda
                       [(struct honu-method (_ name _ _ _ _))
                        (tenv-key=? name method)]
                       [_ #f])
                     (cond
                       [(honu-class? defn) (honu-class-defns defn)]
                       [(honu-mixin? defn) (append (honu-mixin-defns-before defn)
                                                   (honu-mixin-defns-after defn))]))
               (at stx `(,(at-ctxt method)
                          ,@(map (lambda (e)
                                   (honu-translate-expression tenv defn e))
                                 args)))
               (at stx `(super ,(honu-translate-dynamic-method-name tenv
                                                                    method
                                                                    (honu-mixin-arg-type defn))
                               ,@(map (lambda (e)
                                        (honu-translate-expression tenv defn e))
                                      args))))
           (at stx `(send ,(honu-translate-expression tenv defn obj)
                          ,(honu-translate-dynamic-method-name tenv method elab)
                          ,@(map (lambda (e)
                                   (honu-translate-expression tenv defn e))
                                 args))))]
      
      [(struct honu-cast (stx obj type))
       (let ([cast-type (honu-translate-type-name type)])
         (if cast-type
             (at stx `(let ((cast-obj ,(honu-translate-expression tenv defn obj)))
                        (if (is-a? cast-obj ,cast-type)
                            cast-obj
                            (error "Cast failed!"))))
             (honu-translate-expression tenv defn obj)))]
      [(struct honu-isa (stx obj type))
       (let ([isa-type (honu-translate-type-name type)])
         (if isa-type
             (at stx `(is-a? ,(honu-translate-expression tenv defn obj)
                             ,isa-type))
             (honu-translate-expression tenv defn (make-honu-bool stx #t))))]
      
      [(struct honu-new (stx class type arg-names arg-vals))
       (at stx `(new ,(honu-translate-class-name class)
                     ,@(map (lambda (a b) (list a (honu-translate-expression tenv defn b)))
                            arg-names
                            arg-vals)))]
      
      [(struct honu-if (stx cond true false))
       (at stx `(if ,(honu-translate-expression tenv defn cond)
                    ,(honu-translate-expression tenv defn true)
                    ,(honu-translate-expression tenv defn false)))]
      
      [(struct honu-while (stx cond body))
       (at stx `(let loop ()
                  (if ,(honu-translate-expression tenv defn cond)
                      (begin ,(honu-translate-expression tenv defn body)
                             (loop)))))]
      
      [(struct honu-return (stx body))
       (at stx (if body
                   (honu-translate-expression tenv defn body)
                   `(void)))]
      [(struct honu-block (stx binds exps))
       (at stx `(let* ,(map (lambda (b)
                              (honu-translate-binding tenv defn b #f))
                            binds)
                  ,@(map (lambda (e)
                           (honu-translate-expression tenv defn e))
                         exps)))]))
  
  (provide/contract [honu-translate-binding
                     (tenv?
                      (union false/c
                             honu-defn?)
                      honu-binding?
                      (union false/c
                             (lambda (b) (eq? #t b)))
                      . -> .
;                      (syntax/c any/c))])
                      any)])
  (define (honu-translate-binding tenv defn bnd top-level?)
    (match bnd
      [(struct honu-binding (stx name _ rhs))
       (if top-level?
           (at stx `(define ,name 
                      ,(honu-translate-expression tenv defn rhs)))
           (at stx `[,(at-ctxt name) ,(honu-translate-expression tenv defn rhs)]))]))
  )
  
