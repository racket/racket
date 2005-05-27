(module honu-typecheck-exp mzscheme

  (require (lib "struct.ss")
           (lib "contract.ss")
           (lib "plt-match.ss")
           (all-except (lib "list.ss" "srfi" "1") any))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "../../read-error-with-stx.ss")

  ;; expects a symbol syntax, returns a type for that builtin
  (define (get-builtin-type stx)
    (case (printable-key stx)
      [(printStr)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-void-type stx)
        stx)]
      [(printLine)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-void-type stx)
        stx)]
      [(error)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-error-type stx)
        stx)]
      [(readChar)
       (honu-func-type-from-exp
        (list)
        (honu-char-type stx)
        stx)]
      [(readLine)
       (honu-func-type-from-exp
        (list)
        (honu-str-type stx)
        stx)]
      [(strToInt)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-int-type stx)
        stx)]
      [(strToFloat)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-float-type stx)
        stx)]
      [(intToStr)
       (honu-func-type-from-exp
        (list (honu-int-type stx))
        (honu-str-type stx)
        stx)]
      [(floatToStr)
       (honu-func-type-from-exp
        (list (honu-float-type stx))
        (honu-str-type stx)
        stx)]
      [(charToStr)
       (honu-func-type-from-exp
        (list (honu-char-type stx))
        (honu-str-type stx)
        stx)]
      [(strLen)
       (honu-func-type-from-exp
        (list (honu-str-type stx))
        (honu-int-type stx)
        stx)]
      [(substr)
       (honu-func-type-from-exp
        (list (honu-str-type stx)
              (honu-int-type stx)
              (honu-int-type stx))
        (honu-str-type stx)
        stx)]
      [(charAt)
       (honu-func-type-from-exp
        (list (honu-str-type stx)
              (honu-int-type stx))
        (honu-char-type stx)
        stx)]
      [else #f]))
  
  (define (check-prim-type exp typ type-func)
    (let* ([stx (honu-ast-src-stx exp)]
           [new-typ (type-func stx)])
      (if (and typ (not (honu-type-equal? typ new-typ)))
          (raise-type-error-with-stx typ new-typ stx)
          (values exp new-typ))))
  
  ;; honu-typecheck-exp : HPgm * Env * CEnv -> HExp * Typ -> HExp * Typ
  ;;
  ;; honu-typecheck-exp typechecks a honu expression given the honu
  ;; program in which it appears along with the current local
  ;; environment and class environment.  Now it also takes an expected
  ;; type (if one is known), so we can do some minor type inference in
  ;; the case of new statements, plus give better error messages.
  ;;
  ;; Note that we curry the expression out to the right -- we do this
  ;; because for most expressions, nothing will change except for
  ;; running the typechecker recursively on subexpressions.
  ;;
  ;; We could likewise curry the program out to the left similarly
  ;; since the program will never change for a given program (haha),
  ;; but there would be better ways of handling that in either a
  ;; functional and imperative style such as either having a
  ;; global/parameterized "current-program" variable or by currying
  ;; out to the left and then writing one function that given a
  ;; program, applies the curried functions to it once, returning the
  ;; partially applied functions.  I should get things working before
  ;; getting that silly though.
;  (provide honu-typecheck-exp)
  (provide/contract [honu-typecheck-exp
                     (tenv?
                      any/c
                      any/c
                      . -> .
                      ((honu-exp? (union false/c honu-type?))
                       . ->* .
                       (honu-exp? honu-type?)))]) 
  (define (honu-typecheck-exp tenv env cenv)
    (define (f exp typ)
      (match exp
       ;;            P |- t
       ;; ----------------------------
       ;; P, G, D |- null |=> null : t
       ;;
       ;; Since there's no easy way to do the above in this style of
       ;; typechecker, we'll create a "null" type that for every
       ;; type t such that P |- t, null <: t.
       [(struct honu-null (stx))
        (if typ
            (if (<:_P tenv (honu-null-type stx) typ)
                (values exp typ)
                (raise-read-error-with-stx
                 "Attempt to use null in a non-interface type context."
                 stx))
            (values exp (honu-null-type stx)))]
       ;; P, G, D |- n |=> n : int
       [(struct honu-int (stx n))
        (check-prim-type exp typ honu-int-type)]
       ;; P, G, D |- f |=> f : float
       [(struct honu-float (stx f))
        (check-prim-type exp typ honu-float-type)]
       ;; P, G, D |- b |=> b : bool
       [(struct honu-bool (stx b))
        (check-prim-type exp typ honu-bool-type)]
       ;; P, G, D |- s |=> s : str
       [(struct honu-str (stx s))
        (check-prim-type exp typ honu-str-type)]
       ;; P, G, D |- c |=> c : char
       [(struct honu-char (stx c))
        (check-prim-type exp typ honu-char-type)]
       [(struct honu-uprim (stx op op-stx op-type body))
        (case op
          [(minus)
           (let-values (((e1 t1) (f body #f)))
             (cond
               [(honu-type-equal? t1 (honu-int-type (honu-uprim-body exp)))
                (values (copy-struct honu-uprim exp
                          (honu-uprim-op-type t1)
                          (honu-uprim-body  e1))
                        (honu-int-type exp))]
               [(honu-type-equal? t1 (honu-float-type (honu-uprim-body exp)))
                (values (copy-struct honu-uprim exp
                          (honu-uprim-op-type t1)
                          (honu-uprim-body  e1))
                        (honu-float-type exp))]
             [else
              (raise-read-error-with-stx
               "Unary minus takes an integer or floating point argument."
               (honu-ast-src-stx (honu-uprim-body exp)))]))]
          [(not)
           (let-values (((e1 t1) (f body (honu-bool-type body))))
             (values (copy-struct honu-uprim exp
                       (honu-uprim-op-type t1)
                       (honu-uprim-body  e1))
                     (honu-bool-type exp)))]
          [else (raise-read-error-with-stx
                 "Unknown unary primitive operation."
                 (honu-uprim-op-stx exp))])]
       [(struct honu-prim (stx op op-stx op-type left right))
        (case (honu-prim-op exp)
            ;; +, -, *, /, and % are int * int -> int operators.
            ;;
            ;;  P, G, D |- e1 |=> e1' : int  P, G, D |- e2 |=> e2' : int
            ;;  --------------------------------------------------------
            ;;           P, G, D |- e1 op e2 |=> e1' op e2' : int
          [(plus)
           ;; we can just pass typ to the two sides (if it's an appropriate type)
           ;; because it's always the case that + returns the same type as its
           ;; operands.  Similar for minus, times, divide.
           (if typ
               (cond
                 ;; this should work because we should never be passing a
                 ;; void type as typ (#f should be passed instead).
                 [(not (honu-prim-type? typ))
                  (raise-read-error-with-stx
                   "Cannot use primitive operators in a object context."
                   stx)]
                 [(not (member (honu-prim-type-name typ) '(int float str)))
                  (raise-read-error-with-stx
                   "The result of + must be used as either an int, float, or str."
                   stx)]))
           (let-values (((e1 t1) (f left typ))
                        ((e2 t2) (f right typ)))
             (cond
               [typ ;; if we knew the correct context, then checking has already been done.
                (values (copy-struct honu-prim exp
                          (honu-prim-op-type t1)
                          (honu-prim-left  e1)
                          (honu-prim-right e2))
                        typ)]
               [(and (honu-type-equal? t1 (honu-int-type left))
                     (honu-type-equal? t2 (honu-int-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-int-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type left))
                     (honu-type-equal? t2 (honu-float-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-float-type exp))]
               [(and (honu-type-equal? t1 (honu-str-type left))
                     (honu-type-equal? t2 (honu-str-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-str-type exp))]
               [else
                (raise-read-error-with-stx
                 "Operands are of different types or an incompatible type."
                 stx)]))]
          [(minus times div)
           (if typ
               (cond
                 [(not (honu-prim-type? typ))
                  (raise-read-error-with-stx
                   "Cannot use primitive operators in a object context."
                   stx)]
                 [(not (member (honu-prim-type-name typ) '(int float)))
                  (raise-read-error-with-stx
                   (format "~a must be used in either an int or float context."
                           (case op [(minus) '-] [(times) '*] [(div) '/]))
                   stx)]))
           (let-values (((e1 t1) (f left typ))
                        ((e2 t2) (f right typ)))
             (cond
               [typ
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                        typ)]
               [(and (honu-type-equal? t1 (honu-int-type left))
                     (honu-type-equal? t2 (honu-int-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-int-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type left))
                     (honu-type-equal? t2 (honu-float-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-float-type exp))]
               [else
                (raise-read-error-with-stx
                 "Operands are of different types or an incompatible type."
                 stx)]))]
          [(mod)
           ;; mod is only defined on ints, so check left and right side appropriately.
           (let-values (((e1 t1) (f left (honu-int-type left)))
                        ((e2 t2) (f right (honu-int-type right))))
             ;; if we made it here, both must have been ints
             (values (copy-struct honu-prim exp
                       (honu-prim-op-type t1)
                       (honu-prim-left  e1)
                       (honu-prim-right e2))
                     (honu-int-type exp)))]
            [(lt le gt ge)
             ;; relational operators don't tell us about their operands, so must use #f
             (let-values (((e1 t1) (f left #f))
                          ((e2 t2) (f right #f)))
             (cond
               [(and (honu-type-equal? t1 (honu-int-type left))
                     (honu-type-equal? t2 (honu-int-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-float-type left))
                     (honu-type-equal? t2 (honu-float-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-str-type left))
                     (honu-type-equal? t2 (honu-str-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [(and (honu-type-equal? t1 (honu-char-type left))
                     (honu-type-equal? t2 (honu-char-type right)))
                (values (copy-struct honu-prim exp
                               (honu-prim-op-type t1)
                               (honu-prim-left  e1)
                               (honu-prim-right e2))
                             (honu-bool-type exp))]
               [else
                (raise-read-error-with-stx
                 "Types of operands do not match or are not of appropriate types."
                 (honu-ast-src-stx exp))]))]
            ;; && and || are bool * bool -> bool operators.
            ;;
            ;;  P, G, D |- e1 |=> e1' : bool  P, G, D |- e2 |=> e2' : bool
            ;;  ----------------------------------------------------------
            ;;           P, G, D |- e1 op e2 |=> e1' op e2' : bool
            [(and or)
             (if (and typ (not (honu-type-equal? typ (honu-bool-type stx))))
                 (raise-type-error-with-stx typ (honu-bool-type stx) stx))
             (let-values (((e1 t1) (f left (honu-bool-type left)))
                          ((e2 t2) (f right (honu-bool-type right))))
               (values (copy-struct honu-prim exp
                         (honu-prim-op-type t1)
                         (honu-prim-left  e1)
                         (honu-prim-right e2))
                       (honu-bool-type exp)))]
            ;; For now we just have that the operands to an equality
            ;; operator can be of any type and that the types of the
            ;; operands do not need to be equal.  Might it be the
            ;; case that we want to check if we're comparing two
            ;; primitives and reject if they're not the same type?
            ;;
            ;; Yes, and so we do below.
            ;;
            ;; (old type rule)
            ;;  == is a 'a * 'b -> bool operator.
            ;;
            ;;  P, G, D |- e1 |=> e1' : t1    P, G, D |- e2 |=> e2' : t2
            ;;  --------------------------------------------------------
            ;;          P, G, D |- e1 == e2 |=> e1' == e2' : bool
          [(neq equal)
           (if (and typ (not (honu-type-equal? typ (honu-bool-type stx))))
               (raise-type-error-with-stx typ (honu-bool-type stx) stx))
           ;; there's no telling what the operands should be here.
           (let-values (((e1 t1) (f left #f))
                        ((e2 t2) (f right #f)))
             (cond
               [(and (<:_P tenv t1 (honu-any-type left))
                     (<:_P tenv t2 (honu-any-type right)))
                (values (copy-struct honu-prim exp
                          (honu-prim-op-type (honu-any-type left))
                          (honu-prim-left  e1)
                          (honu-prim-right e2))
                        (honu-bool-type exp))]
               [(honu-type-equal? t1 t2)
                (values (copy-struct honu-prim exp
                          (honu-prim-op-type t1)
                          (honu-prim-left  e1)
                          (honu-prim-right e2))
                        (honu-bool-type exp))]
               [else (raise-read-error-with-stx
                      "Attempt to check two unrelated types for (in)equality."
                      stx)]))]
          [(clseq)
           (if (and typ (not (honu-type-equal? typ (honu-bool-type stx))))
               (raise-type-error-with-stx typ (honu-bool-type stx) stx))
           ;; here at least we know they should be class types.
           (let-values (((e1 t1) (f left (honu-any-type left)))
                        ((e2 t2) (f right (honu-any-type right))))
             (values (copy-struct honu-prim exp
                       (honu-prim-op-type (honu-any-type left))
                       (honu-prim-left e1)
                       (honu-prim-right e2))
                     (honu-bool-type exp)))]
          [else (raise-read-error-with-stx
                 "Unknown binary primitive operation."
                 op-stx)])]
       [(struct honu-lambda (stx arg-names arg-types body))
        (cond
          [(not typ)
           (let ([env (fold (lambda (n t e)
                              (extend-env e n t))
                            env arg-names arg-types)])
             (let-values (((e1 t1) ((honu-typecheck-exp tenv env cenv) body #f)))
               (values (copy-struct honu-lambda exp
                                    (honu-lambda-body e1))
                       (honu-func-type-from-exp (honu-lambda-arg-types exp) t1 exp))))]
            ;; if typ is not #f, then it should be a func type, and the return
            ;; type of the func should be the same as the lambda body.
          [else
           (if (and typ (not (honu-func-type? typ)))
               (raise-read-error-with-stx
                "Found lambda in non-function type context."
                stx))
           (let ([typ-args (honu-func-type-args typ)]
                 [typ-ret  (honu-func-type-return typ)])
             (if (not (= (length typ-args)
                         (length arg-types)))
                 (raise-read-error-with-stx
                  "Number of arguments in lambda do not match number of arguments expected."
                  stx))
             ;; for a function to be a subtype, its arguments must be
             ;; supertypes
             (for-each (lambda (t1 t2)
                         (if (not (<:_P tenv t2 t1))
                             (raise-read-error-with-stx
                              (format "Type ~a is not a supertype of ~a"
                                      (printable-type t1)
                                      (printable-type t2))
                              (honu-ast-src-stx t1))))
                       arg-types typ-args)
             (let ([env (fold (lambda (n t e)
                           (extend-env e n t))
                         env arg-names arg-types)])
               ;; we'll use typ-ret as the expected type for the body
               (let-values (((e1 t1) ((honu-typecheck-exp tenv env cenv) body typ-ret)))
                 (values (copy-struct honu-lambda exp
                           (honu-lambda-body e1))
                         (honu-func-type-from-exp arg-types t1 exp)))))])]
       [(struct honu-facc (stx obj elab field))
        (if (eqv? obj 'my)
            ;;            D(fd) = t
            ;; ------------------------------
            ;; P, G, D |- my.fd |=> my.fd : t
            (let ([cenv-typ (cenv field)])
              (if cenv-typ
                  (if (and typ (not (<:_P tenv cenv-typ typ)))
                      (raise-type-error-with-stx typ cenv-typ stx)
                      (values exp cenv-typ))
                  (if (env #'this)
                      ;; We're inside a class or mixin, so this is just an invalid name.
                      ;; We do also have the extra case that if we're inside a method, this
                      ;; may have been an init field's name (which are not contained in the
                      ;; class environment passed to honu-typecheck-exp for method bodies).
                      (raise-read-error-with-stx
                       "No local field with this name or attempt to use init field in method."
                       field)
                      (raise-read-error-with-stx
                       "Attempt to use static field access outside of class or mixin body."
                       stx))))
            ;; P, G, D |- e |=> e' : t'  <fd, t> in t'
            ;; ---------------------------------------
            ;;     P, G, D |- e.fd |=> e'.fd : t
            (let-values (((e1 t1) (f obj (honu-any-type obj))))
              (let ((field-type (get-field-type tenv t1 field)))
                (if field-type
                    (if (and typ (not (<:_P tenv field-type typ)))
                        (raise-type-error-with-stx typ field-type stx)
                        (values (copy-struct honu-facc exp
                                  ;; Make sure to elaborate the type
                                  (honu-facc-elab t1)
                                  (honu-facc-obj e1))
                                field-type))
                    (raise-read-error-with-stx
                     "Field not found in type of object."
                     (honu-facc-field exp))))))]
       [(struct honu-fassn (stx obj elab field rhs))
        (if (and typ (not (honu-type-equal? typ (honu-void-type stx))))
            (raise-read-error-with-stx
             "Assignment used in non-void context."
             stx))
        (if (eqv? obj 'my)
            ;; D(fd) = t  P, G, D |- e |=> e' : t'  t' <: t
            ;; --------------------------------------------
            ;;  P, G, D |- my.fd = e |=> my.fd = e' : void
            (let ([cenv-typ (cenv field)])
              (if cenv-typ
                  (let-values (((e2 t2) (f rhs cenv-typ)))
                    (values (copy-struct honu-fassn exp
                              (honu-fassn-rhs e2))
                            (honu-void-type exp)))
                  (if (env #'this)
                      ;; We're inside a class or mixin, so this is just an invalid name.
                      ;; We do also have the extra case that if we're inside a method, this
                      ;; may have been an init field's name (which are not contained in the
                      ;; class environment passed to honu-typecheck-exp for method bodies).
                      (raise-read-error-with-stx
                       "No local field with this name or attempt to use init field in method."
                       field)
                      (raise-read-error-with-stx
                       "Attempt to use static field assignment outside of class or mixin body."
                       stx))))
            ;; P, G, D |- e1 |=> e1' : t'        <fd, t> in t'
            ;; P, G, D |- e2 |=> e2' : t''            t'' <: t
            ;; -----------------------------------------------
            ;;  P, G, D |- e1.fd = e2 |=> e1'.fd = e2' : void
            (let-values (((e1 t1) (f obj (honu-any-type obj))))
              (let ((field-type (get-field-type tenv t1 field)))
                (if field-type
                    (let-values (((e2 t2) (f rhs field-type)))
                      (values (copy-struct honu-fassn exp
                                (honu-fassn-obj e1)
                                ;; Make sure to elaborate the type
                                (honu-fassn-elab t1)
                                (honu-fassn-rhs e2))
                              (honu-void-type exp)))
                    (raise-read-error-with-stx
                     "Field not found in type of object."
                     field)))))]
       [(struct honu-mcall (stx obj elab method args))
        ;; FIXME : need to change to use typ appropriately!
        
        ;; We need the arg elaborations and types no matter what, so...
        (let-values ([(new-args new-types)
                      ;; obviously eventually we'll want to use the
                      ;; real method arg types instead of the map below.
                      (map-two-values f args (map (lambda (_) #f) args))])
          (if (eqv? (honu-mcall-obj exp) 'my)
              ;; D(md) = t_1 ... t_n -> t
              ;; P, G, D |- e_i |=> e_i' : t_i'   t_i' <: t_i  (NOT t_i <: t_i')
              ;; --------------------------------------------
              ;; P, G, D |- my.md(e_1, ..., e_n) |=>
              ;;            my'.md(e_1', ..., e_n') : t
              (if (cenv (honu-mcall-method exp))
                  (let ((method-type (cenv (honu-mcall-method exp))))
                    (if method-type
                        (let loop ((n 0)
                                   (dec-types (honu-dispatch-type-args method-type))
                                   (calc-types new-types))
                          (cond
                            ((null? dec-types)
                             (if (null? calc-types)
                                 ;; We reached the end of both lists, so return
                                 ;; the new expression and the return type.
                                 (values (copy-struct honu-mcall exp
                                                      (honu-mcall-args new-args))
                                         (honu-dispatch-type-return method-type))
                                 ;; calc-types isn't null, so too many arguments
                                 ;; were given in the mcall expression.
                                 (raise-read-error-with-stx
                                  "Too many arguments for method."
                                  (honu-ast-src-stx exp))))
                            ;; dec-types isn't null, so we have too few arguments.
                            ((null? calc-types)
                             (raise-read-error-with-stx
                              "Not enough arguments for method."
                              (honu-ast-src-stx exp)))
                            ;; t_i' <: t_i, so check the next one.
                            ((<:_P tenv (car calc-types) (car dec-types))
                             (loop (+ n 1) (cdr dec-types) (cdr calc-types)))
                            ;; t_i was _not_ <: t_i', so blame the appropriate
                            ;; expression.
                            (else
                             (raise-read-error-with-stx
                              "Argument type is not subtype of declared type."
                              (honu-ast-src-stx (list-ref (honu-mcall-args exp) n))))))
                        ;; method-type was #f, so it couldn't be found in D.
                        (raise-read-error-with-stx
                         "Method not found in current class or mixin."
                         (honu-mcall-method exp))))
                  (if (env 'this)
                      (raise-read-error-with-stx
                       "No local method with this name."
                       (honu-mcall-method exp))
                      (raise-read-error-with-stx
                       "Attempt to use static method call outside of class or mixin body."
                       (honu-ast-src-stx exp))))
              ;; P, G, D |- e |=> e' : t'  <md, t_1 ... t_n -> t> in t'
              ;; P, G, D |- e_i |=> e_i' : t_i'             t_i' <: t_i  (NOT t_i <: t_i')
              ;; ------------------------------------------------------
              ;; P, G, D |- e.md(e_1, ..., e_n) |=>
              ;;            e'.md(e_1', ..., e_n') : t
              (let-values (((e0 t0) (f obj (honu-any-type obj))))
                (let ((method-type (get-method-type tenv t0
                                                    (honu-mcall-method exp))))
                  (if method-type
                      (let loop ((n 0)
                                 (dec-types (honu-dispatch-type-args method-type))
                                 (calc-types new-types))
                        (cond
                         ((null? dec-types)
                          (if (null? calc-types)
                              ;; We reached the end of both lists, so return
                              ;; the new expression and the return type.
                              (values (copy-struct honu-mcall exp
                                        (honu-mcall-obj e0)
                                        (honu-mcall-elab t0)
                                        (honu-mcall-args new-args))
                                      (honu-dispatch-type-return method-type))
                              ;; calc-types isn't null, so too many arguments
                              ;; were given in the mcall expression.
                              (raise-read-error-with-stx
                               "Too many arguments for method."
                               (honu-ast-src-stx exp))))
                         ;; dec-types isn't null, so we have too few arguments.
                         ((null? calc-types)
                          (raise-read-error-with-stx
                           "Not enough arguments for method."
                           (honu-ast-src-stx exp)))
                         ;; t_i' <: t_i, so check the next one.
                         ((<:_P tenv (car calc-types) (car dec-types))
                          (loop (+ n 1) (cdr dec-types) (cdr calc-types)))
                         ;; t_i was _not_ <: t_i', so blame the appropriate
                         ;; expression.
                         (else
                          (raise-read-error-with-stx
                           "Argument type is not subtype of declared type."
                           (honu-ast-src-stx (list-ref (honu-mcall-args exp) n))))))
                      ;; method-type was #f, so it couldn't be found in t1.
                      (raise-read-error-with-stx
                       "Method not found in type of object."
                       (honu-mcall-method exp)))))))]
       ;; P, G, D |- id |=> id : G(id)
       [(struct honu-var (stx name builtin?))
        (cond
          [(env name)
           =>
           (lambda (t)
             (if (and typ (not (<:_P tenv t typ)))
                 (raise-type-error-with-stx typ t stx)
                 (values exp t)))]
          [(get-builtin-type name)
           =>
           (lambda (t)
             (if (and typ (not (<:_P tenv t typ)))
                 (raise-type-error-with-stx typ t stx)
                 (values (copy-struct honu-var exp
                           (honu-var-builtin? #t))
                         t)))]
          [else (raise-read-error-with-stx
                 "Variable not bound in local environment."
                 name)])]
       ;; E(id) = t   P, G, D |- e |=> e' : t'   t' <: t
       ;; ----------------------------------------------
       ;;      P, G, D |- id = e |=> id = e' : void
       [(struct honu-assn (stx name rhs))
        (if (and typ (not (honu-type-equal? typ (honu-void-type stx))))
            (raise-read-error-with-stx
             "Assignment found in non-void context."
             stx))
        (let ((var-type (env (honu-assn-name exp))))
          (if (not var-type)
              (raise-read-error-with-stx
               "Variable not bound in local environment."
               name)
              (let-values (((e1 t1) (f rhs var-type)))
                (values (copy-struct honu-assn exp
                          (honu-assn-rhs e1))
                        (honu-void-type exp)))))]
       [(struct honu-call (stx name args builtin?))
        (cond
          [(env name)
           =>
           (lambda (t)
             (honu-typecheck-call tenv f exp t typ #f))]
          [(get-builtin-type name)
           =>
           (lambda (t)
             (honu-typecheck-call tenv f exp t typ #t))]
          [else 
           (raise-read-error-with-stx
            (format "Function ~a not found" (printable-key name))
            name)])]
       ;; P, G, D |- this |=> this : G(this)
       [(struct honu-this (stx))
        (let ([this-type (env #'this)])
          (if this-type
              (if (and typ (not (<:_P tenv this-type typ)))
                  (raise-type-error-with-stx typ this-type stx)
                  (values exp this-type))
              (raise-read-error-with-stx
               "Use of this outside of a class or mixin body."
               stx)))]
       ;; P, G, D |- e1 |=> e1' : t'       P |- t  
       ;; ---------------------------------------
       ;; P, G, D |- cast e1 t |=> cast e1' t : t
       ;;
       ;; Note that we don't check for primitive types (fuller
       ;; explanation under isa below), and also we don't do any
       ;; checking of how t' relates to t -- that's not the point
       ;; of a cast.  At runtime it will be checked that the object
       ;; that e1 results in is of a class that implements t.
       [(struct honu-cast (stx obj type))
        ;; since we're casting, object can be of any (interface) type
        (let-values (((e1 t1) (f obj (honu-any-type obj))))
          (if (honu-iface-type-in-tenv? tenv type)
              (values (copy-struct honu-cast exp
                        (honu-cast-obj e1))
                      (honu-cast-type exp))
              (raise-read-error-with-stx
               "Attempt to cast to invalid type."
               (honu-ast-src-stx type))))]
       ;; P, G, D |- e1 |=> e1' : t'        P |- t
       ;; ----------------------------------------
       ;; P, G, D |- e1 isa t |=> e1' isa t : bool
       ;;
       ;; Note that we don't check to see if e1's type is a primitive
       ;; type and fail with an appropriate message if so.  How
       ;; primitive do we want to treat primitives?  Might they stay
       ;; "primitives", or might they eventually be changed into
       ;; classes?
       [(struct honu-isa (stx obj type))
        ;; since we're checking isa, the object can be any (interface) stype
        (let-values (((e1 t1) (f obj (honu-any-type obj))))
          (if (honu-iface-type-in-tenv? tenv type)
              (values (copy-struct honu-isa exp
                        (honu-isa-obj e1))
                      (honu-bool-type exp))
              (raise-read-error-with-stx
               "Attempt to check isa against invalid type."
               (honu-ast-src-stx type))))]
       ;; P, G, D |- e0 |=> e0' : bool  P, G, D |- e1 |=> e1' : t
       ;;               P, G, D |- e2 |=> e2' : t
       ;; -------------------------------------------------------
       ;;  P, G, D |- if e0  then e1  else e2  |=>
       ;;             if e0' then e1' else e2' : t
       ;;
       ;; We can make this a weaker rule by only requiring either
       ;;   a) t1 <: t2; or
       ;;   b) t2 <: t1
       ;; and returning the supertype as the type of the if expression.
       ;; Would this cause any problems (other than complicating the
       ;; type rule/code)?
       [(struct honu-if (stx test true false))
        (let-values (((e0 t0) (f test (honu-bool-type test)))
                     ((e1 t1) (f true typ))
                     ((e2 t2) (f false typ)))
          (cond
            [(<:_P tenv t1 t2)
             (values (copy-struct honu-if exp
                       (honu-if-cond  e0)
                       (honu-if-true  e1)
                       (honu-if-false e2))
                     t2)]
            [(<:_P tenv t2 t1)
             (values (copy-struct honu-if exp
                       (honu-if-cond  e0)
                       (honu-if-true  e1)
                       (honu-if-false e2))
                     t1)]
            [else
             (raise-read-error-with-stx
              "Branches of if expression are of unrelated types."
              stx)]))]
       [(struct honu-while (stx cond body))
        (let-values (((e1 t1) (f cond (honu-bool-type cond)))
                     ((e2 t2) (f body #f)))
          (values (copy-struct honu-while exp
                    (honu-while-cond e1)
                    (honu-while-body e2))
                  (honu-void-type exp)))]
       ;; P, G, D |- e_i |=> e_i' : t_i                     c [= t
       ;; each init arg corresponding to id_i has type t_i' where
       ;; t_i <: t_i'
       ;; --------------------------------------------------------
       ;; P, G, D |- new c : t (id_1 = e_1, ..., id_n = e_n) |=>
       ;;            new c : t (id_1 = e_1', ..., id_n = e_n') : t
       ;;
       [(struct honu-new (stx class type arg-names arg-vals))
        (cond
          [(and typ type (not (<:_P tenv type typ)))
           (raise-type-error-with-stx typ type stx)]
          [(not (get-class-entry class tenv))
           (raise-read-error-with-stx
            "Undefined class"
            class)]
          [(and type (not (honu-iface-type-in-tenv? tenv type)))
           (raise-read-error-with-stx
            "Undefined type or non-interface type"
            (honu-ast-src-stx type))]
          [(and (not type) (not typ))
           (raise-read-error-with-stx
            "Type to create in new statement needs to be explicitly stated."
            stx)]
          ;; if there was no explicit type given...
          [(not type)
           (if (Implements_P tenv class typ)
                (let-values ([(new-args new-types)
                              (map-two-values f arg-vals (map (lambda (_) #f) arg-vals))])
                  (let ((remainder (fold (lambda (n t i)
                                           (check-init-type-for-name tenv i n t))
                                         (get-init-names-and-types tenv class)
                                         (honu-new-arg-names exp)
                                         new-types)))
                    (if (or (null? remainder)
                            (not (ormap tenv-init-optional? remainder))) ; checks to see if all optional
                        (values (copy-struct honu-new exp
                                  (honu-new-type typ)
                                  (honu-new-arg-vals new-args))
                                typ)
                        (raise-read-error-with-stx
                         "Too few initialization arguments in new expression."
                         stx))))
                (raise-read-error-with-stx
                 (format "Class for new expression does not implement type ~a."
                         (printable-type typ))
                 stx))]
          ;; FIXME: still need to do appropriate things with typ in here
          [type
            (if (Implements_P tenv class type)
                (let-values ([(new-args new-types)
                              (map-two-values f arg-vals (map (lambda (_) #f) arg-vals))])
                  (let ((remainder (fold (lambda (n t i)
                                           (check-init-type-for-name tenv i n t))
                                         (get-init-names-and-types tenv class)
                                         (honu-new-arg-names exp)
                                         new-types)))
                    (if (or (null? remainder)
                            (not (ormap tenv-init-optional? remainder))) ; checks to see if all optional
                        (values (copy-struct honu-new exp
                                  (honu-new-arg-vals new-args))
                                (honu-new-type exp))
                        (raise-read-error-with-stx
                         "Too few initialization arguments in new expression."
                         stx))))
                (raise-read-error-with-stx
                 "Class for new expression does not implement type in new expression."
                 stx))])]
       ;; P, G_i, D |- tid_i id_i = rhs_i |=> tid_i id_i = rhs_i', G_(i+1)
       ;; P, G_(m+1), D |- e_i |=> e_i' : t_i
       ;; ----------------------------------------------------------------
       ;; P, G_0, D |- { tid_0 id_0 = rhs_0; ...; tid_m id_m = rhs_m;
       ;;                e_0; ...; e_n; } |=>
       ;;              { tid_0 id_0 = rhs_0'; ...; tid_m id_m = rhs_m';
       ;;                e_0'; ...; e_n'; } : t_n
       [(struct honu-block (stx binds exps))
        (let*-values ([(new-bind-f) (honu-typecheck-binding tenv cenv)]
                      [(new-binds new-env)
                       (map-and-fold new-bind-f env binds)]
                      [(new-f) (honu-typecheck-exp tenv new-env cenv)]
                      [(new-exps last-type)
                       (let loop ([exps exps]
                                  [new-exps '()]
                                  [new-types '()])
                         ;; we know we must have at least one expression,
                         ;; so here's our base case.
                         (if (null? (cdr exps))
                             ;; type of last expression should fit block context.
                             (let-values ([(e1 t1) (new-f (car exps) typ)])
                               (values (reverse (cons e1 new-exps))
                                       ;; just need the last expression's type
                                       t1))
                             ;; since we don't care about the types of any but the
                             ;; last expression in a block, just pass in #f
                             (let-values ([(e1 t1) (new-f (car exps) #f)])
                               (loop (cdr exps)
                                     (cons e1 new-exps)
                                     (cons t1 new-types)))))])
          (values (copy-struct honu-block exp
                    (honu-block-binds new-binds)
                    (honu-block-exps  new-exps))
                  last-type))]
       ;;       P, G, D |- e |=> e' : t
       ;; -------------------------------------
       ;; P, G, D |- return e |=> return e' : t
       [(struct honu-return (stx body))
        (if body
            (let-values ([(e1 t1) (f body typ)])
              (values (copy-struct honu-return exp
                        (honu-return-body e1))
                      t1))
            (if typ
                (raise-read-error-with-stx
                 "Found void return in non-void context"
                 stx)
                (values exp (honu-void-type exp))))]
       [else
        (raise-read-error-with-stx
         "Unexpected type of Honu expression."
         (honu-ast-src-stx exp))]))
    f)

  ;;       P, G, D |- rhs |=> rhs' : t'    t' <: t
  ;; --------------------------------------------------
  ;; P, G, D |- t id = rhs |=> t id = rhs', G[id |-> t]
;  (provide honu-typecheck-binding)
  (provide/contract [honu-typecheck-binding
                     (tenv?
                      any/c
                      . -> .
                      ((honu-binding? any/c)
                       . ->* .
                       (honu-binding? any/c)))]) 
  (define (honu-typecheck-binding tenv cenv)
    (lambda (bind env)
      (match-let ([(struct honu-binding (ast name type rhs)) bind])
        (let-values (((e1 t1) ((honu-typecheck-exp tenv env cenv) rhs type)))
          (values (copy-struct honu-binding bind
                    (honu-binding-rhs e1))
                  (extend-env env name type))))))

  (define (honu-typecheck-call tenv f exp t typ builtin?)
    (match-let ([(struct honu-call (stx name args builtin?)) exp])
      (if (not (honu-func-type? t))
          (raise-read-error-with-stx
           "Expression is not a function."
           name))
      (if (and typ (not (<:_P tenv (honu-func-type-return t) typ)))
          (raise-type-error-with-stx typ (honu-func-type-return t) stx))
      (let-values ([(arg-exps arg-types)
                    (map-two-values (lambda (e t) (f e t))
                                    args (honu-func-type-args t))])
        (let ([formals-length (length (honu-func-type-args t))]
              [actuals-length (length arg-types)])
          (cond
            [(< formals-length actuals-length)
             (raise-read-error-with-stx
              (format "~a function got ~a more argument~a than expected"
                      (if builtin? "Built-in" "Declared")
                      (- actuals-length formals-length)
                      (if (= (- actuals-length formals-length) 1) "" "s"))
              stx)]
            [(> formals-length actuals-length)
             (raise-read-error-with-stx
              (format "~a function got ~a fewer argument~a than expected"
                      (if builtin? "Built-in" "Declared")
                      (- formals-length actuals-length)
                      (if (= (- formals-length actuals-length) 1) "" "s"))
              stx)]
            [else (values (copy-struct honu-call exp
                            (honu-call-args arg-exps)
                            (honu-call-builtin? builtin?))
                          (honu-func-type-return t))])))))
  )
