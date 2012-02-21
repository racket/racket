#lang eopl

(require "lang.rkt")
(require "static-classes.rkt")
(require "static-data-structures.rkt")

(provide type-to-external-form type-of type-of-program)

;; type-of-program : Program -> Type
;; Page: 358 
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls exp1)
                 (initialize-static-class-env! class-decls)
                 (for-each check-class-decl! class-decls)
                 (type-of exp1 (init-tenv))))))

;; type-of : Exp -> Tenv
;; Page: 360 and 364
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      
      (const-exp (num) (int-type))
      
      (var-exp (var) (apply-tenv tenv var))
      
      (diff-exp (exp1 exp2)
                (let ((type1 (type-of exp1 tenv))
                      (type2 (type-of exp2 tenv)))
                  (check-equal-type! type1 (int-type) exp1)
                  (check-equal-type! type2 (int-type) exp2)
                  (int-type)))
      
      (sum-exp (exp1 exp2)
               (let ((type1 (type-of exp1 tenv))
                     (type2 (type-of exp2 tenv)))
                 (check-equal-type! type1 (int-type) exp1)
                 (check-equal-type! type2 (int-type) exp2)
                 (int-type)))
      
      (zero?-exp (exp1)
                 (let ((type1 (type-of exp1 tenv)))
                   (check-equal-type! type1 (int-type) exp1)
                   (bool-type)))
      
      (if-exp (test-exp true-exp false-exp)
              (let
                  ((test-type (type-of test-exp tenv))
                   (true-type (type-of true-exp tenv))
                   (false-type (type-of false-exp tenv)))
                ;; these tests either succeed or raise an error
                (check-equal-type! test-type (bool-type) test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      
      (let-exp (ids rands body) 
               (let ((new-tenv 
                      (extend-tenv 
                       ids
                       (types-of-exps rands tenv)
                       tenv)))
                 (type-of body new-tenv)))
      
      (proc-exp (bvars bvar-types body)
                (let ((result-type
                       (type-of body
                                (extend-tenv bvars bvar-types tenv))))
                  (proc-type bvar-types result-type)))
      
      (call-exp (rator rands) 
                (let ((rator-type (type-of rator tenv))
                      (rand-types  (types-of-exps rands tenv)))
                  (type-of-call rator-type rand-types rands exp)))
      
      (letrec-exp (proc-result-types proc-names
                                     bvarss bvar-typess proc-bodies
                                     letrec-body)
                  (let ((tenv-for-letrec-body
                         (extend-tenv 
                          proc-names
                          (map proc-type bvar-typess proc-result-types)
                          tenv)))
                    (for-each
                     (lambda (proc-result-type bvar-types bvars proc-body)
                       (let ((proc-body-type
                              (type-of proc-body
                                       (extend-tenv
                                        bvars
                                        bvar-types
                                        tenv-for-letrec-body)))) ;; !!
                         (check-equal-type!
                          proc-body-type proc-result-type proc-body)))
                     proc-result-types bvar-typess bvarss proc-bodies)
                    (type-of letrec-body tenv-for-letrec-body)))
      
      (begin-exp (exp1 exps)
                 (letrec 
                     ((type-of-begins
                       (lambda (e1 es)
                         (let ((v1 (type-of e1 tenv)))
                           (if (null? es)
                               v1
                               (type-of-begins (car es) (cdr es)))))))
                   (type-of-begins exp1 exps)))
      
      (assign-exp (id rhs)
                  (check-is-subtype!
                   (type-of rhs tenv)
                   (apply-tenv tenv id)
                   exp)
                  (void-type))
      
      (list-exp (exp1 exps) 
                (let ((type-of-car (type-of exp1 tenv)))
                  (for-each
                   (lambda (exp)
                     (check-equal-type!
                      (type-of exp tenv)
                      type-of-car
                      exp))
                   exps)
                  (list-type type-of-car)))
      
      ;; object stuff begins here
      
      (new-object-exp (class-name rands)
                      (let ((arg-types (types-of-exps rands tenv))
                            (c (lookup-static-class class-name)))
                        (cases static-class c
                          (an-interface (method-tenv)
                                        (report-cant-instantiate-interface class-name))
                          (a-static-class (super-name i-names
                                                      field-names field-types method-tenv)
                                          ;; check the call to initialize
                                          (type-of-call
                                           (find-method-type
                                            class-name
                                            'initialize) 
                                           arg-types
                                           rands
                                           exp)
                                          ;; and return the class name as a type
                                          (class-type class-name)))))
      
      (self-exp ()
                (apply-tenv tenv '%self))
      
      (method-call-exp (obj-exp method-name rands)
                       (let ((arg-types (types-of-exps rands tenv))
                             (obj-type (type-of obj-exp tenv)))
                         (type-of-call
                          (find-method-type
                           (type->class-name obj-type)
                           method-name)
                          arg-types
                          rands
                          exp)))
      
      (super-call-exp (method-name rands)
                      (let ((arg-types (types-of-exps rands tenv))
                            (obj-type (apply-tenv tenv '%self)))
                        (type-of-call
                         (find-method-type
                          (apply-tenv tenv '%super)
                          method-name)
                         arg-types
                         rands
                         exp)))
      
      ;; this matches interp.scm:  interp.scm calls
      ;; object->class-name, which fails on a non-object, so we need
      ;; to make sure that obj-type is in fact a class type.
      ;; interp.scm calls is-subclass?, which never raises an error,
      ;; so we don't need to do anything with class-name here.
      
      (cast-exp (exp class-name)
                (let ((obj-type (type-of exp tenv)))
                  (if (class-type? obj-type)
                      (class-type class-name)
                      (report-bad-type-to-cast obj-type exp))))
      
      ;; instanceof in interp.scm behaves the same way as cast:  it
      ;; calls object->class-name on its argument, so we need to
      ;; check that the argument is some kind of object, but we
      ;; don't need to look at class-name at all.
      
      (instanceof-exp (exp class-name)
                      (let ((obj-type (type-of exp tenv)))
                        (if (class-type? obj-type)
                            (bool-type)
                            (report-bad-type-to-instanceof obj-type exp))))
      
      )))

(define report-cant-instantiate-interface
  (lambda (class-name)
    (eopl:error 'type-of-new-obj-exp
                "Can't instantiate interface ~s"
                class-name)))

(define types-of-exps
  (lambda (rands tenv)
    (map (lambda (exp) (type-of exp tenv)) rands)))

;; type-of-call : Type * Listof(Type) * Listof(Exp) -> Type
;; Page: 360
(define type-of-call
  (lambda (rator-type rand-types rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (unless (= (length arg-types) (length rand-types))
                   (report-wrong-number-of-arguments arg-types rand-types
                                                     exp))
                 (for-each check-is-subtype! rand-types arg-types rands)
                 result-type)
      (else
       (report-rator-not-of-proc-type
        (type-to-external-form rator-type)
        exp)))))

(define report-rator-not-of-proc-type
  (lambda (external-form-rator-type exp)
    (eopl:error 'type-of-call
                "rator ~s is not of proc-type ~s"
                exp external-form-rator-type)))

(define report-wrong-number-of-arguments
  (lambda (arg-types rand-types exp)
    (eopl:error 'type-of-call
                "These are not the same: ~s and ~s in ~s"
                (map type-to-external-form arg-types)
                (map type-to-external-form rand-types)
                exp)))

;; check-class-decl! : ClassDecl -> Unspecified
;; Page: 367
(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (an-interface-decl (i-name abs-method-decls)
                         #t)
      (a-class-decl (class-name super-name i-names 
                                field-types field-names method-decls)
                    (let ((sc (lookup-static-class class-name)))
                      (for-each 
                       (lambda (method-decl)
                         (check-method-decl! method-decl
                                             class-name super-name 
                                             (static-class->field-names sc)
                                             (static-class->field-types sc)))
                       method-decls))
                    (for-each 
                     (lambda (i-name)
                       (check-if-implements! class-name i-name))
                     i-names)
                    ))))


;; check-method-decl! :
;;   MethodDecl * ClassName * ClassName * Listof(FieldName) * \Listof(Type) 
;;    -> Unspecified
;; Page: 368
(define check-method-decl!
  (lambda (m-decl self-name s-name f-names f-types)
    (cases method-decl m-decl
      (a-method-decl (res-type m-name vars var-types body)
                     (let ((tenv
                            (extend-tenv
                             vars var-types
                             (extend-tenv-with-self-and-super
                              (class-type self-name)
                              s-name
                              (extend-tenv f-names f-types
                                           (init-tenv))))))
                       (let ((body-type (type-of body tenv)))
                         (check-is-subtype! body-type res-type m-decl)
                         (if (eqv? m-name 'initialize) #t
                             (let ((maybe-super-type
                                    (maybe-find-method-type
                                     (static-class->method-tenv
                                      (lookup-static-class s-name))
                                     m-name)))
                               (if maybe-super-type
                                   (check-is-subtype! 
                                    (proc-type var-types res-type)
                                    maybe-super-type body)
                                   #t)))))))))

;; check-if-implements! : ClassName * InterfaceName -> Bool
;; Page: 369
(define check-if-implements!
  (lambda (c-name i-name)
    (cases static-class (lookup-static-class i-name)
      (a-static-class (s-name i-names f-names f-types
                              m-tenv)
                      (report-cant-implement-non-interface 
                       c-name i-name))
      (an-interface (method-tenv)
                    (let ((class-method-tenv
                           (static-class->method-tenv
                            (lookup-static-class c-name))))
                      (for-each
                       (lambda (method-binding)
                         (let ((m-name (car method-binding))
                               (m-type (cadr method-binding)))
                           (let ((c-method-type
                                  (maybe-find-method-type
                                   class-method-tenv
                                   m-name)))
                             (if c-method-type
                                 (check-is-subtype!
                                  c-method-type m-type c-name)
                                 (report-missing-method
                                  c-name i-name m-name)))))
                       method-tenv))))))

(define report-cant-implement-non-interface
  (lambda (c-name i-name)
    (eopl:error 'check-if-implements
                "class ~s claims to implement non-interface ~s"
                c-name i-name)))

(define report-missing-method
  (lambda (c-name i-name i-m-name)
    (eopl:error 'check-if-implements
                "class ~s claims to implement ~s, missing method ~s"
                c-name i-name i-m-name)))

;;;;;;;;;;;;;;;; types ;;;;;;;;;;;;;;;;

(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (equal? t1 t2)
        #t
        (eopl:error 'type-of
                    "Types didn't match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp))))

;; check-is-subtype! : Type * Type * Exp -> Unspecified
;; Page: 363
(define check-is-subtype!
  (lambda (ty1 ty2 exp)
    (if (is-subtype? ty1 ty2)
        #t
        (report-subtype-failure
         (type-to-external-form ty1)
         (type-to-external-form ty2)
         exp))))

(define report-subtype-failure
  (lambda (external-form-ty1 external-form-ty2 exp)
    (eopl:error 'check-is-subtype!
                "~s is not a subtype of ~s in ~%~s"
                external-form-ty1
                external-form-ty2
                exp)))

;; need this for typing cast expressions
;; is-subtype? : Type * Type -> Bool
;; Page: 363
(define is-subtype? 
  (lambda (ty1 ty2)
    (cases type ty1
      (class-type (name1)
                  (cases type ty2
                    (class-type (name2)
                                (statically-is-subclass? name1 name2))
                    (else #f)))
      (proc-type (args1 res1)
                 (cases type ty2
                   (proc-type (args2 res2)
                              (and
                               (every2? is-subtype? args2 args1)
                               (is-subtype? res1 res2)))
                   (else #f)))
      (else (equal? ty1 ty2)))))

(define andmap
  (lambda (pred lst1 lst2)
    (cond
      ((and (null? lst1) (null? lst2)) #t)
      ((or (null? lst1) (null? lst2)) #f) ; or maybe throw error
      ((pred (car lst1) (car lst2))
       (andmap pred (cdr lst1) (cdr lst2)))
      (else #f))))

(define every2? andmap)

;; statically-is-subclass? : ClassName * ClassName -> Bool
;; Page: 363
(define statically-is-subclass?
  (lambda (name1 name2)
    (or
     (eqv? name1 name2)
     (let ((super-name
            (static-class->super-name
             (lookup-static-class name1))))
       (if super-name
           (statically-is-subclass? super-name name2)
           #f))
     (let ((interface-names
            (static-class->interface-names
             (lookup-static-class name1))))
       (memv name2 interface-names)))))

(define report-bad-type-to-cast 
  (lambda (type exp)
    (eopl:error 'bad-type-to-case
                "can't cast non-object; ~s had type ~s"
                exp
                (type-to-external-form type))))

(define report-bad-type-to-instanceof
  (lambda (type exp)
    (eopl:error 'bad-type-to-case
                "can't apply instanceof to non-object; ~s had type ~s"
                exp
                (type-to-external-form type))))

