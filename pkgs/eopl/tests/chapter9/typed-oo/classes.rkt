#lang eopl

(require "store.rkt")
(require "lang.rkt")

;; object interface
(provide object object? new-object object->class-name object->fields)

;; method interface
(provide method method? a-method find-method)

;; class interface
(provide lookup-class initialize-class-env! class->super-name)

;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;; an object consists of a symbol denoting its class, and a list of
;; references representing the managed storage for the all the
;; fields. 

(define-datatype object object? 
  (an-object
   (class-name symbol?)
   (fields (list-of reference?))))

(define new-object                      
  (lambda (class-name)
    (an-object
     class-name
     (map 
      (lambda (field-id)
        (newref (list 'uninitialized-field field-id)))
      (class->field-names (lookup-class class-name))))))

;;;;;;;;;;;;;;;; methods and method environments ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body expression?)
   (super-name symbol?)
   (field-names (list-of symbol?))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

;; a method environment looks like ((method-name method) ...)

(define method-environment?
  (list-of 
   (lambda (p)
     (and 
      (pair? p)
      (symbol? (car p))
      (method? (cadr p))))))

;; method-env * id -> (maybe method)
(define assq-method-env
  (lambda (m-env id)
    (cond
      ((assq id m-env) => cadr)
      (else #f))))

;; find-method : Sym * Sym -> Method
;; Page: 345
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
            (report-method-not-found name))))))

(define report-method-not-found
  (lambda (name)
    (eopl:error 'find-method "unknown method ~s" name)))

;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
;; Page: 345
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
;; Page: 345
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
         (a-method-decl (result-type method-name vars var-types body)
                        (list method-name
                              (a-method vars body super-name field-names)))))
     m-decls)))

;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (method-env method-environment?)))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;; the-class-env will look like ((class-name class) ...)

;; the-class-env : ClassEnv
;; Page: 343
(define the-class-env '())

;; add-to-class-env! : ClassName * Class -> Unspecified
;; Page: 343
(define add-to-class-env!
  (lambda (class-name class)
    (set! the-class-env
          (cons
           (list class-name class)
           the-class-env))))

;; lookup-class : ClassName -> Class
(define lookup-class                    
  (lambda (name)
    (let ((maybe-pair (assq name the-class-env)))
      (if maybe-pair (cadr maybe-pair)
          (report-unknown-class name)))))

(define report-unknown-class
  (lambda (name)
    (eopl:error 'lookup-class "Unknown class ~s" name)))

;; constructing classes

;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
;; Page: 344
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env 
          (list
           (list 'object (a-class #f '() '()))))
    (for-each initialize-class-decl! c-decls)))

'(define initialize-class-env!
   (lambda (c-decls)
     (set! the-class-env 
           (list
            (list 'object (a-class #f '() '()))))
     (for-each initialize-class-decl! c-decls)))

;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      ;; interfaces don't affect runtime
      (an-interface-decl (interface-name method-decls) '())
      (a-class-decl (class-name super-name interface-names field-types field-names method-decls)
                    (let ((field-names
                           (append-field-names
                            (class->field-names (lookup-class super-name))
                            field-names)))
                      (add-to-class-env!
                       class-name
                       (a-class
                        super-name
                        field-names
                        (merge-method-envs
                         (class->method-env (lookup-class super-name))
                         (method-decls->method-env
                          method-decls super-name field-names)))))))))

;; append-field-names :  Listof(FieldName) * Listof(FieldName) 
;;                       -> Listof(FieldName)
;; Page: 344
;; like append, except that any super-field that is shadowed by a
;; new-field is replaced by a gensym
(define append-field-names
  (lambda (super-fields new-fields)
    (cond
      ((null? super-fields) new-fields)
      (else
       (cons 
        (if (memq (car super-fields) new-fields)
            (fresh-identifier (car super-fields))
            (car super-fields))
        (append-field-names
         (cdr super-fields) new-fields))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name  field-names method-env)
               super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
               field-names))))

(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
      (a-class (super-name field-names method-env)
               method-env))))


(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
                 class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
                 fields))))

(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)  
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
        (symbol->string identifier)
        "%"             ; this can't appear in an input identifier
        (number->string sn))))))

(define maybe
  (lambda (pred)
    (lambda (v)
      (or (not v) (pred v)))))
