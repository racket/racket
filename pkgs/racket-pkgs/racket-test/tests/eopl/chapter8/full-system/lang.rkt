#lang eopl

;; grammar for full module system.
;; based on CHECKED.

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  
  '(
    
    (program
     ((arbno module-definition)
      expression)
     a-program)
    
    (module-definition
     ("module" identifier 
               "interface" interface
               "body" module-body)
     a-module-definition)
    
    
    (interface
        ("[" (arbno declaration) "]") 
      simple-iface)
    
    (interface
        ("(" "(" identifier ":" interface ")" "=>" interface ")") 
      proc-iface)
    
    
    (declaration
     ("opaque" identifier)
     opaque-type-decl)
    
    (declaration
     ("transparent" identifier "=" type)
     transparent-type-decl)
    
    (declaration    
     (identifier ":" type)
     val-decl)
    
    
    (module-body
     ("[" (arbno definition) "]")
     defns-module-body)
    
    (module-body
     ("module-proc" "(" identifier ":" interface ")" module-body)
     proc-module-body)
    
    (module-body
     (identifier)
     var-module-body)
    
    (module-body
     ("(" identifier identifier ")")
     app-module-body)
    
    
    (definition
      (identifier "=" expression)
      val-defn)
    
    (definition
      ("type" identifier "=" type)
      type-defn)
    
    ;; new expression:
    
    (expression
     ("from" identifier "take" identifier)
     qualified-var-exp)
    
    ;; new types
    
    (type
     (identifier)
     named-type)                
    
    (type
     ("from" identifier "take" identifier)
     qualified-type)
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; no changes in grammar below here
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (expression (number) const-exp)
    
    (expression
     (identifier)
     var-exp)
    
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
    
    (expression
     ("proc" "(" identifier ":" type ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("letrec"
      type identifier "(" identifier ":" type ")"
      "=" expression "in" expression)
     letrec-exp)
    
    (type
     ("int")
     int-type)
    
    (type
     ("bool")
     bool-type)
    
    (type
     ("(" type "->" type ")")
     proc-type)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

;;;; for types

(define atomic-type?
  (lambda (ty)
    (cases type ty
      (proc-type (ty1 ty2) #f)
      (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (t1 t2) #t)
      (else #f))))

(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) arg-type)
      (else (eopl:error 'proc-type->arg-type
                        "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-type result-type) result-type)
      (else (eopl:error 'proc-type->result-types
                        "Not a proc type: ~s" ty)))))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
                 (list
                  (type-to-external-form arg-type)
                  '->
                  (type-to-external-form result-type)))
      (named-type (name) name)
      (qualified-type (modname varname)
                      (list 'from modname 'take varname))
      )))


;;;; for module definitions

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Maybe(Defn)
(define maybe-lookup-module-in-list
  (lambda (name module-defs)
    (if (null? module-defs)
        #f
        (let ((name1 (module-definition->name (car module-defs))))
          (if (eqv? name1 name)
              (car module-defs)
              (maybe-lookup-module-in-list name (cdr module-defs)))))))

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Defn OR Error
(define lookup-module-in-list
  (lambda (name module-defs)
    (cond
      ((maybe-lookup-module-in-list name module-defs)
       => (lambda (mdef) mdef))
      (else 
       (eopl:error 'lookup-module-in-list
                   "unknown module ~s"
                   name)))))

(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type m-body)
                           m-name))))

(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type m-body)
                           m-type))))

(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
      (a-module-definition (m-name m-type m-body)
                           m-body))))

(define val-decl?
  (lambda (decl)
    (cases declaration decl
      (val-decl (name ty) #t)
      (else #f))))

(define transparent-type-decl?
  (lambda (decl)
    (cases declaration decl
      (transparent-type-decl (name ty) #t)
      (else #f))))

(define opaque-type-decl?
  (lambda (decl)
    (cases declaration decl
      (opaque-type-decl (name) #t)
      (else #f))))

(define decl->name
  (lambda (decl)
    (cases declaration decl
      (opaque-type-decl (name) name)
      (transparent-type-decl (name ty) name)
      (val-decl (name ty) name))))

(define decl->type
  (lambda (decl)
    (cases declaration decl
      (transparent-type-decl (name ty) ty)
      (val-decl (name ty) ty)
      (opaque-type-decl (name)
                        (eopl:error 'decl->type
                                    "can't take type of abstract type declaration ~s"
                                    decl)))))

