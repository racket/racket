#lang eopl

;; grammar for the TYPED-OO language.  Based on IMPLICIT-REFS:
;; plus
;;   multiple-argument procedures
;;   multiple-declaration letrecs, and
;;   multiple-declaration lets, 
;;   types a la CHECKED (not INFERRED)
;;   lists of expressed values

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
  '((program ((arbno class-decl) expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("+" "(" expression "," expression ")")
     sum-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)   
    
    (expression
     ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
     proc-exp)
    
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    
    (expression
     ("letrec"
      (arbno type identifier "(" (separated-list identifier ":" type ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    
    (expression
     ("set" identifier "=" expression)
     assign-exp)
    
    ;; non-empty lists for typechecked version
    (expression
     ("list" "(" expression (arbno "," expression) ")" )
     list-exp)
    
    ;; new productions for oop
    
    (class-decl                         
     ("class" identifier 
              "extends" identifier 
              (arbno "implements" identifier)
              (arbno "field" type identifier)
              (arbno method-decl)
              )
     a-class-decl)
    
    (method-decl
     ("method" type identifier
               "("  (separated-list identifier  ":" type ",") ")" ; method formals
               expression
               )
     a-method-decl)
    
    (expression 
     ("new" identifier "(" (separated-list expression ",") ")")
     new-object-exp)
    
    ;; this is special-cased to prevent it from mutation
    (expression
     ("self")
     self-exp)
    
    (expression
     ("send" expression identifier
             "("  (separated-list expression ",") ")")
     method-call-exp)
    
    (expression                                
     ("super" identifier    "("  (separated-list expression ",") ")")
     super-call-exp)
    
    ;; new productions for typed-oo
    
    (class-decl
     ("interface" identifier (arbno abstract-method-decl))
     an-interface-decl)
    
    
    (abstract-method-decl
     ("method" type identifier 
               "("  (separated-list identifier ":" type ",") ")" )
     an-abstract-method-decl)
    
    (expression
     ("cast" expression identifier)
     cast-exp)
    
    (expression
     ("instanceof" expression identifier)
     instanceof-exp)
    
    (type ("int") int-type)             
    (type ("bool") bool-type)           
    (type ("void") void-type)      
    (type                               
     ("(" (separated-list type "*") "->" type ")")
     proc-type)
    (type
     ("listof" type)
     list-type)
    
    (type (identifier) class-type) ;; new for typed oo
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;

(define type->class-name
  (lambda (ty)
    (cases type ty
      (class-type (name) name)
      (else (eopl:error 'type->class-name
                        "Not a class type: ~s"
                        ty)))))

(define class-type?
  (lambda (ty)
    (cases type ty
      (class-type (name) #t)
      (else #f))))

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (void-type () 'void)
      (class-type (name) name)
      (list-type (ty) (list 'listof (type-to-external-form ty)))
      (proc-type (arg-types result-type)
                 (append
                  (formal-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define formal-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (formal-types-to-external-form (cdr types))))))))


