#lang eopl

;; grammar for the CLASSES language.  Based on IMPLICIT-REFS, plus
;; multiple-argument procedures, multiple-declaration letrecs, and
;; multiple-declaration lets. 

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
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)
    
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)
    
    (expression
     ("letrec"
      (arbno identifier "(" (separated-list identifier ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    
    (expression
     ("set" identifier "=" expression)
     assign-exp)
    
    (expression
     ("list" "(" (separated-list expression ",") ")" )
     list-exp)
    
    ;; new productions for oop
    
    (class-decl                         
     ("class" identifier 
              "extends" identifier                   
              (arbno "field" identifier)
              (arbno method-decl)
              )
     a-class-decl)
    
    (method-decl
     ("method" identifier 
               "("  (separated-list identifier ",") ")" ; method formals
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
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
