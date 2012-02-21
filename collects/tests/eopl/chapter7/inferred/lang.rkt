#lang eopl

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
  '((program (expression) a-program)
    
    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("zero?" "(" expression ")")
     zero?-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)   
    
    (expression
     ("proc" "("  identifier ":" optional-type ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("letrec"
      optional-type identifier "(" identifier ":" optional-type ")"
      "=" expression "in" expression)
     letrec-exp)
    
    (optional-type
     ("?")
     no-type)
    
    (optional-type
     (type)
     a-type)
    
    (type
     ("int")
     int-type)
    
    (type
     ("bool")
     bool-type)
    
    (type
     ("(" type "->" type ")")
     proc-type)
    
    (type
     ("%tvar-type" number)
     tvar-type)
    
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

(define atomic-type?
  (lambda (ty)
    (cases type ty
      (proc-type (ty1 ty2) #f)
      (tvar-type (sn) #f)
      (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (t1 t2) #t)
      (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (serial-number) #t)
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

;; type-to-external-form : Type -> List
;; Page: 266
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
      (tvar-type (serial-number)
                 (string->symbol
                  (string-append
                   "tvar"
                   (number->string serial-number)))))))
