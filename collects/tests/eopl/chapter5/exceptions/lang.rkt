#lang eopl

;; grammar for the EXCEPTIONS language.  This is a somewhat cut-down
;; version of the LETREC language.

;; exercise: allow the "list" operator to take expressions instead
;; of just numbers.

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
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression (identifier) var-exp)
    
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    
    (expression
     ("letrec"
      identifier "(" identifier ")" "=" expression
      "in" expression)
     letrec-exp)
    
    ;; Lists.  We will have lists of literal numbers only.
    
    (expression
     ("list" "(" (separated-list number ",") ")")
     const-list-exp)
    
    (expression
     (unary-op "(" expression ")")
     unop-exp)
    
    (expression
     ("try" expression "catch" "(" identifier ")" expression)
     try-exp)
    
    (expression
     ("raise" expression)
     raise-exp)
    
    (unary-op ("null?") null?-unop)
    (unary-op ("car")   car-unop)
    (unary-op ("cdr" )  cdr-unop)
    (unary-op ("zero?") zero?-unop)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))
