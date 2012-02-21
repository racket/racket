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
    
    ;; like list(n1,...,nk) in exceptions language. Sorry about that.
    (expression 
     ("[" (separated-list number ",") "]")
     const-list-exp)
    
    (expression (identifier) var-exp)   
    
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)
    
    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)
    
    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)
    
    (expression
     ("(" expression expression ")")
     call-exp)
    
    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)
    
    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)
    
    (expression 
     ;; arbitrary number of unary procedures
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)
    
    (expression
     ("set" identifier "=" expression)
     set-exp)
    
    (expression
     ("spawn" "(" expression ")")
     spawn-exp)
    
    (expression 
     ("yield" "(" ")")
     yield-exp)
    
    (expression
     ("mutex" "(" ")")
     mutex-exp)
    
    (expression
     ("wait" "(" expression ")")
     wait-exp)
    
    (expression
     ("signal" "(" expression ")")
     signal-exp)
    
    ;; other unary operators
    
    (expression
     (unop "(" expression ")")
     unop-exp)
    
    (unop ("car") car-unop)
    (unop ("cdr") cdr-unop)
    (unop ("null?") null?-unop)
    (unop ("zero?") zero?-unop)
    (unop ("print") print-unop)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

