#lang eopl

;; output language from the cps converter, including explicit references

(provide (all-defined-out))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define cps-out-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define cps-out-grammar
  '((cps-out-program (tfexp) cps-a-program)
    
    (simple-expression (number) cps-const-exp)
    
    (simple-expression (identifier) cps-var-exp)
    
    (simple-expression
     ("-" "(" simple-expression "," simple-expression ")")
     cps-diff-exp)
    
    (simple-expression
     ("zero?" "(" simple-expression ")")
     cps-zero?-exp)
    
    (simple-expression
     ("+" "(" (separated-list simple-expression ",") ")")
     cps-sum-exp)
    
    (simple-expression
     ("proc" "(" (arbno identifier) ")" tfexp)
     cps-proc-exp)
    
    (tfexp
     (simple-expression)
     simple-exp->exp)   
    
    (tfexp
     ("let" identifier "=" simple-expression "in" tfexp)
     cps-let-exp)   
    
    (tfexp
     ("letrec" 
      (arbno identifier "(" (arbno identifier) ")"
             "=" tfexp)
      "in"
      tfexp)
     cps-letrec-exp)
    
    (tfexp
     ("if" simple-expression "then" tfexp "else" tfexp)
     cps-if-exp)
    
    (tfexp
     ("(" simple-expression (arbno simple-expression) ")")      
     cps-call-exp)
    
    (tfexp
     ("printk" "(" simple-expression ")" ";" tfexp)
     cps-printk-exp)
    
    (tfexp
     ("newrefk" "(" simple-expression "," simple-expression ")")
     cps-newrefk-exp)
    
    (tfexp
     ("derefk" "(" simple-expression  "," simple-expression ")")
     cps-derefk-exp)
    
    (tfexp
     ("setrefk"
      "(" simple-expression "," simple-expression ")" ";"
      tfexp )
     cps-setrefk-exp)
    
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-out-show-the-datatypes
  (lambda () (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

(define cps-out-scan&parse
  (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

(define cps-just-scan
  (sllgen:make-string-scanner cps-out-lexical-spec cps-out-grammar))

