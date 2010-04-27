(module combinator-example scheme/base

(require scheme/unit
         parser-tools/lex
         combinator-parser/combinator-unit)

(define-unit support
  (import)
  (export error-format-parameters^
          language-format-parameters^
          language-dictionary^)
  
  (define src? #t)
  (define input-type "file")
  (define show-options #f)
  (define max-depth 1)
  (define max-choice-depth 2)
  
  (define class-type "keyword")
  (define (input->output-name t) (token-name t))
  
  (define (misspelled s1 s2) 
    (and (equal? s1 "lam")
         (equal? s2 "lambda")))
  (define (misscap s1 s2) 
    (and (equal? s1 "lam")
         (equal? s2 "Lam")))
  (define (missclass s1 s2) #f)
  )

(define-signature parser^ (parse-prog))

(define-unit lambda-calc
  (import combinator-parser^)
  (export parser^)
  
  (define-simple-terminals keywords
    (lam (O_paren "(") (C_paren ")")))
  
  (define string->symbol*
    (case-lambda
      [(one) (string->symbol one)]
      [(one two three) (error 'string->symbol* "Cannot accept so many arguments")]))
  
  (define-terminals ids
    ((id "variable" string->symbol*) (number (lambda (x) (read (open-input-string x))))))
  
  (define app
    (sequence (O_paren (repeat (eta expr)) C_paren)  
              (lambda (id) id)
              "application"))
  
  (define func
    (sequence (O_paren lam O_paren (repeat id) (eta expr))
              (lambda (id) id)
              "function"))
  
  (define expr (choose (id number app func) "expression"))
  
  (define parse-prog (parser expr))
  )
  
  )
