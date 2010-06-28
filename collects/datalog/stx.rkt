#lang racket
(require (for-syntax syntax/parse)
         datalog/ast
         datalog/eval)

(define-syntax (:- stx)
  (raise-syntax-error ':- "only allowed inside ! and ~" stx))
(define-syntax (! stx)
  (raise-syntax-error '! "only allowed inside datalog" stx))
(define-syntax (~ stx)
  (raise-syntax-error '~ "only allowed inside datalog" stx))
(define-syntax (? stx)
  (raise-syntax-error '? "only allowed inside datalog" stx))

(define ->answer
  (match-lambda
    [(? void?)
     empty]
    [(? list? ls)
     (map literal->sexp ls)]))

(define literal->sexp
  (match-lambda
    [(literal _ pred ts)
     (list* pred (map term->datum ts))]))

(define term->datum
  (match-lambda
    [(constant _ v)
     v]))

(define-syntax (datalog stx)
  (syntax-case stx ()
    [(_ thy-expr stmt ...)
     (syntax/loc stx
       (parameterize ([current-theory thy-expr])
         (->answer (eval-statement (datalog-stmt stmt)))
         ...))]))

(define-syntax (datalog! stx)
  (syntax-case stx ()
    [(_ thy-expr stmt ...)
     (syntax/loc stx
       (parameterize ([current-theory thy-expr])
         (eval-top-level-statement (datalog-stmt stmt))
         ...))]))

(define-syntax (datalog-stmt stx)
  (syntax-parse 
   stx
   #:literals (! ~ ?)
   [(_ (! c))
    (quasisyntax/loc stx
      (assertion #'#,stx (datalog-clause c)))]
   [(_ (~ c))
    (quasisyntax/loc stx
      (retraction #'#,stx (datalog-clause c)))]
   [(_ (? l))
    (quasisyntax/loc stx
      (query #'#,stx (datalog-literal l)))]))

(define-syntax (datalog-clause stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ (:- head body ...))
    (quasisyntax/loc stx
      (clause #'#,stx (datalog-literal head) 
              (list (datalog-literal body) ...)))]
   [(_ e)
    (quasisyntax/loc stx
      (clause #'#,stx (datalog-literal e) empty))]))

(define-syntax (datalog-literal stx)
  (syntax-parse 
   stx
   [(_ sym:id)
    (quasisyntax/loc stx
      (literal #'#,stx 'sym empty))]
   [(_ (sym:id e ...))
    (quasisyntax/loc stx
      (literal #'#,stx 'sym 
               (list (datalog-term e)
                     ...)))]))

(define-syntax (datalog-term stx)
  (syntax-parse 
   stx
   [(_ sym:str)
    (quasisyntax/loc stx
      (constant #'#,stx 'sym))]
   [(_ sym:id)
    (cond
      [(identifier-binding #'sym 0)
       (quasisyntax/loc stx
         (constant #'#,stx sym))]
      [(char-upper-case? (string-ref (symbol->string (syntax->datum #'sym)) 0))
       (quasisyntax/loc stx
         (variable #'#,stx 'sym))]
      [else
       (quasisyntax/loc stx
         (constant #'#,stx 'sym))])]))

(provide datalog datalog!
         :- ! ~ ?)
