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
    [(external _ pred-sym _ args anss)
     `(,pred-sym ,@(map term->datum args)
                 :-
                 ,@(map term->datum anss))]
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
   [(_ (~and tstx (! c)))
    (quasisyntax/loc #'tstx
      (assertion #'#,#'tstx (datalog-clause c)))]
   [(_ (~and tstx (~ c)))
    (quasisyntax/loc #'tstx
      (retraction #'#,#'tstx (datalog-clause c)))]
   [(_ (~and tstx (? l)))
    (quasisyntax/loc #'tstx
      (query #'#,#'tstx (datalog-literal l)))]))

(define-syntax (datalog-clause stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ (~and tstx (:- head body ...)))
    (quasisyntax/loc #'tstx
      (clause #'#,#'tstx (datalog-literal head) 
              (list (datalog-literal body) ...)))]
   [(_ e)
    (quasisyntax/loc #'e
      (clause #'#,#'e (datalog-literal e) empty))]))

(define-syntax (datalog-literal stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ sym:id)
    (quasisyntax/loc #'sym
      (literal #'#,#'sym 'sym empty))]
   [(_ (~and tstx (sym:id arg ... :- ans ...)))
    (quasisyntax/loc #'tstx
      (external #'#,#'tstx 'sym sym
                (list (datalog-term arg) ...)
                (list (datalog-term ans) ...)))]
   [(_ (~and tstx (sym:id e ...)))
    (quasisyntax/loc #'tstx
      (literal #'#,#'tstx 'sym 
               (list (datalog-term e)
                     ...)))]))

(define-syntax (datalog-term stx)
  (syntax-parse 
   stx
   [(_ sym:id)
    (cond
      [(identifier-binding #'sym 0)
       (quasisyntax/loc #'sym
         (constant #'#,#'sym sym))]
      [(char-upper-case? (string-ref (symbol->string (syntax->datum #'sym)) 0))
       (quasisyntax/loc #'sym
         (variable #'#,#'sym 'sym))]
      [else
       (quasisyntax/loc #'sym
         (constant #'#,#'sym 'sym))])]
   [(_ sym:expr)
    (quasisyntax/loc #'sym
      (constant #'#,#'sym sym))]))

(provide datalog datalog!
         :- ! ~ ?)
