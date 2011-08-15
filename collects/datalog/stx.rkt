#lang racket/base
(require (for-syntax racket/base
                     syntax/parse
                     racket/local
                     racket/function
                     racket/list)
         racket/list
         racket/match
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

(define (->substitutions sel ls)
  (if (void? ls) empty
      (map sel ls)))

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
         (void)
         (->substitutions
          (datalog-stmt-var-selector stmt)
          (eval-statement (datalog-stmt stmt)))
         ...))]))

(define-syntax (datalog! stx)
  (syntax-case stx ()
    [(_ thy-expr stmt ...)
     (syntax/loc stx
       (parameterize ([current-theory thy-expr])
         (void)
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

(define-syntax (datalog-stmt-var-selector stx)
  (syntax-parse 
   stx
   #:literals (! ~ ?)
   [(_ (~and tstx (! c)))
    (quasisyntax/loc #'tstx (λ (l) (hasheq)))]
   [(_ (~and tstx (~ c)))
    (quasisyntax/loc #'tstx (λ (l) (hasheq)))]
   [(_ (~and tstx (? l)))
    (quasisyntax/loc #'tstx (datalog-literal-var-selector l))]))

(define-syntax (datalog-clause stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ (~and tstx (:- head body ...)))
    (local [(define (datalog-literal-variables stx)
              (syntax-parse 
               stx
               #:literals (:-)
               [sym:id
                empty]
               [(~and tstx (sym:id arg ... :- ans ...))
                (append-map datalog-term-variables
                            (syntax->list #'(arg ... ans ...)))]
               [(~and tstx (sym:id e ...))
                (append-map datalog-term-variables
                            (syntax->list #'(e ...)))]))
            (define (datalog-term-variables stx)
              (syntax-parse 
               stx
               [sym:id
                (cond
                  [(identifier-binding #'sym 0)
                   empty]
                  [(char-upper-case? (string-ref (symbol->string (syntax->datum #'sym)) 0))
                   (list #'sym)]
                  [else
                   empty])]
               [sym:expr
                empty]))
            (define head-vars (datalog-literal-variables #'head))
            (define body-vars 
              (append-map datalog-literal-variables (syntax->list #'(body ...))))
            (define body-vars-in-head
              (filter
               (λ (bv)
                 (findf (curry bound-identifier=? bv)
                        head-vars))
               body-vars))
            (define fake-lam
              (quasisyntax/loc #'tstx
                (lambda #,head-vars
                  (void #,@body-vars-in-head))))]
      (syntax-local-lift-expression
       fake-lam))
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

(define-syntax (datalog-literal-var-selector stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ sym:id)
    (quasisyntax/loc #'sym (λ (l) (hasheq)))]
   [(_ (~and tstx (sym:id arg ... :- ans ...)))
    (quasisyntax/loc #'tstx 
      (match-lambda
        [(external _srcloc _predsym _pred args anss)
         (terms->hasheq (list (datalog-term arg) ...
                              (datalog-term ans) ...)
                        (append args anss))]))]
   [(_ (~and tstx (sym:id e ...)))
    (quasisyntax/loc #'tstx
      (match-lambda
        [(literal _srcloc _predsym ts)
         (terms->hasheq (list (datalog-term e) ...)
                        ts)]))]))

(define (terms->hasheq src-ts res-ts)
  (for/fold ([h (hasheq)])
    ([src (in-list src-ts)]
     [res (in-list res-ts)])
    (if (variable? src)
        (hash-set h (variable-sym src) (constant-value res))
        h)))

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
