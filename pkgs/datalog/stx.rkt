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
     (list* (if '(predicate-sym? pred)
                '(predicate-sym-sym pred)
                pred)
            (map term->datum ts))]))

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
      (assertion #,(srcloc-list #'tstx) (datalog-clause c)))]
   [(_ (~and tstx (~ c)))
    (quasisyntax/loc #'tstx
      (retraction #,(srcloc-list #'tstx) (datalog-clause c)))]
   [(_ (~and tstx (? l)))
    (quasisyntax/loc #'tstx
      (query #,(srcloc-list #'tstx) (datalog-literal/ref l)))]))

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
      (clause #,(srcloc-list #'tstx) (datalog-literal/bind head) 
              (list (datalog-literal/ref body) ...)))]
   [(_ e)
    (quasisyntax/loc #'e
      (clause #,(srcloc-list #'e) (datalog-literal/bind e) empty))]))

(define-syntax (datalog-literal/bind stx) (datalog-literal/b stx #t))
(define-syntax (datalog-literal/ref stx) (datalog-literal/b stx #f))

(begin-for-syntax
  (define-syntax-class table-id
    #:literals (unsyntax)
    (pattern sym:id
             #:attr ref #''sym
             #:attr val #'sym)
    (pattern (unsyntax sym:expr)
             #:attr ref #'sym
             #:attr val #'sym))
  (define (datalog-literal/b stx binding?)
    (syntax-parse 
        stx
      #:literals (:-)
      [(_ sym:table-id)
       (syntax-property 
        (quasisyntax/loc #'sym
          (literal #,(srcloc-list #'sym) sym.ref empty))
        (if binding? 'disappeared-binding 'disappeared-use)
        (syntax-local-introduce #'sym))]
      [(_ (~and tstx (sym:table-id arg ... :- ans ...)))
       (quasisyntax/loc #'tstx
         (external #,(srcloc-list #'tstx) 'sym sym.val
                   (list (datalog-term arg) ...)
                   (list (datalog-term ans) ...)))]
      [(_ (~and tstx (sym:table-id e ...)))
       (syntax-property
        (quasisyntax/loc #'tstx
          (literal #,(srcloc-list #'tstx) sym.ref 
                   (list (datalog-term e)
                         ...)))
        (if binding? 'disappeared-binding 'disappeared-use)
        (syntax-local-introduce #'sym))])))

(define-syntax (datalog-literal-var-selector stx)
  (syntax-parse 
   stx
   #:literals (:-)
   [(_ sym:table-id)
    (quasisyntax/loc #'sym (λ (l) (hasheq)))]
   [(_ (~and tstx (sym:table-id arg ... :- ans ...)))
    (quasisyntax/loc #'tstx 
      (match-lambda
        [(external _srcloc _predsym _pred args anss)
         (terms->hasheq (list (datalog-term arg) ...
                              (datalog-term ans) ...)
                        (append args anss))]))]
   [(_ (~and tstx (sym:table-id e ...)))
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
   #:literals (unsyntax)
   [(_ sym:id)
    (cond
      [(identifier-binding #'sym 0)
       (quasisyntax/loc #'sym
         (constant #,(srcloc-list #'sym) sym))]
      [(char-upper-case? (string-ref (symbol->string (syntax->datum #'sym)) 0))
       (quasisyntax/loc #'sym
         (variable #,(srcloc-list #'sym) 'sym))]
      [else
       (quasisyntax/loc #'sym
         (constant #,(srcloc-list #'sym) 'sym))])]
   [(_ (unsyntax sym:expr))
    (quasisyntax/loc #'sym
      (constant #,(srcloc-list #'sym) sym))]
   [(_ sym:expr)
    (quasisyntax/loc #'sym
      (constant #,(srcloc-list #'sym) sym))]))

(define-for-syntax (srcloc-list stx)
  (define src (syntax-source stx))
  `(list ,(if (path? src)
             `(bytes->path ,(path->bytes src))
             `',src)
         ',(syntax-line stx)
         ',(syntax-column stx)
         ',(syntax-position stx)
         ',(syntax-span stx)))

(provide datalog datalog!
         :- ! ~ ?)
