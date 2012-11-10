#lang racket/base

(require (except-in racket/base compile)
         racket/function
         racket/match)
(require (except-in redex make-bind plug))
(require "match.rkt")
(require racket/set)
(require profile)
(require (only-in "../../private/matcher.rkt"
                  make-bindings
                  make-bind
                  make-mtch
                  build-flat-context
                  the-hole
                  )
         racket/list)

(define plug (λ (x y)
               (cond 
                 ((cons? x)
                  (cons (plug (car x) y) (plug (cdr x) y)))
                 ((or (equal? x the-hole) (equal? x (term hole)))
                  y)
                 (else x))))

(provide test-red-rel
         test-non-term
         test-redex-match)

; holds symbols for language
(define lit-table (make-hash))

; holds or pattern versions of the input
(define or-table (make-hash))

; holds compiled code 
(define nt-table (make-hash))

(define-struct nt-struct (match-bool match-set))

(define (compile-define-language-nts dl)
  (match dl
    [`(define-language ,(? symbol? name)
        ,non-terms ...)
     (map (λ (x) (car x))
          non-terms)]
    [_
     'error]))

(define (compile-define-language-lit dl nts)
  (match dl
    [`(define-language ,(? symbol? name)
        ,non-terms ...)
     (for-each (λ (x) (extract-literals/pat nts (cdr x) lit-table))
               non-terms)]
    [_
     'error]))

(define (extract-literals/pat nts pat ht)
  (let loop ([pat pat])
    (match pat
      [`any (void)]
      [`number (void)]
      [`string (void)]
      [`natural (void)]
      [`integer (void)]
      [`real (void)]
      [`variable (void)]
      [`(variable-except ,s ...) (void)]
      [`(variable-prefix ,s) (void)]
      [`variable-not-otherwise-mentioned (void)]
      [`hole (void)]
      [(? symbol? s) 
       (unless (regexp-match #rx"_" (symbol->string s))
         (unless (regexp-match #rx"^\\.\\.\\." (symbol->string s))
           (unless (memq s nts)
             (hash-set! ht s #t)
             )))]
      [`(name ,name ,pat) (loop pat)]
      [`(in-hole ,p1 ,p2) 
       (loop p1)
       (loop p2)]
      [`(hide-hole ,p) (loop p)]
      [`(side-condition ,p ,g ,e)
       (loop p)]
      [`(cross ,s) (void)]
      [_
       (let l-loop ([l-pat pat])
         (when (pair? l-pat) 
           (loop (car l-pat))
           (l-loop (cdr l-pat))))])))

(define (compile-define-language-or dl nts)
  (match dl
    [`(define-language ,(? symbol? name)
        ,non-terms ...)
     (for-each (λ (x) (hash-set! or-table (car x) (build-or (cdr x) nts (hash-map lit-table (λ (x y) x)) #t)))
          non-terms)]
    [_
     'error]))

(define (build-or pat-l nts syms dl)
  (let loop ([pat-l pat-l]
             [nts nts]
             [syms syms])
    (if (pair? pat-l)
        (if (eqv? (length pat-l) 1)
            (translate-redex (car pat-l) nts syms dl)
            `(or ,(translate-redex (car pat-l) nts syms dl)
                 ,(loop (cdr pat-l) nts syms)))
        (translate-redex pat-l nts syms dl))))

(define (translate-redex pat nts syms dl)
  (let loop ([pat pat])
    (match pat
      [`(,repeated ,(? (λ (x) (eq?  x '...)) ep))
       `(repeat ,(loop repeated) '())]
      [`(,repeated ,(? (λ (x) (eq?  x '...)) ep) ,next ...)
       `(repeat ,(loop repeated) ,(loop next))]
      [`any (if dl 'wc '(lit-name any wc))]
      [`number (if dl 'lit-number '(lit-name number lit-number))]
      [`string (if dl 'lit-string '(lit-name string lit-string))]
      [`natural (if dl 'lit-natural '(lit-name natural lit-natural))]
      [`integer (if dl 'lit-integer '(lit-name integer lit-integer))]
      [`real (if dl 'lit-real '(lit-name real lit-real))]
      [`variable (if dl 'lit-variable '(lit-name variable lit-variable))]
      [`(variable-except ,s ...) `(lit-variable-except ,@s)]
      [`(variable-prefix ,s) `(lit-variable-prefix ,s)]
      [`variable-not-otherwise-mentioned (if dl `(lit-variable-except ,@syms) `(lit-name variable-not-otherwise-mentioned (lit-variable-except ,@syms)))]
      [`hole 'lit-hole]
      [(? symbol? s)
       (if (memq s nts) 
           (if dl
               `(nt ,s)
               `(lit-name ,s (nt ,s)))
           (if (has-underscore? s)
               (let ((split (split-underscore s)))
                 (cond
                   ((equal? split 'any) `(lit-name ,s wc))
                   ((equal? split 'number) `(lit-name ,s lit-number))
                   ((equal? split 'string) `(lit-name ,s lit-string))
                   ((equal? split 'natural) `(lit-name ,s lit-natural))
                   ((equal? split 'integer) `(lit-name ,s lit-integer))
                   ((equal? split 'real) `(lit-name ,s lit-real))
                   ((equal? split 'variable) `(lit-name ,s lit-variable))
                   ((equal? split 'variable-not-otherwise-mentioned) `(lit-name ,s (lit-variable-except ,@syms)))
                   ((equal? split 'hole) `(lit-name ,s lit-hole))
                   (else `(lit-name ,s (nt ,split)))
                   )
                 )
               `',s))]
      [`(name ,name ,pat) `(lit-name ,name ,(loop pat))]
      [`(in-hole ,p1 ,p2) 
       `(lit-in-hole
         ,(loop p1)
         ,(loop p2))]
      [`(hide-hole ,p) `(lit-hide-hole ,(loop p))]
      [`(side-condition ,p ,e)
       `(lit-side-condition ,(loop p) ,e)]
      [`(side-condition ,p ,e ,e2)
       `(lit-side-condition ,(loop p) (,e ,e2))]
      [`(cross ,s) (void)]
      [e
       (if (pair? pat) 
           `(cons ,(loop (car pat))
                  ,(loop (cdr pat)))
           (if (empty? pat)
               ''()
               e))]
      )))

;; split-underscore : symbol -> symbol
;; returns the text before the underscore in a symbol (as a symbol)
;; raise an error if there is more than one underscore in the input
(define (split-underscore sym)
  (let ([str (symbol->string sym)])
    (cond
      [(regexp-match #rx"^([^_]*)_[^_]*$" str)
       =>
       (λ (m) (string->symbol (cadr m)))]
      [(regexp-match #rx"^([^_]*)_!_[^_]*$" str)
       =>
       (λ (m) (string->symbol (cadr m)))]
      [else
       (error 'compile-pattern "found a symbol with multiple underscores: ~s" sym)])))

;; has-underscore? : symbol -> boolean
(define (has-underscore? sym)
  (memq #\_ (string->list (symbol->string sym))))

(define build-hole-table
  (λ (old-ht)
    (unless (equal? hole-table old-ht)
      (let ((prev (make-hash)))
        (hash-for-each
         hole-table
         (λ (key val)
           (hash-set! prev key val)))
        (hash-for-each
         or-table
         (λ (key val)
           (hash-set! hole-table key (term (detect-hole2 0 ,val)))))
        (build-hole-table prev))))
  )

(define state '())
(define hole-var '())
(define nt-func '())

(define wrap-production-with-name
  (λ (x)
    (set! state x)
    (set! hole-var '())
    (set! nt-func '())
    (wrap-production-with-name-helper x)
    )
  )

(define wrap-production-with-name-helper
  (λ (exp)
    (match exp
      [`(cons ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p1)
              ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p2))
       `(cons ,(wrap-production-with-name-helper p1) ,(wrap-production-with-name-helper p2))]
      [`(cons ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p1)
              ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p2))
       `(cons ,(wrap-production-with-name-helper p1) ,(wrap-production-with-name-helper p2))]
      [`(cons ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p1)
              ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p2))
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         `(lit-name ,x (cons ,p1 ,p2)))]
      [`(lit-name ,id ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p))
       `(lit-name ,id ,(wrap-production-with-name-helper p))]
      [`(lit-name ,id ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p))
       `(lit-name ,id ,p)]
      [`(lit-side-condition ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p) ,any)
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         `(lit-name ,x (lit-side-condition ,p ,any))
         )]
      [`(lit-side-condition ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p) ,any)
       (wrap-production-with-name-helper p)]
      [`(repeat ,p1 ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p2))
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         `(lit-name ,x (repeat ,p1 ,p2))
         )]
      [`(repeat ,p1 ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p2))
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         `(repeat (lit-name ,x ,p1) ,(wrap-production-with-name-helper p2))
         )]
      [`(lit-in-hole ,p1 ,(? (λ (x) (eqv? 0 (term (detect-hole2 0 ,x)))) p2))
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         `(lit-name ,x (lit-in-hole ,p1 ,p2))
         )]
      [`(lit-in-hole ,p1 ,(? (λ (x) (eqv? 1 (term (detect-hole2 0 ,x)))) p2))
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         (let ((y (variable-not-in state 'y)))
           (set! hole-var y)
           `(lit-in-hole (lit-name ,x ,p1) (lit-name ,y ,p2))
           )
         )]
      ['lit-hole
       (let ((x (variable-not-in state 'x)))
         (set! state (cons x state))
         (set! hole-var x)
         `(lit-name ,x lit-hole)
         )]
      [`(nt ,id)
       (if (eqv? 1 (term (detect-hole2 0 (nt ,id))))
           (let ((x (variable-not-in state 'x)))
             (set! state (cons x state))
             (set! hole-var x)
             (set! nt-func id)
             `(lit-name ,x wc)
             )
           (let ((x (variable-not-in state 'x)))
             (set! state (cons x state))
             `(lit-name ,x (nt ,id))
             )
           )]
      [`',(? (λ (x) (or (symbol? x) (equal? '() x))) ex)
       exp]
      [_ (if (or (number? exp) (string? exp) (boolean? exp))
             exp
             (let ((x (variable-not-in state 'x)))
               (set! state (cons x state))
               `(lit-name ,x ,exp)
               ))])
    )
  )

(define build-right-hand-side
  (λ (x)
    (if (equal? nt-func '())
        (if (equal? hole-var '()) 
            `(set! results (cons (cons ,(build-right-hand-side-helper x) '()) results))
            `(set! results (cons (cons ,(build-right-hand-side-helper x) (term ,hole-var)) results))
            )
        `(for ((,hole-var (in-list (,(string->symbol (format "~s~s" nt-func '-list)) (term ,hole-var)))))
              (set! results (cons (cons ,(build-right-hand-side-helper x) (cdr ,hole-var)) results))
              )
        )
    )
  )

(define build-right-hand-side-helper
  (λ (exp)
    (match exp
      [`(lit-name ,(? (λ (x) (not (equal? hole-var x))) x) ,p)
       (if (or (not (cons? p)) (and (cons? p) (not (equal? (car p) 'lit-name))))
           `(term ,x)
           (build-right-hand-side-helper p))]
      [`(lit-name ,(? (λ (x) (equal? hole-var x)) x) lit-hole)
       'the-hole]
      [`(lit-name ,(? (λ (x) (equal? hole-var x)) x) wc)
       `(car ,hole-var)]
      [`(lit-name ,(? (λ (x) (equal? hole-var x)) x) ,p)
       `(term ,x)]
      [`(cons ,p1 ,p2)
       `(cons ,(build-right-hand-side-helper p1) ,(build-right-hand-side-helper p2))]
      [`(lit-in-hole ,p1 ,p2)
       (build-right-hand-side-helper p1)]
      [`(repeat ,p1 ,p2)
       `(append ,(build-right-hand-side-helper p1) ,(build-right-hand-side-helper p2))]
      [_ 
       exp]
      )))

(define make-or-list
  (λ (exp)
    (match exp
      [`(or ,pro1 ,pro2)
       (cons pro1 (make-or-list pro2))]
      [_ (cons exp '())]
      )
    )
  )

(define-namespace-anchor here)

;; compile-dl : sexp[lang] -> (listof sexp[def])
(define (compile-dl lang)
  (let* ([lang lang]
         [nts (compile-define-language-nts lang)]
         [lit-table lit-table]
         [or-table or-table]
         [nt-table nt-table]
         [hole-table hole-table])
    
    
    (compile-define-language-lit lang nts)
    (compile-define-language-or lang nts)
    (caching-enabled? #f)
    ; Initialize the hole table
    (hash-for-each 
     or-table 
     (λ (key val)
       (hash-set! hole-table key (term (detect-hole 0 ,val)))
       ))
    
    (build-hole-table '())
    (caching-enabled? #t)
    
    (hash-for-each
     or-table
     (λ (key val)
       (let ((compiled-bool (begin #;(printf "~a\n\n" (term (matrix (a)
                                                                  (
                                                                   ((,val -> (set! results (cons #t results))) ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,val () ())))))
                                                                   )
                                                                  ()
                                                                  ()
                                                                  0
                                                                  #f))) 
                                   (car (apply-reduction-relation* red
                                                                   (term (matrix (a)
                                                                                 (
                                                                                  ((,val -> (set! results (cons #t results))) ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,val () ())))))
                                                                                  )
                                                                                 ()
                                                                                 ()
                                                                                 0
                                                                                 #f))))
                                   )
                            )
             (compiled-set (begin #;(traces red (term (matrix (a)
                                                                 ( ,@(map (λ (x) 
                                                                            (let ((row (wrap-production-with-name x))) 
                                                                              `((,row -> ,(build-right-hand-side row)) 
                                                                                
                                                                                ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,row () ())))))))
                                                                          (make-or-list val)))
                                                                 ()
                                                                 ()
                                                                 0
                                                                 #f)))
                                  (match (apply-reduction-relation* red 
                                                                  (term (matrix (a)
                                                                                ( ,@(map (λ (x) 
                                                                                           (let ((row (wrap-production-with-name x))) 
                                                                                             `((,row -> ,(build-right-hand-side row)) 
                                                                                               
                                                                                               ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,row () ())))))))
                                                                                         (make-or-list val)))
                                                                                ()
                                                                                ()
                                                                                0
                                                                                #f)))
                                    ((list x) x))
                                  )
                           ))
         (hash-set! nt-table
                    key
                    (make-nt-struct
                     (term (define ,(string->symbol (format "~s~s" key '-bool))
                             (λ (a)
                               (let ((results (list)))
                                 ,compiled-bool
                                 (and (andmap values results) (positive? (length results)))))))
                     (term (define ,(string->symbol (format "~s~s" key '-list))
                             (λ (a)
                               (let ((results (list)))
                                 ,compiled-set
                                 results)))))))))
    
    (append (hash-map nt-table (λ (_ n) (nt-struct-match-bool n)))
            (hash-map nt-table (λ (_ n) (nt-struct-match-set n))))))

; compile-reduction-relation: sexp[reduction-relation] (listof symbol[non-terminals]) (listof symbols) -> sexp[def]
(define (compile-reduction-relation rr nts syms)
  `(λ (a)
     (let ([results '()])
       ,(car
         (apply-reduction-relation* 
          red
          (let loop ([e rr])
            (match e
              [`(reduction-relation ,L ,rules ...)
               (term (matrix (a) ,(map loop rules) () () 0 #f))]
              [`(--> ,pat ,t)
               (let ((p (translate-redex pat nts syms #f)))
                 `((,p -> (set! results (cons (term ,t) results)))
                   ,@(map (λ (x) (list x #f)) 
                          (remove-duplicates (term (Get-Free-Name-Patterns ,p () ()))))))]))))
       results)))

(define (make-test-mtch a b c) (make-mtch a (build-flat-context b) c))

; compile-redex-match: sexp[pattern] (listof symbol[non-terminals]) (listof symbols) -> sexp[def]
(define (compile-redex-match pat nts syms)
  ; prints for debuging
  (printf "~a\n\n"
          `(matrix (a) (,(let ((p (translate-redex pat nts syms #f)))
                           `((,p -> 
                                 (set! results (cons (make-test-mtch (make-bindings (list ,@(map (λ (x) `(make-bind ',(string->symbol (format "~s" (term (Get-Pvar ,x)))) (term ,x))) 
                                                                                                 (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))) ))
                                                                     a
                                                                     'none)
                                                     results))
                                 ) 
                             ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))))
                           )
                        ) () () 0 #f)
          )
  (printf "~a\n\n" 
          (apply-reduction-relation* 
           red
           `(matrix (a) (,(let ((p (translate-redex pat nts syms #f)))
                            `((,p -> 
                                  (set! results (cons (make-test-mtch (make-bindings (list ,@(map (λ (x) `(make-bind ',(string->symbol (format "~s" (term (Get-Pvar ,x)))) (term ,x))) 
                                                                                                  (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))) ))
                                                                      a
                                                                      'none)
                                                      results))
                                  ) 
                              ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))))
                            )
                         ) () () 0 #f))
          )
  `(λ (a)
     (let ([results '()])
       ,(begin
          (car
           (apply-reduction-relation* 
            red
            `(matrix (a) (,(let ((p (translate-redex pat nts syms #f)))
                             `((,p -> 
                                   (set! results (cons (make-test-mtch (make-bindings (list ,@(map (λ (x) `(make-bind ',(string->symbol (format "~s" (term (Get-Pvar ,x)))) (term ,x))) 
                                                                                                   (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))) ))
                                                                       a
                                                                       'none)
                                                       results))
                                   ) 
                               ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,p () ())))))
                             )
                          ) () () 0 #f)
            )))
       results)))

;; make-lang-namespace: sexp[lang] -> namespace
(define (make-lang-namespace lang)
  (define lang-defs (compile-dl lang))
  (define namespace (namespace-anchor->namespace here))
  #;(pretty-print lang-defs)
  (for-each (curryr eval namespace) lang-defs)
  namespace)

;; test-red-rel: sexp[lang] -> sexp[red-rel] (listof sexp[nts]) (listof symbol) -> sexp[term] -> sexp[term]
(define (test-red-rel lang)
  (define namespace (make-lang-namespace lang))
  (define nts (compile-define-language-nts lang))
  (define syms (compile-define-language-lit lang nts))
  (λ (rel)
    (eval (compile-reduction-relation rel nts syms) namespace)))

;; test-red-match: sexp[lang] -> sexp[pat] (listof sexp[nts]) (listof symbol) -> sexp[term] -> sexp[term]
(define (test-redex-match lang)
  (define namespace (make-lang-namespace lang))
  (define nts (compile-define-language-nts lang))
  (define syms (compile-define-language-lit lang nts))
  (λ (pat)
    #;(pretty-print (compile-redex-match pat nts syms))
    (eval (compile-redex-match pat nts syms) namespace)))

;; sexp[lang] -> sexp[non-terminal] -> sexp[term] -> boolean
(define (test-non-term lang)
  (define namespace (make-lang-namespace lang))
  (λ (nt)
    (eval `(λ (t) (,(string->symbol (format "~s-bool" nt)) t)) namespace)))        

(caching-enabled? #f)

(define ∅ #f)
(define ∪ set-union)
(define singleton set)

(define natural?
  (λ (x) (and 
          (exact-integer? x) 
          (not (negative? x)))))

(define (variable-prefix? x y) 
  (let* ([prefix-str (symbol->string x)]
         [prefix-len (string-length prefix-str)])
    (and (symbol? y)
         (let ([str (symbol->string y)])
           (and ((string-length str) . >= . prefix-len)
                (string=? (substring str 0 prefix-len) prefix-str))))))

(define no-context #f)

(define in-context #t)

(define context-match (make-parameter no-context))

(define rev (λ (x) 
              (if (cons? x)
                  (reverse x)
                  x)))
