#lang racket
(require redex)
(require "match.rkt")
(require racket/set)
(require profile)

(provide test-red-rel
         test-non-term)

(define lit-table (make-hash))
(define or-table (make-hash))
(define nt-table (make-hash))

(define set-from-list
  (λ (lst)
    (apply set lst)))

(define-struct nt-struct (match-bool match-set))

#;(define lang '(define-language T
                (n (number_1 ... number_1 ...))
                (m ((number_1 number_1) ...))
                (o ((number_1 ... number_1 ...) ...))
                (q ((number_1 ... number_1 ...) ... (number_1 ... number_1 ...) ...))
                ))

#;(define lang '(define-language T
                (e (e e ...) (if0 e e e) x v)
                (v (λ (x ...) e) number * +)
                (E (v ... E e ...) (if0 E e e) hole)
                (x (variable-except if0 λ * +)))
  )

#;(define lang-rr
    '(reduction-relation T
                         (--> (in-hole C (and false B))
                              (in-hole C false)
                              )
                         (--> (in-hole C (and true B))
                              (in-hole C B)
                              )
                         (--> (in-hole C (or false B))
                              (in-hole C B))
                         (--> (in-hole C (or true B))
                              (in-hole C true))
                         ))

#;(define-metafunction λv
    subst-vars : ((x any) ... any) -> any
    [(subst-vars ((x_1 any_1) x_1)) any_1]
    [(subst-vars ((x_1 any_1) (any_2 ...)))
     ((subst-vars ((x_1 any_1) any_2)) ...)]
    [(subst-vars (side-condition
                  ((x_1 any_1) any_2)
                  (not (eq? (term x_1) (term any_2)))))
     any_2]
    [(subst-vars ((x_1 any_1) (x_2 any_2) ... any_3))
     (subst-vars ((x_1 any_1) (subst-vars ((x_2 any_2) ... any_3))))]
    [(subst-vars (any)) any])

(define lang-rr
  '(reduction-relation 
    λv
    (--> ((x_1 any_1) x_1) 
         any_1)
    (--> ((x_1 any_1) (any_2 ...))
         ,(map subst-vars (term (((x_1 any_1) any_2) ...))) )
    (--> (side-condition
          ((x_1 any_1) any_2)
          (and (not (list? any_2)) 
               (not (eq? (term x_1) (term any_2)))))
         any_2)
    (--> ((x_1 any_1) (x_2 any_2) (x_3 any_3) ... any_4)
         ,(subst-vars (term ((x_1 any_1) ,(subst-vars (term ((x_2 any_2) (x_3 any_3) ... any_4)))))))
    (--> (any)
         any)
    )
  )


#;(define-metafunction λv 
    subst : (x any any) -> any
    ;; 1. x_1 bound, so don't continue in λ body  
    [(subst (x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2)))
     (λ (x_2 ... x_1 x_3 ...) any_2)]
    ;; 2. general purpose capture avoiding case  
    [(subst (side-condition
             (x_1 any_1 (λ (x_2 ...) any_2))
             (not (memq (term x_1) (term (x_2 ...))))))
     ,(term-let ([(x_new ...) (variables-not-in (term (x_1 any_1 any_2)) (term (x_2 ...)))])
                (term
                 (λ (x_new ...) (subst (x_1 any_1 (subst-vars ((x_2 x_new) ... any_2)))))))]  
    ;; 3. replace x_1 with e_1  
    [(subst (x_1 any_1 x_1)) any_1]
    ;; 4. x_1 and x_2 are different, so don't replace  
    [(subst (side-condition
             (x_1 any_1 x_2)
             (not (eq? (term x_1) (term x_2))))) 
     x_2]
    ;; the last cases cover all other expressions  
    [(subst (x any (e_1 e_2 ...)))
     (,(subst (x any e_1)) (subst (x any e_2)) ...)]
    
    [(subst (x any (if0 e_1 e_2 e_3)))
     (if0 (subst (x any e_1)) (subst (x any e_2)) (subst (x any e_3)))]
    [(subst (x any number)) number]
    [(subst (x any +)) +]
    [(subst (x any *)) *])

#;(define lang-rr 
    '(reduction-relation
      λv
      (--> (x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
           (λ (x_2 ... x_1 x_3 ...) any_2))
      (--> (side-condition
            (x_1 any_1 (λ (x_2 ...) any_2))
            (not (memq (term x_1) (term (x_2 ...)))))
           ,(term-let ([(x_new ...) (variables-not-in (term (x_1 any_1 any_2)) (term (x_2 ...)))])
                      (term
                       (λ (x_new ...) ,(subst (term (x_1 any_1 ,(subst-vars (term ((x_2 x_new) ... any_2))))) )))) )
      (--> (x_1 any_1 x_1) 
           any_1)
      (--> (side-condition
            (x_1 any_1 x_2)
            (not (eq? (term x_1) (term x_2))))
           x_2)
      (--> (x any (e_1 e_2 ...))
           (,(subst (term (x any e_1))) ,@(map subst (term ((x any e_2) ...)))) )
      (--> (x any (if0 e_1 e_2 e_3))
           (if0 ,(subst (term (x any e_1))) ,(subst (term (x any e_2))) ,(subst (term (x any e_3)))))
      (--> (x any number)
           number)
      (--> (x any +)
           +)
      (--> (x any *)
           *)
      )
    )

#;(define-metafunction λv 
    subst-n : ((x any) ... any) -> any
    [(subst-n ((x_1 any_1) (x_2 any_2) ... any_3))
     (subst (x_1 any_1 (subst-n (x_2 any_2) ... any_3)))]
    [(subst-n (any_3)) any_3])

#;(define lang-rr '(reduction-relation 
                    λv
                    (--> ((x_1 any_1) (x_2 any_2) ... any_3)
                         ,(subst (term (x_1 any_1 ,(subst-n (term ((x_2 any_2) ... any_3)))))))
                    (--> (any_3)
                         any_3)
                    ) 
    )

#;(define lang-rr '(reduction-relation 
                    λv
                    (--> (in-hole E (* number_1 number_2)) 
                         (in-hole E ,(* (term number_1) (term number_2))))
                    (--> (in-hole E (+ number_1 number_2)) 
                         (in-hole E ,(+ (term number_1) (term number_2))))
                    (--> (in-hole E (if0 0 e_1 e_2))
                         (in-hole E e_1))
                    (--> (in-hole E (if0 (side-condition number_1 (not (zero? (term number_1)))) e_1 e_2))
                         (in-hole E e_2))
                    (--> (in-hole E (side-condition ((λ (x ...) e) v ...)
                                                    (= (length (term (x ...)))
                                                       (length (term (v ...))))))
                         (in-hole E ,(subst-n (term ((x v) ... e)))))) 
    )

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
     (map (λ (x) (extract-literals/pat nts (cdr x) lit-table))
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
     (map (λ (x) (hash-set! or-table (car x) (build-or (cdr x) nts (hash-map lit-table (λ (x y) x)) #t)))
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
      ; for now if it has an underscore assume it's a non-terminal
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
      [`(side-condition ,p #;,g ,e)
       `(lit-side-condition ,(loop p) ,e)]
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
           ;(printf "~a: ~a\n" key `(term (detect-hole2 0 ,val)))
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
       (let ((compiled-bool (car (apply-reduction-relation* red
                                                            (term (matrix (a)
                                                                          (
                                                                           ((,val -> (set! results (cons #t results))) ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,val () ())))))
                                                                           )
                                                                          ()
                                                                          ()
                                                                          0
                                                                          #f)))))
             (compiled-set (car (apply-reduction-relation* red 
                                                           (term (matrix (a)
                                                                         ( ,@(map (λ (x) 
                                                                                    (let ((row (wrap-production-with-name x))) 
                                                                                      `((,row -> ,(build-right-hand-side row)) 
                                                                                        
                                                                                        ,@(map (λ (x) (list x #f)) (remove-duplicates (term (Get-Free-Name-Patterns ,row () ())))))))
                                                                                  (make-or-list val)))
                                                                         ()
                                                                         ()
                                                                         0
                                                                         #f))))
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

;; make-lang-namespace: sexp[lang] -> namespace
(define (make-lang-namespace lang)
  (define lang-defs (compile-dl lang))
  (define namespace (namespace-anchor->namespace here))
  (for-each (curryr eval namespace) lang-defs)
  namespace)

;; test-red-rel: sexp[lang] -> sexp[red-rel] (listof sexp[nts]) (listof symbol) -> sexp[term] -> sexp[term]
(define (test-red-rel lang)
  (define namespace (make-lang-namespace lang))
  (λ (rel nts syms)
    (eval (compile-reduction-relation rel nts syms) namespace)))

;; sexp[lang] -> sexp[non-terminal] -> sexp[term] -> boolean
(define (test-non-term lang)
  (define namespace (make-lang-namespace lang))
  (λ (nt)
    (eval `(λ (t) (,(string->symbol (format "~s-bool" nt)) t)) namespace)))

(define-language T
  (B true
     false
     (and B B))
  (C (and C B)
     hole))

(define bool-red-test
  (reduction-relation T
                      (--> (in-hole C (and false B))
                           (in-hole C false)
                           )
                      (--> (in-hole C (and true B))
                           (in-hole C B)
                           )))             

(caching-enabled? #f)

(define ∅ #f)
(define ∪ set-union)
(define singleton set)

(define no-context #f)

(define in-context #t)

(define context-match (make-parameter no-context))

(define rev (λ (x) 
              (if (cons? x)
                  (reverse x)
                  x)))
(define or-fun (λ (x ...) (or x ...)))

(define sing-fun (λ (x) #t))

(define the-hole (term hole))
