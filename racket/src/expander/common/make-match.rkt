#lang racket/base
(require (for-syntax racket/base))

(provide define-define-match)

;; Yet another pattern matcher along the lines of `syntax-rules`, but
;; intended for relatively simple and small patterns.
;;
;; The `define-match` form generated here has the following syntax to
;; match the result of <s-expr> against <pattern>:
;;
;;  (define-match <m-id> <s-expr> <guard> <try> '<pattern>)
;;
;;   <guard> = <epsilon> | #:when <expr> | #:unless <expr>
;;   <try>   = <epsilon> | #:try
;;
;;   <pattern> = <id>      ; matches anything
;;             | id:<id>   ; matches only identifiers
;;             | (<pattern> ...)  ; zero or more
;;             | (<pattern> ...+) ; one or more
;;             | (<pattern> . <pattern>)
;;
;; Note that the ' before <pattern> doesn't produce a symbol or list;
;; it's just a literal to textually highlight the pattern.
;;
;; The <m-id> bound by `define-match` is used as either
;;   
;;   (<m-id>)
;;
;; to check whether the match suceeded (which makes sense only if a
;; guard or `#:try` is included) or
;;
;;    (<m-id> '<pattern-id>)
;;
;; to access the value for a match. Again, the ' here does not produce
;; a symbol, but serves only as visual highlighting.
;;
;; Unlike `syntax-rules`/`syntax-case`/`syntax-parse`, there's no
;; template system and no help in making sure your uses of variables
;; before `...` expect the right shape. For example, with
;;
;;   (define-match m s '(a ...))
;;
;; then `(m 'a)` will always produce a list of matches of `a`.
;;
;; If a pattern doesn't match and there's no `#:try`, then a syntax
;; error is reported.
;;
;; The `define-define-match` form is a macro-generating macro so that
;; it can be used with different underlying notions of syntax, as
;; specific by the `rt-syntax?`, etc., macro arguments.

(define-syntax-rule (define-define-match define-match
                      rt-syntax? rt-syntax-e rt-raise-syntax-error)
  (...
   (begin
     (define-for-syntax (extract-pattern-ids pattern)
       (cond
        [(identifier? pattern)
         (if (or (eq? '... (syntax-e pattern))
                 (eq? '...+ (syntax-e pattern)))
             null
             (list pattern))]
        [(symbol? pattern)
         (if (or (eq? '... pattern)
                 (eq? '...+ pattern))
             null
             (list pattern))]
        [(syntax? pattern) (extract-pattern-ids (syntax-e pattern))]
        [(pair?  pattern)
         (append (extract-pattern-ids (car pattern))
                 (extract-pattern-ids (cdr pattern)))]
        [else null]))
     
     ;; This pattern compiler has bad time complexity for complex
     ;; patterns, because it keeps recomputing the set of pattern
     ;; variables, but we're only going to use it on simple patterns
     
     (define-for-syntax (identifier-pattern? pattern)
       (regexp-match? #rx"^id(:|$)" (symbol->string pattern)))
     
     (define-for-syntax (compile-pattern pattern already-checked?)
       (cond
        [(symbol? pattern)
         (if (identifier-pattern? pattern)
             (if already-checked?
                 #'s
                 #`(if (or (and (rt-syntax? s)
                                (symbol? (rt-syntax-e s)))
                           (symbol? s))
                       s
                       (rt-raise-syntax-error #f "not an identifier" orig-s s)))
             #'s)]
        [else
         #`(let ([s (if (rt-syntax? s) (rt-syntax-e s) s)])
             #,(cond
                [(and (list? pattern)
                      (= (length pattern) 2)
                      (or (eq? '... (cadr pattern))
                          (eq? '...+ (cadr pattern))))
                 (with-syntax ([(pattern-id ...) (extract-pattern-ids (car pattern))])
                   #`(let ([flat-s (to-syntax-list s)])
                       (cond
                        [#,(if already-checked? #'#f #'(not flat-s))
                         (rt-raise-syntax-error #f "bad syntax" orig-s)]
                        [#,(if (and (eq? '...+ (cadr pattern)) (not already-checked?)) #'(null? flat-s) #'#f)
                         (rt-raise-syntax-error #f "bad syntax" orig-s)]
                        [else
                         #,(if (and (symbol? (car pattern))
                                    (or (not (identifier-pattern? (car pattern)))
                                        already-checked?))
                               #`flat-s
                               #`(for/lists (pattern-id ...) ([s (in-list flat-s)])
                                            #,(compile-pattern (car pattern) already-checked?)))])))]
                [(pair? pattern)
                 (with-syntax ([(a-pattern-id ...) (generate-temporaries (extract-pattern-ids (car pattern)))]
                               [(d-pattern-id ...) (generate-temporaries (extract-pattern-ids (cdr pattern)))])
                   #`(if #,(if already-checked? #'#t #'(pair? s))
                         (let-values ([(a-pattern-id ...) (let ([s (car s)]) #,(compile-pattern (car pattern)
                                                                                                 already-checked?))]
                                      [(d-pattern-id ...) (let ([s (cdr s)]) #,(compile-pattern (cdr pattern)
                                                                                                 already-checked?))])
                           (values a-pattern-id ... d-pattern-id ...))
                         (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [(null? pattern)
                 (if already-checked?
                     #'(values)
                     #'(if (null? s)
                           (values)
                           (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [(or (keyword? pattern)
                     (boolean? pattern))
                 (if already-checked?
                     #'(values)
                     #`(if (eq? '#,pattern s)
                           (values)
                           (rt-raise-syntax-error #f "bad syntax" orig-s)))]
                [else
                 (raise-syntax-error 'define-match "bad pattern" pattern)]))]))
     
     (define-for-syntax (compile-pattern-check pattern)
       (cond
        [(symbol? pattern)
         (if (identifier-pattern? pattern)
             #`(or (and (rt-syntax? s)
                        (symbol? (rt-syntax-e s)))
                   (symbol? s))
             #'#t)]
        [else
         #`(let ([s (if (rt-syntax? s) (rt-syntax-e s) s)])
             #,(cond
                [(and (list? pattern)
                      (= (length pattern) 2)
                      (or (eq? '... (cadr pattern))
                          (eq? '...+ (cadr pattern))))
                 (with-syntax ([(pattern-id ...) (extract-pattern-ids (car pattern))])
                   #`(let ([flat-s (to-syntax-list s)])
                       (cond
                        [(not flat-s) #f]
                        [#,(if (eq? '...+ (cadr pattern)) #'(null? flat-s) #'#f) #f]
                        [else #,(if (and (symbol? (car pattern))
                                         (not (identifier-pattern? (car pattern))))
                                    #`#t
                                    #`(for/and ([s (in-list flat-s)])
                                        #,(compile-pattern-check (car pattern))))])))]
                [(pair? pattern)
                 (with-syntax ([(a-pattern-id ...) (extract-pattern-ids (car pattern))]
                               [(d-pattern-id ...) (extract-pattern-ids (cdr pattern))])
                   #`(and (pair? s)
                          (let ([s (car s)]) #,(compile-pattern-check (car pattern)))
                          (let ([s (cdr s)]) #,(compile-pattern-check (cdr pattern)))))]
                [(null? pattern)
                 #'(null? s)]
                [(or (keyword? pattern)
                     (boolean? pattern))
                 #`(eq? '#,pattern s)]
                [else
                 (raise-syntax-error 'define-match "bad pattern" pattern)]))]))
     
     (define (to-syntax-list s)
       (cond
        [(list? s) s]
        [(pair? s)
         (define r (to-syntax-list (cdr s)))
         (and r (cons (car s) r))]
        [(rt-syntax? s) (to-syntax-list (rt-syntax-e s))]
        [else #f]))
     
     (define-syntax (define-match stx)
       (syntax-case stx (quote)
         [(_ id expr 'pattern)
          #'(do-define-match id expr 'pattern #:when #t #:try? #f)]
         [(_ id expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when #t #:try? #t)]
         [(_ id expr #:when guard-expr 'pattern)
          #'(do-define-match id expr 'pattern #:when guard-expr #:try? #f)]
         [(_ id expr #:when guard-expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when guard-expr #:try? #t)]
         [(_ id expr #:unless guard-expr 'pattern)
          #'(do-define-match id expr 'pattern #:when (not guard-expr) #:try? #f)]
         [(_ id expr #:unless guard-expr #:try 'pattern)
          #'(do-define-match id expr 'pattern #:when (not guard-expr) #:try? #t)]))

     (define-syntax (do-define-match stx)
       (syntax-case stx (quote)
         [(_ id expr 'pattern #:when guard-expr #:try? try?)
          (let ([pattern-ids (extract-pattern-ids #'pattern)]
                [try? (syntax-e #'try?)])
            (with-syntax ([(pattern-id ...) pattern-ids]
                          [(pattern-result-id ...) (generate-temporaries pattern-ids)]
                          [(false-result ...) (map (lambda (x) #'#f) pattern-ids)]
                          [matcher (compile-pattern (syntax->datum #'pattern) try?)])
              #`(begin
                  (define-values (ok? pattern-result-id ...)
                    (let ([s expr])
                      (if (and guard-expr
                               #,(if try?
                                     (compile-pattern-check (syntax->datum #'pattern))
                                     #'#t))
                          (let ([orig-s s]) 
                            (let-values ([(pattern-result-id ...) matcher])
                              (values #t pattern-result-id ...)))
                          (values #f false-result ...))))
                  (define-syntax id
                    (syntax-rules (quote pattern-id ...)
                      [(m) ok?]
                      [(m (quote pattern-id))
                       pattern-result-id]
                      ...)))))])))))
