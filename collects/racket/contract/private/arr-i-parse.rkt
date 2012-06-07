#lang racket/base
(require (rename-in syntax/private/boundmap
                    ;; the private version of the library 
                    ;; (the one without contracts)
                    ;; has these old, wrong names in it.
                    [make-module-identifier-mapping make-free-identifier-mapping]
                    [module-identifier-mapping-get free-identifier-mapping-get]
                    [module-identifier-mapping-put! free-identifier-mapping-put!])
         (for-template racket/base
                       "misc.rkt"))

#|

The ->i contract first parses its input into an istx struct 
and then operates on it to generate the expanded form. This
code does the parsing and validation of the syntax.

|#

;; args : (listof arg?)
;; rst  : (or/c #f arg/res?)
;; pre  : (listof pre/post?)
;; ress : (or/c #f (listof eres?) (listof lres?))
;; post : (listof pre/post?)
(struct istx (args rst pre ress post) #:transparent)
;; NOTE: the ress field may contain a mixture of eres and lres structs
;;       but only temporarily; in that case, a syntax error
;;       is signaled and the istx struct is not used afterwards

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(struct arg/res (var vars ctc) #:transparent)

;; kwd  : (or/c #f syntax[kwd])
;; optional? : boolean?
(struct arg arg/res (kwd optional?) #:transparent)

;; these represent res contracts that came from _s (and thus should be evaluated early)
;; eid : identifier?  --- extra variable to be bound to the result 
;;                    --- of evaluating the result contract early
(struct eres arg/res (eid) #:transparent)

;; these represent res contracts that do not come from _s (and thus should be evaluated later)
(struct lres arg/res () #:transparent)

;; vars : (listof identifier?)
;; exp  : syntax[expr]
;; str  : (or/c #f syntax[expr])
(struct pre/post (vars str exp) #:transparent)

(define (parse-->i stx)
  (if (identifier? stx)
      (raise-syntax-error #f "expected ->i to follow an open parenthesis" stx)
      (let-values ([(raw-mandatory-doms raw-optional-doms
                                        id/rest-id pre-cond range post-cond)
                    (pull-out-pieces stx)])
        (let ([candidate
               (istx (append (parse-doms stx #f raw-mandatory-doms)
                             (parse-doms stx #t raw-optional-doms))
                     id/rest-id
                     pre-cond
                     (parse-range stx range)
                     post-cond)])
          (ensure-wf-names stx candidate)
          (ensure-no-cycles stx candidate)
          candidate))))

(define (ensure-wf-names stx istx)
  (let ([km (make-hash)]
        [nm (make-free-identifier-mapping)])
    
    (define (no-var-dups var)
      (cond
        [(free-identifier-mapping-get nm var (λ () #f))
         =>
         (λ (other)
           (raise-syntax-error #f "duplicate dependent variables"
                               stx other (list var)))]
        [else
         (free-identifier-mapping-put! nm var var)]))
    
    (define (no-kwd-dups kwd-stx)
      (let ([kwd (syntax-e kwd-stx)])
        (cond
          [(hash-ref km kwd #f)
           =>
           (λ (that)
             (raise-syntax-error #f "duplicate keywords" 
                                 stx that (list kwd-stx)))]
          [else
           (hash-set! km kwd kwd-stx)])))
    
    (define (ensure-bound vars)
      (for ([var (in-list vars)])
        (unless (free-identifier-mapping-get nm var (λ () #f))
          (raise-syntax-error #f "dependent variable not bound"
                              stx var))))
    
    ;; not-range-bound : (listof identifier[used-by-an-arg]) -> void
    (define (not-range-bound arg-vars arg?)
      (when (istx-ress istx)
        (for ([arg-var (in-list arg-vars)])
          (when (ormap (λ (a-res) (free-identifier=? (arg/res-var a-res) arg-var))
                       (istx-ress istx))
            (raise-syntax-error #f
                                (if arg? 
                                    "an argument cannot depend on a result"
                                    "a #:pre or #:pre/name condition cannot depend on a result")
                                stx arg-var)))))
    
    ;; no dups in the domains
    (for ([dom (in-list (istx-args istx))])
      (when (arg-kwd dom)
        (no-kwd-dups (arg-kwd dom)))
      (no-var-dups (arg/res-var dom)))
    
    ;; no dups in the ranges
    (when (istx-ress istx)
      (let ([any-eres? #f]
            [all-eres? #t])
        (for ([res (in-list (istx-ress istx))])
          (cond
            [(eres? res)
             (set! any-eres? #t)]
            [else 
             (set! all-eres? #f)
             (no-var-dups (arg/res-var res))]))
        (when any-eres?
          (unless all-eres?
            (raise-syntax-error
             #f
             "either all or none of the dependent range variables must be _"
             stx #f (map arg/res-var (istx-ress istx)))))))
    
    ;; no dups in the rest var
    (when (istx-rst istx)
      (when (arg/res-vars (istx-rst istx))
        (not-range-bound (arg/res-vars (istx-rst istx)) #t))
      (no-var-dups (arg/res-var (istx-rst istx))))
    
    ;; dependent arg variables are all bound, but not to a range variable
    (for ([an-arg (in-list (istx-args istx))])
      (let ([a-vars (arg/res-vars an-arg)])
        (when a-vars
          (ensure-bound a-vars)
          (not-range-bound a-vars #t))))
    
    ;; pre-condition variables are all bound, but not to a range variable
    (for ([pre (in-list (istx-pre istx))])
      (let ([vars (pre/post-vars pre)])
        (ensure-bound vars)
        (not-range-bound vars #f)))

    ;; dependent range variables are all bound.
    (when (istx-ress istx)
      (for ([a-res (in-list (istx-ress istx))])
        (when (arg/res-vars a-res)
          (ensure-bound (arg/res-vars a-res)))))
    
    ;; post-condition variables are all bound
    (for ([post (in-list (istx-post istx))])
      (let ([vars (pre/post-vars post)])
        (ensure-bound vars)))))

(define (ensure-no-cycles stx istx)
  (let ([neighbors (make-free-identifier-mapping)]
        [safe (make-free-identifier-mapping)]
        [sp '()])

    (define (link from to)
      (set! sp (cons from sp))
      (free-identifier-mapping-put!
       neighbors from
       (cons to (free-identifier-mapping-get neighbors from (λ () '())))))
    
    (define (no-links from)
      (set! sp (cons from sp))
      (free-identifier-mapping-put! neighbors from '()))
    
    (define (handle-arg/ress arg/ress)
      (for ([a-res (in-list arg/ress)])
        (cond
          [(arg/res-vars a-res)
           (for ([nvar (in-list (arg/res-vars a-res))])
             (link (arg/res-var a-res) nvar))]
          [else
           (no-links (arg/res-var a-res))])))
    
    (handle-arg/ress (istx-args istx))
    
    (when (istx-ress istx)
      (handle-arg/ress (istx-ress istx)))
    
    (let ([a-rst (istx-rst istx)])
      (when a-rst
        (cond
          [(arg/res-vars a-rst)
           (for ([nvar (in-list (arg/res-vars a-rst))])
             (link (arg/res-var a-rst) nvar))]
          [else
           (no-links (arg/res-var a-rst))])))
           
    (for ([var (in-list sp)])
      (let loop ([var var]
                 [visited '()])
        (cond
          [(free-identifier-mapping-get safe var (λ () #f))
           (void)]
          [(memf (λ (x) (free-identifier=? x var)) visited)
           (let ([ids (trim-at var visited)])
             (raise-syntax-error #f 
                                 "cyclic dependencies are not allowed"
                                 stx
                                 (car ids)
                                 (cdr ids)))]
          [else
           (let ([new-visited (cons var visited)])
             (for ([neighbor (in-list (free-identifier-mapping-get neighbors var))])
               (loop neighbor new-visited)
               (free-identifier-mapping-put! safe var #t)))])))))

;; trim-at : identifier? (listof identifier?) -> (listof identifier?)
;; returns the shortest prefix of vars that ends with var
(define (trim-at var vars)
  (let loop ([vars vars])
    (cond
      [(null? vars) (error 'trim-at "not found")]
      [else (let ([fst (car vars)])
              (if (free-identifier=? fst var)
                  (list fst)
                  (cons fst (loop (cdr vars)))))])))

(define (parse-doms stx optional? doms)
  (let loop ([doms doms])
    (syntax-case doms ()
      [(kwd [id ctc-expr] . rest)
       (keyword? (syntax-e #'kwd))
       (begin
         (check-id stx #'id)
         (cons (arg #'id #f #'ctc-expr #'kwd optional?)
               (loop #'rest)))]
      [(kwd [id (id2 ...) ctc-expr] . rest)
       (keyword? (syntax-e #'kwd))
       (begin
         (check-id stx #'id)
         (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
         (cons (arg #'id (syntax->list #'(id2 ...)) #'ctc-expr #'kwd optional?)
               (loop #'rest)))]
      [([id ctc-expr] . rest)
       (begin
         (check-id stx #'id)
         (cons (arg #'id #f #'ctc-expr #f optional?)
               (loop #'rest)))]
      [([id (id2 ...) ctc-expr] . rest)
       (begin
         (check-id stx #'id)
         (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
         (cons (arg #'id (syntax->list #'(id2 ...)) #'ctc-expr #f optional?)
               (loop #'rest)))]
      [() '()]
      [(a . rest)
       (raise-syntax-error #f "expected an argument specification" stx #'a)])))

(define (parse-range stx range)
  (syntax-case range (any values _)
    [(values ctc-pr ...)
     (map (λ (x) (syntax-case x (_)
                   [[id ctc] 
                    (begin
                      (check-id stx #'id)
                      (if (free-identifier=? #'_ #'id) 
                          (eres #'id #f #'ctc (car (generate-temporaries '(eres))))
                          (lres #'id #f #'ctc)))]
                   [[id (id2 ...) ctc]
                    (begin
                      (check-id stx #'id)
                      (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
                      (if (free-identifier=? #'_ #'id) 
                          (eres #'id (syntax->list #'(id2 ...)) #'ctc (car (generate-temporaries '(eres))))
                          (lres #'id (syntax->list #'(id2 ...)) #'ctc)))]
                   [(a ...)
                    (let ([len (length (syntax->list #'(a ...)))])
                      (unless (or (= 2 len) (= 3 len))
                        (raise-syntax-error #f "wrong number of pieces in range portion of the contract, expected id+ctc" stx #'x))
                      (raise-syntax-error #f "expected id+ctc in range portion of contract" stx #'x))]
                   [x 
                    (raise-syntax-error #f "expected id+ctc in range portion of contract" stx #'x)]))
          (syntax->list #'(ctc-pr ...)))]
    [any #f]
    [[_ ctc]
     (list (eres #'_ #f #'ctc (car (generate-temporaries '(eres)))))]
    [[id ctc]
     (begin
       (check-id stx #'id)
       (list (lres #'id #f #'ctc)))]
    [[_ (id2 ...) ctc] 
     (begin
       (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
       (list (eres #'_ (syntax->list #'(id2 ...)) #'ctc (car (generate-temporaries '(eres))))))]
    [[id (id2 ...) ctc] 
     (begin
       (check-id stx #'id)
       (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
       (list (lres #'id (syntax->list #'(id2 ...)) #'ctc)))]
    [x (raise-syntax-error #f "expected the range portion" stx #'x)]))

(define (check-id stx id)
  (unless (identifier? id)
    (raise-syntax-error #f "expected an identifier" stx id)))

;; pull-out-pieces : stx -> (values raw-mandatory-doms raw-optional-doms id/rest-id pre-cond range post-cond) 
(define (pull-out-pieces stx)
  (let*-values ([(raw-mandatory-doms leftover) 
                 (syntax-case stx ()
                   [(_ (raw-mandatory-doms ...) . leftover)
                    (values (syntax->list #'(raw-mandatory-doms ...)) 
                            #'leftover)]
                   [(_ a . leftover)
                    (raise-syntax-error #f "expected a sequence of mandatory domain elements" stx #'a)]
                   [_
                    (raise-syntax-error #f "expected a sequence of mandatory domain elements" stx)])]
                [(raw-optional-doms leftover)
                 (syntax-case leftover ()
                   [(kwd . leftover2)
                    (keyword? (syntax-e #'kwd))
                    (values '() leftover)]
                   [(dep-range)
                    (values '() leftover)]
                   [(dep-range #:post . stuff)
                    (values '() leftover)]
                   [(dep-range #:post/name . stuff)
                    (values '() leftover)]
                   [((opts ...) . rest)
                    (values #'(opts ...) #'rest)]
                   [_ (values '() leftover)])]
                [(id/rest-id leftover) 
                 (syntax-case leftover ()
                   [(#:rest [id rest-expr] . leftover)
                    (begin
                      (check-id stx #'id)
                      (values (arg/res #'id #f #'rest-expr)
                              #'leftover))]
                   [(#:rest [id (id2 ...) rest-expr] . leftover)
                    (begin
                      (check-id stx #'id)
                      (for-each (λ (x) (check-id stx x))
                                (syntax->list #'(id2 ...)))
                      (values (arg/res #'id 
                                       (syntax->list #'(id2 ...))
                                       #'rest-expr)
                              #'leftover))]
                   [(#:rest other . leftover)
                    (raise-syntax-error #f "expected an id+ctc"
                                        stx
                                        #'other)]
                   [(x)
                    (eq? (syntax-e #'x) '#:rest)
                    (raise-syntax-error 
                     #f 
                     "expected something to follow #:rest"
                     stx #'x)]
                   [_ (values #f leftover)])]
                [(pre-conds leftover)
                 (let loop ([leftover leftover]
                            [conditions '()])
                   (syntax-case leftover ()
                     [(#:pre (id ...) pre-cond . pre-leftover)
                      (begin
                        (syntax-case #'pre-leftover ()
                          [() (raise-syntax-error 
                               #f
                               (string-append
                                "expected #:pre to be followed by at least three subterms"
                                " (a sequence of identifiers, the pre-condition, and the"
                                " range contract), but found only two")
                               stx
                               (car (syntax->list leftover)))]
                          [x (void)])
                        (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                        (loop #'pre-leftover 
                              (cons (pre/post (syntax->list #'(id ...)) #f #'pre-cond) conditions)))]
                     [(#:pre . rest)
                      (raise-syntax-error #f
                                          "expected a sequence of identifiers and an expression to follow #:pre"
                                          stx
                                          (car (syntax->list leftover)))]
                     [(#:pre/name (id ...) str pre-cond . pre-leftover)
                      (begin
                        (syntax-case #'pre-leftover ()
                          [() (raise-syntax-error 
                               #f
                               (string-append
                                "expected #:pre/name to be followed by at least four subterms"
                                " (a sequence of identifiers, a name, the pre-condition, and the"
                                " range contract), but found only three")
                               stx
                               (car (syntax->list leftover)))]
                          [x (void)])
                        (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                        (unless (string? (syntax-e #'str))
                          (raise-syntax-error 
                           #f
                           "expected #:pre/name to have a string after the sequence of variables"
                           stx
                           #'str))
                        (loop #'pre-leftover
                              (cons (pre/post (syntax->list #'(id ...)) (syntax-e #'str) #'pre-cond) conditions)))]
                     [(#:pre/name . rest)
                      (raise-syntax-error #f
                                          "expected a sequence of identifiers, a string, and an expression to follow #:pre/name"
                                          stx
                                          (car (syntax->list leftover)))]
                     [_ (values (reverse conditions) leftover)]))]
                [(range leftover) 
                 (begin
                   (syntax-case leftover ()
                     [(range . leftover) 
                      (not (keyword? (syntax-e #'range)))
                      (values #'range #'leftover)]
                     [(a . b)
                      (raise-syntax-error #f "expected a range expression" stx #'a)]
                     [()
                      (raise-syntax-error #f "expected a range expression, but found nothing" stx)]))]
                [(post-conds leftover) 
                 (let loop ([leftover leftover]
                            [post-conds '()])
                   (syntax-case leftover ()
                     [(#:post (id ...) post-cond . leftover)
                      (begin
                        (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                        (syntax-case range (any)
                          [any (raise-syntax-error #f "cannot have a #:post with any as the range" stx #'post-cond)]
                          [_ (void)])
                        (loop #'leftover
                              (cons (pre/post (syntax->list #'(id ...)) #f #'post-cond) post-conds)))]
                     [(#:post a b . stuff)
                      (begin
                        (raise-syntax-error #f "expected a sequence of variables to follow #:post" stx #'a))]
                     [(#:post a)
                      (begin
                        (raise-syntax-error #f "expected a sequence of variables and an expression to follow #:post" stx #'a))]
                     [(#:post/name (id ...) str post-cond . leftover)
                      (begin
                        (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                        (syntax-case range (any)
                          [any (raise-syntax-error #f "cannot have a #:post with any as the range" stx #'post-cond)]
                          [_ (void)])
                        (unless (string? (syntax-e #'str))
                          (raise-syntax-error #f
                                              "expected the error message part of a #:post/name declaraction to be a string"
                                              stx
                                              #'str))
                        (loop #'leftover
                              (cons (pre/post (syntax->list #'(id ...)) (syntax-e #'str) #'post-cond) post-conds)))]
                     [(#:post/name . stuff)
                      (begin
                        (raise-syntax-error #f "expected a sequence of variables, a string, and an expression to follow #:post/name" 
                                            stx
                                            (car (syntax-e leftover))))]
                     
                     [_
                      (values (reverse post-conds) leftover)]))])
    (syntax-case leftover ()
      [() 
       (values raw-mandatory-doms raw-optional-doms id/rest-id pre-conds range post-conds)]
      [(a . b)
       (raise-syntax-error #f "bad syntax" stx #'a)]
      [_
       (raise-syntax-error #f "bad syntax" stx)])))

(provide
 parse-->i
 (struct-out istx)
 (struct-out arg/res)
 (struct-out arg)
 (struct-out lres)
 (struct-out eres)
 (struct-out pre/post))
