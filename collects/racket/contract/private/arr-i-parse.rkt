#lang racket/base
(require (rename-in syntax/private/boundmap
                    ;; the private version of the library 
                    ;; (the one without contracts)
                    ;; has these old, wrong names in it.
                    [make-module-identifier-mapping make-free-identifier-mapping]
                    [module-identifier-mapping-get free-identifier-mapping-get]
                    [module-identifier-mapping-put! free-identifier-mapping-put!])
         (for-template racket/base
                       "guts.rkt"))

#|

The ->i contract first parses its input into an istx struct 
and then operates on it to generate the expanded form. This
code does the parsing and validation of the syntax.

|#

;; args : (listof arg?)
;; rst  : (or/c #f rst?)
;; pre  : (or/c pre/post? #f)
;; ress : (or/c #f (listof eres?) (listof lres?))
;; post : (or/c pre/post? #f)
(struct istx (args rst pre ress post))
;; NOTE: the ress field may contain a mixture of eres and lres structs
;;       but only temporarily; in that case, a syntax error
;;       is signaled and the istx struct is not used afterwards

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; optional? : boolean?
(struct arg/res (var vars ctc) #:constructor-name ___do-not-use-this-constructor)

;; kwd  : (or/c #f syntax[kwd])
;; ctc  : syntax[expr]
(struct arg arg/res (kwd optional?))

;; these represent res contracts that came from _s (and thus should be evaluated early)
(struct eres arg/res ())

;; these represent res contracts that do not come from _s (and thus should be evaluated later)
(struct lres arg/res ())


;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(struct rst (var vars ctc))

;; vars : (listof identifier?)
;; exp  : syntax[expr]
(struct pre/post (vars exp))

(define (parse-->i stx)
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
      candidate)))

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
                                    "the #:pre condition cannot depend on a result")
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
             stx #f (map res-var (istx-ress istx)))))))
    
    ;; no dups in the rest var
    (when (istx-rst istx)
      (when (rst-vars (istx-rst istx))
        (not-range-bound (rst-vars (istx-rst istx)) #t))
      (no-var-dups (rst-var (istx-rst istx))))
    
    ;; dependent arg variables are all bound, but not to a range variable
    (for ([an-arg (in-list (istx-args istx))])
      (let ([a-vars (arg/res-vars an-arg)])
        (when a-vars
          (ensure-bound a-vars)
          (not-range-bound a-vars #t))))
    
    ;; pre-condition variables are all bound, but not to a range variable
    (when (istx-pre istx)
      (let ([vars (pre/post-vars (istx-pre istx))])
        (ensure-bound vars)
        (not-range-bound vars #f)))

    ;; dependent range variables are all bound.
    (when (istx-ress istx)
      (for ([a-res (in-list (istx-ress istx))])
        (when (arg/res-vars a-res)
          (ensure-bound (arg/res-vars a-res)))))
    
    ;; post-condition variables are all bound
    (when (istx-post istx)
      (let ([vars (pre/post-vars (istx-post istx))])
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
          [(rst-vars a-rst)
           (for ([nvar (in-list (rst-vars a-rst))])
             (link (rst-var a-rst) nvar))]
          [else
           (no-links (rst-var a-rst))])))
           
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
                      ((if (free-identifier=? #'_ #'id) eres lres)
                       #'id #f #'ctc))]
                   [[id (id2 ...) ctc]
                    (begin
                      (check-id stx #'id)
                      (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
                      ((if (free-identifier=? #'_ #'id) eres lres)
                       #'id (syntax->list #'(id2 ...)) #'ctc))]
                   [x (raise-syntax-error #f "expected binding pair" stx #'x)]))
          (syntax->list #'(ctc-pr ...)))]
    [any #f]
    [[_ ctc]
     (begin
       (check-id stx #'id)
       (list (eres #'id #f #'ctc)))]
    [[id ctc]
     (begin
       (check-id stx #'id)
       (list (lres #'id #f #'ctc)))]
    [[_ (id2 ...) ctc] 
     (begin
       (check-id stx #'id)
       (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
       (list (eres #'id (syntax->list #'(id2 ...)) #'ctc)))]
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
                   [((opts ...) . rest)
                    (values #'(opts ...) #'rest)]
                   [_ (values '() leftover)])]
                [(id/rest-id leftover) 
                 (syntax-case leftover ()
                   [(#:rest [id rest-expr] . leftover)
                    (begin
                      (check-id stx #'id)
                      (values (rst #'id #f #'rest-expr)
                              #'leftover))]
                   [(#:rest [id (id2 ...) rest-expr] . leftover)
                    (begin
                      (check-id stx #'id)
                      (for-each (λ (x) (check-id stx x))
                                (syntax->list #'(id2 ...)))
                      (values (rst #'id 
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
                [(pre-cond leftover)
                 (syntax-case leftover ()
                   [(#:pre (id ...) pre-cond . leftover)
                    (begin
                      (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                      (values (pre/post (syntax->list #'(id ...)) #'pre-cond) #'leftover))]
                   [_ (values #f leftover)])]
                [(range leftover) 
                 (syntax-case leftover ()
                   [(range . leftover) 
                    (not (keyword? (syntax-e #'range)))
                    (values #'range #'leftover)]
                   [(a . b)
                    (raise-syntax-error #f "expected a range expression" stx #'a)]
                   [()
                    (raise-syntax-error #f "expected a range expression, but found nothing" stx leftover)])]
                [(post-cond leftover) 
                 (syntax-case leftover ()
                   [(#:post (id ...) post-cond . leftover)
                    (begin
                      (for-each (λ (x) (check-id stx x)) (syntax->list #'(id ...)))
                      (syntax-case range (any)
                        [any (raise-syntax-error #f "cannot have a #:post with any as the range" stx #'post-cond)]
                        [_ (void)])
                      (values (pre/post (syntax->list #'(id ...)) #'post-cond) #'leftover))]
                   [_ (values #f leftover)])])
    (syntax-case leftover ()
      [() 
       (values raw-mandatory-doms raw-optional-doms id/rest-id pre-cond range post-cond)]
      [(a . b)
       (raise-syntax-error #f "bad syntax" stx #'a)]
      [_
       (raise-syntax-error #f "bad syntax" stx)])))

;(define (ensure-no-cycles istx)
;  (let (;; cm : id -o> {'pending, 'no-cycle}
;        [cm (make-free-identifier-map)])
;    (for ([dom (in-list (istx-args istx))])
;      (let loop ([id (

(provide
 parse-->i
 (struct-out istx)
 (struct-out arg/res)
 (struct-out arg)
 (struct-out res)
 (struct-out lres)
 (struct-out eres)
 (struct-out rst)
 (struct-out pre/post))