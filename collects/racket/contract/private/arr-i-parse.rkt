#lang racket/base
(require syntax/private/boundmap
         (for-template racket/base
                       "guts.rkt"))
;; the private version of the library 
;; (the one without contracts)
;; has these old, wrong names in it.
(define make-free-identifier-mapping make-module-identifier-mapping)
(define free-identifier-mapping-get module-identifier-mapping-get)
(define free-identifier-mapping-put! module-identifier-mapping-put!)

#|

The ->i contract first parses its input into an istx struct 
and then operates on it to generate the expanded form

|#

;; args : (listof arg?)
;; rest : (or/c #f rst?)
;; pre  : (or/c stx[expr] #f)
;; ress : (or/c #f (listof eres?) (listof lres?))
;; post : (or/c stx[expr] #f)
(struct istx (args rst pre ress post))
;; NOTE: the ress field may contain a mixture of eres and lres structs
;;       but only temporarily; after it is constructed, a syntax error
;;       is signalled and the istx struct is not used afterwards


;; kwd  : (or/c #f syntax[kwd])
;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; optional? : boolean?
;; ctc  : syntax[expr]
(struct arg (kwd var vars optional? ctc))

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(struct res (var vars ctc) #:constructor-name ___do-not-use-this-constructor)

;; these represent res contracts that came from _s (and thus should be evaluated early)
(struct eres res ())

;; these represent res contracts that came from _s (and thus should be evaluated later)
(struct lres res ())


;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(struct rst (var vars ctc))

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
    
    ;; no dups in the domains
    (for ([dom (in-list (istx-args istx))])
      (when (arg-kwd dom)
        (no-kwd-dups (arg-kwd dom)))
      (no-var-dups (arg-var dom)))
    
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
             (no-var-dups (res-var res))]))
        (when any-eres?
          (unless all-eres?
            (raise-syntax-error
             #f
             "either all or none of the dependent range variables must be _"
             stx #f (map res-var (istx-ress istx)))))))
    
    ;; no dups in the rest var
    (when (istx-rst istx)
      (no-var-dups (rst-var (istx-rst istx))))
    
    
    ;; dependent arg variables are all bound.
    (for ([an-arg (in-list (istx-args istx))])
      (when (arg-vars an-arg)
        (ensure-bound (arg-vars an-arg))))
    
    ;; dependent range variables are all bound.
    (when (istx-ress istx)
      (for ([a-res (in-list (istx-ress istx))])
        (when (res-vars a-res)
          (ensure-bound (res-vars a-res)))))))

(define (ensure-no-cycles stx istx)
  (let ([neighbors (make-hash)]
        [sp '()]
        [safe (make-hash)]
        [vars->stx (make-hash)])

    (define (link from to)
      (printf "linking ~s => ~s\n" from to)
      (set! sp (cons (syntax-e from) sp))
      (hash-set! vars->stx (syntax-e from) from)
      (hash-set! neighbors (syntax-e from)
                 (cons (syntax-e to)
                       (hash-ref neighbors (syntax-e from) '()))))
    
    (for ([an-arg (in-list (istx-args istx))])
      (when (arg-vars an-arg)
        (for ([nvar (in-list (arg-vars an-arg))])
          (link (arg-var an-arg) nvar))))
    
    (when (istx-ress istx)
      (for ([a-res (in-list (istx-ress istx))])
        (when (res-vars a-res)
          (for ([nvar (in-list (res-vars a-res))])
            (link (res-var a-res) nvar)))))
    
    (for ([var (in-list sp)])
      (let loop ([var var]
                 [visited '()])
        (cond
          [(hash-ref safe var #f)
           (void)]
          [(member var visited)
           (let ([ids (map (λ (x) (hash-ref vars->stx x))
                           (trim-at var visited))])
             (raise-syntax-error #f 
                                 "cyclic dependencies are not allowed"
                                 stx
                                 (car ids)
                                 (cdr ids)))]
          [else
           (let ([new-visited (cons var visited)])
             (for ([neighbor (in-list (hash-ref neighbors var))])
               (loop neighbor new-visited)
               (hash-set! safe var #t)))])))))

;; trim-at : X (listof X) -> (listof X)
;; returns the shortest prefix of vars that ends with var
(define (trim-at var vars)
  (let loop ([vars vars])
    (cond
      [(null? vars) (error 'trim-at "not found")]
      [else (let ([fst (car vars)])
              (if (eq? fst var)
                  (list fst)
                  (cons fst (loop (cdr vars)))))])))

(define (parse-doms stx optional? doms)
  (let loop ([doms doms])
    (syntax-case doms ()
      [(kwd [id ctc-expr] . rest)
       (keyword? (syntax-e #'kwd))
       (begin
         (check-id stx #'id)
         (cons (arg #'kwd #'id #f optional? #'ctc-expr)
               (loop #'rest)))]
      [(kwd [id (id2 ...) ctc-expr] . rest)
       (keyword? (syntax-e #'kwd))
       (begin
         (check-id stx #'id)
         (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
         (cons (arg #'kwd #'id (syntax->list #'(id2 ...)) optional? #'ctc-expr)
               (loop #'rest)))]
      [([id ctc-expr] . rest)
       (begin
         (check-id stx #'id)
         (cons (arg #f #'id #f optional? #'ctc-expr)
               (loop #'rest)))]
      [([id (id2 ...) ctc-expr] . rest)
       (begin
         (check-id stx #'id)
         (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
         (cons (arg #f #'id (syntax->list #'(id2 ...)) optional? #'ctc-expr)
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
                   [(dep-range #:post-cond expr)
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
                   [(#:pre-cond pre-cond . leftover)
                    (values #'pre-cond #'leftover)]
                   [_ (values #f leftover)])]
                [(range leftover) 
                 (syntax-case leftover ()
                   [(range . leftover) (values #'range #'leftover)]
                   [()
                    (raise-syntax-error #f "expected a range expression, but found nothing" stx leftover)])]
                [(post-cond leftover) 
                 (syntax-case leftover ()
                   [(#:post-cond post-cond . leftover)
                    (begin
                      (syntax-case range (any)
                        [any (raise-syntax-error #f "cannot have a #:post-cond with any as the range" stx #'post-cond)]
                        [_ (void)])
                      (values #'post-cond #'leftover))]
                   [_ (values #f leftover)])])
    (syntax-case leftover ()
      [() 
       (values raw-mandatory-doms raw-optional-doms id/rest-id pre-cond range post-cond)]
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
 (struct-out res)
 (struct-out arg)
 (struct-out rst))