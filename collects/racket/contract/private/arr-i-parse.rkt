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

;; doms : (listof arg?)
;; rest : (or/c #f rst?)
;; pre  : (or/c stx[expr] #f)
;; rngs : (or/c #f (listof res?))
;; post : (or/c stx[expr] #f)
(struct istx (args rst pre ress post))

;; kwd  : (or/c #f syntax[kwd])
;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; optional? : boolean?
;; ctc  : syntax[expr]
(struct arg (kwd var vars optional? ctc))

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(struct res (var vars ctc))

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
      ;(ensure-no-cycles stx candidate)
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
    
    (for ([dom (in-list (istx-args istx))])
      (when (arg-kwd dom)
        (no-kwd-dups (arg-kwd dom)))
      (no-var-dups (arg-var dom)))
    
    (when (istx-ress istx)
      (let ([any-_? #f]
            [all-_? #t])
        (for ([rng (in-list (istx-ress istx))])
          (cond
            [(free-identifier=? #'_ (res-var rng))
             (set! any-_? #t)]
            [else 
             (set! all-_? #f)
             (no-var-dups (res-var rng))]))
        (when any-_?
          (unless all-_?
            (raise-syntax-error #f "either all of the dependent range variables must be _ or none of them"
                                stx (map res-var (istx-ress istx)))))))
    
    (when (istx-rst istx)
      (no-var-dups (rst-var (istx-rst istx))))))
    
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
  (syntax-case range (any values) #;(λ (x y) (eq? (syntax-e x) (syntax-e y)))
    [(values ctc-pr ...)
     (map (λ (x) (syntax-case x ()
                   [[id ctc] 
                    (begin
                      (check-id stx #'id)
                      (res #'id #f #'ctc))]
                   [[id (id2 ...) ctc]
                    (begin
                      (check-id stx #'id)
                      (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
                      (res #'id (syntax->list #'(id2 ...)) #'ctc))]
                   [x (raise-syntax-error #f "expected binding pair" stx #'x)]))
          (syntax->list #'(ctc-pr ...)))]
    [any #f]
    [[id ctc]
     (begin
       (check-id stx #'id)
       (list (res #'id #f #'ctc)))]
    [[id (id2 ...) ctc] 
     (begin
       (check-id stx #'id)
       (for-each (λ (x) (check-id stx x)) (syntax->list #'(id2 ...)))
       (list (res #'id (syntax->list #'(id2 ...)) #'ctc)))]
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