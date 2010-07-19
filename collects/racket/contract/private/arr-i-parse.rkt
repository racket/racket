#lang racket/base
#|

The ->i contract first parses its input into an istx struct 
and then operates on it to generate the expanded form

|#

;; doms : (listof arg?)
;; pre  : (or/c stx[expr] #f)
;; rngs : (listof res?)
;; rest : (or/c #f rst?)
;; post : (or/c stx[expr] #f)
(define-struct istx (doms pre rngs rest post))

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(define-struct res (var vars ctc))

;; kwd  : (or/c #f syntax[kwd])
;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; optional? : boolean?
;; ctc  : syntax[expr]
(define-struct arg (kwd var vars optional? ctc))

;; var  : identifier?
;; vars : (or/c #f (listof identifier?))
;; ctc  : syntax[expr]
(define-struct rst (var vars ctc))

(define (parse-->i stx)
  (let-values ([(raw-mandatory-doms raw-optional-doms id/rest-id pre-cond range post-cond)
                (pull-out-pieces stx)])
    (make-istx (append (map parse-dom raw-mandatory-doms)
                       (map parse-dom raw-optional-doms))
               pre-cond
               range
               rest
               post)))

;; pull-out-pieces : stx -> (values raw-mandatory-doms raw-optional-doms id/rest-id pre-cond range post-cond) 
(define (pull-out-pieces stx)
  (let*-values ([(raw-mandatory-doms leftover) 
                 (syntax-case stx ()
                   [((raw-mandatory-doms ...) . leftover)
                    (values (syntax->list #'(raw-mandatory-doms ...)) 
                            #'leftover)]
                   [(a . leftover)
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
                   [(#:rest id rest-expr . leftover)
                    (and (identifier? #'id)
                         (not (keyword? (syntax-e #'rest-expr))))
                    (values #'(id rest-expr) #'leftover)]
                   [(#:rest id (id2 ...) rest-expr . leftover)
                    (and (identifier? #'id)
                         (andmap identifier? (syntax->list #'(id2 ...)))
                         (not (keyword? (syntax-e #'rest-expr))))
                    (values #'(id rest-expr) #'leftover)]
                   [(#:rest id rest-expr . leftover)
                    (begin
                      (unless (identifier? #'id)
                        (raise-syntax-error #f "expected an identifier" stx #'id))
                      (when (keyword? (syntax-e #'rest-expr))
                        (raise-syntax-error #f "expected an expression, not a keyword" stx #'rest-expr)))]
                   [_ (values #f leftover)])]
                [(pre-cond leftover)
                 (syntax-case leftover ()
                   [(#:pre-cond pre-cond . leftover)
                    (values #'pre-cond #'leftover)]
                   [_ (values #f leftover)])]
                [(range leftover) 
                 (syntax-case leftover ()
                   [(range . leftover) (values #'range #'leftover)]
                   [_
                    (raise-syntax-error #f "expected a range expression, but found nothing" stx)])]
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

(provide
 parse-->i
 (struct-out istx)
 (struct-out res)
 (struct-out arg)
 (struct-out rst))