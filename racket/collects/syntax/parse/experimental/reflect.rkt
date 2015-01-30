#lang racket/base
(require (for-syntax racket/base
                     racket/lazy-require
                     syntax/parse/private/residual-ct) ;; keep abs.path
         racket/contract/base
         racket/contract/combinator
         "../private/minimatch.rkt"
         "../private/keywords.rkt"
         "../private/runtime-reflect.rkt"
         "../private/kws.rkt")
(begin-for-syntax
 (lazy-require
  [syntax/parse/private/rep-data ;; keep abs. path
   (get-stxclass
    stxclass-delimit-cut?)]))
;; FIXME: workaround for phase>0 bug in racket/runtime-path (and thus lazy-require)
;; Without this, dependencies don't get collected.
(require racket/runtime-path (for-meta 2 '#%kernel))
(define-runtime-module-path-index _unused_ 'syntax/parse/private/rep-data)

(define-syntax (reify-syntax-class stx)
  (if (eq? (syntax-local-context) 'expression)
      (syntax-case stx ()
        [(rsc sc)
         (let* ([stxclass (get-stxclass #'sc)]
                [splicing? (stxclass-splicing? stxclass)])
           (unless (stxclass-delimit-cut? stxclass)
             (raise-syntax-error #f "cannot reify syntax class with #:no-delimit-cut option"
                                 stx #'sc))
           (with-syntax ([name (stxclass-name stxclass)]
                         [parser (stxclass-parser stxclass)]
                         [arity (stxclass-arity stxclass)]
                         [(#s(attr aname adepth _) ...) (stxclass-attrs stxclass)]
                         [ctor
                          (if splicing?
                              #'reified-splicing-syntax-class
                              #'reified-syntax-class)])
             #'(ctor 'name parser 'arity '((aname adepth) ...))))])
      #`(#%expression #,stx)))

(define (reified-syntax-class-arity r)
  (match (reified-arity r)
    [(arity minpos maxpos _ _)
     (to-procedure-arity minpos maxpos)]))

(define (reified-syntax-class-keywords r)
  (match (reified-arity r)
    [(arity _ _ minkws maxkws)
     (values minkws maxkws)]))

(define (reified-syntax-class-attributes r)
  (reified-signature r))

(define reified-syntax-class-curry
  (make-keyword-procedure
   (lambda (kws1 kwargs1 r . rest1)
     (match r
       [(reified name parser arity1 sig)
        (let ()
          (check-curry arity1 (length rest1) kws1
                       (lambda (msg)
                         (raise-mismatch-error 'reified-syntax-class-curry
                                               (string-append msg ": ") r)))
          (let* ([curried-arity
                  (match arity1
                    [(arity minpos maxpos minkws maxkws)
                     (let* ([rest1-length (length rest1)]
                            [minpos* (- minpos rest1-length)]
                            [maxpos* (- maxpos rest1-length)]
                            [minkws* (sort (remq* kws1 minkws) keyword<?)]
                            [maxkws* (sort (remq* kws1 maxkws) keyword<?)])
                       (arity minpos* maxpos* minkws* maxkws*))])]
                 [curried-parser
                  (make-keyword-procedure
                   (lambda (kws2 kwargs2 x cx pr es fh cp rl success . rest2)
                     (let-values ([(kws kwargs) (merge2 kws1 kws2 kwargs1 kwargs2)])
                       (keyword-apply parser kws kwargs x cx pr es fh cp rl success
                                      (append rest1 rest2)))))]
                 [ctor
                  (cond [(reified-syntax-class? r)
                         reified-syntax-class]
                        [(reified-splicing-syntax-class? r)
                         reified-splicing-syntax-class]
                        [else
                         (error 'curry-reified-syntax-class "INTERNAL ERROR: ~e" r)])])
            (ctor name curried-parser curried-arity sig)))]))))

(define (merge2 kws1 kws2 kwargs1 kwargs2)
  (cond [(null? kws1)
         (values kws2 kwargs2)]
        [(null? kws2)
         (values kws1 kwargs1)]
        [(keyword<? (car kws1) (car kws2))
         (let-values ([(m-kws m-kwargs)
                       (merge2 (cdr kws1) kws2 (cdr kwargs1) kwargs2)])
           (values (cons (car kws1) m-kws) (cons (car kwargs1) m-kwargs)))]
        [else
         (let-values ([(m-kws m-kwargs)
                       (merge2 kws1 (cdr kws2) kwargs1 (cdr kwargs2))])
           (values (cons (car kws2) m-kws) (cons (car kwargs2) m-kwargs)))]))

;; ----

(provide reify-syntax-class
         ~reflect
         ~splicing-reflect)

(provide/contract
 [reified-syntax-class?
  (-> any/c boolean?)]
 [reified-splicing-syntax-class?
  (-> any/c boolean?)]
 [reified-syntax-class-attributes
  (-> (or/c reified-syntax-class? reified-splicing-syntax-class?)
      (listof (list/c symbol? exact-nonnegative-integer?)))]
 [reified-syntax-class-arity
  (-> (or/c reified-syntax-class? reified-splicing-syntax-class?)
      procedure-arity?)]
 [reified-syntax-class-keywords
  (-> (or/c reified-syntax-class? reified-splicing-syntax-class?)
      (values (listof keyword?)
              (listof keyword?)))]
 [reified-syntax-class-curry
  (make-contract #:name '(->* ((or/c reified-syntax-class? reified-splicing-syntax-class/c))
                              (#:<kw> any/c ...)
                              #:rest list?
                              (or/c reified-syntax-class? reified-splicing-syntax-class/c))
                 #:projection
                 (lambda (blame)
                   (let ([check-reified
                          ((contract-projection
                            (or/c reified-syntax-class? reified-splicing-syntax-class?))
                           (blame-swap blame))])
                     (lambda (f)
                       (if (and (procedure? f)
                                (procedure-arity-includes? f 1))
                           (make-keyword-procedure
                            (lambda (kws kwargs r . args)
                              (keyword-apply f kws kwargs (check-reified r) args)))
                           (raise-blame-error
                            blame
                            f
                            "expected a procedure of at least one argument, given ~e"
                            f)))))
                 #:first-order
                 (lambda (f)
                   (and (procedure? f) (procedure-arity-includes? f))))])

