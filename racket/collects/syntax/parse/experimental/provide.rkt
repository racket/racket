#lang racket/base
(require racket/contract/base
         racket/contract/combinator
         syntax/location
         (for-syntax racket/base
                     racket/syntax
                     "../private/minimatch.rkt"
                     syntax/parse/pre
                     syntax/parse/private/residual-ct ;; keep abs. path
                     "../private/kws.rkt"
                     unstable/wrapc))
(provide provide-syntax-class/contract
         syntax-class/c
         splicing-syntax-class/c)

;; FIXME:
;;   - seems to get first-requiring-module wrong, not surprising
;;   - extend to contracts on attributes?
;;   - syntax-class/c etc just a made-up name, for now
;;     (connect to dynamic syntax-classes, eventually)

(define-syntaxes (syntax-class/c splicing-syntax-class/c)
  (let ([nope
         (lambda (stx)
           (raise-syntax-error #f "not within `provide-syntax-class/contract form" stx))])
    (values nope nope)))

(begin-for-syntax
 (define-struct ctcrec (mpcs mkws mkwcs opcs okws okwcs) #:prefab
   #:omit-define-syntaxes))

(begin-for-syntax
 ;; do-one-contract : stx id stxclass ctcrec id -> stx
 (define (do-one-contract stx scname stxclass rec pos-module-source)
   ;; First, is the contract feasible?
   (match (stxclass-arity stxclass)
     [(arity minpos maxpos minkws maxkws)
      (let* ([minpos* (length (ctcrec-mpcs rec))]
             [maxpos* (+ minpos* (length (ctcrec-opcs rec)))]
             [minkws* (sort (map syntax-e (ctcrec-mkws rec)) keyword<?)]
             [maxkws* (sort (append minkws* (map syntax-e (ctcrec-okws rec))) keyword<?)])
        (define (err msg . args)
          (apply wrong-syntax scname msg args))
        (unless (<= minpos minpos*)
          (err (string-append "expected a syntax class with at most ~a "
                              "required positional arguments, got one with ~a")
               minpos* minpos))
        (unless (<= maxpos* maxpos)
          (err (string-append "expected a syntax class with at least ~a "
                              "total positional arguments (required and optional), "
                              "got one with ~a")
               maxpos* maxpos))
        (unless (null? (diff/sorted/eq minkws minkws*))
          (err (string-append "expected a syntax class with at most the "
                              "required keyword arguments ~a, got one with ~a")
               (join-sep (map kw->string minkws*) "," "and")
               (join-sep (map kw->string minkws) "," "and")))
        (unless (null? (diff/sorted/eq maxkws* maxkws))
          (err (string-append "expected a syntax class with at least the optional "
                              "keyword arguments ~a, got one with ~a")
               (join-sep (map kw->string maxkws*) "," "and")
               (join-sep (map kw->string maxkws) "," "and")))
        (with-syntax ([scname scname]
                      [#s(stxclass name arity attrs parser splicing? options integrate)
                       stxclass]
                      [#s(ctcrec (mpc ...) (mkw ...) (mkwc ...)
                                 (opc ...) (okw ...) (okwc ...))
                       rec]
                      [arity* (arity minpos* maxpos* minkws* maxkws*)]
                      [(parser-contract contracted-parser contracted-scname)
                       (generate-temporaries #`(contract parser #,scname))])
          (with-syntax ([(mpc-id ...) (generate-temporaries #'(mpc ...))]
                        [(mkwc-id ...) (generate-temporaries #'(mkwc ...))]
                        [(opc-id ...) (generate-temporaries #'(opc ...))]
                        [(okwc-id ...) (generate-temporaries #'(okwc ...))])
            (with-syntax ([((mkw-c-part ...) ...) #'((mkw mkwc-id) ...)]
                          [((okw-c-part ...) ...) #'((okw okwc-id) ...)]
                          [((mkw-name-part ...) ...) #'((mkw ,(contract-name mkwc-id)) ...)]
                          [((okw-name-part ...) ...) #'((okw ,(contract-name okwc-id)) ...)])
              #`(begin
                  (define parser-contract
                    (let ([mpc-id mpc] ...
                          [mkwc-id mkwc] ...
                          [opc-id opc] ...
                          [okwc-id okwc] ...)
                      (rename-contract
                       (->* (any/c any/c any/c any/c any/c any/c any/c any/c
                             mpc-id ... mkw-c-part ... ...)
                            (okw-c-part ... ...)
                            any)
                       `(,(if 'splicing? 'splicing-syntax-class/c 'syntax-class/c)
                         [,(contract-name mpc-id) ... mkw-name-part ... ...]
                         [okw-name-part ... ...]))))
                  (define-module-boundary-contract contracted-parser
                    parser parser-contract #:pos-source #,pos-module-source)
                  (define-syntax contracted-scname
                    (make-stxclass 
                     (quote-syntax name)
                     'arity*
                     'attrs
                     (quote-syntax contracted-parser)
                     'splicing?
                     'options
                     #f)) ;; must disable integration
                  (provide (rename-out [contracted-scname scname])))))))])))

(define-syntax (provide-syntax-class/contract stx)

  (define-syntax-class stxclass-ctc
    #:description "syntax-class/c or splicing-syntax-class/c form"
    #:literals (syntax-class/c splicing-syntax-class/c)
    #:attributes (rec)
    #:commit
    (pattern ((~or syntax-class/c splicing-syntax-class/c)
              mand:ctclist
              (~optional opt:ctclist))
             #:attr rec (make-ctcrec (attribute mand.pc.c)
                                     (attribute mand.kw)
                                     (attribute mand.kwc.c)
                                     (or (attribute opt.pc.c) '())
                                     (or (attribute opt.kw) '())
                                     (or (attribute opt.kwc.c) '()))))

  (define-syntax-class ctclist
    #:attributes ([pc.c 1] [kw 1] [kwc.c 1])
    #:commit
    (pattern ((~or pc:expr (~seq kw:keyword kwc:expr)) ...)
             #:with (pc.c ...) (for/list ([pc-expr (in-list (syntax->list #'(pc ...)))])
                                 (wrap-expr/c #'contract? pc-expr))
             #:with (kwc.c ...) (for/list ([kwc-expr (in-list (syntax->list #'(kwc ...)))])
                                  (wrap-expr/c #'contract? kwc-expr))))

  (syntax-parse stx
    [(_ [scname c:stxclass-ctc] ...)
     #:declare scname (static stxclass? "syntax class")
     (parameterize ((current-syntax-context stx))
       #`(begin (define pos-module-source (quote-module-name))
                #,@(for/list ([scname (in-list (syntax->list #'(scname ...)))]
                              [stxclass (in-list (attribute scname.value))]
                              [rec (in-list (attribute c.rec))])
                     (do-one-contract stx scname stxclass rec #'pos-module-source))))]))

;; Copied from unstable/contract,
;; which requires racket/contract, not racket/contract/base

;; rename-contract : contract any/c -> contract
;; If the argument is a flat contract, so is the result.
(define (rename-contract ctc name)
  (let ([ctc (coerce-contract 'rename-contract ctc)])
    (if (flat-contract? ctc)
        (flat-named-contract name (flat-contract-predicate ctc))
        (let* ([ctc-fo (contract-first-order ctc)]
               [proj (contract-projection ctc)])
          (make-contract #:name name
                           #:projection proj
                           #:first-order ctc-fo)))))
