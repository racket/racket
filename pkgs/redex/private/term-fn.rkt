#lang racket/base

(require (for-template racket/base "defined-checks.rkt"))
(provide make-term-fn
         term-fn?
         term-fn-get-id
         (struct-out term-id)
         
         (struct-out judgment-form)
         
         (struct-out defined-term)
         defined-term-id?
         defined-check
         not-expression-context
         
         metafunc-proc-clause-names
         metafunc-proc-pict-info
         metafunc-proc-lang
         metafunc-proc-multi-arg?
         metafunc-proc-name
         metafunc-proc-in-dom?
         metafunc-proc-dom-pat
         metafunc-proc-cases
         metafunc-proc-gen-clauses
         metafunc-proc?
         make-metafunc-proc
         
         make-language-id
         language-id-nts
         language-id-nt-identifiers
         pattern-symbols
         
         build-disappeared-use)

(define-values (struct-type make-term-fn term-fn? term-fn-get term-fn-set!) 
  (make-struct-type 'term-fn #f 1 0))
(define term-fn-get-id (make-struct-field-accessor term-fn-get 0))

(define-struct term-id (id depth prev-id))

(define (transformer-predicate p? stx)
  (and (identifier? stx)
       (cond [(syntax-local-value stx (λ () #f)) => p?]
             [else #f])))

(define-struct judgment-form (name mode proc mk-proc lang lws rule-names gen-clauses mk-gen-clauses term-proc relation?)
  #:transparent)

(define-struct defined-term (value))
(define (defined-term-id? stx)
  (transformer-predicate defined-term? stx))

(define (defined-check id desc #:external [external id])
  (if (eq? (identifier-binding id) 'lexical)
      (quasisyntax/loc external (check-defined-lexical #,id '#,external #,desc))
      (quasisyntax/loc external (check-defined-module (λ () #,id) '#,external #,desc))))

(define (not-expression-context stx)
  (when (eq? (syntax-local-context) 'expression)
    (raise-syntax-error #f "not allowed in an expression context" stx)))

(define-values (language-id make-language-id language-id? language-id-get language-id-set) 
  (make-struct-type 'language-id #f 3 0 #f '() #f 0))

(define (language-id-nts stx id) (language-id-getter stx id 1))
(define (language-id-getter stx id n)
  (unless (identifier? stx)
    (raise-syntax-error id "expected an identifier defined by define-language" stx))
  (let ([val (syntax-local-value stx (λ () #f))])
    (unless (and (set!-transformer? val)
                 (language-id? (set!-transformer-procedure val)))
      (raise-syntax-error id "expected an identifier defined by define-language" stx))
    (language-id-get (set!-transformer-procedure val) n)))
(define (language-id-nt-identifiers stx id) (language-id-getter stx id 2))

(define pattern-symbols '(any number natural integer real string variable 
                              variable-not-otherwise-mentioned hole symbol))

(define-values (struct:metafunc-proc make-metafunc-proc metafunc-proc? metafunc-proc-ref metafunc-proc-set!)
  (make-struct-type 'metafunc-proc #f 10 0 #f null (current-inspector) 0))
(define metafunc-proc-clause-names (make-struct-field-accessor metafunc-proc-ref 1))
(define metafunc-proc-pict-info (make-struct-field-accessor metafunc-proc-ref 2))
(define metafunc-proc-lang (make-struct-field-accessor metafunc-proc-ref 3))
(define metafunc-proc-multi-arg? (make-struct-field-accessor metafunc-proc-ref 4))
(define metafunc-proc-name (make-struct-field-accessor metafunc-proc-ref 5))
(define metafunc-proc-in-dom? (make-struct-field-accessor metafunc-proc-ref 6))
(define metafunc-proc-dom-pat (make-struct-field-accessor metafunc-proc-ref 7))
(define metafunc-proc-cases (make-struct-field-accessor metafunc-proc-ref 8))
(define metafunc-proc-gen-clauses (make-struct-field-accessor metafunc-proc-ref 9))


(define (build-disappeared-use id-stx-table nt id-stx)
  (cond
    [id-stx-table
     (define table-entry (hash-ref id-stx-table nt #f))
     (cond
       [table-entry
        (define the-srcloc (vector
                            (syntax-source id-stx)
                            (syntax-line id-stx)
                            (syntax-column id-stx)
                            (syntax-position id-stx)
                            ;; shorten the span so it covers only up to the underscore
                            (string-length (symbol->string nt))))
        (define the-id (datum->syntax table-entry nt the-srcloc id-stx))
        (syntax-property the-id 'original-for-check-syntax #t)]
       [else
        #f])]
    [else
     #f]))
