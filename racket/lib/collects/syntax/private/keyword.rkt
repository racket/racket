#lang racket/base

;; No-contract version...

(require syntax/stx
         racket/private/dict) ;; no contracts
(provide parse-keyword-options
         parse-keyword-options/eol
         options-select
         options-select-row
         options-select-value

         check-expression
         check-identifier
         check-stx-boolean
         check-stx-string
         check-stx-listof)

;; Parsing keyword arguments

;; KeywordTable = (listof (cons keyword (listof CheckProc)))
;; Options = (listof (list* keyword syntax-keyword (listof any)))

;; CheckProc = syntax syntax -> any
;; The first arg is syntax to check, second arg is context.

;; incompatible-handler : keyword keyword Options syntax syntax -> (values Options syntax)
(define (default-incompatible kw1 kw2 chunks stx ctx)
  (if (eq? kw1 kw2)
      (raise-syntax-error #f "duplicate keyword option" ctx (stx-car stx))
      (raise-syntax-error #f
                          (format "~s option not allowed after ~s option" kw2 kw1)
                          ctx (stx-car stx))))

;; too-short-handler : keyword Options syntax syntax -> (values Options syntax)
(define (default-too-short kw chunks stx ctx)
  (raise-syntax-error #f "too few arguments for keyword" ctx (stx-car stx)))

;; not-in-table-handler : keyword syntax syntax -> (values Options syntax)
(define ((default-not-in-table kws) kw stx ctx)
  (raise-syntax-error #f
                      (format "unexpected keyword, expected one of ~s" kws)
                      ctx (stx-car stx)))

;; not-eol-handler : Options syntax syntax -> (values Options syntax)
(define (default-not-eol chunks stx ctx)
  (raise-syntax-error #f
                      "terms left over after keyword options"
                      ctx
                      stx))

(define (parse-keyword-options/eol stx table
                                   #:context [ctx #f]
                                   #:no-duplicates? [no-duplicates? #f]
                                   #:incompatible [incompatible null]
                                   #:on-incompatible [incompatible-handler default-incompatible]
                                   #:on-too-short [too-short-handler default-too-short]
                                   #:on-not-in-table [not-in-table-handler
                                                      (default-not-in-table (map car table))]
                                   #:on-not-eol [not-eol-handler default-not-eol])
  (define-values (chunks rest)
    (parse-keyword-options stx table
                           #:context ctx
                           #:no-duplicates? no-duplicates?
                           #:incompatible incompatible
                           #:on-incompatible incompatible-handler
                           #:on-too-short too-short-handler
                           #:on-not-in-table not-in-table-handler))
  (if (stx-null? rest)
      chunks
      (not-eol-handler chunks stx ctx)))

(define (list-ne-tails lst)
  (if (pair? lst)
      (cons lst (list-ne-tails (cdr lst)))
      null))

;; parse-keyword-options : syntax KeywordTable ... -> (values Options syntax)
;; incompatible-handler is also used for duplicates (same kw arg)
;; incompatible is (listof (list keyword keyword)); reflexive closure taken
(define (parse-keyword-options stx table
                               #:context [ctx #f]
                               #:no-duplicates? [no-duplicates? #f]
                               #:incompatible [incompatible null]
                               #:on-incompatible [incompatible-handler default-incompatible]
                               #:on-too-short [too-short-handler default-too-short]
                               #:on-not-in-table [not-in-table-handler
                                                  (default-not-in-table (map car table))])
  (define interfere-table
    (let ([table (make-hash)])
      (for ([entry incompatible])
        (for ([tail (list-ne-tails entry)])
          (for ([next (cdr tail)])
            (hash-set! table (list (car tail) next) #t)
            (hash-set! table (list next (car tail)) #t))))
      table))
  (define (interferes kw seen)
    (for/or ([seen-kw (in-dict-keys seen)])
      (and (hash-ref interfere-table (list seen-kw kw) #f)
           seen-kw)))
  (define (loop stx rchunks seen)
    (syntax-case stx ()
      [(kw . more)
       (keyword? (syntax-e #'kw))
       (let* ([kw-value (syntax-e #'kw)]
              [entry (assq kw-value table)])
         (cond [(and no-duplicates?
                     (hash-ref seen kw-value #f))
                (incompatible-handler kw-value kw-value (reverse rchunks) stx ctx)]
               [(interferes kw-value seen) =>
                (lambda (seen-kw)
                  (incompatible-handler seen-kw kw-value (reverse rchunks) stx ctx))]
               [entry
                (let* ([arity (cdr entry)]
                       [args+rest (stx-split #'more arity)])
                  (if args+rest
                      (let ([args (for/list ([arg (car args+rest)] [proc arity])
                                    (proc arg ctx))]
                            [rest (cdr args+rest)])
                        (loop rest
                              (cons (list* kw-value #'kw args) rchunks)
                              (hash-set seen kw-value #t)))
                      (too-short-handler kw-value (reverse rchunks) stx ctx)))]
               [else
                (not-in-table-handler kw-value stx ctx)]))]
      [_
       (values (reverse rchunks) stx)]))
  (loop stx null (make-immutable-hasheq '())))

;; stx-split : stx (listof any) -> (cons (listof stx) stx)
(define (stx-split stx arity)
  (define (loop stx arity acc)
    (cond [(null? arity)
           (cons (reverse acc) stx)]
          [(stx-pair? stx)
           (loop (stx-cdr stx) (cdr arity) (cons (stx-car stx) acc))]
          [else #f]))
  (loop stx arity null))

;; options-select : Options keyword -> (listof (listof any))
(define (options-select chunks kw)
  (for/list ([chunk chunks]
             #:when (eq? kw (car chunk)))
    (cddr chunk)))

;; options-select-row : Options keyword -> any
(define (options-select-row chunks kw #:default default)
  (let ([results (options-select chunks kw)])
    (cond [(null? results)
           default]
          [(null? (cdr results))
           (car results)]
          [else
           (error 'options-select-row
                  "multiple occurrences of ~s keyword" kw)])))

;; options-select-value : Options keyword -> any
(define (options-select-value chunks kw #:default default)
  (let ([results (options-select chunks kw)])
    (cond [(null? results)
           default]
          [(null? (cdr results))
           (let ([row (car results)])
             (cond [(null? row)
                    (error 'options-select-value
                           "keyword ~s has no arguments" kw)]
                   [(null? (cdr row))
                    (car row)]
                   [else
                    (error 'options-select-value
                           "keyword ~s has more than one argument" kw)]))]
          [else
           (error 'options-select-value
                  "multiple occurrences of ~s keyword" kw)])))

;; Check Procedures

;; check-identifier : stx stx -> identifier
(define (check-identifier stx ctx)
  (unless (identifier? stx)
    (raise-syntax-error #f "expected identifier" ctx stx))
  stx)

;; check-expression : stx stx -> stx
(define (check-expression stx ctx)
  (when (keyword? (syntax-e stx))
    (raise-syntax-error #f "expected expression" ctx stx))
  stx)

;; check-stx-string : stx stx -> stx
(define (check-stx-string stx ctx)
  (unless (string? (syntax-e stx))
    (raise-syntax-error #f "expected string" ctx stx))
  stx)

;; check-stx-boolean : stx stx -> stx
(define (check-stx-boolean stx ctx)
  (unless (boolean? (syntax-e stx))
    (raise-syntax-error #f "expected boolean" ctx stx))
  stx)

#|
;; check-nat/f : stx stx -> stx
(define (check-nat/f stx ctx)
  (let ([d (syntax-e stx)])
    (unless (or (eq? d #f) (exact-nonnegative-integer? d))
      (raise-syntax-error #f "expected exact nonnegative integer or #f" ctx stx))
    stx))
|#

;; check-stx-listof : (stx stx -> A) -> stx stx -> (listof A)
(define ((check-stx-listof check) stx ctx)
  (unless (stx-list? stx)
    (raise-syntax-error #f "expected list" ctx stx))
  (for/list ([x (stx->list stx)])
    (check x ctx)))
