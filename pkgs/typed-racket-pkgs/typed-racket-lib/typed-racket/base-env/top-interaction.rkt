#lang racket/base

(require
  (for-syntax racket/base racket/lazy-require syntax/parse))

(begin-for-syntax
  (lazy-require [(submod "." implementation)
                 (:type-impl :print-type-impl :query-type/args-impl :query-type/result-impl)]))

(provide
  (for-syntax
    interactive-command?
    interactive-command-procedure)
  :type :print-type :query-type/args :query-type/result)

(define-for-syntax (fail _ stx)
  (syntax-parse stx
    [_:id
     (raise-syntax-error #f "must be applied to arguments" stx)]
    [_ (raise-syntax-error #f "only valid at the top-level of an interaction" stx)]))


(begin-for-syntax
  (struct interactive-command (procedure)
    #:property prop:procedure fail))


(define-syntax :type (interactive-command :type-impl))
(define-syntax :print-type (interactive-command :print-type-impl))
(define-syntax :query-type/args (interactive-command :query-type/args-impl))
(define-syntax :query-type/result (interactive-command :query-type/result-impl))

(module implementation racket/base
  (require
    "../utils/utils.rkt"
    racket/base
    racket/match
    racket/format
    racket/string
    racket/list
    syntax/stx
    syntax/parse
    "../tc-setup.rkt"
    (private parse-type syntax-properties)
    (types utils abbrev printer)
    (typecheck tc-toplevel tc-app-helper)
    (rep type-rep)
    (utils tc-utils)
    (for-template racket/base))
  (provide
    :type-impl :print-type-impl :query-type/args-impl :query-type/result-impl)

  (define (:type-impl stx)
    (syntax-parse stx
      [(_ (~optional (~and #:verbose verbose-kw)) ty:expr)
       (parameterize ([current-print-type-fuel
                       (if (attribute verbose-kw) +inf.0 1)]
                      ;; This makes sure unions are totally flat for the
                      ;; infinite fuel case. If fuel that's not 0, 1, or +inf.0
                      ;; is ever used, more may need to be done.
                      [current-type-names
                       (if (attribute verbose-kw) '() (current-type-names))]
                      [current-print-unexpanded (box '())])
         (define type (pretty-format-type (parse-type #'ty)))
         (define unexpanded
           (remove-duplicates (unbox (current-print-unexpanded))))
         (define cue (if (null? unexpanded)
                         ""
                         (format "[can expand further: ~a]"
                                 (string-join (map ~a unexpanded)))))
         #`(display #,(format "~a\n~a" type cue)))]
      [form
       (raise-syntax-error #f "must be applied to exactly one argument" #'form)]))

  ;; TODO what should be done with stx
  ;; Prints the _entire_ type. May be quite large.
  (define (:print-type-impl stx)
    (syntax-parse stx
      [(_ e)
       (tc-toplevel/full stx #'e
         (λ (expanded type)
           #`(display
              #,(if (eq? type 'no-type)
                    "This form has no type (it does not produce a value)."
                    (pretty-format-type
                     (match type
                       [(tc-result1: t f o) t]
                       [(tc-results: t) (-values t)]
                       [(tc-any-results: f) (-AnyValues f)]))))))]
      [form
       (raise-syntax-error #f "must be applied to exactly one argument" #'form)]))

  ;; given a function and input types, display the result type
  (define (:query-type/args-impl stx)
    (syntax-parse stx
      [(_ op arg-type ...)
       (with-syntax ([(dummy-arg ...) (generate-temporaries #'(arg-type ...))])
         (tc-toplevel/full
           stx
           ;; create a dummy function with the right argument types
           #`(lambda #,(stx-map type-label-property
                                #'(dummy-arg ...) #'(arg-type ...))
               (op dummy-arg ...))
           (λ (expanded type)
             #`(display
                #,(pretty-format-type
                   (match type
                     [(tc-result1: (and t (Function: _)) f o) t]))))))]
      [form
       (raise-syntax-error #f "must be applied to at least one argument" #'form)]))

  ;; given a function and a desired return type, fill in the blanks
  (define (:query-type/result-impl stx)
    (syntax-parse stx
      [(_ op desired-type)
       (let ([expected (parse-type #'desired-type)])
         (tc-toplevel/full stx #'op
           (λ (expanded type)
             (match type
               [(tc-result1: (and t (Function: _)) f o)
                (let ([cleaned (cleanup-type t expected #f)])
                  #`(display
                     #,(match cleaned
                         [(Function: '())
                          "Desired return type not in the given function's range.\n"]
                         [(Function: arrs)
                          (pretty-format-type cleaned)])))]
               [_ (error (format "~a: not a function" (syntax->datum #'op)))]))))]
      [form
       (raise-syntax-error #f "must be applied to exactly two arguments" #'form)])))
