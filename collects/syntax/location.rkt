#lang racket/base
(require syntax/srcloc
         (for-syntax racket/base syntax/srcloc setup/path-to-relative))
(provide quote-srcloc
         quote-source-file
         quote-line-number
         quote-column-number
         quote-character-position
         quote-character-span
         quote-module-path
         quote-module-name)

(define-syntax (quote-srcloc stx)
  (syntax-case stx ()
    [(_) #`(quote-srcloc #,stx)]
    [(_ loc)
     (let* ([src (build-source-location #'loc)])
       (cond
        [(and (path-string? (srcloc-source src))
              (path->relative-string/library (srcloc-source src) #f))
         =>
         (lambda (rel)
           (with-syntax ([src rel]
                         [line (srcloc-line src)]
                         [col (srcloc-column src)]
                         [pos (srcloc-position src)]
                         [span (srcloc-span src)])
             #'(make-srcloc 'src 'line 'col 'pos 'span)))]
        [else (with-syntax ([loc (identifier-prune-to-source-module
                                  (datum->syntax #'loc 'loc #'loc #'loc))])
                #'(build-source-location (quote-syntax loc)))]))]))

(define-syntax-rule (define-quote-srcloc-accessors [name accessor] ...)
  (define-syntaxes [ name ... ]
    (values
     (lambda (stx)
       (syntax-case stx ()
         [(_) #`(name #,stx)]
         [(_ loc) #`(accessor (quote-srcloc loc))]))
     ...)))

(define-quote-srcloc-accessors
  [quote-source-file source-location-source]
  [quote-line-number source-location-line]
  [quote-column-number source-location-column]
  [quote-character-position source-location-position]
  [quote-character-span source-location-span])

(define-syntax-rule (quote-module-name)
  (module-source->module-name
   (variable-reference->module-source
    (#%variable-reference))))

(define-syntax-rule (quote-module-path)
  (module-source->module-path
   (variable-reference->module-source
    (#%variable-reference))))

(define (module-source->module-name src)
  (or src 'top-level))

(define (module-source->module-path src)
  (cond
   [(path? src) `(file ,(path->string src))]
   [(symbol? src) `(quote ,src)]
   [else 'top-level]))
