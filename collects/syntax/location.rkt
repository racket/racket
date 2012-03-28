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

(define-syntax-rule (module-source)
 (variable-reference->module-source
  (#%variable-reference)))

(define-syntax (quote-module-name stx)
  (syntax-case stx ()
    [(_) #'(module-name-fixup (module-source) null)]
    [(_ "." path ...) #'(module-name-fixup (module-source) (list path ...))]
    [(_ ".." path ...) #'(module-name-fixup (module-source) (list ".." path ...))]
    [(_ path ...) #'(module-name-fixup null (list path ...))]))

(define (module-name-fixup src path)
  (or
    (cond 
      [(null? path) src]
      [(list? src) (append src path)]
      [else (append (list src) path)])
    'top-level))

(define-syntax (quote-module-path stx)
  (syntax-case stx ()
    [(_) #'(module-path-fixup (module-source) null)]
    [(_ "." path ...) #'(module-path-fixup (module-source) (list path ...))]
    [(_ ".." path ...) #'(module-path-fixup (module-source) (list ".." path ...))]
    [(_ path ...) #'(module-path-fixup null (list path ...))]))

(define (module-path-fixup src path)
  (define (map-path->string l)
    (for/list ([i l])
      (cond 
        [(path? i) (path->bytes i)]
        [else i])))
  (define (last-pass src)
    (cond
     [(path? src) `(file ,(path->bytes src))]
     [(symbol? src) `(quote ,src)]
     [(list? src) (map-path->string `(submod ,@src))]
     [else 'top-level]))
  (last-pass
    (cond 
      [(null? path) src]
      [(list? src) (append src path)]
      [else (append (list src) path)])))
