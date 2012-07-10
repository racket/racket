#lang racket/base
(require syntax/srcloc
         (for-syntax racket/base syntax/srcloc setup/path-to-relative))
(provide (protect-out module-name-fixup)
         quote-srcloc
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

(define (variable-reference->module-source/submod vr)
  (define src (variable-reference->module-source vr))
  (define rname (variable-reference->resolved-module-path vr))
  (define name (and rname (resolved-module-path-name rname)))
  (if (pair? name)
      (cons src (cdr name))
      src))

(define-syntax-rule (module-source)
 (variable-reference->module-source/submod
  (#%variable-reference)))

(define-for-syntax (do-quote-module stx fixup)
  (syntax-case stx ()
    [(_ path ...) 
     (for ([path (in-list (syntax->list #'(path ...)))]
           [i (in-naturals)])
       (unless (or (symbol? (syntax-e path))
                   (equal? (syntax-e path) ".."))
         (raise-syntax-error #f "not a submodule path element" stx path)))
     (with-syntax ([fixup fixup])
       #'(fixup (module-source) (list 'path ...)))]))

(define-syntax (quote-module-name stx)
  (do-quote-module stx #'module-name-fixup))

(define (module-name-fixup src path)
  (do-fixup src path #f))

(define-syntax (quote-module-path stx)
  (do-quote-module stx #'module-path-fixup))
    
(define (module-path-fixup src path)
  (do-fixup src path #t))

(define (do-fixup src path as-modpath?)
  (define (last-pass src)
    (cond
     [(path? src) src]
     [(symbol? src) (if as-modpath?
                        `(quote ,src)
                        src)]
     [(list? src) 
      (define base (last-pass (car src)))
      (define sm (cdr src))
      (if as-modpath?
          `(submod ,base ,@sm)
          (cons base sm))]
     [else 'top-level]))
  (last-pass
    (cond 
      [(null? path) src]
      [(pair? src) (append src path)]
      [else (cons src path)])))
