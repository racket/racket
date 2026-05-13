#lang racket/base

(require
 racket/format
 "template.rkt"
 "render.rkt"
 (for-syntax
  racket/base
  "parse.rkt"
  "source-transform.rkt"
 ) ; end for-syntax
) ; end require

(provide
 tpl
 fpl
) ; end provide

(define-syntax (tpl stx)
  (syntax-case stx ()
    ((_ input)
     (string? (syntax-e #'input))
     (expand-template 'tpl stx #'input)
    ) ; end literal string case
    (_
     (raise-syntax-error 'tpl "expected a string literal" stx)
    ) ; end invalid syntax
  ) ; end syntax-case
) ; end define-syntax tpl

(define-syntax (fpl stx)
  (syntax-case stx ()
    ((_ input)
     (string? (syntax-e #'input))
     (expand-fstring 'fpl stx #'input)
    ) ; end literal string case
    (_
     (raise-syntax-error 'fpl "expected a string literal" stx)
    ) ; end invalid syntax
  ) ; end syntax-case
) ; end define-syntax fpl

(define-for-syntax (expand-template who context-stx input-stx)
  (define-values (strings expression-stxs)
    (parse-template-parts who context-stx input-stx)
  ) ; end define-values
  #`(template (list #,@(map datum->syntax-literal strings))
              (list #,@(map interpolation-syntax expression-stxs))
    ) ; end template
) ; end define-for-syntax expand-template

(define-for-syntax (expand-fstring who context-stx input-stx)
  (define-values (strings expression-stxs)
    (parse-template-parts who context-stx input-stx)
  ) ; end define-values
  (cond
    ((null? expression-stxs)
     (datum->syntax-literal (car strings))
    ) ; end static f-string
    (else
     #`(string-append #,@(fstring-append-parts strings expression-stxs))
    ) ; end dynamic f-string
  ) ; end cond
) ; end define-for-syntax expand-fstring

(define-for-syntax (parse-template-parts who context-stx input-stx)
  (with-handlers ((exn:fail?
                   (lambda (exn)
                     (raise-syntax-error who
                                         (exn-message exn)
                                         context-stx
                                         input-stx
                     ) ; end raise-syntax-error
                   ) ; end lambda
                  ) ; end exn:fail?
                 ) ; end handlers
    (define-values (strings expression-sources)
      (parse-template-string (syntax-e input-stx))
    ) ; end define-values
    (values strings
            (map (lambda (source)
                   (source-string->syntax who context-stx source)
                 ) ; end lambda
                 expression-sources
            ) ; end map
    ) ; end values
  ) ; end with-handlers
) ; end define-for-syntax parse-template-parts

(define-for-syntax (datum->syntax-literal value)
  #`'#,value
) ; end define-for-syntax datum->syntax-literal

(define-for-syntax (source-string->syntax who context-stx source)
  (define transformed-source (transform-template-prefixes source))
  (define port (open-input-string transformed-source))
  (define read-stx (read-syntax 'template-interpolation port))
  (when (eof-object? read-stx)
    (raise-syntax-error who "empty interpolation is not allowed" context-stx)
  ) ; end when eof
  (define extra-stx (read-syntax 'template-interpolation port))
  (unless (eof-object? extra-stx)
    (raise-syntax-error who "interpolation must contain exactly one expression" context-stx)
  ) ; end unless eof
  (datum->syntax context-stx
                 (syntax->datum read-stx)
                 context-stx
                 context-stx
  ) ; end datum->syntax
) ; end define-for-syntax source-string->syntax

(define-for-syntax (fstring-append-parts strings expression-stxs)
  (let loop ((strings strings)
             (expression-stxs expression-stxs)
        ) ; end loop bindings
    (cond
      ((null? expression-stxs)
       (list (datum->syntax-literal (car strings)))
      ) ; end last static string
      (else
       (cons (datum->syntax-literal (car strings))
             (cons #`(~a #,(car expression-stxs))
                   (loop (cdr strings)
                         (cdr expression-stxs)
                   ) ; end loop
             ) ; end cons expression
       ) ; end cons string
      ) ; end more expressions
    ) ; end cond
  ) ; end let loop
) ; end define-for-syntax fstring-append-parts

(define-for-syntax (interpolation-syntax expression-stx)
  (define kind
    (if (identifier? expression-stx)
        'identifier
        'expression
    ) ; end if
  ) ; end define kind
  #`(interpolation #,expression-stx
                   (quote-syntax #,expression-stx)
                   '#,kind
                   (quote-syntax #,expression-stx)
    ) ; end interpolation
) ; end define-for-syntax interpolation-syntax
