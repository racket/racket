#lang racket/base

(require
 racket/format
 "template.rkt"
 "render.rkt"
 (for-syntax
  racket/base
  racket/port
  racket/string
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
  (define-values (strings expression-infos)
    (parse-template-parts who context-stx input-stx)
  ) ; end define-values
  #`(template (list #,@(map datum->syntax-literal strings))
              (list #,@(map interpolation-syntax expression-infos))
    ) ; end template
) ; end define-for-syntax expand-template

(define-for-syntax (expand-fstring who context-stx input-stx)
  (define-values (strings expression-infos)
    (parse-template-parts who context-stx input-stx)
  ) ; end define-values
  (cond
    ((null? expression-infos)
     (datum->syntax-literal (car strings))
    ) ; end static f-string
    (else
     #`(string-append #,@(fstring-append-parts strings expression-infos))
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
                   (source-string->expression-info who context-stx source)
                 ) ; end lambda
                 expression-sources
            ) ; end map
    ) ; end values
  ) ; end with-handlers
) ; end define-for-syntax parse-template-parts

(define-for-syntax (source-string->expression-info who context-stx source)
  (define transformed-source (transform-template-prefixes source))
  (define port (open-input-string transformed-source))
  (define expression-stx (read-syntax 'template-interpolation port))
  (when (eof-object? expression-stx)
    (raise-syntax-error who "empty interpolation is not allowed" context-stx)
  ) ; end when eof
  (define-values (conversion format-spec)
    (parse-interpolation-suffix (port->string port))
  ) ; end define-values
  (list (datum->syntax context-stx
                       (syntax->datum expression-stx)
                       context-stx
                       context-stx
        ) ; end datum->syntax
        format-spec
        conversion
  ) ; end list
) ; end define-for-syntax source-string->expression-info

(define-for-syntax (parse-interpolation-suffix suffix)
  (define text (string-trim suffix))
  (cond
    ((equal? text "")
     (values "" #f)
    ) ; end empty suffix
    (else
     (define parts (regexp-split #px"\\s+" text))
     (define first-part (car parts))
     (cond
       ((conversion-token? first-part)
        (values first-part
                (let ((rest (string-trim (substring text (string-length first-part)))))
                  (if (equal? rest "") #f rest)
                ) ; end let
        ) ; end values
       ) ; end conversion and maybe format spec
       (else
        (values "" text)
       ) ; end format spec only
     ) ; end cond
    ) ; end non-empty suffix
  ) ; end cond
) ; end define-for-syntax parse-interpolation-suffix

(define-for-syntax (conversion-token? text)
  (regexp-match? #px"^[A-Za-z]+$" text)
) ; end define-for-syntax conversion-token?

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

(define-for-syntax (fstring-append-parts strings expression-infos)
  (let loop ((strings strings)
             (expression-infos expression-infos)
        ) ; end loop bindings
    (cond
      ((null? expression-infos)
       (list (datum->syntax-literal (car strings)))
      ) ; end last static string
      (else
       (cons (datum->syntax-literal (car strings))
             (cons (fstring-interpolation-syntax (car expression-infos))
                   (loop (cdr strings)
                         (cdr expression-infos)
                   ) ; end loop
             ) ; end cons expression
       ) ; end cons string
      ) ; end more expressions
    ) ; end cond
  ) ; end let loop
) ; end define-for-syntax fstring-append-parts

(define-for-syntax (fstring-interpolation-syntax expression-info)
  (define expression-stx (list-ref expression-info 0))
  (define format-spec (list-ref expression-info 1))
  (define conversion (list-ref expression-info 2))
  #`(format-fstring-value #,expression-stx
                          '#,format-spec
                          '#,conversion
    ) ; end format-fstring-value
) ; end define-for-syntax fstring-interpolation-syntax

(define-for-syntax (interpolation-syntax expression-info)
  (define expression-stx (list-ref expression-info 0))
  (define format-spec (list-ref expression-info 1))
  (define conversion (list-ref expression-info 2))
  #`(interpolation #,expression-stx
                   (quote-syntax #,expression-stx)
                   '#,format-spec
                   '#,conversion
    ) ; end interpolation
) ; end define-for-syntax interpolation-syntax

(define-for-syntax (nested-template-var-usages source)
  (define length (string-length source))
  (let loop ((index 0)
             (vars '())
        ) ; end loop bindings
    (cond
      ((= index length)
       vars
      ) ; end end of source
      ((char=? (string-ref source index) #\")
       (loop (find-racket-string-end source index)
             vars
       ) ; end loop
      ) ; end ordinary string
      ((template-prefix-at? source index)
       (define end-index (find-template-source-end source (add1 index)))
       (define content (substring source (+ index 2) (sub1 end-index)))
       (define-values (_strings expression-sources)
         (parse-template-string content)
       ) ; end define-values
       (define nested-vars
         (foldl (lambda (expression-source acc)
                  (append-unique acc (source-string-var-usages expression-source))
                ) ; end lambda
                '()
                expression-sources
         ) ; end foldl
       ) ; end define nested-vars
       (loop end-index
             (append-unique vars nested-vars)
       ) ; end loop
      ) ; end nested template
      (else
       (loop (add1 index)
             vars
       ) ; end loop
      ) ; end source character
    ) ; end cond
  ) ; end let loop
) ; end define-for-syntax nested-template-var-usages

(define-for-syntax (source-string-var-usages source)
  (define expression-stx (source-string->syntax 'tpl #'here source))
  (append-unique (syntax-var-usages expression-stx)
                 (nested-template-var-usages source)
  ) ; end append-unique
) ; end define-for-syntax source-string-var-usages

(define-for-syntax (append-unique left right)
  (let loop ((items right)
             (result left)
        ) ; end loop bindings
    (cond
      ((null? items)
       result
      ) ; end no more items
      ((memq (car items) result)
       (loop (cdr items)
             result
       ) ; end loop
      ) ; end already present
      (else
       (loop (cdr items)
             (append result (list (car items)))
       ) ; end loop
      ) ; end new item
    ) ; end cond
  ) ; end let loop
) ; end define-for-syntax append-unique

(define-for-syntax (syntax-var-usages stx)
  (define seen '())
  (define ordered '())
  (define (add! sym)
    (unless (memq sym seen)
      (set! seen (cons sym seen))
      (set! ordered (cons sym ordered))
    ) ; end unless seen
  ) ; end define add!
  (define (walk datum quoted?)
    (cond
      ((symbol? datum)
       (unless quoted?
         (unless (memq datum '(tpl fpl))
           (add! datum)
         ) ; end unless tstring macro
       ) ; end unless quoted
      ) ; end symbol
      ((pair? datum)
       (define head (car datum))
       (define next-quoted?
         (or quoted?
             (and (symbol? head)
                  (memq head '(quote quote-syntax))
             ) ; end and
         ) ; end or
       ) ; end define next-quoted?
       (for ((item (in-list datum)))
         (walk item next-quoted?)
       ) ; end for
      ) ; end pair
      ((vector? datum)
       (for ((item (in-vector datum)))
         (walk item quoted?)
       ) ; end for
      ) ; end vector
      (else
       (void)
      ) ; end other datum
    ) ; end cond
  ) ; end define walk
  (walk (syntax->datum stx) #f)
  (reverse ordered)
) ; end define-for-syntax syntax-var-usages
