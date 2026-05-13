#lang racket/base

(require
 racket/format
 "template.rkt"
) ; end require

(provide
 render-template
 render-fstring
 template->sql
 html-render
) ; end provide

(define (checked-template-parts who tpl)
  (unless (template? tpl)
    (raise-argument-error who "template?" tpl)
  ) ; end unless template?
  (define strings (template-strings tpl))
  (define interpolations (template-interpolations tpl))
  (unless (and (list? strings)
               (andmap string? strings)
          ) ; end and
    (raise-argument-error who "(listof string?)" strings)
  ) ; end unless strings
  (unless (and (list? interpolations)
               (andmap interpolation? interpolations)
          ) ; end and
    (raise-argument-error who "(listof interpolation?)" interpolations)
  ) ; end unless interpolations
  (unless (= (length strings)
             (add1 (length interpolations))
          ) ; end =
    (raise-arguments-error who
                           "template strings and interpolations have incompatible lengths"
                           "strings"
                           strings
                           "interpolations"
                           interpolations
    ) ; end raise-arguments-error
  ) ; end unless length invariant
  (values strings interpolations)
) ; end define checked-template-parts

(define (render-template tpl
                         #:interpolation->string (interpolation->string interpolation-value)
                         #:value->string (value->string ~a)
        ) ; end define formals
  (define-values (strings interpolations)
    (checked-template-parts 'render-template tpl)
  ) ; end define-values
  (define out (open-output-string))
  (let loop ((strings strings)
             (interpolations interpolations)
        ) ; end loop bindings
    (write-string (car strings) out)
    (cond
      ((null? interpolations)
       (void)
      ) ; end no more interpolations
      (else
       (write-string (value->string (interpolation->string (car interpolations))) out)
       (loop (cdr strings)
             (cdr interpolations)
       ) ; end loop
      ) ; end interpolation part
    ) ; end cond
  ) ; end let loop
  (get-output-string out)
) ; end define render-template

(define (render-fstring tpl)
  (render-template tpl)
) ; end define render-fstring

(define (template->sql tpl)
  (define-values (strings interpolations)
    (checked-template-parts 'template->sql tpl)
  ) ; end define-values
  (define out (open-output-string))
  (define params
    (let loop ((strings strings)
               (interpolations interpolations)
          ) ; end loop bindings
      (write-string (car strings) out)
      (cond
        ((null? interpolations)
         '()
        ) ; end no more interpolations
        (else
         (define part (car interpolations))
         (unless (identifier? (interpolation-syntax part))
           (raise-arguments-error 'template->sql
                                  "SQL template only accepts identifier slots"
                                  "interpolation"
                                  part
           ) ; end raise-arguments-error
         ) ; end unless identifier
         (write-string "?" out)
         (cons (interpolation-value part)
               (loop (cdr strings)
                     (cdr interpolations)
               ) ; end loop
         ) ; end cons
        ) ; end interpolation part
      ) ; end cond
    ) ; end let loop
  ) ; end define params
  (values (get-output-string out)
          params
  ) ; end values
) ; end define template->sql

(define (html-render tpl #:interpolation->string (interpolation->string interpolation-value))
  (render-template tpl
                   #:interpolation->string interpolation->string
                   #:value->string html-escape
  ) ; end render-template
) ; end define html-render

(define (html-escape value)
  (define out (open-output-string))
  (for ((ch (in-string (~a value))))
    (write-string
     (case ch
       ((#\<) "&lt;")
       ((#\>) "&gt;")
       ((#\&) "&amp;")
       ((#\") "&quot;")
       (else (string ch))
     ) ; end case
     out
    ) ; end write-string
  ) ; end for
  (get-output-string out)
) ; end define html-escape
