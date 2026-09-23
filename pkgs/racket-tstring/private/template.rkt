#lang racket/base

(provide
 template
 template?
 template-parts
 template-strings
 template-interpolations
 interpolation
 interpolation?
 interpolation-value
 interpolation-syntax
 interpolation-format-spec
 interpolation-conversion
) ; end provide

(struct template-data (strings interpolations) #:transparent #:reflection-name 'template)
(struct interpolation (value syntax format-spec conversion) #:transparent)

(define (template strings interpolations)
  (template-data strings interpolations)
) ; end define template

(define template? template-data?)
(define template-strings template-data-strings)
(define template-interpolations template-data-interpolations)

(define (template-parts tpl)
  (let loop ((strings (template-strings tpl))
             (interpolations (template-interpolations tpl))
        ) ; end loop bindings
    (cond
      ((null? interpolations)
       strings
      ) ; end no more interpolations
      (else
       (cons (car strings)
             (cons (car interpolations)
                   (loop (cdr strings)
                         (cdr interpolations)
                   ) ; end loop
             ) ; end cons interpolation
       ) ; end cons string
      ) ; end more interpolations
    ) ; end cond
  ) ; end let loop
) ; end define template-parts
