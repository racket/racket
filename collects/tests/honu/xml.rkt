#lang racket/base

(require honu/core/api
         racket/match
         (only-in honu [/ honu-/])
         (for-syntax syntax/parse
                     racket/base))

(provide xml
         xml->string
         < >)

(define-literal < >)

(struct xml:object (tag elements))

(begin-for-syntax
(define (debug . x)
  (void)
  #;
  (apply printf x)))

(define (xml->string xml)
  (match xml
    [(struct xml:object (tag elements))
     (format "<~a>~a</~a>"
             tag
             (apply string-append (map xml->string elements))
             tag)]
    [else (format "~a" xml)]))

(define (make-xml name . stuff)
  (xml:object name stuff))

(define-honu-syntax xml
  (lambda (code)
    (define-literal-set xml-literals (< > honu-/))

    (define-splicing-syntax-class non-xml-id #:literal-sets (xml-literals)
      [pattern (~and (~not <) x:id) #:with q (begin (debug "non xml id ~a\n" #'x) #'x)]
      [pattern x #:with q #'0 #:when (begin (debug "not an xml id ~a\n" #'x) #f)]
      )

    (define-splicing-syntax-class node
      #:literal-sets (xml-literals)
      [pattern x #:with result #'0 #:when (begin (debug "xml node ~a\n" #'x) #f)]
      [pattern (~seq < start:id > more:node ... < honu-/ end:id >)
               #:with result (racket-syntax (make-xml 'start more.result ...))]
      [pattern body:honu-body #:with result #'body.result]
      [pattern (~seq plain:non-xml-id plain*:non-xml-id ...) #:with result #''(plain plain* ...)]
      [pattern x #:with result #'0 #:when (begin (debug "xml2 node ~a\n" #'x) #f)]
      )

    (debug "Parse xml ~a\n" code)
    (syntax-parse code
      [(_ node:node . rest)
       (define out #'node.result)
       (debug "Result ~a\n" out)
       (values out #'rest #f)])))
