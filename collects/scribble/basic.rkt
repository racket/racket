#lang scheme/base

(require "base.rkt"
         "core.rkt"
         "decode.rkt")

(provide title
         section
         subsection
         subsubsection
         subsubsub*section
         include-section

         author
         author+email

         intern-taglet
         module-path-index->taglet
         module-path-prefix->string
         
         itemize item item?

         hspace
         elem aux-elem
         italic bold smaller
         tt
         subscript superscript

         section-index index index* as-index index-section
         get-index-entries index-block

         table-of-contents
         local-table-of-contents

         span-class)

(define (span-class classname . str)
  (make-element classname (decode-content str)))

(define (aux-elem . s)
  (make-element (make-style #f (list 'aux)) (decode-content s)))

(define (itemize #:style [style #f] . items)
  (let ([items (filter (lambda (v) (not (whitespace? v))) items)])
    (apply itemlist #:style style items)))

