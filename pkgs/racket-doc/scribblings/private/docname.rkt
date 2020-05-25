#lang at-exp racket/base
(require scribble/manual syntax/parse/define (for-syntax racket/syntax racket/base))

(define-simple-macro (define-title+link link-id:id s:str mod:str)
  #:with title-id (format-id #'link-id "~a-title" #'link-id)
  (begin
    (provide title-id link-id)
    (define title-id s)
    (define (link-id #:section [section "top"] [content (list "the" title-id "documentation")])
      (seclink section #:indirect? #t #:doc `(lib ,mod) content))))

(define-title+link Quick
  "Quick: An Introduction to Racket with Pictures"
  "scribblings/quick/quick.scrbl")

(define-title+link Continue
  "Continue: Web Applications in Racket"
  "web-server/scribblings/tutorial/continue.scrbl")

(define-title+link R6RS "R6RS: Scheme" "r6rs/scribblings/r6rs.scrbl")
(define-title+link R5RS "R5RS: Legacy Scheme" "r5rs/r5rs.scrbl")

(define-title+link Draw "Racket Drawing Toolkit" "scribblings/draw/draw.scrbl")
(define-title+link GUI "Racket Graphical Interface Toolkit" "scribblings/gui/gui.scrbl")
(define-title+link Slideshow
  "Slideshow: Figure and Presentation Tools"
  "scribblings/slideshow/slideshow.scrbl")

(define-title+link Web
  "Web Applications in Racket"
  "web-server/scribblings/web-server.scrbl")

(define-title+link HtDP-doc "How to Design Programs language"
  "scribblings/htdp-langs/htdp-langs.scrbl")

(define-title+link TR-guide "Typed Racket Guide"
  "typed-racket/scribblings/ts-guide.scrbl")


