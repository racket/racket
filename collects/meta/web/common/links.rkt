#lang at-exp racket/base

(require scribble/html)

(define-syntax-rule (define* id E) (begin (define id E) (provide id)))

(define ((make-link url . text) . alternate)
  (a href: url (if (null? alternate) text alternate)))

;; ----------------------------------------------------------------------------
;; Pages that are made outside of this system

(define doc-url "http://docs.racket-lang.org/")

(define* -docs @make-link[doc-url]{Documentation})

(define-syntax-rule (define-doc-link id desc)
  (define* id @make-link[`(,doc-url id "/")]{
                @strong{@(string-titlecase (symbol->string 'id))}: @desc @;
                @nbsp @small{@a[href: `(,doc-url "pdf/" id ".pdf")]{[pdf]}}}))

@define-doc-link[quick]{An Introduction to Racket with Pictures}
@define-doc-link[more ]{Systems Programming with Racket}
@define-doc-link[guide]{Racket}
@define-doc-link[continue]{Continue}

(define* intros (list quick more guide))

;; ----------------------------------------------------------------------------
;; External links

(define* -htdp
  @make-link["http://htdp.org/"]{@i{How to Design Programs}})

(define* -redex
  @make-link["http://redex.racket-lang.org/"]{Redex})

(define* -pbd
  @make-link["http://programbydesign.org/"]{Program by Design})

(define* -schemers
  @make-link["http://schemers.org/"]{@tt{schemers.org}})

(define* -plai
  @make-link["http://www.plai.org/"]{
    @i{Programming Languages: Application and Interpretation}})

(define* -bootstrap @make-link["http://www.bootstrapworld.org/"]{Bootstrap})

(provide -rcon)
(define (-rcon #:year [year #f] 
               [body (if year 
                         (format "~a" year)
                         "RacketCon")])
  (make-link (string-append
              "http://con.racket-lang.org/"
              (case year
                [(2011) "2011/"]
                [(#f 2012) ""]
                [else (error 'rcon "unknown year: ~e" year)]))
             body))
