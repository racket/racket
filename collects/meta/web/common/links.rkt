#lang at-exp s-exp scribble/html

(define-syntax-rule (define* id E) (begin (define id E) (provide id)))

(define ((make-link url . text) . alternate)
  (a href: url (if (null? alternate) text alternate)))

;; ----------------------------------------------------------------------------
;; Pages that are made outside of this system

(define doc-url "http://docs.racket-lang.org/")

(define* -docs @make-link[doc-url]{Documentation})

(define-syntax-rule (define-doc-link id desc)
  (define* id @make-link[`(,doc-url id "/")]{
                @strong{@(string-titlecase (symbol->string 'id))}: @desc}))

@define-doc-link[quick]{An Introduction to Racket with Pictures}
@define-doc-link[more ]{Systems Programming with Racket}
@define-doc-link[guide]{Racket}
@define-doc-link[continue]{Continue}

(define* intros (list quick more guide))

;; ----------------------------------------------------------------------------
;; External links

(define* -htdp
  @make-link["http://www.htdp.org/"]{@i{How to Design Programs}})

(define* -redex
  @make-link["http://redex.plt-scheme.org/"]{Redex})

(define* -teachscheme
  @make-link["http://www.teach-scheme.org/"]{TeachScheme!})

(define* -cookbook
  @make-link["http://schemecookbook.org/"]{Schematics Scheme Cookbook})

(define* -schematics
  @make-link["http://sourceforge.net/projects/schematics/"]{Schematics})

(define* -schemers
  @make-link["http://schemers.org/"]{@tt{schemers.org}})

(define* -plai
  @make-link["http://www.plai.org/"]{
    @i{Programming Languages: Application and Interpretation}})

(define* -bootstrap @make-link["http://www.bootstrapworld.org/"]{Bootstrap})
