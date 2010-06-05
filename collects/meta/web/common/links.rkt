#lang at-exp s-exp meta/web/html

(define-syntax-rule (define* id E) (begin (define id E) (provide id)))

;; ----------------------------------------------------------------------------
;; Pages that are made outside of this system

(define* -planet @a[href: "http://planet.racket-lang.org/"]{PLaneT})

(define doc-url "http://docs.racket-lang.org/")

(define* -docs @a[href: doc-url]{Documentation})

(define-syntax-rule (define-doc-link id desc)
  (define* id @a[href: `(,doc-url id "/")]{
                @strong{@(string-titlecase (symbol->string 'id))}: @desc}))

@define-doc-link[quick]{An Introduction to Racket with Pictures}
@define-doc-link[more ]{Systems Programming with Racket}
@define-doc-link[guide]{Racket}

(define* intros (list quick more guide))

;; ----------------------------------------------------------------------------
;; External links

(define* -htdp
  @a[href: "http://www.htdp.org/"]{@i{How to Design Programs}})

(define* -teachscheme
  @a[href: "http://www.teach-scheme.org/"]{TeachScheme!})

(define* -cookbook
  @a[href: "http://schemecookbook.org/"]{Schematics Scheme Cookbook})

(define* -schematics
  @a[href: "http://sourceforge.net/projects/schematics/"]{Schematics})

(define* -schemers
  @a[href: "http://schemers.org/"]{@tt{schemers.org}})

(define* -plai
  @a[href: "http://www.plai.org/"]{
    @i{Programming Languages: Application and Interpretation}})

(define* -bootstrap @a[href: "http://www.bootstrapworld.org/"]{Bootstrap})
