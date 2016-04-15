#lang scribble/doc
@(require scribble/manual
          (for-label (except-in racket require)
                     setup/getinfo
                     (only-in info require)))

@(begin
   (define-syntax-rule (define-racket-require id)
     (begin
       (require (for-label (only-in racket require)))
       (define id @racket[require])))
   (define-racket-require racket:require))

@title[#:tag "info.rkt"]{@filepath{info.rkt} File Format}

@defmodulelang*[(info setup/infotab)]

In each collection, a special module file @filepath{info.rkt} provides
general information about a collection for use by various tools. For
example, an @filepath{info.rkt} file specifies how to build the
documentation for a collection, and it lists plug-in tools for
DrRacket or commands for @exec{raco} that the collection provides.

@margin-note{
  The fields specified in an @filepath{info.rkt} file are documented
  in @secref["metadata" #:doc '(lib "pkg/scribblings/pkg.scrbl")] for
  packages and in @secref["setup-info" #:doc '(lib "scribblings/raco/raco.scrbl")]
  for collections.
}

Although an @filepath{info.rkt} file contains a module declaration, the
declaration has a highly constrained form. It must match the following
grammar of @racket[_info-module]:

@racketgrammar*[
#:literals (info lib setup/infotab module define quote quasiquote if
                 cons car cdr list list* reverse append
                 string-append path->string build-path
                 equal?
                 make-immutable-hash hash hash-set hash-set* hash-remove hash-clear hash-update
                 collection-path
                 system-library-subpath
                 getenv)
[info-module (module info info-mod-path
               decl
               ...)]
[info-mod-path info
               setup/infotab
               (lib "info/main.rkt")
               (lib "setup/infotab.ss")
               (lib "setup/infotab.rkt")
               (lib "main.rkt" "info")
               (lib "infotab.rkt" "setup")
               (lib "infotab.ss" "setup")]
[decl (define id info-expr)]
[info-expr (@#,racket[quote] datum)
           (@#,racket[quasiquote] datum)
           (if info-expr info-expr info-expr)
           (info-primitive info-expr ...)
           id
           string
           number
           boolean]
[info-primitive cons car cdr list
                list* reverse append
                equal?
                string-append
                make-immutable-hash hash hash-set hash-set* hash-remove hash-clear hash-update
                path->string build-path collection-path
                system-library-subpath
                getenv]
]

For example, the following declaration could be the @filepath{info.rkt}
library of the @filepath{games} collection. It contains definitions for
three info tags, @racket[name], @racket[gracket-launcher-libraries], and
@racket[gracket-launcher-names].

@racketmod[
info
(define name "Games")
(define gracket-launcher-libraries '("main.rkt"))
(define gracket-launcher-names     '("PLT Games"))
]

As illustrated in this example, an @filepath{info.rkt} file can use
@hash-lang[] notation, but only with the @racketmodname[info] (or
@racketmodname[setup/infotab]) language.

Although @racket[getenv] is allowed in an @racketmodname[info] module,
the @racket[get-info] function loads the module with an environment that
prunes any variable not listed in the @indexed-envvar{PLT_INFO_ALLOW_VARS}
environment variable, which holds a list of @litchar{;}-separated
variable names. By default, the set of allowed environment variables
is empty.

See also @racket[get-info] from @racketmodname[setup/getinfo].

@history[#:changed "6.5.0.2" @elem{Added @racket[if], @racket[equal?], and @racket[getenv].}]
