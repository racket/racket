#lang scribble/doc
@(require scribble/manual
          (for-label scheme
                     string-constants
                     setup/getinfo))

@title[#:tag "info.rkt"]{@filepath{info.rkt} File Format}

@defmodulelang[setup/infotab]

In each collection, a special module file @filepath{info.rkt} provides
general information about a collection for use by various tools. For
example, an @filepath{info.rkt} file specifies how to build the
documentation for a collection, and it lists plug-in tools for
DrRacket or commands for @exec{raco} that the collection provides.

Although an @filepath{info.rkt} file contains a module declaration, the
declaration has a highly constrained form. It must match the following
grammar of @scheme[_info-module]:

@schemegrammar*[
#:literals (info lib setup/infotab quote quasiquote
                 cons car cdr list list* reverse append
                 string-append path->string build-path
                 collection-path
                 system-library-subpath
                 string-constant)
[info-module (module info intotab-mod-path
               (define id info-expr)
               ...)]
[intotab-mod-path setup/infotab
                  (lib "setup/infotab.ss")
                  (lib "setup/infotab.rkt")
                  (lib "infotab.rkt" "setup")
                  (lib "infotab.ss" "setup")]
[info-expr (quote datum)
           (quasiquote datum)
           (info-primitive info-expr ...)
           id
           string
           number
           boolean
           (string-constant identifier)]
[info-primitive cons car cdr list
                list* reverse append
                string-append
                path->string build-path collection-path
                system-library-subpath]
]

For example, the following declaration could be the @filepath{info.rkt}
library of the @filepath{games} collection. It contains definitions for
three info tags, @scheme[name], @scheme[racket-launcher-libraries], and
@scheme[racket-launcher-names].

@schememod[
setup/infotab
(define name "Games")
(define gracket-launcher-libraries '("main.rkt"))
(define gracket-launcher-names     '("PLT Games"))
]

As illustrated in this example, an @filepath{info.rkt} file can use
@hash-lang[] notation, but only with the @schememodname[setup/infotab]
language.

See also @scheme[get-info] from @schememodname[setup/getinfo].
