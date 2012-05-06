#lang scribble/doc
@(require scribble/manual
          (for-label (except-in racket require)
                     string-constants
                     setup/getinfo
                     (only-in setup/infotab require)))

@(begin
   (define-syntax-rule (define-racket-require id)
     (begin
       (require (for-label (only-in racket require)))
       (define id @racket[require])))
   (define-racket-require racket:require))

@title[#:tag "info.rkt"]{@filepath{info.rkt} File Format}

@defmodulelang[setup/infotab]

In each collection, a special module file @filepath{info.rkt} provides
general information about a collection for use by various tools. For
example, an @filepath{info.rkt} file specifies how to build the
documentation for a collection, and it lists plug-in tools for
DrRacket or commands for @exec{raco} that the collection provides.

Although an @filepath{info.rkt} file contains a module declaration, the
declaration has a highly constrained form. It must match the following
grammar of @racket[_info-module]:

@racketgrammar*[
#:literals (info lib setup/infotab quote quasiquote
                 cons car cdr list list* reverse append
                 string-append path->string build-path
                 collection-path
                 system-library-subpath
                 require string-constant)
[info-module (module info intotab-mod-path
               decl
               ...)]
[intotab-mod-path setup/infotab
                  (lib "setup/infotab.ss")
                  (lib "setup/infotab.rkt")
                  (lib "infotab.rkt" "setup")
                  (lib "infotab.ss" "setup")]
[decl (define id info-expr)
      (require allowed-path)]
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
[allowed-path (lib "string-constant.ss" "string-constants")
              (lib "string-constants/string-constant.ss")
              string-constants/string-constant
              string-constants]
]

For example, the following declaration could be the @filepath{info.rkt}
library of the @filepath{games} collection. It contains definitions for
three info tags, @racket[name], @racket[gracket-launcher-libraries], and
@racket[gracket-launcher-names].

@racketmod[
setup/infotab
(define name "Games")
(define gracket-launcher-libraries '("main.rkt"))
(define gracket-launcher-names     '("PLT Games"))
]

As illustrated in this example, an @filepath{info.rkt} file can use
@hash-lang[] notation, but only with the @racketmodname[setup/infotab]
language.

See also @racket[get-info] from @racketmodname[setup/getinfo].

@defform[(require module-path)]{

Like @|racket:require|, but constrained to @racket[_allowed-path] as
shown in the grammar above.}
