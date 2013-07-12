#lang scribble/base
@(require scribble/manual
          "guide-utils.rkt")

@title[#:tag "dialects" #:style 'toc]{Dialects of Racket and Scheme}

We use ``Racket'' to refer to a specific dialect of the Lisp language,
and one that is based on the Scheme branch of the Lisp family.
Despite Racket's similarity to Scheme, the @hash-lang[] prefix on
modules is a particular feature of Racket, and programs that start
with @hash-lang[] are unlikely to run in other implementations of
Scheme. At the same time, programs that do not start with @hash-lang[]
do not work with the default mode of most Racket tools.

``Racket'' is not, however, the only dialect of Lisp that is supported
by Racket tools. On the contrary, Racket tools are designed to support
multiple dialects of Lisp and even multiple languages, which allows
the Racket tool suite to serve multiple communities. Racket also gives
programmers and researchers the tools they need to explore and create
new languages.

@local-table-of-contents[]

@; --------------------------------------------------

@section[#:tag "more-hash-lang"]{More Rackets}

``Racket'' is more of an idea about programming languages than a
language in the usual sense. Macros can extend a base language (as
described in @secref["macros"]), and alternate parsers can
construct an entirely new language from the ground up (as described in
@secref["languages"]).

The @hash-lang[] line that starts a Racket module declares the
base language of the module. By ``Racket,'' we usually mean
@hash-lang[] followed by the base language @racketmodname[racket] or
@racketmodname[racket/base] (of which @racketmodname[racket] is an
extension). The Racket distribution provides additional languages,
including the following:

@itemize[

 @item{@racketmodname[typed/racket] --- like
       @racketmodname[racket], but statically typed; see
       @other-manual['(lib "typed-racket/scribblings/ts-guide.scrbl")]}

 @item{@racketmodname[lazy #:indirect] --- like
       @racketmodname[racket/base], but avoids evaluating an
       expression until its value is needed; see @seclink["top" #:doc
       '(lib "lazy/lazy.scrbl") #:indirect? #t]{the Lazy Racket
       documentation}.}

 @item{@racketmodname[frtime #:indirect] --- changes evaluation in an
       even more radical way to support reactive programming; see
       @seclink["top" #:doc '(lib "frtime/scribblings/frtime.scrbl")
       #:indirect? #t]{the FrTime documentation}.}
       
 @item{@racketmodname[scribble/base] --- a language, which looks more
       like Latex than Racket, for writing documentation; see
       @other-manual['(lib "scribblings/scribble/scribble.scrbl")]}

]

Each of these languages is used by starting module with the language
name after @hash-lang[]. For example, this source of this
document starts with @racket[@#,hash-lang[] scribble/base].

Furthermore, Racket users can define their own languages, as discussed
in @secref["languages"]. Typically, a language name maps to its
implementation through a module path by adding
@racketidfont{/lang/reader}; for example, the language name
@racketmodname[scribble/base] is expanded to
@racket[scribble/base/lang/reader], which is the module that
implements the surface-syntax parser. Some language names act as
language loaders; for example, @racket[@#,hash-lang[]
@#,racketmodname[planet] _planet-path] downloads, installs, and uses a
language via @seclink["top" #:doc '(lib
"planet/planet.scrbl")]{@|PLaneT|}.

@; --------------------------------------------------

@section[#:tag "standards"]{Standards}

Standard dialects of Scheme include the ones defined by @|r5rs| and
@|r6rs|.

@subsection[#:tag "r5rs"]{@|r5rs|}

``@|r5rs|'' stands for @link["../r5rs-std/index.html"]{The
Revised@superscript{5} Report on the Algorithmic Language Scheme}, and
it is currently the most widely implemented Scheme standard.

Racket tools in their default modes do not conform to @|r5rs|,
mainly because Racket tools generally expect modules, and @|r5rs|
does not define a module system. Typical single-file @|r5rs| programs
can be converted to Racket programs by prefixing them with
@racket[@#,hash-lang[] @#,racketmodname[r5rs]], but other Scheme
systems do not recognize @racket[@#,hash-lang[]
@#,racketmodname[r5rs]]. The @exec{plt-r5rs} executable (see
@secref[#:doc '(lib "r5rs/r5rs.scrbl") "plt-r5rs"]) more directly
conforms to the @|r5rs| standard.

Aside from the module system, the syntactic forms and functions of
@|r5rs| and Racket differ. Only simple @|r5rs| become Racket
programs when prefixed with @racket[@#,hash-lang[] racket], and
relatively few Racket programs become @|r5rs| programs when a
@hash-lang[] line is removed. Also, when mixing ``@|r5rs| modules''
with Racket modules, beware that @|r5rs| pairs correspond to
Racket mutable pairs (as constructed with @racket[mcons]).

See @other-manual['(lib "r5rs/r5rs.scrbl")] for more
information about running @|r5rs| programs with Racket.

@subsection{@|r6rs|}

``@|r6rs|'' stands for @link["../r6rs-std/index.html"]{The
Revised@superscript{6} Report on the Algorithmic Language Scheme},
which extends @|r5rs| with a module system that is similar to the
Racket module system.

When an @|r6rs| library or top-level program is prefixed with
@racketmetafont{#!}@racketmodname[r6rs] (which is valid @|r6rs|
syntax), then it can also be used as a Racket program. This works
because @racketmetafont{#!} in Racket is treated as a shorthand
for @hash-lang[] followed by a space, so
@racketmetafont{#!}@racketmodname[r6rs] selects the
@racketmodname[r6rs] module language. As with @|r5rs|, however, beware
that the syntactic forms and functions of @|r6rs| differ from
Racket, and @|r6rs| pairs are mutable pairs.

See @other-manual['(lib "r6rs/scribblings/r6rs.scrbl")] for more
information about running @|r6rs| programs with Racket.

@; --------------------------------------------------

@section[#:tag "teaching-langs"]{Teaching}

The @|HtDP| textbook relies on pedagogic variants of Racket that
smooth the introduction of programming concepts for new programmers.
See @other-doc['(lib "scribblings/htdp-langs/htdp-langs.scrbl")
#:indirect @list{@|HtDP| language}].

The @|HtDP| languages are typically not used with @hash-lang[]
prefixes, but are instead used within DrRacket by selecting the
language from the @onscreen{Choose Language...} dialog.
