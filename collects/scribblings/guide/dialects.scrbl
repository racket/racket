#lang scribble/doc
@(require scribble/manual
          "guide-utils.ss")

@(define r6rs @elem{R@superscript{6}RS})
@(define r5rs @elem{R@superscript{5}RS})

@title[#:tag "dialects" #:style 'toc]{Dialects of Scheme}

PLT Scheme is one dialect of the Scheme programming language, and
there are many others. Indeed, ``Scheme'' is perhaps more of an idea
than a specific language.

The @hash-lang[] prefix on modules is a particular feature of PLT
Scheme, and programs that start with @hash-lang[] are unlikely to run
in other implementations of Scheme. At the same time, programs that do
not start with @hash-lang[] (or another PLT Scheme module form) do not
work with the default mode of most PLT Scheme tools.

``PLT Scheme'' is not, however, the only dialect of Scheme that is supported
by PLT Scheme tools. On the contrary, PLT Scheme tools are designed to
support multiple dialects of Scheme and even multiple languages, which
allows the PLT Scheme tool suite to serve multiple communities. PLT
Scheme also gives programmers and researchers the tools they need to
explore and create new languages.

@local-table-of-contents[]

@; --------------------------------------------------

@section[#:tag "standards"]{Standards}

Standard dialects of Scheme include the ones defined by @|r5rs| and
@|r6rs|.

@subsection{@|r5rs|}

``@|r5rs|'' stands for @link["../r5rs-std/index.html"]{The
Revised@superscript{5} Report on the Algorithmic Language Scheme}, and
it is currently the most widely implemented Scheme standard.

PLT Scheme tools in their default modes do not conform to @|r5rs|,
mainly because PLT Scheme tools generally expect modules, and @|r5rs|
does not define a module system. Typical single-file @|r5rs| programs
can be converted to PLT Scheme programs by prefixing them with
@scheme[@#,hash-lang[] @#,schememodname[r5rs]], but other Scheme
systems do not recognize @scheme[@#,hash-lang[]
@#,schememodname[r5rs]]. The @exec{plt-r5rs} executable (see
@secref[#:doc '(lib "r5rs/r5rs.scrbl") "plt-r5rs"]) more directly
conforms to the @|r5rs| standard.

Aside from the module system, the syntactic forms and functions of
@|r5rs| and PLT Scheme differ. Only simple @|r5rs| become PLT Scheme
programs when prefixed with @scheme[@#,hash-lang[] scheme], and
relatively few PLT Scheme programs become @|r5rs| programs when a
@hash-lang[] line is removed. Also, when mixing ``@|r5rs| modules''
with PLT Scheme modules, beware that @|r5rs| pairs correspond to PLT
Scheme mutable pairs (as constructed with @scheme[mcons]).

See @other-manual['(lib "r5rs/r5rs.scrbl")] for more
information about running @|r5rs| programs with PLT Scheme.

@subsection{@|r6rs|}

``@|r6rs|'' stands for @link["../r6rs-std/index.html"]{The
Revised@superscript{6} Report on the Algorithmic Language Scheme},
which extends @|r5rs| with a module system that is similar to the PLT
Scheme module system.

When an @|r6rs| library or top-level program is prefixed with
@schememetafont{#!}@schememodname[r6rs] (which is valid @|r6rs|
syntax), then it can also be used as a PLT Scheme program. This works
because @schememetafont{#!} in PLT Scheme is treated as a shorthand
for @hash-lang[] followed by a space, so
@schememetafont{#!}@schememodname[r6rs] selects the
@schememodname[r6rs] module language. As with @|r5rs|, however, beware
that the syntactic forms and functions of @|r6rs| differ from PLT
Scheme, and @|r6rs| pairs are mutable pairs.

See @other-manual['(lib "r6rs/scribblings/r6rs.scrbl")] for more
information about running @|r6rs| programs with PLT Scheme.

@; --------------------------------------------------

@section[#:tag "more-hash-lang"]{More PLT Schemes}

Like ``Scheme'' itself, even ``PLT Scheme'' is more of an idea about
programming languages than a language in the usual sense. Macros can
extend a base language (as described in @secref["macros"]), but macros
and alternate parsers can construct an entirely new language from the
ground up.

The @hash-lang[] line that starts a PLT Scheme module declares the
base language of the module. By ``PLT Scheme,'' we usually mean
@hash-lang[] followed by the base language @schememodname[scheme] or
@schememodname[scheme/base] (of which @schememodname[scheme] is an
extension). The PLT Scheme distribution provides additional languages,
including the following:

@itemize[

 @item{@schememodname[typed/scheme] --- like
       @schememodname[scheme], but statically typed; see
       @other-manual['(lib "typed-scheme/scribblings/ts-guide.scrbl")]}

 @item{@schememodname[lazy] --- like @schememodname[scheme/base], but
       avoids evaluating an expression until its value is needed; see
       @other-manual['(lib "lazy/lazy.scrbl")]}

 @item{@schememodname[frtime] --- changes evaluation in an even more
       radical way to support reactive programming; see
       @other-manual['(lib "frtime/scribblings/frtime.scrbl")]}
       
 @item{@schememodname[scribble/doc] --- a language, which looks more
       like Latex than Scheme, for writing documentation; see
       @other-manual['(lib "scribblings/scribble/scribble.scrbl")]}

]

Each of these languages is used by starting module with the language
name after @hash-lang[]. For example, this source of this
document starts with @scheme[@#,hash-lang[] scribble/doc].

PLT Scheme users can define their own languages.  A language name maps
to its implementation through a module path by adding
@schemeidfont{/lang/reader}. For example, the language name
@schememodname[scribble/doc] is expanded to
@scheme[scribble/doc/lang/reader], which is the module that implements
the surface-syntax parser. The parser, in turn, generates a
@scheme[module] form, which determines the base language at the level
of syntactic forms an functions.

Some language names act as language loaders. For example,
@schememodname[s-exp] as a language uses the usual PLT Scheme parser
for surface-syntax reading, and then it uses the module path after
@schememodname[s-exp] for the language's syntactic forms. Thus,
@scheme[@#,hash-lang[] @#,schememodname[s-exp] "mylang.ss"] parses
the module body using the normal PLT Scheme reader, by then imports
the initial syntax and functions for the module body from
@scheme["mylang.ss"]. Similarly, @scheme[@#,hash-lang[]
@#,schememodname[planet] _planet-path] loads a language via
@seclink["top" #:doc '(lib "planet/planet.scrbl")]{@|PLaneT|}.

@; --------------------------------------------------

@section[#:tag "teaching-langs"]{Teaching}

The @|HtDP| textbook relies on pedagogic variants of Scheme that
smooth the introduction of programming concepts for new programmers.
The languages are documented in @other-manual['(lib
"scribblings/htdp-langs/htdp-langs.scrbl")].

The @|HtDP| languages are typically not used with @hash-lang[]
prefixes, but are instead used within DrScheme by selecting the
language from the @onscreen{Choose Language...} dialog.
