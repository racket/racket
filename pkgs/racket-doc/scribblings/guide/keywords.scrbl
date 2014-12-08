#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "keywords"]{Keywords}

A @deftech{keyword} value is similar to a symbol (see
@secref["symbols"]), but its printed form is prefixed with
@litchar{#:}.

@refdetails/gory["parse-keyword"]{the syntax of keywords}

@examples[
(string->keyword "apple")
'#:apple
(eq? '#:apple (string->keyword "apple"))
]

More precisely, a keyword is analogous to an identifier; in the same
way that an identifier can be quoted to produce a symbol, a keyword
can be quoted to produce a value. The same term ``keyword'' is used in
both cases, but we sometimes use @defterm{keyword value} to refer more
specifically to the result of a quote-keyword expression or of
@racket[string->keyword]. An unquoted keyword is not an expression,
just as an unquoted identifier does not produce a symbol:

@examples[
not-a-symbol-expression
#:not-a-keyword-expression
]

Despite their similarities, keywords are used in a different way than
identifiers or symbols. Keywords are intended for use (unquoted) as
special markers in argument lists and in certain syntactic forms.  For
run-time flags and enumerations, use symbols instead of keywords.  The
example below illustrates the distinct roles of keywords and symbols.

@examples[
(code:line (define dir (find-system-path 'temp-dir)) (code:comment @#,t{not @racket['#:temp-dir]}))
(with-output-to-file (build-path dir "stuff.txt")
  (lambda () (printf "example\n"))
  (code:comment @#,t{optional @racket[#:mode] argument can be @racket['text] or @racket['binary]})
  #:mode 'text
  (code:comment @#,t{optional @racket[#:exists] argument can be @racket['replace], @racket['truncate], ...})
  #:exists 'replace)
]

@interaction-eval[(delete-file (build-path (find-system-path 'temp-dir) "stuff.txt"))]
