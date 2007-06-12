#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:keywords"]{Keywords}

A @defterm{keyword} value is similar to a symbol (see
@secref["guide:symbols"]), but its printed form is prefixed with
@litchar{#:}.

@refdetails/gory["mz:parse-keyword"]{the syntax of keywords}

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
@scheme[string->keyword]. An unquoted keyword is not an expression,
just as an unquoted identifier does not produce a symbol:

@examples[
not-a-symbol-expression
#:not-a-keyword-expression
]

Despite their similarities, keywords are used in a different way than
identifiers or symbols. Keywords are intented for use (unquoted) as
special markers in argument lists and in certain syntactic forms.

@italic{Need some examples here, once we have more keyword-based
procedures and syntax in place...}

Keywords should not be used simply as another kind of symbol. Use
symbols, instead of keywords, for run-time flags and enumerations.

@examples[
(code:line (bytes->path #"/usr/tmp" 'unix) (code:comment #, @t{@scheme['unix], not @scheme['#:unix]}))
]
