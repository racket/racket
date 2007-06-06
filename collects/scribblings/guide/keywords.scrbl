#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "keywords"]{Keywords}

A @defterm{keyword} is similar to a symbol (see @secref["symbols"]),
but its printed form is prefixed with @schemefont{#:}.

@refdetails["mz:parse-keyword"]{the syntax of keywords}

@examples[
(string->keyword "apple")
'#:apple
(eq? '#:apple (string->keyword "apple"))
]

Although keywords are values, an unquoted keyword is not an
expression, just as an unquoted identifier does not produce a symbol:

@examples[
not-a-symbol-expression
#:not-a-keyword-expression
]

Despite their similarities, keywords are used differently than
symbols. Keywords are intented for use (unquoted) as special markers
in argument lists and in certain syntactic forms.

@italic{Need some examples here, once we have more keyword-based
procedures and syntax in place...}

Keywords should not be used simply as another kind of symbol. Use
symbols, instead of keywords, for run-time flags and enumerations.

@examples[
(code:line (bytes->path #"/usr/tmp" 'unix) (code:comment #, @t{@scheme['unix], not @scheme[#:unix]}))
]
