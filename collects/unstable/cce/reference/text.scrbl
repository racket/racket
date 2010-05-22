#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "../scribble.ss"
          "eval.ss")
@(require (for-label scheme unstable/cce/text))

@title[#:style 'quiet #:tag "cce-text"]{Text Representations}

@defmodule[unstable/cce/text]

This module provides tools for manipulating and converting textual data.

@section{Contracts and Predicates}

@deftogether[(
@defthing[text/c flat-contract?]{}
@defproc[(text? [v any/c]) boolean?]{}
)]{

This contract and predicate recognize text values: strings, byte strings,
symbols, and keywords, as well as syntax objects containing them.

@defexamples[
#:eval (evaluator 'unstable/cce/text)
(text? "text")
(text? #"text")
(text? 'text)
(text? '#:text)
(text? #'"text")
(text? #'#"text")
(text? #'text)
(text? #'#:text)
(text? '(not text))
]

}

@deftogether[(
@defproc[(string-literal? [v any/c]) boolean?]{}
@defproc[(bytes-literal? [v any/c]) boolean?]{}
@defproc[(keyword-literal? [v any/c]) boolean?]{}
)]{

These predicates recognize specific text types stored in syntax objects.

@defexamples[
#:eval (evaluator 'unstable/cce/text)
(string-literal? #'"literal")
(string-literal? "not literal")
(bytes-literal? #'#"literal")
(bytes-literal? #"not literal")
(keyword-literal? #'#:literal)
(keyword-literal? '#:not-literal)
]

}

@section{Text Conversions and Concatenation}

@deftogether[(
@defproc[(text->string [#:before before text/c ""]
                       [#:between between text/c ""]
                       [#:after after text/c ""]
                       [text text/c] ...) string?]{}
@defproc[(text->bytes [#:before before text/c ""]
                      [#:between between text/c ""]
                      [#:after after text/c ""]
                      [text text/c] ...) bytes?]{}
@defproc[(text->symbol [#:before before text/c ""]
                       [#:between between text/c ""]
                       [#:after after text/c ""]
                       [text text/c] ...) symbol?]{}
@defproc[(text->keyword [#:before before text/c ""]
                        [#:between between text/c ""]
                        [#:after after text/c ""]
                        [text text/c] ...) keyword?]{}
)]{

These functions convert text values to specific types.  They concatenate each
@scheme[text] argument, adding @scheme[before] and @scheme[after] to the front
and back of the result and @scheme[between] between each argument.

@defexamples[
#:eval (evaluator 'unstable/cce/text)
(text->string #"concat" #'enate)
(text->bytes #:between "-" 'concat #'#:enate)
(text->symbol #:before "(" #:after ")" '#:concat #'"enate")
(text->keyword #:before #'< #:between #'- #:after #'> "concat" #'#"enate")
]

}

@deftogether[(
@defproc[(text->string-literal [#:before before text/c ""]
                               [#:between between text/c ""]
                               [#:after after text/c ""]
                               [#:stx stx (or/c syntax? false/c) #f]
                               [text text/c] ...)
         string-literal?]{}
@defproc[(text->bytes-literal [#:before before text/c ""]
                              [#:between between text/c ""]
                              [#:after after text/c ""]
                              [#:stx stx (or/c syntax? false/c) #f]
                              [text text/c] ...)
         bytes-literal?]{}
@defproc[(text->identifier [#:before before text/c ""]
                           [#:between between text/c ""]
                           [#:after after text/c ""]
                           [#:stx stx (or/c syntax? false/c) #f]
                           [text text/c] ...)
         identifier?]{}
@defproc[(text->keyword-literal [#:before before text/c ""]
                                [#:between between text/c ""]
                                [#:after after text/c ""]
                                [#:stx stx (or/c syntax? false/c) #f]
                                [text text/c] ...)
         keyword-literal?]{}
)]{

These functions convert text values to specific syntax object types, deriving
syntax object properties from the @scheme[stx] argument.  They concatenate each
@scheme[text] argument, adding @scheme[before] and @scheme[after] to the front
and back of the result and @scheme[between] between each argument.

@defexamples[
#:eval (evaluator 'unstable/cce/text)
(text->string-literal #"concat" #'enate)
(text->bytes-literal #:between "-" 'concat #'#:enate)
(text->identifier #:before "(" #:after ")"
                   #:stx #'props
                  '#:concat #'"enate")
(text->keyword-literal #:before #'< #:between #'- #:after #'>
                       #:stx #'props
                       "concat" #'#"enate")
]

}

@section{Text Comparisons}

@deftogether[(
@defproc[(text=? [one text/c] [two text/c]) boolean?]
@defproc[(text<? [one text/c] [two text/c]) boolean?]
@defproc[(text<=? [one text/c] [two text/c]) boolean?]
@defproc[(text>? [one text/c] [two text/c]) boolean?]
@defproc[(text>=? [one text/c] [two text/c]) boolean?]
)]{

These predicates compare the character content of two text values.  They are
equivalent to:

@schemeblock[
(text=? one two) = (string=? (text->string one) (text->string two))
(text<? one two) = (string<? (text->string one) (text->string two))
(text<=? one two) = (string<=? (text->string one) (text->string two))
(text>? one two) = (string>? (text->string one) (text->string two))
(text>=? one two) = (string>=? (text->string one) (text->string two))
]

@defexamples[
#:eval (evaluator 'unstable/cce/text)
(text=? #"x" #'y)
(text<? #"x" #'y)
(text<=? #"x" #'y)
(text>? #"x" #'y)
(text>=? #"x" #'y)
]

}
