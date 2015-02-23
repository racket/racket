#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title{Simple Values}

Racket values include numbers, booleans, strings, and byte strings. In
DrRacket and documentation examples (when you read the documentation
in color), value expressions are shown in green.

@defterm{Numbers} are written in the usual way, including fractions
and imaginary numbers:

@moreguide["numbers"]{numbers}

@racketblock[
1       3.14
1/2     6.02e+23
1+2i    9999999999999999999999
]

@defterm{Booleans} are @racket[#t] for true and @racket[#f] for
false. In conditionals, however, all non-@racket[#f] values are
treated as true.

@moreguide["booleans"]{booleans}

@defterm{Strings} are written between doublequotes. Within a string,
backslash is an escaping character; for example, a backslash followed
by a doublequote includes a literal doublequote in the string. Except
for an unescaped doublequote or backslash, any Unicode character can
appear in a string constant.

@moreguide["strings"]{strings}

@racketblock[
"Hello, world!"
"Benjamin \"Bugsy\" Siegel"
"\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx"
]

When a constant is evaluated in the @tech{REPL}, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrRacket's @tech{REPL},
results are printed in blue instead of green to highlight the
difference between an input expression and a printed result.

@examples[
(eval:alts (unsyntax (racketvalfont "1.0000")) 1.0000)
(eval:alts (unsyntax (racketvalfont "\"Bugs \\u0022Figaro\\u0022 Bunny\"")) "Bugs \u0022Figaro\u0022 Bunny")
]
