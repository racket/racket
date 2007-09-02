#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Simple Values}

Scheme values include numbers, booleans, strings, and byte strings. In
DrScheme and documentation examples (when you read the documentation
in color), value expression are shown in green.

@defterm{Numbers} are written in the usual way, including fractions
and imagnary numbers:

@moreguide["numbers"]{numbers}

@schemeblock[
1                        1.0
1/2                      0.5
9999999999999999999999   1e+22
1+2i                     1.0+2.0i
]

@defterm{Booleans} are @scheme[#t] for true and @scheme[#f] for
false. In conditionals, however, all non-@scheme[#f] values are
treated as true.

@moreguide["booleans"]{booleans}

@defterm{Strings} are written between doublequotes. Within a string,
backslash is an escaping character; for example, a backslash followed
by a doublequote includes a little doublequote in the string. Except
for an unescaped doublequote or backslash, any Unicode character can
appear in a string constant.

@moreguide["strings"]{strings}

@schemeblock[
"hello world"
"A \"fancy\" string"
"\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx"
]

When a constant is evaluated in the @tech{REPL}, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrScheme's @tech{REPL},
results are printed in blue instead of green to highlight the
difference between an input expression and a printed result.

@examples[
(eval-example-string "1.0000")
(eval-example-string "\"A \\u0022fancy\\u0022 string\"")
]
