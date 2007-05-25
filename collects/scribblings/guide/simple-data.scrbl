#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title{Simple Values}

Scheme values include numbers, booleans, strings, and byte strings. In
DrScheme and documentation examples (when you read the documentation
in color), value expression are shown in green.

@defterm{Numbers} are written in the usual way, including fractions
and imagnary numbers. Numbers that use decimal points or exponent
markers are implemented as double-precision floating-point numbers,
and they are called @defterm{inexact} numbers in Scheme
terminology. Other numbers are implemented as @defterm{exact} with
arbitrary precision. In the example number constants below, the ones
on the left are exact, and the ones on the right are inexact
approximations:

@schemeblock[
1                        1.0
1/2                      0.5
1+2i                     1.0+2i
9999999999999999999999   1e+22
]

@defterm{Booleans} are @scheme[#t] for true and @scheme[#f] for
false. In conditionals, however, all non-@scheme[#f] values are
treated as true.

@defterm{Strings} are written between double quotes. Within a string,
backslash is an escaping character; for example, a backslash followed
by a double-quote includes a little double-quote in the string. Except
for an unescaped double-quote or backslash, any Unicode character can
appear in a string constant.

@schemeblock[
"hello world"
"A \"fancy\" string"
"\u03BBx:(\u03BC\u03B1.\u03B1\u2192\u03B1).xx"
]

When a constant is evaluated in the REPL, it typically prints the same
as its input syntax. In some cases, the printed form is a normalized
version of the input syntax. In documentation and in DrScheme's REPL,
results are printed in blue instead of green to highlight the
difference between an input expression and a printed result.

@examples[
(eval-example-string "1.0000")
(eval-example-string "\"A \\u0022fancy\\u0022 string\"")
]
