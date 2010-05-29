#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket unstable/regexp))

@title{Regular Expressions}

@defmodule[unstable/regexp]

This module provides tools for building strings which can be compiled to regular
expressions.  In particular, the constructors wrap their arguments in
appropriate delimeters to prevent misparsing after concatenation.

@defproc[(regexp-sequence [#:start start string? ""]
                          [#:between between string? ""]
                          [#:end end string? ""]
                          [re string?] ...)
         string?]{

Produces a regular expression string that matches @scheme[start], followed by
each @scheme[re] interleaved with @scheme[between], followed by @scheme[end].

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re
  (pregexp
   (regexp-sequence "[0-9]+" "[0-9]+" "[0-9]+"
                    #:start (regexp-quote "(")
                    #:between (regexp-quote ",")
                    #:end (regexp-quote ")"))))
(regexp-match-exact? re "(1,10,100)")
(regexp-match-exact? re "(1,10)")
(regexp-match-exact? re " ( 1 , 10 , 100 ) ")
]

}

@defproc[(regexp-or [re string?] ...+) string?]{

Produces a regular expression string that matches any of the given @scheme[re]s.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re (pregexp (regexp-or "[0-9]+" "[a-z]")))
(regexp-match-exact? re "123")
(regexp-match-exact? re "c")
(regexp-match-exact? re "12c")
]

}

@defproc[(regexp-maybe [re string?] ...+) string?]{

Produces a regular expression string that matches either the empty string, or
the concatenation of all the given @scheme[re]s.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re (pregexp (regexp-maybe "[0-9]+" "[.]" "[0-9]+")))
(regexp-match-exact? re "123.456")
(regexp-match-exact? re "")
(regexp-match-exact? re "123")
]

}

@defproc[(regexp-star [re string?] ...+) string?]{

Produces a regular expression string that matches zero or more consecutive
occurrences of the concatenation of the given @scheme[re]s.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re (pregexp (regexp-star "a" "b" "c")))
(regexp-match-exact? re "")
(regexp-match-exact? re "abc")
(regexp-match-exact? re "abcabcabc")
(regexp-match-exact? re "a")
]

}

@defproc[(regexp-plus [re string?] ...+) string?]{

Produces a regular expression string that matches one or more consecutive
occurrences of the concatenation of the given @scheme[re]s.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re (pregexp (regexp-plus "a" "b" "c")))
(regexp-match-exact? re "")
(regexp-match-exact? re "abc")
(regexp-match-exact? re "abcabcabc")
(regexp-match-exact? re "a")
]

}

@defproc[(regexp-save [re string?] ...+) string?]{

Produces a regular expression string that matches the concatenation of the given
@scheme[re]s and saves the result.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re
  (pregexp (regexp-sequence (regexp-save "[0-9]+") "\\1")))
(regexp-match-exact? re "11")
(regexp-match-exact? re "123123")
(regexp-match-exact? re "123456")
]

}

@defproc[(regexp-multi [re string?] ...+) string?]{

Produces a regular expression string that matches the concatenation of the given
@scheme[re]s in multiple-line mode.

@defexamples[
#:eval (eval/require 'unstable/regexp)
(define re (pregexp (regexp-multi "^abc$")))
(regexp-match? re "abc")
(regexp-match? re "xyz\nabc\ndef")
]

}
