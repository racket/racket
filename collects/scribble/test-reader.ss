#!/bin/sh
#| -*- scheme -*-
exec mzscheme -r "$0" "$@"
|#

(require (lib "string.ss"))

#reader(lib "reader.ss" "scribble")
(define tests
  `(

    [@foo
     foo]

    [@foo{}
     (foo)]

    [@foo[]
     (foo)]

    [@foo[]{}
     (foo)]

    [fo@o
     ,(string->symbol "fo@o")]

    [@foo{bar}
     (foo "bar")]

    [@foo{bar baz
          blah}
     (foo "bar baz" "\n" "blah")]

    ['@foo{bar}
     '(foo "bar")]

    [@'foo{bar}
     '(foo "bar")]

    ,@'( ; <- avoid the above openning quasiquote for these

    [@'`,foo{bar}
     '`,(foo "bar")]

    [@'`,@,foo{bar}
     '`,@,(foo "bar")]

    [@`',@foo{blah}
     `@',@foo{blah}]

    [@`',@foo{blah}
     `',@@foo{blah}]

    )

    [@(lambda (x) x){blah}
     ((lambda (x) x) "blah")]

    [@{foo bar
       baz}
     ("foo bar" "\n" "baz")]

    [@'{foo bar
        baz}
     '("foo bar" "\n" "baz")]

    [(1 2 @; comment
      3 4)
     (1 2 3 4)]

    [(1 2@; comment, 2 touches 3 but there is a comment syntax between them
      3 4)
     (1 2 3 4)]

    [@foo{bar @; comment, note the extra space
          baz}
     (foo "bar baz")]

    [@foo{bar@; comment, no space
          baz}
     (foo "barbaz")]

    #; ;!!!
    [@foo{bar @; comment, with space and newline

          baz}
     (foo "bar " "\n" "baz")]

    [@@foo{bar}{baz}
     ((foo "bar") "baz")]

    [(define |@foo| '\@bar)
     ,(read-from-string "(define @foo '@bar)")]

    [@foo{
       bar
     }
     @foo{bar}]

    [@foo{ bar }
     (foo " bar ")]

    [@foo{ bar
     }
     (foo " bar")]

    [@foo{
       bar }
     (foo "bar ")]

    [@foo{a @bar{b} c}
     (foo "a " (bar "b") " c")]

    [@foo{a @bar c}
     (foo "a " bar " c")]

    [@foo{a @(bar 2) c}
     (foo "a " (bar 2) " c")]

    ))

(define failures 0)

(define (test val expect)
  (unless (equal? val expect)
    (set! failures (add1 failures))
    (printf "Failure, got: ~s\n    expected: ~s\n" val expect)))

(for-each (lambda (t) (apply test t)) tests)

(if (zero? failures)
  (printf "All tests passed\n")
  (begin (printf "~s failures\n" failures) (exit 1)))
