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

    [foo@
     ,(string->symbol "foo@")]

    [fo@o
     ,(string->symbol "fo@o")]

    [\@foo
     ,(string->symbol "@foo")]

    [|@foo|
     ,(string->symbol "@foo")]

    [(define |@foo| '\@bar@baz)
     ,(read-from-string "(define @foo '@bar@baz)")]

    [@foo{bar}
     (foo "bar")]

    [@foo{bar baz
          blah}
     (foo "bar baz" "\n" "blah")]

    [@foo{bar @baz[3]
          blah}
     (foo "bar " (baz 3) "\n" "blah")]

    [@foo{bar @baz{3}
          blah}
     (foo "bar " (baz "3") "\n" "blah")]

    [@foo{bar @baz[2 3]{4 5}
          blah}
     (foo "bar " (baz 2 3 "4 5") "\n" "blah")]

    [@foo{bar @baz[2 3] {4 5}}
     (foo "bar " (baz 2 3) " {4 5}")]

    [,(let* ([formatter (lambda (fmt)
                          (lambda args
                            (format fmt (apply string-append args))))]
             [bf (formatter "*~a*")]
             [it (formatter "/~a/")]
             [ul (formatter "_~a_")]
             [text string-append])
        @text{@it{Note}: @bf{This is @ul{not} a pipe}.})
     "/Note/: *This is _not_ a pipe*."]

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

    [@`(unquote foo){blah}
     `(,foo "blah")]

    [@{foo bar
       baz}
     ("foo bar" "\n" "baz")]

    [@'{foo bar
        baz}
     '("foo bar" "\n" "baz")]

    [(1 2 @; comment
      3 4)
     (1 2 3 4)]

    [@foo{bar @; comment, note the extra space
          baz}
     (foo "bar baz")]

    [@foo{bar@; comment, no space
          baz}
     (foo "barbaz")]

    [@foo{bar @; comment, with space and newline

          baz}
     (foo "bar " "\n" "baz")]

    [@foo{x @y z}
     (foo "x " y " z")]

    [@foo{x @(* y 2) z}
     (foo "x " (* y 2) " z")]

    [@{@foo bar}
     (foo " bar")]

    [@@foo{bar}{baz}
     ((foo "bar") "baz")]

    [@foo[1 (* 2 3)]{bar}
     (foo 1 (* 2 3) "bar")]

    [@foo[@bar{...}]{blah}
     (foo (bar "...") "blah")]

    [@foo[bar]
     (foo bar)]

    [@foo{bar @f[x] baz}
     (foo "bar " (f x) " baz")]

    [@foo[]{bar}
     (foo "bar")]

    [@foo[]
     (foo)]

    [@foo
     foo]

    [@foo{}
     (foo)]

    [@foo[#:style 'big]{bar}
     (foo #:style 'big "bar")]

    [@foo{f{o}o}
     (foo "f{o}o")]

    [@foo{{{}}{}}
     (foo "{{}}{}")]

    [@foo{bar}
     (foo "bar")]

    [@foo{ bar }
     (foo " bar ")]

    [@foo[1]{ bar }
     (foo 1 " bar ")]

    [@foo{a @bar{b} c}
     (foo "a " (bar "b") " c")]

    [@foo{a @bar c}
     (foo "a " bar " c")]

    [@foo{a @(bar 2) c}
     (foo "a " (bar 2) " c")]

    [@foo{This @"}" is a closing brace}
     (foo "This } is a closing brace")]

    [@foo{The command prefix is @"@".}
     (foo "The command prefix is @.")]

    [@foo{@"@foo{bar}" reads as (foo "bar")}
     (foo "@foo{bar} reads as (foo \"bar\")")]

    [@foo|{...}|
     (foo "...")]

    [@foo|{close with "}", open with "{"}|
     (foo "close with \"}\", open with \"{\"")]

    [@foo|{Nesting |{is}| ok}|
     (foo "Nesting |{is}| ok")]

    [@foo|{Maze
           |@bar{is}
           Life!}|
     (foo "Maze" "\n" (bar "is") "\n" "Life!")]

    [@foo|{Works for |@bar|{subforms}| too}|
     (foo "Works for " (bar "subforms") " too")]

    [@foo|<<<{Some @x{more} |@{text}|.}>>>|
     (foo "Some @x{more} |@{text}|.")]

    [@foo|!!{Blah |!!@bold{blah}...}!!|
     (foo "Blah " (bold "blah") "...")]

    [@foo{foo@bar.}
     (foo "foo" bar.)]

    [@foo{foo@|bar|.}
     (foo "foo" bar ".")]

    [@foo{foo@3.}
     (foo "foo" 3.0)]

    [@foo{foo@|3|.}
     (foo "foo" 3 ".")]

    [@foo{foo@|(f 1)|{bar}}
     (foo "foo" (f 1) "{bar}")]

    [@foo{foo@|bar|[1]{baz}}
     (foo "foo" bar "[1]{baz}")]

    [@foo{x@"y"z}
     (foo "xyz")]

    [@foo{x@|"y"|z}
     (foo "x" "y" "z")]

    [@foo{x@|1 (+ 2 3) 4|y}
     (foo "x" 1 (+ 2 3) 4 "y")]

    [@foo{x@|1 (+ 2 3) 4|y}
     (foo "x" 1 (+ 2 3) 4 "y")]

    [@foo{x@|*
             *|y}
     (foo "x" * * "y")]

    [@foo{Alice@||Bob@|
          |Carol}
     (foo "Alice" "Bob" "Carol")]

    [@|{blah}|
     ("blah")]

    [@foo{First line@;{there is still a
                       newline at this point;}
          Second line}
     (foo "First line" "\n" "Second line")]

    [@foo{This is @;
          a pretty long @;
          single string-@;
          argument.}
     (foo "This is a pretty long single string-argument.")]

    [@foo{bar}
     (foo "bar")]

    [@foo{ bar }
     (foo " bar ")]


    [@foo{ bar
          baz }
     (foo " bar" "\n" "baz ")]

    [@foo{bar
     }
     (foo "bar")]

    [@foo{
       bar
     }
     (foo "bar")]

    [@foo{

       bar

     }
     (foo "\n" "bar" "\n")]

    [@foo{
       bar

       baz
     }
     (foo "bar" "\n" "\n" "baz")]

    [@foo{
     }
     (foo "\n")]

    [@foo{

     }
     (foo "\n" "\n")]

    [@foo{


     }
     (foo "\n" "\n" "\n")]

    [,(let ([nl (car @'{
                       })]
            [o (open-output-string)])
        (for-each (lambda (x) (display (if (eq? x nl) "\n... " x) o))
                  @`{foo
                     @,@(list "bar" "\n" "baz")
                     blah})
        (newline o)
        (get-output-string o))
     "foo\n... bar\nbaz\n... blah\n"]

    [@foo{
       bar
       baz
       blah
     }
     (foo "bar" "\n" "baz" "\n" "blah")]

    [@foo{
       begin
         x++;
       end}
     (foo "begin" "\n" "  " "x++;" "\n" "end")]

    [@foo{
         a
        b
       c}
     (foo "  " "a" "\n" " " "b" "\n" "c")]

    [@foo{bar
            baz
          bbb}
     (foo "bar" "\n" "  " "baz" "\n" "bbb")]

    [@foo{ bar
             baz
           bbb}
     (foo " bar" "\n" "   " "baz" "\n" " " "bbb")]

    [@foo{bar
        baz
        bbb}
     (foo "bar" "\n" "baz" "\n" "bbb")]

    [@foo{ bar
        baz
        bbb}
     (foo " bar" "\n" "baz" "\n" "bbb")]

    [@foo{ bar
        baz
          bbb}
     (foo " bar" "\n" "baz" "\n" "  " "bbb")]

    [@text{Some text@footnote{And a
       footnote comment.}.  More text.}
     (text "Some text"
           (footnote "And a" "\n" "footnote comment.")
           ".  More text.")]

    [@code{
       begin
         i = 1, r = 1
         @bold{while i < n do
                 r *= i++
               done}
       end
     }
     (code "begin" "\n"
           "  " "i = 1, r = 1" "\n"
           "  " (bold "while i < n do" "\n"
                      "  " "r *= i++" "\n"
                      "done") "\n"
           "end")]

    [@foo{
       @|| bar @||
       @|| baz}
     (foo " bar " "\n" " baz")]

    [@foo{bar
          @|baz| bbb
          @|x1 x2| x3 x4
          @|| waaaah
         }
     (foo "bar" "\n" baz " bbb" "\n" x1 x2 " x3 x4" "\n" " waaaah")]

    [@foo{x1
          x2@;
                   y2
          x3@;{
                   ;}y3
          x4@|
                   |y4
          x5}
     (foo "x1" "\n" "x2y2" "\n" "x3y3" "\n" "x4" "y4" "\n" "x5")]


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
