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

    [@foo{bar @; comment
          baz@;
          blah}
     (foo "bar bazblah")]

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

    [@foo{A @"}" marks the end}
     (foo "A } marks the end")]

    [@foo{The prefix: @"@".}
     (foo "The prefix: @.")]

    [@foo{@"@x{y}" --> (x "y")}
     (foo "@x{y} --> (x \"y\")")]

    [@foo|{...}|
     (foo "...")]

    [@foo|{"}" follows "{"}|
     (foo "\"}\" follows \"{\"")]

    [@foo|{Nesting |{is}| ok}|
     (foo "Nesting |{is}| ok")]

    [@foo|{Maze
           |@bar{is}
           Life!}|
     (foo "Maze" "\n" (bar "is") "\n" "Life!")]

    [@t|{In |@i|{sub|@"@"s}| too}|
     (t "In " (i "sub@s") " too")]

    [@foo|<<<{@x{foo} |@{bar}|.}>>>|
     (foo "@x{foo} |@{bar}|.")]

    [@foo|!!{X |!!@b{Y}...}!!|
     (foo "X " (b "Y") "...")]

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
                       newline here;}
          Second line}
     (foo "First line" "\n" "Second line")]

    [@foo{A long @;
          single-@;
          string arg.}
     (foo "A long single-string arg.")]

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

    [@text{Some @b{bold
       text}, and
       more text.}
     (text "Some " (b "bold" "\n" "text") ", and" "\n" "more text.")]

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

    [,(let-syntax ([foo
                    (lambda (stx)
                      (let ([p (syntax-property stx 'scribble)])
                        (syntax-case stx ()
                          [(_ x ...)
                           (and (pair? p) (eq? (car p) 'form) (even? (cadr p)))
                           (let loop ([n (/ (cadr p) 2)]
                                      [as '()]
                                      [xs (syntax->list #'(x ...))])
                             (if (zero? n)
                               #`(list 'foo `#,(reverse as) #,@xs)
                               (loop (sub1 n)
                                     (cons #`(#,(car xs) ,#,(cadr xs)) as)
                                     (cddr xs))))])))])
        @foo[x 1 y (* 2 3)]{blah})
     (foo ((x 1) (y 6)) "blah")]

    [,(let-syntax ([verb
                    (lambda (stx)
                      (syntax-case stx ()
                        [(_ cmd item ...)
                         #`(cmd
                            #,@(let loop ([items (syntax->list #'(item ...))])
                                 (if (null? items)
                                   '()
                                   (let* ([fst  (car items)]
                                          [prop (syntax-property fst 'scribble)]
                                          [rst  (loop (cdr items))])
                                     (cond [(eq? prop 'indentation) rst]
                                           [(not (and (pair? prop)
                                                      (eq? (car prop)
                                                           'newline)))
                                            (cons fst rst)]
                                           [else (cons (datum->syntax-object
                                                        fst (cadr prop) fst)
                                                       rst)])))))]))])
        @verb[string-append]{
          foo
            bar
        })
     "foo\n            bar"]

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
