#lang scribble/doc
@(require scribble/manual
          scribble/core scribble/html-properties scribble/latex-properties
          "utils.rkt"
          (for-label racket/base
                     ;; FIXME: need to get this in
                     ;; scribble/text
                     ))
@initialize-tests

@title[#:tag "text"
       #:style (make-style #f (list (make-tex-addition "shaded.tex")
                                    (make-css-addition "shaded.css")))
      ]{Text Generation}
@section-index["Preprocessor"]

@defmodulelang[scribble/text]{The @racketmodname[scribble/text] language
provides everything from @racket[racket/base] with a few changes that
make it suitable as a text generation or a preprocessor language:

@itemize[

  @item{The language uses @racket[read-syntax-inside] to read the body
        of the module, similar to @secref["docreader"].  This means that
        by default, all text is read in as Racket strings; and
        @seclink["reader"]|{@-forms}| can be used to use Racket
        functions and expression escapes.}

  @item{Values of expressions are printed with a custom @racket[output]
        function.  This function displays most values in a similar way
        to @racket[display], except that it is more convenient for a
        textual output.}]

}

@; TODO:
@; * make all example sections be subsections,
@; * add a reference section,
@; * a section on "scribble/text.rkt"
@; * maybe a section on additional utilities: begin/text

@;--------------------------------------------------------------------
@section{Writing Text Files}

The combination of the two features makes text in files in the
@racket[scribble/text] language be read as strings, which get printed
out when the module is @racket[require]d, for example, when a file is
given as an argument to @exec{racket}.  (In these example the left
part shows the source input, and the right part the printed result.)

@example|-{#lang scribble/text
           Programming languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.
           ---***---
           Programming languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.}-|

Using @seclink["reader"]|{@-forms}|, we can define and use Racket
functions.

@example|-{#lang scribble/text
           @(require racket/list)
           @(define Foo "Preprocessing")
           @(define (3x . x)
              ;; racket syntax here
              (add-between (list x x x) " "))
           @Foo languages should
           be designed not by piling
           feature on top of feature, but
           @3x{blah}.
           ---***---
           Preprocessing languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.}-|

As demonstrated in this case, the @racket[output] function simply
scans nested list structures recursively, which makes them convenient
for function results.  In addition, @racket[output] prints most values
similarly to @racket[display] --- notable exceptions are void and
false values which cause no output to appear.  This can be used for
convenient conditional output.

@example|-{#lang scribble/text
           @(define (errors n)
              (list n
                    " error"
                    (and (not (= n 1)) "s")))
           You have @errors[3] in your code,
           I fixed @errors[1].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

Using the scribble @seclink["reader"]|{@-forms}| syntax, you can write
functions more conveniently too.

@example|-{#lang scribble/text
           @(define (errors n)
              ;; note the use of `unless'
              @list{@n error@unless[(= n 1)]{s}})
           You have @errors[3] in your code,
           I fixed @errors[1].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

Following the details of the scribble reader, you may notice that in
these examples there are newline strings after each definition, yet
they do not show in the output.  To make it easier to write
definitions, newlines after definitions and indentation spaces before
them are ignored.

@example|-{#lang scribble/text

           @(define (plural n)
              (unless (= n 1) "s"))

           @(define (errors n)
              @list{@n error@plural[n]})

           You have @errors[3] in your code,
             @(define fixed 1)
           I fixed @errors[fixed].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

These end-of-line newline strings are not ignored when they follow
other kinds of expressions, which may lead to redundant empty lines in
the output.

@example|-{#lang scribble/text
           @(define (count n str)
              (for/list ([i (in-range 1 (add1 n))])
                @list{@i @str,@"\n"}))
           Start...
           @count[3]{Mississippi}
           ... and I'm done.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,

           ... and I'm done.}-|

There are several ways to avoid having such empty lines in your
output.  The simplest way is to arrange for the function call's form
to end right before the next line begins, but this is often not too
convenient.  An alternative is to use a @litchar|{@;}| comment, which
makes the scribble reader ignore everything that follows it up to and
including the newline.  (These methods can be applied to the line that
precedes the function call too, but the results are likely to have
what looks like erroneous indentation.  More about this below.)

@example|-{#lang scribble/text
           @(define (count n str)
              (for/list ([i (in-range 1 (+ n 1))])
                @list{@i @str,@"\n"}))
           Start...
           @count[3]{Mississippi
           }... done once.

           Start again...
           @count[3]{Massachusetts}@;
           ... and I'm done again.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           ... done once.

           Start again...
           1 Massachusetts,
           2 Massachusetts,
           3 Massachusetts,
           ... and I'm done again.}-|

A better approach is to generate newlines only when needed.

@example|-{#lang scribble/text
           @(require racket/list)
           @(define (counts n str)
              (add-between
               (for/list ([i (in-range 1 (+ n 1))])
                 @list{@i @str,})
               "\n"))
           Start...
           @counts[3]{Mississippi}
           ... and I'm done.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           ... and I'm done.}-|

In fact, this is common enough that the @racket[scribble/text]
language provides a convenient facility: @racket[add-newlines] is a
function that is similar to @racket[add-between] using a newline
string as the default separator, except that false and void values are
filtered out before doing so.

@example|-{#lang scribble/text
           @(define (count n str)
              (add-newlines
               (for/list ([i (in-range 1 (+ n 1))])
                 @list{@i @str,})))
           Start...
           @count[3]{Mississippi}
           ... and I'm done.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           ... and I'm done.}-|

@example|-{#lang scribble/text
           @(define (count n str)
              (add-newlines
               (for/list ([i (in-range 1 (+ n 1))])
                 @(and (even? i) @list{@i @str,}))))
           Start...
           @count[6]{Mississippi}
           ... and I'm done.
           ---***---
           Start...
           2 Mississippi,
           4 Mississippi,
           6 Mississippi,
           ... and I'm done.}-|

The separator can be set to any value.

@example|-{#lang scribble/text
           @(define (count n str)
              (add-newlines #:sep ",\n"
               (for/list ([i (in-range 1 (+ n 1))])
                 @list{@i @str})))
           Start...
           @count[3]{Mississippi}.
           ... and I'm done.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi.
           ... and I'm done.}-|


@;--------------------------------------------------------------------
@section{Defining Functions and More}

(Note: most of the tips in this section are applicable to any code
that uses the Scribble @tech{@"@"-form} syntax.)

Because the Scribble reader is uniform, you can use it in place of any
expression where it is more convenient.  (By convention, we use a
plain S-expression syntax when we want a Racket expression escape, and
an @tech{@"@"-form} for expressions that render as text, which, in the
@racket[scribble/text] language, is any value-producing expression.)
For example, you can use an @tech{@"@"-form} for a function that you define.

@example|-{#lang scribble/text
           @(define @bold[text] @list{*@|text|*})
           An @bold{important} note.
           ---***---
           An *important* note.
           }-|

This is not commonly done, since most functions that operate with text
will need to accept a variable number of arguments.  In fact, this
leads to a common problem: what if we want to write a function that
consumes a number of ``text arguments'' rathen than a single
``rest-like'' body?  The common solution for this is to provide the
separate text arguments in the S-expression part of an @tech{@"@"-form}.

@example|-{#lang scribble/text
           @(define (choose 1st 2nd)
              @list{Either @1st, or @|2nd|@"."})
           @(define who "us")
           @choose[@list{you're with @who}
                   @list{against @who}]
           ---***---
           Either you're with us, or against us.
           }-|

You can even use @tech{@"@"-forms} with a Racket quote or quasiquote as the
``head'' part to make it shorter, or use a macro to get grouping of
sub-parts without dealing with quotes.

@example|-{#lang scribble/text
           @(define (choose 1st 2nd)
              @list{Either @1st, or @|2nd|@"."})
           @(define who "us")
           @choose[@list{you're with @who}
                   @list{against @who}]
           @(define-syntax-rule (compare (x ...) ...)
              (add-newlines
               (list (list "* " x ...) ...)))
           Shopping list:
           @compare[@{apples}
                    @{oranges}
                    @{@(* 2 3) bananas}]
           ---***---
           Either you're with us, or against us.
           Shopping list:
           * apples
           * oranges
           * 6 bananas
           }-|

Yet another solution is to look at the text values and split the input
arguments based on a specific token.  Using @racket[match] can make it
convenient --- you can even specify the patterns with @tech{@"@"-forms}.

@example|-{#lang scribble/text
           @(require racket/match)
           @(define (features . text)
              (match text
                [@list{@|1st|@...
                       ---
                       @|2nd|@...}
                 @list{>> Pros <<
                       @1st;
                       >> Cons <<
                       @|2nd|.}]))
           @features{fast,
                     reliable
                     ---
                     expensive,
                     ugly}
           ---***---
           >> Pros <<
           fast,
           reliable;
           >> Cons <<
           expensive,
           ugly.
           }-|

In particular, it is often convenient to split the input by lines,
identified by delimiting @racket["\n"] strings.  Since this can be
useful, a @racket[split-lines] function is provided.

@example|-{#lang scribble/text
           @(require racket/list)
           @(define (features . text)
              (add-between (split-lines text)
                           ", "))
           @features{red
                     fast
                     reliable}.
           ---***---
           red, fast, reliable.
           }-|

Finally, the Scribble reader accepts @emph{any} expression as the head
part of an @"@"-form --- even an @"@" form.  This makes it possible to
get a number of text bodies by defining a curried function, where each
step accepts any number of arguments.  This, however, means that the
number of body expressions must be fixed.

@example|-{#lang scribble/text
           @(define ((choose . 1st) . 2nd)
              @list{Either you're @1st, or @|2nd|.})
           @(define who "me")
           @@choose{with @who}{against @who}
           ---***---
           Either you're with me, or against me.
           }-|


@;--------------------------------------------------------------------
@section{Using Printouts}

Because the text language simply displays each toplevel value as the
file is run, it is possible to print text directly as part of the
output.

@example|-{#lang scribble/text
           First
           @display{Second}
           Third
           ---***---
           First
           Second
           Third}-|

Taking this further, it is possible to write functions that output
some text @emph{instead} of returning values that represent the text.

@example|-{#lang scribble/text
           @(define (count n)
              (for ([i (in-range 1 (+ n 1))])
                (printf "~a Mississippi,\n" i)))
           Start...
           @count[3]@; avoid an empty line
           ... and I'm done.
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           ... and I'm done.}-|

This can be used to produce a lot of output text, even infinite.

@example|-{#lang scribble/text
           @(define (count n)
              (printf "~a Mississippi,\n" n)
              (count (add1 n)))
           Start...
           @count[1]
           this line is never printed!
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           4 Mississippi,
           5 Mississippi,
           ...}-|

However, you should be careful not to mix returning values with
printouts, as the results are rarely desirable.

@example|-{#lang scribble/text
           @list{1 @display{two} 3}
           ---***---
           two1  3}-|

Note that you don't need side-effects if you want infinite output.
The @racket[output] function iterates thunks and (composable)
promises, so you can create a loop that is delayed in either form.
@; Note: there is some sfs-related problem in racket that makes it not
@; run in bounded space, so don't show it for nowx.

@example|-{#lang scribble/text
           @(define (count n)
              (cons @list{@n Mississippi,@"\n"}
                    (lambda ()
                      (count (add1 n)))))
           Start...
           @count[1]
           this line is never printed!
           ---***---
           Start...
           1 Mississippi,
           2 Mississippi,
           3 Mississippi,
           4 Mississippi,
           5 Mississippi,
           ...}-|


@;--------------------------------------------------------------------
@section{Indentation in Preprocessed output}

An issue that can be very important in many text generation applications
is the indentation of the output.  This can be crucial in some cases, if
you're generating code for an indentation-sensitive language (e.g.,
Haskell, Python, or C preprocessor directives).  To get a better
understanding of how the pieces interact, you may want to review how the
@seclink["reader"]|{Scribble reader}| section, but also remember that
you can use quoted forms to see how some form is read.

@example|-{#lang scribble/text
           @(format "~s" '@list{
                            a
                              b
                            c})
           ---***---
           (list "a" "\n" "  " "b" "\n" "c")}-|

The Scribble reader ignores indentation spaces in its body.  This is an
intentional feature, since you usually do not want an expression to
depend on its position in the source.  But the question is whether we
@emph{can} render some output text with proper indentation.  The
@racket[output] function achieves that by introducing @racket[block]s.
Just like a list, a @racket[block] contains a list of elements, and when
one is rendered, it is done in its own indentation level.  When a
newline is part of a @racket[block]'s contents, it causes the following
text to appear with indentation that corresponds to the column position
at the beginning of the block.

In addition, lists are also rendered as blocks by default, so they can
be used for the same purpose.  In most cases, this makes the output
appear ``as intended'' where lists are used for nested pieces of text
--- either from a literal @racket[list] expression, or an expression
that evaluates to a list, or when a list is passed on as a value; either
as a toplevel expression, or as a nested value; either appearing after
spaces, or after other output.

@example|-{#lang scribble/text
           foo @block{1
                      2
                      3}
           foo @list{4
                     5
                     6}
           ---***---
           foo 1
               2
               3
           foo 4
               5
               6}-|

@example|-{#lang scribble/text
           @(define (code . text)
              @list{begin
                      @text
                    end})
           @code{first
                 second
                 @code{
                   third
                   fourth}
                 last}
           ---***---
           begin
             first
             second
             begin
               third
               fourth
             end
             last
           end}-|

@example|-{#lang scribble/text
           @(define (enumerate . items)
              (add-newlines #:sep ";\n"
               (for/list ([i (in-naturals 1)]
                          [item (in-list items)])
                 @list{@|i|. @item})))
           Todo: @enumerate[@list{Install Racket}
                            @list{Hack, hack, hack}
                            @list{Profit}].
           ---***---
           Todo: 1. Install Racket;
                 2. Hack, hack, hack;
                 3. Profit.}-|

@example[#:hidden]|-{
  #lang scribble/text
  @; demonstrates how indentation is preserved inside lists
  begin
    a
    b
    @list{c
          d
          @list{e
                  f
                g}
          h
            i
            @list{j
                    k
                  l}
            m
          n
          o}
    p
    q
  end
  ---***---
  begin
    a
    b
    c
    d
    e
      f
    g
    h
      i
      j
        k
      l
      m
    n
    o
    p
    q
  end
  }-|

@example[#:hidden]|-{
  #lang scribble/text

    @list{
      a

      b
    }

    c
  ---***---
  a

  b

  c
  }-|

@example[#:hidden]|-{
  #lang scribble/text
  @; indentation works even when coming from a function
  @(define (((if . c) . t) . e)
     @list{
       if (@c)
         @t
       else
         @e
       fi})
  function foo() {
    @list{if (1 < 2)
            something1
          else
            @@@if{2<3}{something2}{something3}
            repeat 3 {
              @@@if{2<3}{something2}{something3}
              @@@if{2<3}{
                @list{something2.1
                      something2.2}
              }{
                something3
              }
            }
          fi}
    return
  }
  ---***---
  function foo() {
    if (1 < 2)
      something1
    else
      if (2<3)
        something2
      else
        something3
      fi
      repeat 3 {
        if (2<3)
          something2
        else
          something3
        fi
        if (2<3)
          something2.1
          something2.2
        else
          something3
        fi
      }
    fi
    return
  }
  }-|

@example[#:hidden]|-{
  #lang scribble/text
  @; indentation works with a list, even a single string with a newline
  @; in a list, but not in a string by itself
  function foo() {
    prefix
    @list{if (1 < 2)
            something1
          else
            @list{something2
                  something3}
            @'("something4\nsomething5")
            @"something6\nsomething7"
          fi}
    return
  }
  @; can be used with a `display', but makes sense only at the top level
  @; or in thunks (not demonstrated here)
  @(display 123) foo @list{bar1
                           bar2
                           bar2}
  ---***---
  function foo() {
    prefix
    if (1 < 2)
      something1
    else
      something2
      something3
      something4
      something5
      something6
    something7
    fi
    return
  }
  123 foo bar1
          bar2
          bar2
  }-|

There are, however, cases when you need more refined control over the
output.  The @racket[scribble/text] language provides a few functions
for such cases in addition to @racket[block].  The @racket[splice]
function groups together a number of values but avoids introducing a new
indentation context.  Furthermore, lists are not always rendered as
@racket[block]s --- instead, they are rendered as @racket[splice]s when
they are used inside one, so you essentially use @racket[splice] to
avoid the ``indentation group'' behavior, and @racket[block] to restore
it.

@example|-{#lang scribble/text
           @(define (blah . text)
              @splice{{
                blah(@block{@text});
              }})
           start
             @splice{foo();
                     loop:}
             @list{if (something) @blah{one,
                                        two}}
           end
           ---***---
           start
             foo();
           loop:
             if (something) {
               blah(one,
                    two);
             }
           end
           }-|

The @racket[disable-prefix] function disables all indentation
printouts in its contents, including the indentation before the body
of the @racket[disable-prefix] value itself.  It is useful, for
example, to print out CPP directives.

@example|-{#lang scribble/text
           @(define (((IFFOO . var) . expr1) . expr2)
              (define (array e1 e2)
                @list{[@e1,
                       @e2]})
              @list{var @var;
                    @disable-prefix{#ifdef FOO}
                    @var = @array[expr1 expr2];
                    @disable-prefix{#else}
                    @var = @array[expr2 expr1];
                    @disable-prefix{#endif}})

           function blah(something, something_else) {
             @disable-prefix{#include "stuff.inc"}
             @@@IFFOO{i}{something}{something_else}
           }
           ---***---
           function blah(something, something_else) {
           #include "stuff.inc"
             var i;
           #ifdef FOO
             i = [something,
                  something_else];
           #else
             i = [something_else,
                  something];
           #endif
           }
           }-|

If there are values after a @racket[disable-prefix] value on the same
line, they @emph{will} get indented to the goal column (unless the
output is already beyond it).

@example|-{#lang scribble/text
           @(define (thunk name . body)
              @list{function @name() {
                      @body
                    }})
           @(define (ifdef cond then else)
              @list{@disable-prefix{#}ifdef @cond
                      @then
                    @disable-prefix{#}else
                      @else
                    @disable-prefix{#}endif})

           @thunk['do_stuff]{
             init();
             @ifdef["HAS_BLAH"
               @list{var x = blah();}
               @thunk['blah]{
                 @ifdef["BLEHOS"
                   @list{@disable-prefix{#}@;
                           include <bleh.h>
                         bleh();}
                   @list{error("no bleh");}]
               }]
             more_stuff();
           }
           ---***---
           function do_stuff() {
             init();
           # ifdef HAS_BLAH
               var x = blah();
           # else
               function blah() {
           #     ifdef BLEHOS
           #       include <bleh.h>
                   bleh();
           #     else
                   error("no bleh");
           #     endif
               }
           # endif
             more_stuff();
           }
           }-|

There are cases where each line should be prefixed with some string
other than a plain indentation.  The @racket[add-prefix] function
causes its contents to be printed using some given string prefix for
every line.  The prefix gets accumulated to an existing indentation,
and indentation in the contents gets added to the prefix.

@example|-{#lang scribble/text
           @(define (comment . body)
              @add-prefix["// "]{@body})
           @comment{add : int int -> string}
           char *foo(int x, int y) {
             @comment{
               skeleton:
                 allocate a string
                 print the expression into it
                 @comment{...more work...}
             }
             char *buf = malloc(@comment{FIXME!
                                         This is bad}
                                100);
           }
           ---***---
           // add : int int -> string
           char *foo(int x, int y) {
             // skeleton:
             //   allocate a string
             //   print the expression into it
             //   // ...more work...
             char *buf = malloc(// FIXME!
                                // This is bad
                                100);
           }
           }-|

When combining @racket[add-prefix] and @racket[disable-prefix] there
is an additional value that can be useful: @racket[flush].  This is a
value that causes @racket[output] to print the current indentation and
prefix.  This makes it possible to get the ``ignored as a prefix''
property of @racket[disable-prefix] but only for a nested prefix.

@example|-{#lang scribble/text
           @(define (comment . text)
              (list flush
                    @add-prefix[" *"]{
                      @disable-prefix{/*} @text */}))
           function foo(x) {
             @comment{blah
                      more blah
                      yet more blah}
             if (x < 0) {
               @comment{even more
                        blah here
                        @comment{even
                                 nested}}
               do_stuff();
             }
           }
           ---***---
           function foo(x) {
             /* blah
              * more blah
              * yet more blah */
             if (x < 0) {
               /* even more
                * blah here
                * /* even
                *  * nested */ */
               do_stuff();
             }
           }
           }-|

@example[#:hidden]|-{
  #lang scribble/text

  @(begin
     ;; This is a somewhat contrived example, showing how to use lists
     ;; and disable-prefix to control the added prefix
     (define (item . text)
       ;; notes: the `flush' makes the prefix to that point print so the
       ;; disable-prefix "* " is printed after it, which overwrites the
       ;; "| " prefix
       (list flush (add-prefix "| " (disable-prefix "* ") text)))
     ;; note that a simple item with spaces is much easier:
     (define (simple . text) @list{* @text}))

  start
    @item{blah blah blah
          blah blah blah
          @item{more stuff
                more stuff
                more stuff}
          blah blah blah
          blah blah blah}
    @simple{more blah
            blah blah}
  end
  ---***---
  start
    * blah blah blah
    | blah blah blah
    | * more stuff
    | | more stuff
    | | more stuff
    | blah blah blah
    | blah blah blah
    * more blah
      blah blah
  end
  }-|


@;--------------------------------------------------------------------
@section{Using External Files}

Using additional files that contain code for your preprocessing is
trivial: the source text is still source code in a module, so you can
@racket[require] additional files with utility functions.

@example|-{#lang scribble/text
           @(require "itemize.rkt")
           Todo:
           @itemize[@list{Hack some}
                    @list{Sleep some}
                    @list{Hack some
                          more}]
           ---***--- itemize.rkt
           #lang racket
           (provide itemize)
           (define (itemize . items)
             (add-between (map (lambda (item)
                                 (list "* " item))
                               items)
                          "\n"))
           ---***---
           Todo:
           * Hack some
           * Sleep some
           * Hack some
             more
           }-|

Note that the @seclink["at-exp-lang"]{@racket[at-exp] language} can
often be useful here, since such files need to deal with texts.  Using
it, it is easy to include a lot of textual content.

@example|-{#lang scribble/text
           @(require "stuff.rkt")
           Todo:
           @itemize[@list{Hack some}
                    @list{Sleep some}
                    @list{Hack some
                          more}]
           @summary
           ---***--- stuff.rkt
           #lang at-exp racket/base
           (require racket/list)
           (provide (all-defined-out))
           (define (itemize . items)
             (add-between (map (lambda (item)
                                 @list{* @item})
                               items)
                          "\n"))
           (define summary
             @list{If that's not enough,
                   I don't know what is.})
           ---***---
           Todo:
           * Hack some
           * Sleep some
           * Hack some
             more
           If that's not enough,
           I don't know what is.
           }-|

Of course, the extreme side of this will be to put all of your content
in a plain Racket module, using @tech{@"@"-forms} for convenience.  However,
there is no need to use the text language in this case; instead, you can
@racket[(require scribble/text)], which will get all of the bindings
that are available in the @racket[scribble/text] language.  Using
@racket[output], switching from a preprocessed files to a Racket file is
very easy ---- choosing one or the other depends on whether it is more
convenient to write a text file with occasional Racket expressions or
the other way.

@example|-{#lang at-exp racket/base
           (require scribble/text racket/list)
           (define (itemize . items)
             (add-between (map (lambda (item)
                                 @list{* @item})
                               items)
                          "\n"))
           (define summary
             @list{If that's not enough,
                   I don't know what is.})
           (output
            @list{
              Todo:
              @itemize[@list{Hack some}
                       @list{Sleep some}
                       @list{Hack some
                             more}]
              @summary
            })
           ---***---
           Todo:
           * Hack some
           * Sleep some
           * Hack some
             more
           If that's not enough,
           I don't know what is.
           }-|

However, you might run into a case where it is desirable to include a
mostly-text file from a @racket[scribble/text] source file.  It might be
because you prefer to split the source text to several files, or because
you need to use a template file that cannot have a @litchar{#lang}
header (for example, an HTML template file that is the result of an
external editor).  In these cases, the @racket[scribble/text] language
provides an @racket[include] form that includes a file in the
preprocessor syntax (where the default parsing mode is text).

@example|-{#lang scribble/text
           @(require racket/list)
           @(define (itemize . items)
              (list
               "<ul>"
               (add-between
                (map (lambda (item)
                       @list{<li>@|item|</li>})
                     items)
                "\n")
               "</ul>"))
           @(define title "Todo")
           @(define summary
              @list{If that's not enough,
                    I don't know what is.})

           @include["template.html"]
           ---***--- template.html
           <html>
           <head><title>@|title|</title></head>
           <body>
             <h1>@|title|</h1>
             @itemize[@list{Hack some}
                      @list{Sleep some}
                      @list{Hack some
                            more}]
             <p><i>@|summary|</i></p>
           </body>
           </html>
           ---***---
           <html>
           <head><title>Todo</title></head>
           <body>
             <h1>Todo</h1>
             <ul><li>Hack some</li>
                 <li>Sleep some</li>
                 <li>Hack some
                     more</li></ul>
             <p><i>If that's not enough,
                   I don't know what is.</i></p>
           </body>
           </html>
           }-|

(Using @racket[require] with a text file in the @racket[scribble/text]
language will not work as intended: the language will display the text
is when the module is invoked, so the required file's contents will be
printed before any of the requiring module's text does.  If you find
yourself in such a situation, it is better to switch to a
Racket-with-@"@"-expressions file as shown above.)

@;FIXME: add more text on `restore-prefix', `set-prefix', `with-writer'

@;FIXME: add this to the reference section
@;@defform[(include filename)]{
@;
@;Preprocess the @racket[filename] using the same syntax as
@;@racket[scribble/text].  This is similar to using @racket[load] in a
@;namespace that can access names bound in the current file so included
@;code can refer to bindings from the including module.  Note, however,
@;that the including module cannot refer to names that are bound the
@;included file because it is still a plain racket module---for such
@;uses you should still use @racket[require] as usual.}


@; Two random tests
@example[#:hidden]|-{
  #lang scribble/text

  @define[name]{Racket}

  Suggested price list for "@name"

  @; test mutual recursion, throwing away inter-definition spaces
  @; <-- this is needed to get only one line of space above
  @(define (items-num)
     (length items))

  @(define average
     (delay (/ (apply + (map car items)) (length items))))

  @(define items
     (list @list[99]{Home}
           @list[149]{Professional}
           @list[349]{Enterprize}))

  @(for/list ([i items] [n (in-naturals)])
     @list{@|n|. @name @cadr[i] edition: $@car[i].99
           @||})@; <-- also needed

  Total: @items-num items
  Average price: $@|average|.99
  ---***---
  Suggested price list for "Racket"

  0. Racket Home edition: $99.99
  1. Racket Professional edition: $149.99
  2. Racket Enterprize edition: $349.99

  Total: 3 items
  Average price: $199.99
  }-|
@example[#:hidden]|-{
  #lang scribble/text

  --*--
  @(define (angled . body) (list "<" body ">"))
   @(define (shout . body) @angled[(map string-upcase body)])
    @define[z]{blah}

  blah @angled{blah @shout{@z} blah} blah

  @(define-syntax-rule @twice[x]
     (list x ", " x))

  @twice{@twice{blah}}

  @include{inp1}

  @(let ([name "Eli"]) (let ([foo (include "inp2")]) (list foo "\n" foo)))
  Repeating yourself much?
  ---***--- inp1
  Warning: blah overdose might be fatal
  ---***--- inp2
  @(define (foo . xs) (bar xs))
  @(begin (define (isname) @list{is @foo{@name}})
          (define-syntax-rule (DEF x y) (define x y)))
  @(DEF (bar x) (list z " " x))
  @(define-syntax-rule (BEG x ...) (begin x ...))
  @(BEG (define z "zee"))

  My name @isname
  @DEF[x]{Foo!}

    ... and to that I say "@x", I think.

  ---***---
  --*--
  blah <blah <BLAH> blah> blah

  blah, blah, blah, blah

  Warning: blah overdose might be fatal

  My name is zee Eli
    ... and to that I say "Foo!", I think.
  My name is zee Eli
    ... and to that I say "Foo!", I think.
  Repeating yourself much?
  }-|
