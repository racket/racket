#lang scribble/doc

@title{@bold{Quick}: An Introduction to Racket with Pictures}

@author["Matthew Flatt"]

@; ----------------------------------------------------------------------

@(require scribble/manual
          "mreval.ss"
          "keep.ss"
          scribble/urls
          scribble/struct
          scheme/class

          (for-label scheme/base
                     scheme/gui/base
                     scheme/class
                     slideshow
                     slideshow/code
                     slideshow/flash)

          (for-syntax scheme/base))

@; ----------------------------------------------------------------------

This tutorial provides a brief introduction to the Racket
programming language by using one of its picture-drawing
libraries. Even if you don't intend to use Racket for your artistic
endeavours, the picture library supports interesting and enlightening
examples. After all, a picture is worth five hundred ``hello world''s.

Along the same lines, we assume that you will run the examples using
@link[url:download-drscheme]{DrRacket}. Using DrRacket is the fastest
way to get a sense of what the language and system feels like, even if
you eventually use Racket with Emacs, vi, or some other editor.

@; ----------------------------------------------------------------------
@section{Ready...}

@link[url:download-drscheme]{Download Racket}, install, and then
start DrRacket.

@; ----------------------------------------------------------------------
@section{Set...}

To draw pictures, we must first load some picture functions, which are
part of a library for creating slide presentations.  Copy the
following into the @defterm{definitions area}, which is the top text
area that you see in DrRacket:

@schememod[slideshow]

Then click the @onscreen{Run} button. You'll see the text caret move
to the bottom text area, which is the @defterm{interactions area}.

If you've used DrRacket before, you might need to reset DrRacket to
use the language declared in the source via the @menuitem["Language"
"Choose Language..."] menu item before clicking @onscreen{Run}.

@; ----------------------------------------------------------------------
@section{Go!}

When you type an expression after the @onscreen{>} in the interactions
window and hit Enter, DrRacket evaluates the expression and prints its
result. An expression can be just a value, such as the number
@scheme[5] or the string @scheme["art gallery"]:

@mr-interaction[5 "art gallery"]

An expression can also be a function call. To call a function, put
an open parenthesis before the function name, then expressions for the
function arguments, and then a close parenthesis, like this:

@mr-interaction[(circle 10)]

A result from the @scheme[circle] function is a picture value, which
prints as an expression result in much the same way that numbers or
strings print.  The argument to @scheme[circle] determines the
circle's size in pixels.  As you might guess, there's a
@scheme[rectangle] function that takes two arguments instead of one:

@mr-interaction[(rectangle 10 20)]

Try giving @scheme[circle] the wrong number of arguments, just to see
what happens:

@mr-interaction[(circle 10 20)]

Note that DrRacket highlights in pink the expression that triggered
the error (but pink highlighting is not shown in this documentation).

In addition to basic picture constructors like @scheme[circle] and
@scheme[rectangle], there's a @scheme[hc-append] function that
combines pictures. When you start composing function calls in Racket,
it looks like this:

@mr-interaction[(hc-append (circle 10) (rectangle 10 20))]

The hyphen in the name @scheme[hc-append] is just a part of the
identifier; it's not @schemeidfont{hc} minus
@schemeidfont{append}. The function name starts with @scheme[h]
because it combines pictures horizontally, and the next letter is
@scheme[c] because the pictures are centered vertically.

If you wonder what other functions exist---perhaps a way to stack
pictures vertically and left-aligned?---move the text caret to the
name @scheme[hc-append] and press the F1 key in DrRacket. A browser
window will open, and it will give you a link to the documentation for
@scheme[hc-append]. Click the link, and you'll see lots of other
functions.

If you're reading this in HTML form, you can also just click on
@scheme[hc-append] or any other imported identifier that is used in
this tutorial.

@; ----------------------------------------------------------------------
@section{Definitions}

To use a particular circle and rectangle picture many times, it's
simpler to give them names. Move back to the definitions area (the top
area) and add two definitions, so that the complete content of the
definitions area looks like this:

@mr-schememod+eval[
slideshow
(define c (circle 10))
(define r (rectangle 10 20))
]

Then click @onscreen{Run} again. Now, you can just type @scheme[c] or
@scheme[r]:

@mr-interaction[r (hc-append c r) (hc-append 20 c r c)]

As you can see, the @scheme[hc-append] function accepts an optional
number argument before the picture arguments, and it accepts any
number of picture arguments. When a number is provided, it specifies
the amount of space to add between pictures.

We could have evaluated the @scheme[define] forms for @scheme[c] and
@scheme[r] in the interactions area instead of the definitions
area. In practice, though, the definitions area is where your program
lives---it's the file that you save---while the interaction area is
for transient explorations and debugging tasks.

Let's add a function definition to the program. A function definition
uses @scheme[define], just like our shape definitions, but with an
open parenthesis before the function name, and names for the function
arguments before the matching close parenthesis:

@mr-schemeblock+eval[
(define (square n)
  (code:comment @#,t{A semi-colon starts a line comment.})
  (code:comment @#,t{The expression below is the function body.})
  (filled-rectangle n n))
]

The syntax of the definition mirrors the syntax of a function
call:

@mr-interaction[(square 10)]

In the same way that definitions can be evaluated in the interactions
area, expressions can be included in the definitions area. When a
program is run, expression results from the definition area are shown
in the interaction area. From now on, we'll write our example
definitions and expressions together, and you can put them in
whichever area you prefer. The examples will build on each other,
however, so it's best to put at least the definitions in the
definition area.

@; ----------------------------------------------------------------------
@section{Local Binding}

The @scheme[define] form can be used in some places to create local
bindings. For example, it can be used inside a function body:

@mr-def+int[
(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))
(four (circle 10))
]

More typically, Racketeers use the @scheme[let] or @scheme[let*] form
for local binding. An advantage of @scheme[let] is that it can be used
in any expression position. Also, it binds many identifiers at once,
instead of requiring a separate @scheme[define] for each identifier:

@mr-def+int[
(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(checker (colorize (square 10) "red")
         (colorize (square 10) "black"))
]

A @scheme[let] form binds many identifiers at the same time, so the
bindings cannot refer to each other. The @scheme[let*] form, in
contrast, allows later bindings to use earlier bindings:

@mr-def+int[
(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))
]

@; ----------------------------------------------------------------------
@section{Functions are Values}

Instead of calling @scheme[circle] as a function, try evaluating just
@scheme[circle] as an expression:

@mr-interaction[circle]

That is, the identifier @scheme[circle] is bound to a function
(a.k.a. ``procedure''), just like @scheme[c] is bound to a
circle. Unlike a circle picture, there's not a simple way of
completely printing the function, so DrRacket just prints
@procedure{circle}.

This example shows that functions are values, just like numbers and
pictures (even if they don't print as nicely). Since functions are
values, you can define functions that expect other functions as
arguments:

@mr-def+int[
(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)
(series square)
]

When calling a function that accepts a function argument, the
argument function often isn't needed anywhere else. Having to write
down the function via @scheme[define] would be a hassle, because you
have to make up a name and find a place to put the function
definition. The alternative is to use @scheme[lambda], which creates an
anonymous function:

@mr-interaction[(series (lambda (size) (checkerboard (square size))))]

The parenthesized names after a @scheme[lambda] are the arguments to
the function, and the expression after the argument names is the
function body. Using the word ``lambda'' instead of ``function'' or
``procedure'' is part of Racket's history and culture.

A @scheme[define] form for a function is really a shorthand for a
simple @scheme[define] using @scheme[lambda] as the value. For
example, the @scheme[series] definition could be written as

@schemeblock[
(define series
  (lambda (mk)
    (hc-append 4 (mk 5) (mk 10) (mk 20))))
]

Most Racketeers prefer to use the shorthand function form with
@scheme[define] instead of expanding to @scheme[lambda].

@; ----------------------------------------------------------------------
@section{Lexical Scope}

Racket is a lexically scoped language, which means that whenever an
identifier is used as an expression, something in the textual
environment of the expression determines the identifier's
binding. This rule applies to identifiers in a @scheme[lambda] body as
well as anywhere else.

For example, in the following @scheme[rgb-series] function the uses
of @scheme[mk] in each @scheme[lambda] form to refer to the argument of
@scheme[rgb-series], since that's the binding that is textually in
scope:

@mr-def+int[
(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
(rgb-series circle)
(rgb-series square)
]

Here's another example, where @scheme[rgb-maker] takes a function and
returns a new one that remembers and uses the original function.

@mr-def+int[
(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
(series (rgb-maker circle))
(series (rgb-maker square))
]

Note how composing functions via @scheme[rgb-maker] creates a
different alignment of objects within the picture compared to using
@scheme[rgb-series].

@; ----------------------------------------------------------------------
@section{Lists}

Racket inherits much of its style from the language Lisp, whose name
originally stood for ``LISt Processor,'' and lists remain an important
part of Racket.

The @scheme[list] function takes any number of arguments and returns
a list containing the given values:

@mr-interaction[(list "red" "green" "blue")
                (list (circle 10) (square 10))]

As you can see, a list prints as a pair of parentheses wrapped around
the printed form of the list elements. There's room for confusion
here, because parentheses are used for both expressions, such as
@scheme[(circle 10)], and printed results, such as
@schemeresult[("red" "green" "blue")]. This connection between
expressions and printed results is no coincidence, but we save that
bit of culture for @seclink[#:doc '(lib
"scribblings/guide/guide.scrbl") "quoting-lists"]{discussion
elsewhere}. In the documentation and in DrRacket, result parentheses
are printed in blue, unlike expression parentheses.

If you have a list, then you'll eventually want to do something with
each of the elements. The @scheme[map] function takes a list and a
function to apply to each element of the list; it returns a new list
to combine the function's results:

@mr-def+int[
(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
(rainbow (square 5))
]

Another function that works with lists is @scheme[apply]. Like
@scheme[map], it takes a function and a list, but a function given
to @scheme[apply] should take all of the arguments at once, instead of
each one individually. The @scheme[apply] function is especially
useful with functions that take any number of arguments, such as
@scheme[vc-append]:

@mr-interaction[
(apply vc-append (rainbow (square 5)))
]

Note that @scheme[(vc-append (rainbow (square 5)))] would not work,
because @scheme[vc-append] does not want a list as an argument; it
wants a picture as an argument, and it is willing to accept any number
of them. The @scheme[apply] function bridges the gap between a
function that wants many arguments and a list of those arguments as a
single value.

@; ----------------------------------------------------------------------
@section{Modules}

Since your program in the definitions window starts with

@schememod[slideshow]

all of the code that you put in the definitions window is inside a
module. Furthermore, the module initially imports everything from the
module designated by @schememodname[slideshow], which exports
picture-making functions as well as more commonly used functions
such as @scheme[list] and @scheme[map].

To import additional libraries, use the @scheme[require] form. For
example, the library @schememodname[slideshow/flash] provides a
@scheme[filled-flash] function:

@mr-def+int[
(require slideshow/flash)
(filled-flash 40 30)
]

Modules are named and distributed in various ways:

@itemize[

 @item{Some modules are packaged in the Racket distribution or
       otherwise installed into a hierarchy of
       @defterm{collections}. For example, the module name
       @schememodname[slideshow/flash] means ``the module implemented
       in the file @filepath{flash.ss} that is located in the
       @filepath{slideshow} collection.'' When a module name includes
       no slash, then it refers to a @filepath{main.ss} file.}

 @item{Some modules are distributed through the
       @link[url:planet]{@PLaneT} server, and they can be
       downloaded automatically on demand. For example, the first time
       that you evaluate the following fragment:

       @mr-def+int[
        (require (planet "random.ss" ("schematics" "random.plt" 1 0)))
        (random-gaussian)
       ]

       DrRacket automatically downloads version 1.0 of the
       @filepath{random.plt} library and then imports the
       @filepath{random.ss} module.}

 @item{Some modules live relative to other modules, without
       necessarily belonging to any particular collection or package.
       For example, in DrRacket, if you save your definitions so far in a
       file @filepath{quick.ss} and add the line

        @schemeblock[(provide rainbow square)]

       then you can open a new tab or window in DrRacket, type the new
       program @filepath{use.ss} in the same directory as
       @filepath{quick.ss}:

        @schememod[
         scheme
         (require "quick.ss")
         (rainbow (square 5))
        ]

        and when you run @filepath{use.ss}, a rainbow list of squares
        is the output. Note that @filepath{use.ss} is written using
        the initial import @schememodname[scheme], which does not
        supply any picture-making functions itself---but does provide
        @scheme[require] and the function-calling syntax.}

]

Racketeers typically write new programs and libraries as modules that
import each other through relative paths, and that use existing
libraries from collections and @scheme[planet]. When a program or
library developed this way seems useful to others, it can be uploaded
as a @PLaneT package or distributed in the more old-fashioned way as
an installable collection archive (in either case without modifying
the internal relative references among modules).

@; ----------------------------------------------------------------------
@section{Macros}

Here's another library to try:

@mr-def+int[
(require slideshow/code)
(code (circle 10))
]

Instead of a circle, the result is a picture of the code that, if it
were used as an expression, would produce a circle. In other words,
@scheme[code] is not a function, but instead a new syntactic form for
creating pictures; the bit between the opening parenthesis with
@scheme[code] is not an expression, but instead manipulated by the
@scheme[code] syntactic form.

This helps explain what we meant in the previous section when we said
that @schememodname[scheme] provides @scheme[require] and the
function-calling syntax. Libraries are not restricted to exporting
values, such as functions; they can also define new syntactic
forms. In this sense, Racket isn't exactly a language at all; it's
more of an idea for how to structure a language so that you can extend
it or create entirely new languages.

One way to introduce a new syntactic form is through
@scheme[define-syntax] with @scheme[syntax-rules]:

@mr-def+int[
(define-syntax pict+code
  (syntax-rules ()
    [(pict+code expr)
     (hc-append 10
                expr
                (code expr))]))
(pict+code (circle 10))
]

This kind of definition is a macro. The @scheme[(pict+code expr)] part
is a pattern for uses of the macro; instances of the pattern in a
program are replaced by instances of the corresponding template, which
is @scheme[(hc-append 10 expr (code expr))].  In particular,
@scheme[(pict+code (circle 10))] matches the pattern with
@scheme[(circle 10)] as @scheme[expr], so it is replaced with
@scheme[(hc-append 10 (circle 10) (code (circle 10)))].

Of course, the sword of syntactic extension cuts both ways: inventing
a new language can make it easier to say what you want, but harder for
others to understand. As it happens, the developers of Racket are
constantly giving talks and writing papers that involve Racket code,
and it's worthwhile for everyone who works on those products to know
about @scheme[code].

In fact, you might want to take a look at the @keep-file["quick.scrbl"]
@link["quick.scrbl"]{source of this document}. You'll see that it
starts with @schemefont{#lang}, but otherwise doesn't look a lot
like Racket; nevertheless, we build this document by running its
source as a Racket program. We have to use a lot more than
@scheme[syntax-rules] to extend Racket's syntax enough for writing
documents, but Racket's syntactic extension can take you a long way.

@; ----------------------------------------------------------------------
@section{Objects}

An object system is another example of a sophisticated language
extension that is worth learning and using for Racket users. Objects
are sometimes better than functions, even when you have
@scheme[lambda], and objects work especially well for graphical user
interfaces. The API for Racket's GUI and graphics system is expressed
in terms of objects and classes.

The class system itself is implemented by the
@schememodname[scheme/class] library, and the
@schememodname[scheme/gui/base] library provides the GUI and drawing
classes. By convention, the classes are given names that end with
@scheme[%]:

@mr-defs+int[
[(require scheme/class scheme/gui/base)
 (define f (new frame% [label "My Art"]
                       [width 300]
                       [height 300]
                       [alignment '(center center)]))]
(send f show #t)
]

@(mr-interaction-eval (send f show #f))

The @scheme[new] form creates an instance of a class, where
initialization arguments like @scheme[label] and @scheme[width] are
provided by name. The @scheme[send] form calls a method of the object,
such as @scheme[show], with arguments after the method name; the
argument @scheme[#t] in this case is the boolean constant ``true.''

Pictures generated with @schememodname[slideshow] encapsulate a
function that uses the graphics toolbox's drawing commands to render
the picture to a drawing context, such as a canvas in a frame. The
@scheme[make-pict-drawer] function from @schememodname[slideshow]
exposes a picture's drawing function. We can use
@scheme[make-pict-drawer] in a canvas-painting callback to draw a
picture into a canvas:

@mr-def+int[
(define (add-drawing p)
  (let ([drawer (make-pict-drawer p)])
    (new canvas% [parent f]
                 [style '(border)]
                 [paint-callback (lambda (self dc)
                                   (drawer dc 0 0))])))
(add-drawing (pict+code (circle 10)))
(add-drawing (colorize (filled-flash 50 30) "yellow"))
]

@centerline{
@(mr-interaction-eval-show (scale
                            (bitmap
                             (build-path
                              (collection-path "scribblings/quick") 
                              "art.png"))
                            0.5))}

Each canvas stretches to fill an equal portion of the frame, because
that's how a frame manages its children by default.

@; ----------------------------------------------------------------------
@section{Where to Go From Here}

This introduction to Racket purposely avoids many of the
traditional ways of introducing and distinguishing Lisp or Racket:
prefix arithmetic notation, symbols, quoting and quasiquoting lists,
@scheme[eval], first-class continuations, and the idea that all syntax
is really just a @scheme[lambda] in disguise. While those are all part
of Racket, they are not the main ingredients of day-to-day programming
in Racket.

Instead, Racket programmers typically program with functions,
records, objects, exceptions, regular expressions, modules, and
threads. That is, instead of a ``minimalist'' language---which is the
way that Racket is often described---Racket offers a rich language
with an extensive set of libraries and tools.

If you are new to programming or if you have the patience to work
through a textbook, we recommend reading
@italic{@link["http://www.htdp.org/"]{How to Design Programs}}. If you
have already read it, or if you want to see where the book will take
you, then see @other-manual['(lib
"web-server/scribblings/tutorial/continue.scrbl")].

For experienced programmers, to continue touring Racket from a
systems-oriented perspective instead of pictures, your next stop is
@other-manual['(lib "scribblings/more/more.scrbl")].

To instead start learning about the full Racket language and tools
in depth, move on to @other-manual['(lib "guide.scrbl"
"scribblings/guide")].
