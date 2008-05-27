#lang scribble/doc
@(require scribble/manual
          scribble/eval
          scribble/bnf
          "guide-utils.ss")

@(define piece-eval (make-base-eval))

@title[#:tag "intro"]{Welcome to PLT Scheme}

Depending on how you look at it, @bold{PLT Scheme} is

@itemize{

 @item{a @defterm{programming language}---a descendant of Scheme, which
       is a dialect of Lisp;}

 @item{a @defterm{family} of programming languages---variants of
       Scheme, and more; or}

 @item{a set of @defterm{tools}---for using a family of programming languages.}
}

Where there is no room for confusion, we use simply @defterm{Scheme}
to refer to any of these facets of PLT Scheme.

PLT Scheme's two main tools are

@itemize{

 @tool["MzScheme"]{the core compiler, interpreter, and run-time
 system; and}

 @tool["DrScheme"]{the programming environment (which runs on top of
 MzScheme).}

}

Most likely, you'll want to explore PLT Scheme using DrScheme,
especially at the beginning. If you prefer, you can also work with the
command-line @exec{mzscheme} interpreter and your favorite text
editor. The rest of this guide presents the language mostly
independent of your choice of editor.

If you're using DrScheme, you'll need to choose the proper language,
because DrScheme accommodates many different variants of
Scheme. Assuming that you've never used DrScheme before, start it up,
type the line

@schememod[scheme]

in DrScheme's top text area, and then click the @onscreen{Run} button
that's above the text area. DrScheme then understands that you mean to
work in the normal variant of Scheme (as opposed to the smaller
@schememodname[scheme/base], or many other possibilities).

If you've used DrScheme before with something other than a program
that starts @hash-lang[], DrScheme will remember the last language
that you used, instead of inferring the language from the @hash-lang[]
line. In that case, use the @menuitem["Language" "Choose Language..."]
menu item.  In the the dialog that appears, select the first item,
which is @onscreen{Module}. Put the @hash-lang[] line above in the top
text area, still.

@; ----------------------------------------------------------------------
@section{Interacting with Scheme}

DrScheme's bottom text area and the @exec{mzscheme} command-line
program (when started with no options) both act as a kind of
calculator. You type a Scheme expression, hit return, and the answer
is printed. In the terminology of Scheme, this kind of calculator is
called a @idefterm{read-eval-print loop} or @deftech{REPL}.

A number by itself is an expression, and the answer is just the
number:

@interaction[5]

A string is also an expression that evaluates to itself. A string is
written with double quotes at the start and end of the string:

@interaction["hello world"]

Scheme uses parentheses to wrap larger expressions---almost any kind
of expression, other than simple constants. For example, a function
call is written: open parenthesis, function name, argument
expression, and closing parenthesis. The following expression calls
the built-in function @scheme[substring] with the arguments
@scheme["hello world"], @scheme[0], and @scheme[5]:

@interaction[(substring "hello world" 0 5)]

@; ----------------------------------------------------------------------
@section{Definitions and Interactions}

You can define your own functions that work like @scheme[substring] by
using the @scheme[define] form, like this:

@def+int[
#:eval piece-eval
(define (piece str)
  (substring str 0 5))
(piece "howdy universe")
]

Although you can evaluate the @scheme[define] form in the @tech{REPL},
definitions are normally a part of a program that you want to keep and
use later. So, in DrScheme, you'd normally put the definition in the
top text area---called the @deftech{definitions area}---along with the
@hash-lang[] prefix:

@schememod[
scheme
code:blank
(define (piece str)
  (substring str 0 5))
]

If calling @scheme[(piece "howdy universe")] is part of the main
action of your program, that would go in the @tech{definitions area},
too. But if it was just an example expression that you were using to
explore @scheme[piece], then you'd more likely leave the
@tech{definitions area} as above, click @onscreen{Run}, and then
evaluate @scheme[(piece "howdy universe")] in the @tech{REPL}.

With @exec{mzscheme}, you'd save the above text in a file using your
favorite editor. If you save it as @filepath{piece.ss}, then after starting
@exec{mzscheme} in the same directory, you'd evaluate the following
sequence:

@interaction[
#:eval piece-eval
(eval:alts (enter! "piece.ss") (void))
(piece "howdy universe")
]

The @scheme[enter!] function both loads the code and switches the
evaluation context to the inside of the module, just like DrScheme's
@onscreen{Run} button.

@; ----------------------------------------------------------------------
@section{Creating Executables}

If your file (or @tech{definitions area} in DrScheme) contains

@schememod[
scheme

(define (piece str)
  (substring str 0 5))

(piece "howdy universe")
]

then it is a complete program that prints ``howdy'' when run. To
package this program as an executable, choose one of the following
options:

@itemize[

 @item{In DrScheme, you can select the @menuitem["Scheme" "Create
       Executable..."] menu item.}

 @item{From a command-line prompt, run @exec{mzc --exe
       @nonterm{dest-filename} @nonterm{src-filename}}, where
       @nonterm{src-filename} contains the program. See @secref[#:doc
       '(lib "scribblings/mzc/mzc.scrbl") "exe"] for more
       information.}

 @item{With Unix or Mac OS X, you can turn the program file into an
       executable script by inserting the line

       @margin-note{See @secref["scripts"] for more information on
                    script files.}

        @verbatim[#:indent 2]{#! /usr/bin/env mzscheme}

       at the very beginning of the file. Also, change the file
       permissions to executable using @exec{chmod +x
       @nonterm{filename}} on the command line.

       The script works as long as @exec{mzscheme} is in the user's
       executable search path.  Alternately, use a full path to
       @exec{mzscheme} after @tt{#!}  (with a space between @tt{#!}
       and the path), in which case the user's executable search path
       does not matter.}

]

@; ----------------------------------------------------------------------
@section[#:tag "use-module"]{A Note to Readers with Scheme/Lisp Experience}

If you already know something about Scheme or Lisp, you might be
tempted to put just

@schemeblock[
(define (piece str)
  (substring str 0 5))
]

into @filepath{piece.ss} and run @exec{mzscheme} with

@interaction[
#:eval piece-eval
(eval:alts (load "piece.ss") (void))
(piece "howdy universe")
]

That will work, because @exec{mzscheme} is willing to imitate a
traditional Scheme environment, but we strongly recommend against using
@scheme[load] or writing programs outside of a module.

Writing definitions outside of a module leads to bad error messages,
bad performance, and awkward scripting to combine and run
programs. The problems are not specific to @exec{mzscheme}; they're
fundamental limitations of the traditional top-level environment,
which Scheme and Lisp implementations have historically fought with ad
hoc command-line flags, compiler directives, and build tools. The
module system is to designed to avoid the problems, so start with
@hash-lang[], and you'll be happier with PLT Scheme in the long run.

@; ----------------------------------------------------------------------

@close-eval[piece-eval]
