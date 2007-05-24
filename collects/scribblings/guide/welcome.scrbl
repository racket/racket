#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "intro"]{Welcome to PLT Scheme}

Depending on how you look at it, @bold{PLT Scheme} is

@itemize{

 @item{a @defterm{programming language}---a dialect of Scheme, which
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
independent of the tool that you use.

If you read @Quick and you're still using DrScheme, you already know
about interactions and definitions. You can safely skip to
@secref["indentation"], with the caveat that we call the interactions
window the @defterm{REPL} in this guide.

Otherwise, if you're using DrScheme, you'll need to set it in the
right mode, because DrScheme is less biased to a particular Scheme
variant than @exec{mzscheme}. Assuming that you've never used DrScheme
before, start it up, type the line

@schememod[big]

in DrScheme's top text area, and then click the @onscreen{Run} button
that's above the text area. DrScheme then understands that you mean to
work in the @schememodname[big] variant of Scheme (as opposed to
@schememodname[little], @schememodname[medium], or many other
possibilities).

If you've used DrScheme before with something other than a program
that starts @schemefont{#module}, DrScheme will remember the last language
that you used, instead of inferring the language from the
@schemefont{#module} line. In that case, use the @menuitem["Language"
"Choose Language..."]  menu item.  In the the dialog that appears,
select the first item, which is @onscreen{Module}. Put the
@schemefont{#module} line above in the top text area, still.

@; ----------------------------------------------------------------------
@section{Interacting with Scheme}

DrScheme's bottom text area and the @exec{mzscheme} command-line
program (when started with no options) both act as a kind of
calculator. You type a Scheme expression, hit return, and the answer
is printed. In the terminology of Scheme, this kind of calculator is
called a @idefterm{read-eval-print loop} or @idefterm{REPL}.

A number by itself is an expression, and the answer is just the
number:

@interaction[5]

A string is also an expression that evaluates to itself. A string is
written with double quotes at the start and end of the string:

@interaction["hello world"]

Scheme uses parentheses to wrap larger expressions---almost any kind
of expression, other than simple constants. For example, a procedure
call is written: open parenthesis, procedure name, argument
expression, and closing parenthesis. The following expression calls
the built-in procedure @scheme[substring] with the arguments
@scheme["hello world"], @scheme[0], and @scheme[5]:

@interaction[(substring "hello world" 0 5)]

@; ----------------------------------------------------------------------
@section{Definitions and Interactions}

You can define your own procedures that work like @scheme[subtring] by
using the @scheme[define] form, like this:

@def+int[
(define (piece str)
  (substring str 0 5))
(piece "howdy universe")
]

Although you can evaluate the @scheme[define] form in the REPL,
definitions are normally a part of a program that you want to keep and
use later. So, in DrScheme, you'd normally put the definition in the
top text area---called the @defterm{definitions area}---along with the
@schemefont{#module} prefix:

@schememod[
big
code:blank
(define (piece str)
  (substring str 0 5))
]

If calling @scheme[(piece "howdy universe")] is part of the main action
of your program, that would go in the definitions area, too. But if it
was just an example expression that you were using to explore
@scheme[piece], then you'd more likely leave the definitions area as
above, click @onscreen{Run}, and then evaluate
@scheme[(piece "howdy universe")]
in the REPL.

With @exec{mzscheme}, you'd save the above text in a file using your
favorite editor. If you save it as @file{piece.ss}, then after starting
@exec{mzscheme} in the same directory, you'd evaluate the following
sequence:

@interaction[
(eval:alts (enter! "piece.ss") (void))
(piece "howdy universe")
]

The @scheme[enter!] procedure both loads the code and switches the
evaluation context to the inside of the module, just like DrScheme's
@onscreen{Run} button.

@; ----------------------------------------------------------------------
@section{A Note to Readers with Scheme/Lisp Experience}

If you already know something about Scheme or Lisp, you might be
tempted to put just

@schemeblock[
(define (piece str)
  (substring str 0 5))
]

into @file{piece.ss} and run @exec{mzscheme} with

@interaction[
(eval:alts (load "piece.ss") (void))
(piece "howdy universe")
]

That will work, because @exec{mzscheme} is willing to imitate a
traditional Scheme environment, but we strongly recommend against using
@scheme[load].

Writing definitions outside of a module leads to bad error messages,
bad performance, and awkward scripting to combine and run
programs. The problems are not in @exec{mzscheme}'s implementation;
they're fundamental limitations of the traditional top-level
environment, which Scheme and Lisp implementations have historically
combated through ad hoc command-line flags, compiler directives, and
build tools. The module system is to designed to avoid the problems,
so start with @schemefont{#module}, and you'll be happier with Scheme
in the long run.

@; ----------------------------------------------------------------------
@section[#:tag "indentation"]{Scheme Program Formatting}

Line breaks and indentation are not significant for parsing Scheme
programs, but most Scheme programmer use a standard set of conventions
to make code more readable. For example, the body of a definition is
typically indented under the first line of the definition. Identifiers
are written immediately after an open parenthesis with no extra space,
and closing parentheses never go on their own line.

DrScheme automatically indents according to the standard style when
you type Enter in a program or REPL expression. For example, if you
hit Enter after typing @litchar{(define (piece str)}, then DrScheme
automatically inserts two spaces for the next line.  If you change a
region of code, you can select it in DrScheme and hit Tab, and
DrScheme will re-indent the code (without inserting any line breaks).
Editors like Emacs offer a Scheme mode with similar indentation
support.

Re-indenting not only makes the code easier to read, it gives you
extra feedback that your parentheses are matched in the way that you
intended. For example, if you leave out a closing parenthesis after
the last argument to a procedure, automatic indentation starts the
next line under the first argument, instead of under the
@scheme[define] keyword:

@schemeblock[
(define (piece str
               (substring str 0 5)))
]

Furthermore, when an open parenthesis has no matching close
parenthesis in a program, both @exec{mzscheme} and DrScheme use the
source's indentation information to suggest where it might be missing.
