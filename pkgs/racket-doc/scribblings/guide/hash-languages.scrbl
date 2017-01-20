#lang scribble/doc
@(require scribble/manual scribble/eval scribble/racket
          "guide-utils.rkt" "modfile.rkt"
          (for-syntax racket/base)
          (for-label setup/dirs
                     syntax/strip-context
                     syntax-color/default-lexer))

@(define-syntax ! (make-element-id-transformer (lambda (v) #'@tt{|})))
@(define-syntax !- (make-element-id-transformer (lambda (v) #'@tt{|-})))


@title[#:tag "hash-languages" #:style 'toc]{Defining new @hash-lang[] Languages}

When loading a module as a source program that starts

@racketmod[
@#,racket[_language]
]

the @racket[_language] determines the way that the rest of the module
is parsed at the @tech{reader} level. The @tech{reader}-level parse
must produce a @racket[module] form as a @tech{syntax object}. As
always, the second sub-form after @racket[module] specifies the
@tech{module language} that controls the meaning of the module's body
forms. Thus, a @racket[_language] specified after @hash-lang[]
controls both the @tech{reader}-level and @tech{expander}-level
parsing of a module.

@local-table-of-contents[]

@; ----------------------------------------
@section[#:tag "hash-lang syntax"]{Designating a @hash-lang[] Language}

The syntax of a @racket[_language] intentionally overlaps with the
syntax of a module path as used in @racket[require] or as a
@tech{module language}, so that names like @racketmodname[racket],
@racketmodname[racket/base], @racketmodname[slideshow], or
@racketmodname[scribble/manual] can be used both as @hash-lang[]
languages and as module paths.

At the same time, the syntax of @racket[_language] is far more
restricted than a module path, because only @litchar{a}-@litchar{z},
@litchar{A}-@litchar{Z}, @litchar{0}-@litchar{9},
@litchar{/} (not at the start or end),
@litchar{_}, @litchar{-}, and @litchar{+} are allowed in a
@racket[_language] name. These restrictions keep the syntax of
@hash-lang[] as simple as possible. Keeping the syntax of @hash-lang[]
simple, in turn, is important because the syntax is inherently
inflexible and non-extensible; the @hash-lang[] protocol allows a
@racket[_language] to refine and define syntax in a practically
unconstrained way, but the @hash-lang[] protocol itself must remain
fixed so that various different tools can ``boot'' into the extended
world.

Fortunately, the @hash-lang[] protocol provides a natural way to refer
to languages in ways other than the rigid @racket[_language] syntax:
by defining a @racket[_language] that implements its own nested
protocol. We have already seen one example (in @secref["s-exp"]): the
@racketmodname[s-exp] @racket[_language] allows a programmer to
specify a @tech{module language} using the general @tech{module path}
syntax. Meanwhile, @racketmodname[s-exp] takes care of the
@tech{reader}-level responsibilities of a @hash-lang[] language.

Unlike @racketmodname[racket], @racketmodname[s-exp] cannot be used as a
module path with @racket[require]. Although the syntax of
@racket[_language] for @hash-lang[] overlaps with the syntax of module
paths, a @racket[_language] is not used directly as a module
path. Instead, a @racket[_language] obtains a module path by trying two 
locations:  first, it looks for a @racketidfont{reader} submodule of the 
main module for  @racket[_language]. If this is not a valid module path, 
then @racket[_language]  is suffixed with @racketidfont{/lang/reader}. 
(If neither is a valid module path, an error is raised.) The resulting 
module supplies @racketidfont{read} and @racketidfont{read-syntax}
functions using a protocol that is similar to the one for
@racketmetafont{#reader}.

@guideother{@secref["hash-reader"] introduces @racketmetafont{#reader}.}

A consequence of the way that a @hash-lang[] @racket[_language] is
turned into a module path is that the language must be installed in a
@tech{collection}, similar to the way that @filepath{racket} or
@filepath{slideshow} are collections that are distributed with Racket.
Again, however, there's an escape from this restriction: the
@racketmodname[reader] language lets you specify a @tech{reader}-level
implementation of a language using a general @tech{module path}.

@; ----------------------------------------
@section[#:tag "hash-lang reader"]{Using @racket[@#,hash-lang[] @#,racketmodname[reader]]}

The @racketmodname[reader] language for @hash-lang[] is similar to
@racketmodname[s-exp], in that it acts as a kind of meta-language.
Whereas @racketmodname[s-exp] lets a programmer specify a @tech{module
language} at the @tech{expander} layer of parsing,
@racketmodname[reader] lets a programmer specify a language at the
@tech{reader} level.

A @racket[@#,hash-lang[] @#,racketmodname[reader]] must be followed by
a module path, and the specified module must provide two functions:
@racketidfont{read} and @racketidfont{read-syntax}. The protocol is
the same as for a @racketmetafont{#reader} implementation, but for
@hash-lang[], the @racketidfont{read} and @racketidfont{read-syntax}
functions must produce a @racket[module] form that is based on the
rest of the input file for the module.

The following @filepath{literal.rkt} module implements a language that
treats its entire body as literal text and exports the text as a
@racketidfont{data} string:

@racketmodfile["literal.rkt"]

The @filepath{literal.rkt} language uses @racket[strip-context] on the
generated @racket[module] expression, because a
@racketidfont{read-syntax} function should return a syntax object with
no lexical context. Also, the @filepath{literal.rkt} language creates
a module named @racketidfont{anything}, which is an arbitrary choice;
the language is intended to be used in a file, and the longhand module
name is ignored when it appears in a @racket[require]d file.

The @filepath{literal.rkt} language can be used in a module
@filepath{tuvalu.rkt}:

@racketmodfile["tuvalu.rkt"]

Importing @filepath{tuvalu.rkt} binds @racketidfont{data} to a
string version of the module content:

@interaction[
(require "tuvalu.rkt")
data
]

@; ----------------------------------------
@section[#:tag "syntax/module-reader"]{Using @racket[@#,hash-lang[] @#,racketmodname[s-exp] @#,racketmodname[syntax/module-reader]]}

Parsing a module body is usually not as trivial as in
@filepath{literal.rkt}. A more typical module parser must iterate to
parse multiple forms for a module body. A language is also more likely
to extend Racket syntax---perhaps through a @tech{readtable}---instead
of replacing Racket syntax completely.

The @racketmodname[syntax/module-reader] @tech{module language}
abstracts over common parts of a language implementation to simplify
the creation of new languages. In its most basic form, a language
implemented with @racketmodname[syntax/module-reader] simply specifies
the @tech{module language} to be used for the language, in which case
the @tech{reader} layer of the language is the same as Racket. For
example, with

@racketmod[
#:file "raquet-mlang.rkt"
racket
(provide (except-out (all-from-out racket) lambda)
         (rename-out [lambda function]))
]

and

@racketmod[
#:file "raquet.rkt"
s-exp syntax/module-reader
"raquet-mlang.rkt"
]

then

@racketmod[
reader "raquet.rkt"
(define identity (function (x) x))
(provide identity)
]

implements and exports the @racket[identity] function, since
@filepath{raquet-mlang.rkt} exports @racket[lambda] as
@racket[function].

The @racketmodname[syntax/module-reader] language accepts many optional
specifications to adjust other features of the language. For example,
an alternate @racketidfont{read} and @racketidfont{read-syntax} for
parsing the language can be specified with @racket[#:read] and
@racket[#:read-syntax], respectively. The following
@filepath{dollar-racket.rkt} language uses @filepath{dollar.rkt} (see
@secref["readtable"]) to build a language that is like
@racketmodname[racket] but with a @litchar{$} escape to simple infix
arithmetic:

@racketmodfile["dollar-racket.rkt"]

The @racket[require] form appears at the end of the module,
because all of the keyword-tagged optional specifications for
@racketmodname[syntax/module-reader] must appear before any helper
imports or definitions.

The following module uses @filepath{dollar-racket.rkt} to implement a
@racket[cost] function using a @litchar{$} escape:

@racketmodfile["store.rkt"]

@; ----------------------------------------
@section[#:tag "language-collection"]{Installing a Language}

So far, we have used the @racketmodname[reader] meta-language to
access languages like @filepath{literal.rkt} and
@filepath{dollar-racket.rkt}. If you want to use something like
@racket[@#,hash-lang[] literal] directly, then you must move
@filepath{literal.rkt} into a Racket @tech{collection} named
@filepath{literal} (see also @secref["link-collection"]).
Specifically, move @filepath{literal.rkt} to a @racketidfont{reader} 
submodule of @filepath{literal/main.rkt} for any directory name
@filepath{literal}, like so:

@racketmodfile["literal-main.rkt" "literal/main.rkt"]

Then, install the @filepath{literal}
directory as a package:

@commandline{cd /path/to/literal ; raco pkg install}

After moving the file and installing the package, you can use
@racket[literal] directly after @hash-lang[]:

@racketmod[
@#,racket[literal]
Technology!
System!
Perfect!
]

@margin-note{See @other-manual['(lib "scribblings/raco/raco.scrbl")]
for more information on using @exec{raco}.}

You can also make your language available for others to install by
using the Racket package manager (see @other-doc['(lib
"pkg/scribblings/pkg.scrbl")]). After you create a @filepath{literal}
package and register it with the Racket package catalog (see
@secref["concept:catalog" #:doc '(lib "pkg/scribblings/pkg.scrbl")]),
others can install it using @exec{raco pkg}:

@commandline{raco pkg install literal}

Once installed, others can invoke the language the same way: by using
@racket[@#,hash-lang[] literal] at the top of a source file.

If you use a public source repository (e.g., GitHub), you can link
your package to the source. As you improve the package, others can
update their version using @exec{raco pkg}:

@commandline{raco pkg update literal}

@margin-note{See @other-doc['(lib "pkg/scribblings/pkg.scrbl")] for more
information about the Racket package manager.}

@; ----------------------------------------
@section[#:tag "language-get-info"]{Source-Handling Configuration}

The Racket distribution includes a Scribble language for writing prose
documents, where Scribble extends the normal Racket to better support
text. Here is an example Scribble document:

@verbatim[#:indent 2]|{
#lang scribble/base

@(define (get-name) "Self-Describing Document")

@title[(get-name)]

The title of this document is ``@(get-name).''
}|

If you put that program in DrRacket's @tech{definitions area} and
click @onscreen{Run}, then nothing much appears to happen. The
@racketmodname[scribble/base] language just binds and exports
@racketidfont{doc} as a description of a document, similar to the way
that @filepath{literal.rkt} exports a string as @racketidfont{data}.

Simply opening a module with the language
@racketmodname[scribble/base] in DrRacket, however, causes a
@onscreen{Scribble HTML} button to appear. Furthermore, DrRacket knows
how to colorize Scribble syntax by coloring green those parts of the
document that correspond to literal text. The language name
@racketmodname[scribble/base] is not hard-wired into
DrRacket. Instead, the implementation of the
@racketmodname[scribble/base] language provides button and
syntax-coloring information in response to a query from DrRacket.

If you have installed the @racket[literal] language as described in
@secref["language-collection"], then you can adjust
@filepath{literal/main.rkt} so that DrRacket treats the content
of a module in the @racket[literal] language as plain text instead of
(erroneously) as Racket syntax:

@racketmodfile["literal-main-get-info.rkt" "literal/main.rkt"]

This revised @racket[literal] implementation provides a
@racketidfont{get-info} function. The @racketidfont{get-info} function
is called by @racket[read-language] (which DrRacket calls) with the 
source input stream and location information,
in case query results should depend on the content of the module after
the language name (which is not the case for @racket[literal]). The
result of @racketidfont{get-info} is a function of two arguments. The
first argument is always a symbol, indicating the kind of information
that a tool requests from the language; the second argument is the
default result to be returned if the language does not recognize the
query or has no information for it.

After DrRacket obtains the result of @racketidfont{get-info} for a
language, it calls the function with a @racket['color-lexer] query;
the result should be a function that implements syntax-coloring
parsing on an input stream. For @racket[literal], the
@racketmodname[syntax-color/default-lexer] module provides a
@racket[default-lexer] syntax-coloring parser that is suitable for
plain text, so @racket[literal] loads and returns that parser in
response to a @racket['color-lexer] query.

The set of symbols that a programming tool uses for queries
is entirely between the tool and the languages that choose to
cooperate with it. For example, in addition to @racket['color-lexer],
DrRacket uses a @racket['drracket:toolbar-buttons] query to determine
which buttons should be available in the toolbar to operate on modules
using the language.

The @racketmodname[syntax/module-reader] language lets you specify
@racketidfont{get-info} handling through a @racket[#:info] optional
specification. The protocol for an @racket[#:info] function is
slightly different from the raw @racketidfont{get-info} protocol; the
revised protocol allows @racketmodname[syntax/module-reader] the
possibility of handling future language-information queries
automatically.

@; ----------------------------------------
@section[#:tag "module-runtime-config"]{Module-Handling Configuration}

Suppose that the file @filepath{death-list-5.rkt} contains

@racketmodfile["death-list-5.rkt"]

If you @racket[require] @filepath{death-list-5.rkt} directly, then it
prints the list in the usual Racket result format:

@interaction[
(require "death-list-5.rkt")
]

However, if @filepath{death-list-5.rkt} is required by a
@filepath{kiddo.rkt} that is implemented with @racketmodname[scheme]
instead of @racketmodname[racket]:

@racketmodfile["kiddo.rkt"]

then, if you run @filepath{kiddo.rkt} file in DrRacket or if you run it
directly with @exec{racket}, @filepath{kiddo.rkt} causes
@filepath{death-list-5.rkt} to print its list in traditional Scheme
format, without the leading quote:

@racketblock[
@#,racketoutput{("O-Ren Ishii" "Vernita Green" "Budd" "Elle Driver" "Bill")}
]

The @filepath{kiddo.rkt} example illustrates how the format for
printing a result value can depend on the main module of a program
instead of the language that is used to implement it.

More broadly, certain features of a language are only invoked when
a module written in that language is run directly with @exec{racket}
(as opposed to being imported into another module). One example is
result-printing style (as shown above). Another example is REPL
behavior. These features are part of what's called the
@deftech{run-time configuration} of a language.

Unlike the syntax-coloring property of a language (as described in
@secref["language-get-info"]), the run-time configuration is a
property of a @emph{module} per se as opposed to a property of
the @emph{source text} representing the module.
For that reason, the run-time configuration for a
module needs to be available even if the module is compiled
to bytecode form and the source is unavailable. Therefore,
run-time configuration cannot be handled by the
@racketidfont{get-info} function we're exporting from the language's
parser module.

Instead, it will be handled by a new
@racket[configure-runtime] submodule that we'll add inside
the parsed @racket[module] form. When a module is run directly
with @exec{racket}, @exec{racket} looks for a
@racket[configure-runtime] submodule. If it exists, @exec{racket}
runs it. But if the module is imported into another module,
the @racket['configure-runtime] submodule is ignored. (And if the
@racket[configure-runtime] submodule doesn't exist, @exec{racket}
just evaluates the module as usual.) That means that the
@racket[configure-runtime] submodule can be used for any special
setup tasks that need to happen when the module is run directly.

Going back to the @racket[literal] language (see
@secref["language-get-info"]), we can adjust the language so that
directly running a @racket[literal] module causes it to print out its
string, while using a @racket[literal] module in a larger program
simply provides @racketidfont{data} without printing. To make this
work, we will need an extra module. (For clarity here, we will implement 
this module as a separate file. But it could equally well be 
a submodule of an existing file.)

@racketblock[
.... @#,elem{(the main installation or the user's space)}
!- @#,filepath{literal}
   !- @#,filepath{main.rkt}            @#,elem{(with reader submodule)}
   !- @#,filepath{show.rkt}            @#,elem{(new)}
]

@itemlist[

 @item{The @filepath{literal/show.rkt} module will provide a
       @racketidfont{show} function to be applied to the string
       content of a @racket[literal] module, and also provide a
       @racketidfont{show-enabled} parameter that controls whether
       @racketidfont{show} actually prints the result.}

 @item{The new @racket[configure-runtime] submodule in
       @filepath{literal/main.rkt} will set the
       @racketidfont{show-enabled} parameter to @racket[#t]. The
       net effect is that @racketidfont{show} will print the strings
       that it's given, but only when a module using the @racket[literal]
       language is run directly (because only then will the
       @racket[configure-runtime] submodule be invoked).}

]


These changes are implemented in the following revised
@filepath{literal/main.rkt}:

@racketmodfile["literal-main-language-info.rkt" "literal/main.rkt"]

Then the @filepath{literal/show.rkt} module must provide
the @racketidfont{show-enabled} parameter and @racketidfont{show}
function:

@racketmod[
#:file "literal/show.rkt"
racket

(provide show show-enabled)

(define show-enabled (make-parameter #f))

(define (show v)
  (when (show-enabled)
    (display v)))
]

With all of the pieces for @racket[literal] in place, try running the
following variant of @filepath{tuvalu.rkt} directly and through a
@racket[require] from another module:

@racketmod[
#:file "tuvalu.rkt"
@#,racket[literal]
Technology!
System!
Perfect!
]

When run directly, we'll see the result printed like so, because
our @racket[configure-runtime] submodule will have set the
@racketidfont{show-enabled} parameter to @racket[#t]:

@racketblock[
@#,racketoutput{Technology!
@(linebreak)System!
@(linebreak)Perfect!}
]

But when imported into another module, printing will be suppressed,
because the @racket[configure-runtime] submodule will not be invoked,
and therefore the @racketidfont{show-enabled} parameter will remain
at its default value of @racket[#f].
