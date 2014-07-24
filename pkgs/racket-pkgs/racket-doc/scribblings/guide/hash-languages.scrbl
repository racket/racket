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
path. Instead, a @racket[_language] is suffixed with
@racketidfont{/lang/reader} to obtain a module path, and the resulting
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
Specifically, move @filepath{literal.rkt} to
@filepath{literal/lang/reader.rkt} for any directory name
@filepath{literal}. Then, install the @filepath{literal}
directory as a package.

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

For security reasons, only languages that have been specifically
installed by a user can respond to language-information queries.  If
you have installed the @racket[literal] language as described in
@secref["language-collection"], then you can adjust
@filepath{literal/lang/reader.rkt} so that DrRacket treats the content
of a module in the @racket[literal] language as plain text instead of
(erroneously) as Racket syntax:

@racketmod[
#:file "literal/lang/reader.rkt"
racket
(require syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax])
         get-info)

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([str (port->string in)])
    (strip-context
     #'(module anything racket
         (provide data)
         (define data (quote str))))))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/default-lexer 
                        'default-lexer)]
      [else default])))
]

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

Unlike the syntax-coloring property of a language (as described in
@secref["language-get-info"]), the result-value format is a property of a
@emph{module} (via its language) as opposed to a property of the
module's @emph{source text}. That is, the run-time configuration for a
module should be available even if the module is compiled
to bytecode form and the source is unavailable. Due to this difference,
language properties such as run-time configuration are not reported
via a @racketidfont{get-info} function that exported from the language's
parser module, but instead through a separate module whose name is
attached to the syntax object for a parsed @racket[module] form.

Going back to the @racket[literal] language (see
@secref["language-get-info"]), we can adjust the language so that
directly running a @racket[literal] module causes it to print out its
string, while using a @racket[literal] module in a larger program
simply provides @racketidfont{data} without printing. To make this
work, we will need three extra module files:

@racketblock[
.... @#,elem{(the main installation or the user's space)}
 !- @#,filepath{collects}
      !- @#,filepath{literal}
           !- @#,filepath{lang}
           !    !- @#,filepath{reader.rkt}
           !- @#,filepath{language-info.rkt}   @#,elem{(new)}
           !- @#,filepath{runtime-config.rkt}  @#,elem{(new)}
           !- @#,filepath{show.rkt}            @#,elem{(new)}
]

@itemlist[

 @item{The @filepath{literal/language-info.rkt} module provides
       reflective information about the language of modules written in
       the @racket[literal] language. The name of this module is not
       special; it will be connected to the @racket[literal] language
       through a change to @filepath{literal/lang/reader.rkt}.}

 @item{The @filepath{literal/runtime-config.rkt} module will be
       identified by @filepath{literal/language-info.rkt} as the
       run-time configuration code for a main module that uses the
       @racket[literal] language.}

 @item{The @filepath{literal/show.rkt} module will provide a
       @racketidfont{show} function to be applied to the string
       content of a @racket[literal] module. The run-time
       configuration action in @filepath{literal/runtime-config.rkt}
       will instruct @racketidfont{show} to print the strings that it
       is given, but only when a module using the @racket[literal]
       language is run directly.}

]

Multiple modules are needed to implement the printing change, because
the different modules must run at different times. For example, the
code needed to parse a @racket[literal] module is not needed after the
module has been compiled, while the run-time configuration code is
needed only when the module is run as the main module of a
program. Similarly, when creating a stand-alone executable with
@exec{raco exe}, the main module (in compiled form) must be queried
for its run-time configuration, but the module and its configuration
action should not run until the executable is started. By using
different modules for these different tasks, we avoid loading code at
times when it is not needed.

The three new files are connected to the @racket[literal] language by
changes to @filepath{literal/lang/reader.rkt}:

@itemlist[

 @item{The @racket[module] form generated by the
       @racketidfont{read-syntax} function must import the
       @racket[literal/show] module and call its @racketidfont{show}
       function.}

 @item{The @racket[module] form must be annotated with a
       @racket['language-info] syntax property, whose value points to
       a @racketidfont{get-language-info} function exported by a
       @racket[literal/language-info] module. The
       @racketidfont{get-language-info} function will be responsible
       for reporting the @racket[literal/runtime-config] as the
       run-time configuration action of the language.

       The @racket['language-info] syntax property value is a vector
       that contains a module (in this case
       @racket[literal/language-info]), a symbol for one of the
       module's exports (@racketidfont{get-language-info} in this
       case), and an data value (which is not needed in this
       case). The data component allows information to be propagated
       from the source to the module's language information.}

]

These changes are implemented in the following revised
@filepath{literal/lang/reader.rkt}:

@racketmod[
#:file "literal/lang/reader.rkt"
racket
(require syntax/strip-context)

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax])
         get-info)

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define (literal-read-syntax src in)
  (with-syntax ([str (port->string in)])
    (syntax-property
     (strip-context
      #'(module anything racket
          (require literal/show)
          (provide data)
          (define data (quote str))
          (show data)))
     'module-language
     '#(literal/language-info get-language-info #f))))

(define (get-info in mod line col pos)
  (lambda (key default)
    (case key
      [(color-lexer)
       (dynamic-require 'syntax-color/default-lexer 
                        'default-lexer)]
      [else default])))
]

When a @racket[module] form with a @racket['module-language] property
is compiled, the property value is preserved with the compiled module,
and it is accessible via reflective functions like
@racket[module->language-info]. When @exec{racket} or DrRacket runs a
module, it uses @racket[module->language-info] to obtain a vector that
contains a module name, export name, and data value. The result of the
function applied to the data should be another function that answers
queries, much like the @racketidfont{get-info} function in a language
reader.

For @racket[literal], @filepath{literal/language-info.rkt} is
implemented as:

@racketmod[
#:file "literal/language-info.rkt"
racket

(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#(literal/runtime-config configure #f))]
      [else default])))
]

The function returned by @racketidfont{get-language-info} answers a
@racket['configure-runtime] query with a list of yet more vectors,
where each vector contains a module name, an exported name, and a data
value. For the @racket[literal] language, the run-time configuration
action implemented in @filepath{literal/runtime-config.rkt} is to
enable printing of strings that are sent to @racketidfont{show}:

@racketmod[
#:file "literal/runtime-config.rkt"
racket
(require "show.rkt")

(provide configure)

(define (configure data)
  (show-enabled #t))
]

Finally, the @filepath{literal/show.rkt} module must provide
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

When using @racketmodname[syntax/module-reader] to implement a
language, specify a module's language information through the
@racket[#:language-info] optional specification. The value provided
through @racket[#:language-info] is attached to a @racket[module] form
directly as a syntax property.
