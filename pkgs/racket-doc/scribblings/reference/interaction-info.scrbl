#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "interaction-info"]{Interaction Configuration}

@note-lib-only[racket/interaction-info]

The @racketmodname[racket/interaction-info] library provides a way to
register a langauge's configuration for a
@racket[read]-@racket[eval]-@racket[print] loop and editor.

@defparam[current-interaction-info info (or/c #f (vector/c module-path? symbol? any/c))]{

A @tech{parameter} that provides configuration for a language for use
by interactive development tools, such as a command-line evaluation
prompt with syntax coloring and indentation support. This parameter is
typically set by a @racketidfont{configure-runtime} module; see also
@secref["configure-runtime"].

Instead of providing configuration information directly, the the
@racket[current-interaction-info] parameter specifies a module to
load, a exported function to call, and data to pass as an argument to
the exported function. The result of that function should be another
one that accepts two arguments: a symbol a symbol indicating the kind
of information requested (as defined by external tools), and a default
value that normally should be returned if the symbol is not
recognized.

For information on defining a new @hash-lang[] language, see
@racketmodname[syntax/module-reader].

@history[#:added "8.3.0.2"]}
