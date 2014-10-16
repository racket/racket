#lang scribble/doc
@(require "mz.rkt")

@title{Reading}

@defproc[(read [in input-port? (current-input-port)]) any]{

Reads and returns a single @tech{datum} from @racket[in]. If
@racket[in] has a handler associated to it via
@racket[port-read-handler], then the handler is called. Otherwise, the
default reader is used, as parameterized by the
@racket[current-readtable] parameter, as well as many other
parameters.

See @secref["reader"] for information on the default reader.}

@defproc[(read-syntax [source-name any/c (object-name in)]
                      [in input-port? (current-input-port)])
         (or/c syntax? eof-object?)]{

Like @racket[read], but produces a @tech{syntax object} with
source-location information. The @racket[source-name] is used as the
source field of the syntax object; it can be an arbitrary value, but
it should generally be a path for the source file.

See @secref["reader"] for information on the default reader in
@racket[read-syntax] mode.}

@defproc[(read/recursive [in input-port? (current-input-port)]
                         [start (or/c char? #f) #f]
                         [readtable (or/c readtable? #f) (current-readtable)]
                         [graph? any/c #t])
          any]{

Similar to calling @racket[read], but normally used during the dynamic
extent of @racket[read] within a reader-extension procedure (see
@secref["reader-procs"]). The main effect of using
@racket[read/recursive] instead of @racket[read] is that
graph-structure annotations (see @secref["parse-graph"]) in the
nested read are considered part of the overall read, at least when the
@racket[graph?] argument is true; since the result is wrapped in a
placeholder, however, it is not directly inspectable.

If @racket[start] is provided and not @racket[#f], it is effectively
prefixed to the beginning of @racket[in]'s stream for the read. (To
prefix multiple characters, use @racket[input-port-append].)

The @racket[readtable] argument is used for top-level parsing to
satisfy the read request; recursive parsing within the read (e.g., to
read the elements of a list) instead uses the current readtable as
determined by the @racket[current-readtable] parameter.  A reader
macro might call @racket[read/recursive] with a character and
readtable to effectively invoke the readtable's behavior for the
character.  If @racket[readtable] is @racket[#f], the default
readtable is used for top-level parsing.

When @racket[graph?] is @racket[#f], graph structure annotations in
the read datum are local to the datum.

When called within the dynamic extent of @racket[read], the
@racket[read/recursive] procedure produces either an opaque
placeholder value, a special-comment value, or an end-of-file.  The
result is a special-comment value (see @secref["special-comments"])
when the input stream's first non-whitespace content parses as a
comment. The result is end-of-file when @racket[read/recursive]
encounters an end-of-file. Otherwise, the result is a placeholder that
protects graph references that are not yet resolved. When this
placeholder is returned within an S-expression that is produced by any
reader-extension procedure (see @secref["reader-procs"]) for the
same outermost @racket[read], it will be replaced with the actual read
value before the outermost @racket[read] returns.

See @secref["readtables"] for an extended example that uses
@racket[read/recursive].}

@defproc[(read-syntax/recursive [source-name any/c (object-name in)]
                                [in input-port? (current-input-port)]
                                [start (or/c char? #f) #f]
                                [readtable (or/c readtable? #f) (current-readtable)]
                                [graph? any/c #t])
          any]{

Analogous to calling @racket[read/recursive], but the resulting value
encapsulates S-expression structure with source-location
information. As with @racket[read/recursive], when
@racket[read-syntax/recursive] is used within the dynamic extent of
@racket[read-syntax], the result from
@racket[read-syntax/recursive] is either a special-comment value,
end-of-file, or opaque graph-structure placeholder (not a syntax
object). The placeholder can be embedded in an S-expression or syntax
object returned by a reader macro, etc., and it will be replaced with
the actual syntax object before the outermost @racket[read-syntax]
returns.

Using @racket[read/recursive] within the dynamic extent of
@racket[read-syntax] does not allow graph structure for reading to be
included in the outer @racket[read-syntax] parsing, and neither does
using @racket[read-syntax/recursive] within the dynamic extent of
@racket[read]. In those cases, @racket[read/recursive] and
@racket[read-syntax/recursive] produce results like @racket[read] and
@racket[read-syntax], except that a special-comment value is returned
when the input stream starts with a comment (after whitespace).

See @secref["readtables"] for an extended example that uses
@racket[read-syntax/recursive].}


@defproc[(read-language [in input-port? (current-input-port)]
                        [fail-thunk (-> any) (lambda () (error ...))])
         (or/c (any/c any/c . -> . any) #f)]{

Reads from @racket[in] in the same way as @racket[read], but stopping as
soon as a @tech{reader language} (or its absence) is determined.

A @deftech{reader language} is specified by @litchar{#lang} or
@litchar{#!} (see @secref["parse-reader"]) at the beginning of the
input, though possibly after comment forms. The default
@tech{readtable} is used by @racket[read-language] (instead of the
value of @racket[current-readtable]), and @litchar{#reader} forms
(which might produce comments) are not allowed before @litchar{#lang}
or @litchar{#!}.

@guidealso["language-get-info"]

When it finds a @litchar{#lang} or @litchar{#!} specification, instead
of dispatching to a @racketidfont{read} or @racketidfont{read-syntax}
function as @racket[read] and @racket[read-syntax] do,
@racket[read-language] dispatches to the @racketidfont{get-info}
function (if any) exported by the same module. The result of the
@racketidfont{get-info} function is the result of
@racket[read-language] if it is a function of two arguments; if
@racketidfont{get-info} produces any other kind of result, the
@exnraise[exn:fail:contract]. If no @racketidfont{get-info} function is
exported, @racket[read-language] returns @racket[#f].

The function produced by @racketidfont{get-info} reflects information
about the expected syntax of the input stream. The first argument to the
function serves as a key on such information; acceptable keys and the
interpretation of results is up to external tools, such as DrRacket (see
@seclink["_lang-based_Languages_in_DrRacket"
         #:doc '(lib "scribblings/tools/tools.scrbl")
         #:indirect? #t]{the DrRacket documentation}).
If no information is available for a given key, the result should be
the second argument.
@mz-examples[
((read-language (open-input-string "#lang algol60")) 'color-lexer #f)
((read-language (open-input-string "#lang algol60")) 'something-else #f)
]

The @racketidfont{get-info} function itself is applied to five
arguments: the input port being read, the module path from which the
@racketidfont{get-info} function was extracted, and the source line
(positive exact integer or @racket[#f]), column (non-negative exact
integer or @racket[#f]), and position (positive exact integer or
@racket[#f]) of the start of the @litchar{#lang} or @litchar{#!}
form. The @racketidfont{get-info} function may further read from the
given input port to determine its result, but it should read no
further than necessary. The @racketidfont{get-info} function should
not read from the port after returning a function.

If @racket[in] starts with a @tech{reader language} specification but
the relevant module does not export @racketidfont{get-info} (but
perhaps does export @racketidfont{read} and
@racketidfont{read-syntax}), then the result of @racket[read-language]
is @racket[#f].

If @racket[in] has a @litchar{#lang} or @litchar{#!} specification,
but parsing and resolving the specification raises an exception, the
exception is propagated by @racket[read-language]. Having at least
@litchar{#l} or @litchar{#!} (after comments and whitespace) counts as
starting a @litchar{#lang} or @litchar{#!} specification.

If @racket[in] does not specify a @tech{reader language} with
@litchar{#lang} or @litchar{#!}, then @racket[fail-thunk] is
called. The default @racket[fail-thunk] raises
@racket[exn:fail:read] or @racket[exn:fail:read:eof].}


@defboolparam[read-case-sensitive on?]{

A @tech{parameter} that controls parsing and printing of symbols. When this
parameter's value is @racket[#f], the reader case-folds symbols (e.g.,
producing @racket['hi] when the input is any one of @litchar{hi},
@litchar{Hi}, @litchar{HI}, or @litchar{hI}). The parameter also
affects the way that @racket[write] prints symbols containing
uppercase characters; if the parameter's value is @racket[#f], then
symbols are printed with uppercase characters quoted by a
@litchar{\} or @litchar{|}. The parameter's value is overridden by
quoting @litchar{\} or @litchar{|} vertical-bar quotes and the
@litchar{#cs} and @litchar{#ci} prefixes; see
@secref["parse-symbol"] for more information. While a module is
loaded, the parameter is set to @racket[#t] (see
@racket[current-load]).}

@defboolparam[read-square-bracket-as-paren on?]{

A @tech{parameter} that controls whether @litchar{[} and @litchar{]} 
are treated as parentheses. See @secref["parse-pair"] for more
information.}

@defboolparam[read-curly-brace-as-paren on?]{

A @tech{parameter} that controls whether @litchar["{"] and @litchar["}"] 
are treated as parentheses. See @secref["parse-pair"] for more
information.}

@defboolparam[read-accept-box on?]{

A @tech{parameter} that controls parsing @litchar{#&} input. See
@secref["parse-box"] for more information.}

@defboolparam[read-accept-compiled on?]{

A @tech{parameter} that controls parsing @litchar{#~} compiled input. See
@secref["reader"] and @racket[current-compile] for more
information.}

@defboolparam[read-accept-bar-quote on?]{

A @tech{parameter} that controls parsing and printing of @litchar{|} in
symbols. See @secref["parse-symbol"] and @secref["printing"] for
more information.}

@defboolparam[read-accept-graph on?]{

A parameter value that controls parsing input with sharing. See
@secref["parse-graph"] for more information.}

@defboolparam[read-decimal-as-inexact on?]{

A @tech{parameter} that controls parsing input numbers with a decimal point
or exponent (but no explicit exactness tag). See
@secref["parse-number"] for more information.}

@defboolparam[read-accept-dot on?]{

A @tech{parameter} that controls parsing input with a dot, which is normally
used for literal cons cells. See @secref["parse-pair"] for more
information.}

@defboolparam[read-accept-infix-dot on?]{

A @tech{parameter} that controls parsing input with two dots to trigger infix
 conversion. See @secref["parse-pair"] for more information.}

@defboolparam[read-accept-quasiquote on?]{

A @tech{parameter} that controls parsing input with @litchar{`} or
@litchar{,} which is normally used for @racket[quasiquote],
@racket[unquote], and @racket[unquote-splicing] abbreviations. See
@secref["parse-quote"] for more information.}

@defboolparam[read-accept-reader on?]{

A @tech{parameter} that controls whether @litchar{#reader}, @litchar{#lang},
or @litchar{#!} are allowed for selecting a parser. See
@secref["parse-reader"] for more information.}

@defboolparam[read-accept-lang on?]{

A @tech{parameter} that (along with @racket[read-accept-reader] controls
whether @litchar{#lang} and @litchar{#!} are allowed for selecting a
parser. See @secref["parse-reader"] for more information.}


@defparam[current-readtable readtable (or/c readtable? #f)]{

A parameter whose value determines a readtable that
adjusts the parsing of S-expression input, where @racket[#f] implies the
default behavior. See @secref["readtables"] for more information.}


@defproc[(call-with-default-reading-parameterization [thunk (-> any)])
         any]{

Calls @racket[thunk] in @tech{tail position} of a @racket[parameterize]
to set all reader @tech{parameters} above to their default values.

Using the default parameter values ensures consistency, and it also
provides safety when reading from untrusted sources, since the default
values disable evaluation of arbitrary code via @hash-lang[] or
@litchar{#reader}.}


@defparam[current-reader-guard proc (any/c . -> . any)]{

A parameter whose value converts or rejects (by raising an exception)
a module-path datum following @litchar{#reader}. See
@secref["parse-reader"] for more information.}

@defparam[read-on-demand-source mode (or/c #f #t (and/c path? complete-path?))]{

A @tech{parameter} that enables lazy parsing of compiled code, so that
closure bodies and syntax objects are extracted (and validated) from
marshaled compiled code on demand. Normally, this parameter is set by
the default @tech{load handler} when @racket[load-on-demand-enabled]
is @racket[#t].

A @racket[#f] value for @racket[read-on-demand-source] disables lazy
parsing of compiled code. A @racket[#t] value enables lazy parsing.  A
@tech{path} value furthers enable lazy retrieval from disk---instead
of keeping unparsed compiled code in memory---when the
@as-index{@envvar{PLT_DELAY_FROM_ZO}} environment variable is set (to
any value) on start-up.

If the file at @racket[mode] as a @tech{path} changes before the
delayed code is parsed when lazy retrieval from disk is enabled, then
the on-demand parse most likely will encounter garbage, leading to an
exception.}


@defproc*[([(port-read-handler [in input-port?]) (case->
                                                  (input-port? . -> . any)
                                                  (input-port?  any/c . -> . any))]
           [(port-read-handler [in input-port?]
                               [proc (case->
                                      (input-port? . -> . any)
                                      (input-port? any/c . -> . any))]) 
            void?])]{

Gets or sets the @deftech{port read handler} for @racket[in]. The
handler called to read from the port when the built-in @racket[read]
or @racket[read-syntax] procedure is applied to the port. (The
port read handler is not used for @racket[read/recursive] or
@racket[read-syntax/recursive].)

A port read handler is applied to either one argument or two
arguments:

@itemize[

 @item{A single argument is supplied when the port is used
 with @racket[read]; the argument is the port being read. The return
 value is the value that was read from the port (or end-of-file).}

 @item{Two arguments are supplied when the port is used with
 @racket[read-syntax]; the first argument is the port being read, and
 the second argument is a value indicating the source. The return
 value is a syntax object that was read from the port (or end-of-file).}

]

The default port read handler reads standard Racket expressions with
Racket's built-in parser (see @secref["reader"]). It handles a
special result from a custom input port (see
@racket[make-custom-input-port]) by treating it as a single expression,
except that special-comment values (see
@secref["special-comments"]) are treated as whitespace.

The default port read handler itself can be customized through a
readtable; see @secref["readtables"] for more information.}
