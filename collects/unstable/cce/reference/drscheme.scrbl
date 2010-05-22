#lang scribble/doc
@(require scribble/manual
          "../scribble.ss"
          (for-label scheme/gui
                     drscheme/tool-lib
                     unstable/cce/drscheme))

@title[#:style 'quiet #:tag "cce-drscheme"]{DrScheme Plugins}

@defmodule[unstable/cce/drscheme]

@defthing[language-level@ unit?]{

This unit imports @scheme[drscheme:tool^] and exports @scheme[language-level^].

}

@defsignature[language-level^ ()]{

@defproc[(make-language-level
          [name string?]
          [path module-path?]
          [mixin (-> class? class?)] ...
          [#:number number integer? ...]
          [#:hierarchy hierarchy (listof (cons/c string? integer?)) ...]
          [#:summary summary string? name]
          [#:url url (or/c string? #f) #f]
          [#:reader reader
                    (->* [] [any/c input-port?] (or/c syntax? eof-object?))
                    read-syntax])
         (object-provides/c drscheme:language:language<%>)]{

Constructs a language level as an instance of
@scheme[drscheme:language:language<%>] with the given @scheme[name] based on the
language defined by the module at @scheme[path].  Applies
@scheme[(drscheme:language:get-default-mixin)] and the given @scheme[mixin]s to
@sigelem[language-level^ simple-language-level%] to construct the class, and
uses the optional keyword arguments to fill in the language's description and
reader.

}

@defthing[simple-language-level%
          (class-provides/c drscheme:language:language<%>
                            drscheme:language:module-based-language<%>
                            drscheme:language:simple-module-based-language<%>)]{

Equal to
@scheme[
(drscheme:language:module-based-language->language-mixin
 (drscheme:language:simple-module-based-language->module-based-language-mixin
  drscheme:language:simple-module-based-language%))].

}

@defproc[(language-level-render-mixin [to-sexp (-> any/c any/c)]
                                      [show-void? boolean?])
         (mixin-provides/c [drscheme:language:language<%>] [])]{

Produces a mixin that overrides @method[drscheme:language:language<%>
render-value/format] to apply @scheme[to-sexp] to each value before printing it,
and to skip @scheme[void?] values (pre-transformation) if @scheme[show-void?] is
@scheme[#f].

}

@defproc[(language-level-capability-mixin [dict dict?])
         (mixin-provides/c [drscheme:language:language<%>] [])]{

Produces a mixin that augments @method[drscheme:language:language<%>
capability-value] to look up each key in @scheme[dict], producing the
corresponding value if the key is found and deferring to @scheme[inner]
otherwise.

}

@defthing[language-level-no-executable-mixin
          (mixin-provides/c [drscheme:language:language<%>] [])]{

Overrides @method[drscheme:language:language<%> create-executable] to print an
error message in a dialog box.

}

@defthing[language-level-eval-as-module-mixin
          (mixin-provides/c [drscheme:language:language<%>
                             drscheme:language:module-based-language<%>]
                            [])]{

Overrides @method[drscheme:language:language<%> front-end/complete-program] to
wrap terms from the definition in a module based on the language level's
definition module.  This duplicates the behavior of the HtDP teaching languages,
for instance.

}

@defthing[language-level-macro-stepper-mixin
          (mixin-provides/c [drscheme:language:language<%>
                             language/macro-stepper<%>]
                            [])]{

This mixin enables the macro stepper for its language level.

}

@defthing[language-level-check-expect-mixin
          (mixin-provides/c [drscheme:language:language<%>] [])]{

This mixin overrides @method[drscheme:language:language<%> on-execute] to set up
the @scheme[check-expect] test engine to a language level similarly to the HtDP
teaching languages.

}

@defproc[(language-level-metadata-mixin
          [reader-module module-path?]
          [meta-lines exact-nonnegative-integer?]
          [meta->settings (-> string? any/c any/c)]
          [settings->meta (-> symbol? any/c string?)])
         (mixin-provides/c [drscheme:language:language<%>] [])]{

This mixin constructs a language level that stores metadata in saved files
allowing DrScheme to automatically switch back to this language level upon
opening them.  It overrides @method[drscheme:language:language<%>
get-reader-module], @method[drscheme:language:language<%> get-metadata],
@method[drscheme:language:language<%> metadata->settings], and
@method[drscheme:language:language<%> get-metadata-lines].

The resulting language level uses the reader from @scheme[reader-module], and is
recognized in files that start with a reader directive for that module path
within the first @scheme[meta-lines] lines.  Metadata about the language's
settings is marshalled between a string and a usable value (based on a default
value) by @scheme[meta->settings], and between a usable value for a current
module (with a symbolic name) by @scheme[settings->meta].

}

}
