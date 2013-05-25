#lang scribble/doc
@(require "common.rkt")
@(tools-title "language")

@definterface[drracket:language:simple-module-based-language<%> ()]{

This interface represents the bare essentials when defining a module-based
language.  Use the
@racket[drracket:language:simple-module-based-language->module-based-language-mixin]
mixin to construct an implementation of
@racket[drracket:language:module-based-language<%>] from an implementation of
this interface.

The class @racket[drracket:language:simple-module-based-language%] provides an
implementation of this interface.

@defmethod[(get-language-numbers)
           (cons number (listof number))]{
  Returns a list of numbers, whose length must be the same as the result
  of
  @method[drracket:language:simple-module-based-language<%> get-language-position].
  Each number indicates the sorted order of the language positions in
  the language dialog.
}

@defmethod[(get-language-position)
           (cons string (listof string))]{
  This method is the same as
  @method[drracket:language:language<%> get-language-position].
}

@defmethod[(get-module)
           s-expression]{
  This method specifies the module that defines the language.

  This method replaces
  @method[drracket:language:language<%> front-end/complete-program] and
  @method[drracket:language:language<%> front-end/interaction].

  The result is expected to be the @racket[module] (its initial require)
  except as value, ie @racket[quote]d.
}

@defmethod[(get-one-line-summary)
           (or/c #f string?)]{
  The result of this method is shown in a tooltip in
  the language dialog when the
  user mouses over this language. If the result is
  @racket[#f], no tooltip is shown.
}

@defmethod[(get-reader)
           (->* () (any/c input-port?) (or/c syntax? eof-object?))]{
  This method must return a procedure that is used to read syntax from a
  port in the same manner as @racket[read-syntax].  It is used as the
  reader for this language.
}}

@defclass[drracket:language:simple-module-based-language%
          object%
          (drracket:language:simple-module-based-language<%>)]{

@defconstructor/make[([module s-expression]
                      [language-position (cons string (listof string))]
                      [language-numbers (cons number (listof number)) (map (lambda (x) 0) language-position)]
                      [one-line-summary string? ""]
                      [documentation-reference (or/c false/c something-else) #f]
                      [reader (->* () (any/c input-port?) (or/c syntax? eof-object?))]
                      [language-id string?])]{
  The init args are used as the results of the
  @method[drracket:language:simple-module-based-language% get-module]
  and
  @method[drracket:language:simple-module-based-language% get-language-position]
  methods.
}

@defmethod[#:mode override
           (get-language-numbers)
           (cons number (listof number))]{
  returns the corresponding init arg.
}

@defmethod[#:mode override
           (get-language-position)
           s-expression]{
  returns the corresponding init arg.
}

@defmethod[#:mode override
           (get-module)
           (cons string (listof string))]{
  returns the corresponding init arg.
}

@defmethod[#:mode override
           (get-one-line-summary)
           string?]{
  returns the corresponding initialization argument.
}

@defmethod[#:mode override
           (get-reader)
           (->* () (any/c input-port?) (or/c syntax? eof-object?))]{
  returns the corresponding init arg.
}}


@defmixin[drracket:language:simple-module-based-language->module-based-language-mixin
          (drracket:language:simple-module-based-language<%>)
          (drracket:language:module-based-language<%>)]{

@index{drracket:language:simple-settings}
This mixin uses a struct definition for its settings:
@racketblock[
(define-struct drracket:language:simple-settings
  (case-sensitive  (code:comment @#,t{boolean?})
   printing-style  (code:comment @#,t{(symbols 'constructor 'quasiquote 'write 'print)})
   fraction-style  (code:comment @#,t{(symbols 'mixed-fraction 'mixed-fraction-e})
                   (code:comment @#,t{         'repeating-decimal 'repeating-decimal-e)})
   show-sharing    (code:comment @#,t{boolean?})
   insert-newlines (code:comment @#,t{boolean?})
   annotations))   (code:comment @#,t{(symbols 'none 'debug 'debug/profile})
                   (code:comment @#,t{         'test-coverage)})
]

The settings in this structure reflect the settings show in the language
configuration dialog for languages constructed with this mixin.  The
first controls the input for the language.  The rest specify printing
controls for the language.  The style @racket['print] is the default
style, as normally used in the Racket REPL.  The sharing field
determines if cycles and sharing in values are displayed when the value
is rendered.  The insert newlines field determines if values in the repl
are formatted with @racket[write] style-line printouts, or with
@racket[pretty-print] multi-line printouts.

@defmethod[#:mode override
           (config-panel)
           (case-> (-> settings) (settings -> void))]{
  Constructs a configuration panel that lets the user configure all of
  the settings for this language.

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixin's
  @racket[settings] type.
}

@defmethod[#:mode override
           (default-settings)
           settings]{
  The defaults for the settings are
  @itemize[
  @item{@racket[case-sensitive] is @racket[#f]}
  @item{@racket[printing-style] is @racket['write]}
  @item{@racket[show-sharing] is @racket[#f]}
  @item{@racket[insert-newlines] is @racket[#t]}
  ]

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixins
  @racket[settings] type.
}

@defmethod[#:mode override
           (default-settings?)
           boolean?]{
}

@defmethod[#:mode override
           (get-init-code [settings settings])
           sexpression]{
  Creates an s-expression of a module that sets the
  @racket[current-inspector], @racket[read-case-sensitive], and
  @racket[error-value->string] parameters.  Additionally, it may load
  @racket[errortrace], if debugging is enabled.
}

@defmethod[#:mode override
           (get-transformer-module)
           s-expression]{
  Returns @racket['mzscheme].
}

@defmethod[#:mode override
           (marshall-settings)
           writable]{
  Constructs a vector from the structure.

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixins
  @racket[settings] type.
}

@defmethod[#:mode override
           (on-execute)
           void?]{
  Sets the case sensitivity of the language.

  Sets the structure inspector to a new inspector, saving the original
  inspector for use during printing.

  Sets the @racket[global-port-print-handler] to print based on the
  settings structure, but without any newlines.

  If debugging is enabled, it sets the @racket[current-eval] handler to
  one that annotates each evaluated program with debugging annotations.
  Additionally, it sets the @racket[error-display-handler] to show the
  debugging annotations when an error is raised.

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixin's
  @racket[settings] type.
}

@defmethod[#:mode override
           (render-value)
           void?]{
  Translates the value to a string, based on the settings.

  Restores a super struct inspector to render structs properly.  (See
  also
  @method[drracket:language:simple-module-based-language->module-based-language-mixin% on-execute])

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixin's
  @racket[settings] type.
}

@defmethod[#:mode override
           (render-value/format)
           void?]{
  Translates the value to a string, based on the settings.

  Restores a super struct inspector to render structs properly.  (See
  also
  @method[drracket:language:simple-module-based-language->module-based-language-mixin% on-execute].)

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixin's
  @racket[settings] type.
}

@defmethod[#:mode override
           (unmarshall-settings)
           (or/c false/c settings)]{
  Builds a settings structure from the vector, or @racket[#f] if the
  vector doesn't match the types of the structure.

  See also
  @racket[drracket:language:simple-module-based-language->module-based-language-mixin]
  for details of the simple-settings structure, this mixin's
  @racket[settings] type.
}

@defmethod[#:mode override
           (use-mred-launcher)
           boolean?]{
  Returns @racket[#t].
}}


@definterface[drracket:language:module-based-language<%> ()]{

This interface is for languages that can be implemented with Racket
@racket[module]s.

Use the @racket[drracket:language:module-based-language->language-mixin]
mixin to construct an implementation of
@racket[drracket:language:language<%>] from an implementation of this
interface.

@defmethod[(config-panel [parent (is-a?/c panel%)])
           (case-> (-> settings) (settings -> void))]{
  This method is the same as
  @method[drracket:language:language<%> config-panel].
}

@defmethod[(default-settings)
           settings]{
  This method is the same as
  @method[drracket:language:language<%> default-settings].
}

@defmethod[(default-settings? [settings settings])
           boolean?]{
  This method is the same as
  @method[drracket:language:language<%> default-settings?].
}

@defmethod[(get-init-code [settings settings])
           sexp]{
  Returns a module in sexpression form that is used for creating
  executables. The module must provide a thunk, called
  @racket[init-code].

  When either a stand-alone executable or a launcher is created, the
  module is required, and @racket[init-code] is invoked. This procedure
  is expected to set up the environment, based on the settings.
}

@defmethod[(get-language-numbers)
           (cons number (listof number))]{
  This method is the same as
  @method[drracket:language:language<%> get-language-numbers].
}

@defmethod[(get-language-position)
           (cons string (listof string))]{
  This method is the same as
  @method[drracket:language:language<%> get-language-position].
}

@defmethod[(get-module)
           s-expression]{
  This method specifies the module that defines the language.  It is
  used to initialize the user's namespace.

  The result is expected to be the @racket[module] (its initial require)
  except as value, ie @racket[quote]d.

  See also
  @method[drracket:language:module-based-language<%> get-transformer-module].
}

@defmethod[(get-one-line-summary)
           string?]{
  The result of this method is shown in the language dialog when the
  user selects this language.
}

@defmethod[(get-reader)
           (->* () (any/c input-port?) (or/c syntax? eof-object?))]{
  This method must return a procedure that is used to read syntax from a
  port in the same manner as @racket[read-syntax]. It is used as the
  reader for this language.
}

@defmethod[(get-transformer-module)
           (or/c quoted-module-path #f)]{
  This method specifies the module that defines the transformation
  language.  It is used to initialize the transformer portion of the
  user's namespace.

  The result is expected to be the @racket[module] (its initial require)
  except as value, i.e., @racket[quote]d or @racket[#f].

  If the result is @racket[#f], no module is required into the
  transformer part of the namespace.

  See also
  @method[drracket:language:module-based-language<%> get-module].
}

@defmethod[(marshall-settings [settings settings])
           writable]{
  This method is the same as
  @method[drracket:language:language<%> marshall-settings].
}

@defmethod[(on-execute [settings settings]
                       [run-on-user-thread ((-> void) -> void)])
           void?]{
  This method is the same as
  @method[drracket:language:language<%> on-execute].
}

@defmethod[(render-value [value TST]
                         [settings settings]
                         [port port])
           void?]{
  This method is the same as
  @method[drracket:language:language<%> render-value].
}

@defmethod[(render-value/format [value TST]
                                [settings settings]
                                [port port]
                                [width (or/c number (symbols 'infinity))])
           void?]{
  This method is the same as
  @method[drracket:language:language<%> render-value/format].
}

@defmethod[(unmarshall-settings [input writable])
           (or/c settings false/c)]{
  This method is the same as
  @method[drracket:language:language<%> unmarshall-settings].
}

@defmethod[(use-mred-launcher)
           boolean?]{
  This method is called when an executable is created to determine if
  the executable should use the GRacket or the Racket binary.
}

@defmethod[(use-namespace-require/copy?)
           boolean?]{
@methspec{
  The result of this method controls how the module is attached to the
  user's namespace. If the method returns @racket[#t], the Racket
  primitive @racket[namespace-require/copy] is used and if it returns
  @racket[#f], @racket[namespace-require] is used.
}
@methimpl{
  Returns @racket[#f] by default.
}}}


@defmixin[drracket:language:module-based-language->language-mixin (drracket:language:module-based-language<%>) (drracket:language:language<%>)]{

@defmethod[#:mode override
           (front-end/complete-program)
           (-> (or/c sexp/c syntax? eof-object?))]{
  Reads a syntax object, from @racket[input].  Does not use
  @racket[settings].

  For languages that use these mixins, there is no difference between
  this method and
  @method[drracket:language:module-based-language->language-mixin% front-end/interaction].
}

@defmethod[#:mode override
           (front-end/interaction)
           (-> (or/c sexp/c syntax? eof-object?))]{
  Reads a syntax object, from @racket[input]. Does not use
  @racket[settings].

  For languages that use these mixins, there is no difference between
  this method and
  @method[drracket:language:module-based-language->language-mixin% front-end/complete-program].
}

@defmethod[#:mode override
           (get-language-name)
           string?]{
  Returns the last element of the list returned by
  @method[drracket:language:language<%> get-language-position].
}

@defmethod[#:mode override
           (on-execute)
           void?]{
  Calls the super method.

  Uses @racket[namespace-require] to install the result of
  @method[drracket:language:module-based-language<%> get-module] and
  Uses @racket[namespace-transformer-require] to install the result of
  @method[drracket:language:module-based-language<%>
  get-transformer-module] into the user's namespace.
}}


@definterface[drracket:language:language<%> ()]{

Implementations of this interface are languages that DrRacket supports.

See @secref["adding-languages"] for an overview of adding languages to
DrRacket.

@defmethod[(capability-value [key symbol])
           any]{
@methspec{
  Returns the language-specific value for some capability. See also
  @racket[drracket:language:register-capability].
}
@methimpl{
  By default, returns the value from:
  @racket[drracket:language:get-capability-default].
}}

@defmethod[(config-panel [parent (is-a?/c panel%)])
           (case-> (-> settings) (settings -> void))]{
  This method used by the language configuration dialog to construct the
  ``details'' panel for this language.  It accepts a parent panel and
  returns a get/set function that either updates the GUI to the argument
  or returns the settings for the current GUI.
}

@defmethod[(create-executable [settings settings]
                              [parent (or/c (is-a?/c dialog%) (is-a?/c frame%))]
                              [program-filename string?])
           void?]{
  This method creates an executable in the given language.  The
  @racket[program-filename] is the name of the program to store in the
  executable and @racket[executable-filename] is the name of a file
  where the executable goes.

  See also
  @racket[drracket:language:create-module-based-stand-alone-executable]
  and @racket[drracket:language:create-module-based-launcher].
}

@defmethod[(default-settings)
           settings]{
  Specifies the default settings for this language.
}

@defmethod[(default-settings? [settings settings])
           boolean?]{
  Return @racket[#t] if the input settings matches the default settings
  obtained via @method[drracket:language:language<%> default-settings].
}

@defmethod[(first-opened [settings settings]) void?]{
  This method is called after the language is initialized, but no
  program has yet been run. It is called from the user's eventspace's
  main thread.

  See also @method[drracket:rep:text% initialize-console].

  Calling this method should not escape.  DrRacket calls this method in
  a @racket[parameterize] where the @racket[error-escape-handler] is set
  to an escaping continuation that continues initializing the
  interactions window.  Thus, raising an exception will report the error
  in the user's interactions window as if this were a bug in the user's
  program.  Escaping in any other way, however, can cause DrRacket to
  fail to start up.

  Also, IO system will deadlock if the @racket[first-opened] method does
  IO on the user's IO ports, so the calling context of
  @racket[first-opened] sets the @racket[current-output-port] and
  @racket[current-error-port] to ports that just collect all of the IO
  that happened and then replay it later in the initialization of the
  user's program.

  Contrary to the method contract spec, DrRacket will also invoke this
  method if it has zero arguments, passing nothing; the zero argument
  version is for backwards compatibility and is not recommended.

}

@defmethod[(front-end/complete-program [port port]
                                       [settings settings])
           (-> (or/c sexp/c syntax? eof-object?))]{
  @racket[front-end/complete-program] method reads and parses a program
  in the language. The @racket[port] argument contains all of the data
  to be read (until eof) and the name of the @racket[port] (obtained via
  @racket[object-name]) is a value representing the source of the
  program (typically an editor, but may also be a string naming a file
  or some other value).  The @racket[settings] argument is the current
  settings for the language.

  The @racket[front-end/complete-program] method is expected to return a
  thunk that is called repeatedly to get all of the expressions in the
  program. When all expressions have been read, the thunk is expected to
  return @racket[eof].

  This method is only called for programs in the definitions
  window. Notably, it is not called for programs that are
  @racket[load]ed or @racket[eval]ed.  See @racket[current-load] and
  @racket[current-eval] for those.

  This method is expected to raise an appropriate exception if the
  program is malformed, eg an @racket[exn:syntax] or @racket[exn:read].

  This is called on the user's thread, as is the thunk it returns.

  Implementations of this method should not return fully expanded
  expressions, since there are two forms of expansion, using either
  @racket[expand] or @racket[expand-top-level-with-compile-time-evals]
  and the use of the expanded code dictates which applies.

  See also @method[drracket:language:language<%> front-end/interaction]
  and
  @method[drracket:language:language<%> front-end/finished-complete-program].
}

@defmethod[(front-end/finished-complete-program [settings settings]) any]{
  This method is called when @onscreen{Run} is clicked, but only after
  @method[drracket:language:language<%> front-end/complete-program] has
  been called.  Specifically, @method[drracket:language:language<%>
  front-end/complete-program] is first called to get a thunk that reads
  from the program.  That thunk is called some number of times,
  eventually returning @racket[eof], or raising an exception. Then, this
  method is called.

  This method is called on the user's main eventspace thread, and
  without a prompt or other control delimiter.  It must return without
  raising an error, or else the DrRacket window will be wedged.
}

@defmethod[(front-end/interaction [port input-port]
                                  [settings settings])
           (-> (or/c sexp/c syntax? eof-object?))]{
  This method is just like
  @method[drracket:language:language<%> front-end/complete-program]
  except that it is called with program fragments, for example the
  expressions entered in the interactions window.  It is also used in
  other contexts by tools to expand single expressions.

  See also
  @method[drracket:language:language<%> front-end/finished-complete-program].
}

@defmethod[(get-comment-character)
           (values string? char?)]{
  Returns text to be used for the ``Insert Large Letters'' menu item in
  DrRacket.  The first result is a prefix to be placed at the beginning
  of each line and the second result is a character to be used for each
  pixel in the letters.
}

@defmethod[(get-language-name)
           string?]{
  Returns the name of the language, as shown in the REPL when executing
  programs in the language and in the bottom left of the DrRacket
  window.
}

@defmethod[(get-language-numbers)
           (cons number (listof number))]{
  This method is used in a manner analogous to
  @method[drracket:language:language<%> get-language-position].

  Each element in the list indicates how the names at that point in
  dialog will be sorted. Names with lower numbers appear first.  If two
  languages are added to DrRacket with the same strings (as given by the
  @method[drracket:language:language<%> get-language-position] method)
  the corresponding numbers returned by this method must be the same.
  Additionally, no two languages can have the same set of numbers.

  (Note: this method should always return the same result, for the same
  language.)

}

@defmethod[(get-language-position)
           (cons string (listof string))]{
  This method returns a list of strings that is used to organize this
  language with the other languages.  Each entry in that list is a
  category or subcategory of the language and the last entry in the list
  is the name of the language itself.  In the language dialog, each
  element in the list except for the last will be a nested turn down
  triangle on the left of the dialog.  The final entry will be the name
  that the user can choose to select this language. Names that are the
  same will be combined into the same turndown entry.

  For example, if one language's position is:
  @racketblock[
    (list "General Category" "Specific Category" "My Language")
  ]
  and another's is:
  @racketblock[
    (list "General Category" "Specific Category" "My Other Language")
  ]
  The language dialog will collapse the first two elements in the list,
  resulting in only a pair of nested turn-down triangles, not parallel
  pairs of nested turn-down triangles.

}

@defmethod[(get-language-url)
           (or/c string? false/c)]{
@methspec{
  Returns a url for the language.
}
@methimpl{
  If the result isn't @racket[#f], the name of the language is clickable
  in the interactions window and clicking takes you to this url.
}}

@defmethod[(get-metadata [modname symbol?] [settings any/c])
           string?]{
  This method is only called when
  @method[drracket:language:language<%> get-reader-module] returns an
  sexp.

  It is expected to return a string that contains N lines, where N is
  the result of calling
  @method[drracket:language:language<%> get-metadata-lines].
  The string is prefixed to the buffer before the file is saved by
  DrRacket, and removed from the buffer after it is opened in DrRacket.

  The string is expect to be a prefix to the file that sets up a reader
  for files in this language, using @tt{#reader}.

  The @racket[modname] argument's printed form is the same as the file's
  name, but without the path, and without an extension. The
  @racket[settings] argument is the current language's settings value.

  See also @method[drracket:language:language<%> metadata->settings],
  @method[drracket:language:language<%> get-metadata-lines], and
  @method[drracket:language:language<%> get-reader-module].
}

@defmethod[(get-metadata-lines)
           number]{
  This method is only called when
  @method[drracket:language:language<%> get-reader-module] returns an
  sexp.

  The result of the method is a count of the number of lines in the
  strings that @method[drracket:language:language<%> get-metadata]
  returns. The @method[drracket:language:language<%> get-metadata]
  function does not necessarily return the same string each time it is
  called (see @method[drracket:language:language<%> metadata->settings])
  but it is expected to always return a string with a fixed number of
  lines, as indicated by the result of this method.
}

@defmethod[(get-one-line-summary)
           string?]{
@methspec{
  The result of this method is shown in the language dialog when the
  user selects this language.
}
@methimpl{
}}

@defmethod[(get-reader-module)
           (or/c sexp-representing-a-require-spec false/c)]{
  The result of this method is used when saving or loading files.

  If the result is a sexp, saved files get a prefix inserted at the
  beginning (the prefix is determined by calling
  @method[drracket:language:language<%> get-metadata]).  When the file
  is then loaded, DrRacket recognizes this prefix and sets the language
  back to match the saved file.

  See also @method[drracket:language:language<%> metadata->settings],
  @method[drracket:language:language<%> get-metadata-lines], and
  @method[drracket:language:language<%> get-metadata].
}

@defmethod[(get-style-delta)
           (or/c #f (is-a?/c style-delta%) (listof (list/c (is-a?/c style-delta%) number? number?)))]{
  The style delta that this method returns is used in the language
  dialog and the DrRacket REPL when the language's name is printed.

  When it is @racket[#f], no styling is used.

  If the result is a list, each element is expected to be a list of
  three items, a style-delta, and two numbers. The style delta will be
  applied to the corresponding portion of the name.
}

@defmethod[(extra-repl-information [settings settings] [port output-port?]) void?]{
  This method is called on the DrRacket eventspace main thread to insert
  extra information into the REPL to reflect the state of the program.

  It is used, for example, to print out the ``Teachpack'' lines in the
  HtDP languages.
}

@defmethod[(marshall-settings [settings settings])
           writable]{
  Translates an instance of the settings type into a Racket object that
  can be written out to disk.
}

@defmethod[(metadata->settings [metadata string?])
           settings]{
  This method is only called when
  @method[drracket:language:language<%> get-reader-module] returns an
  sexp.

  When a file is opened in DrRacket, if this language's
  @method[drracket:language:language<%> get-reader-module] returns an
  sexp, the prefix of the file (the first N lines, where N is the number
  returned by @method[drracket:language:language<%> get-metadata-lines])
  is scanned for @racket["#reader"] followed by the result of
  @method[drracket:language:language<%> get-reader-module].  If that
  pattern is found, the language is set to this language.  Also, the
  entire prefix is passed, as a string, to this method which returns a
  @racket[settings] value, used as the settings for this language.
}

@defmethod[(on-execute [settings settings]
                       [run-on-user-thread ((-> any) -> any)])
           any]{
  The @racket[on-execute] method is called on DrRacket's eventspace's
  main thread before any evaluation happens when the Run button is
  clicked. It is also called when a new DrRacket tab (or window) is
  created to initialize the empty interactions window.

  Use this method to initialize Racket's
  @secref[#:doc '(lib "scribblings/reference/reference.scrbl") "parameters"]
  for the user.  When this function is called, the user's thread has
  already been created, as has its custodian.  These parameters have
  been changed from the defaults in Racket:
  @itemize[
  @item{@racket[current-custodian] is set to a new custodian.}
  @item{@racket[current-namespace] has been set to a newly created empty
    namespace.  This namespace has the following modules copied (with
    @racket[namespace-attach-module]) from DrRacket's original
    namespace:
    @itemize[
    @item{@racket['mzscheme]}
    @item{@racket['mred]}
    ]}
  @item{@racket[read-curly-brace-as-paren] is @racket[#t],}
  @item{@racket[read-square-bracket-as-paren] is @racket[#t],}
  @item{The @racket[port-write-handler] and @racket[port-display-handler]
    have been set to procedures that call @racket[pretty-print] and
    @racket[pretty-display] instead of @racket[write] and
    @racket[display].  When @racket[pretty-print] and
    @racket[pretty-display] are called by these parameters, the
    @racket[pretty-print-columns] parameter is set to
    @racket['infinity], so the output looks just like @racket[write] and
    @racket[display].  This is done so that special scheme values can be
    displayed as snips.}
  @item{The @racket[current-print-covert-hook] is to a procedure so that
    @racket[snip%]s are just returned directly to be inserted into the
    interactions @racket[text%] object.}
  @item{The output and input ports are set to point to the interactions
    window with these parameters: @racket[current-input-port],
    @racket[current-output-port], and @racket[current-error-port].}
  @item{The @racket[event-dispatch-handler] is set so that DrRacket can
    perform some initial setup and close down around the user's code.}
  @item{The @racket[current-directory] and
    @racket[current-load-relative-directory] are set to the directory
    where the definitions file is saved, or if it isn't saved, to the
    initial directory where DrRacket started up.}
  @item{The snip-class-list, returned by @racket[get-the-snip-class-list]
    is initialized with all of the snipclasses in DrRacket's
    eventspace's snip-class-list.}
  @item{The @racket[error-print-source-location] parameter is set to
    @racket[#f] and the @racket[error-display-handler] is set to a
    handler that creates an error message from the exception record,
    with font and color information and inserts that error message into
    the definitions window.}
  ]

  The @racket[run-on-user-thread] arguments accepts thunks and runs them
  on the user's eventspace's main thread.  The output ports are not yet
  functioning, so print outs should be directed to the original DrRacket
  output port, if necessary.

  This thunk is wrapped in a @racket[with-handlers] that catches all
  exceptions matching @racket[exn:fail?] and then prints out the
  exception message to the original output port of the DrRacket process.
}

@defmethod[(order-manuals [manuals (listof bytes?)])
           (values (listof bytes?) boolean?)]{
  Returns a sublist of its input, that specifies the manuals
  (and their order) to search in. The boolean result indicates
  if @tt{doc.txt} files should be searched.
}

@defmethod[(render-value [value TST]
                         [settings settings]
                         [port port])
           void?]{
  This method is just like
  @method[drracket:language:language<%> render-value/format] except that
  it is expected to put the entire value on a single line with no
  newline after the value.
}

@defmethod[(render-value/format [value TST]
                                [settings settings]
                                [port port]
                                [width (or/c number (symbols 'infinity))])
           void?]{
  This method is used to print values into a port, for display to a
  user.  The final argument is a maximum width to use (in characters)
  when formatting the value.

  This method is expected to format the value by inserting newlines in
  appropriate places and is expected to render a newline after the
  value.

  See also @method[drracket:language:language<%> render-value].
}

@defmethod[(unmarshall-settings [input writable])
           (or/c settings false/c)]{
  Translates a Racket value into a settings, returning @racket[#f] if
  that is not possible.
}}

@(tools-include "language")
