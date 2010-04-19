#lang scribble/doc
@(require "common.ss")
@(tools-title "language")

@definterface[drscheme:language:simple-module-based-language<%> ()]{

This interface represents the bare essentials when defining
a module-based language. Use the
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin]
mixin to construct an implementation of
@scheme[drscheme:language:module-based-language<%>] from an implementation of this interface.

The class
@scheme[drscheme:language:simple-module-based-language%] provides an implementation of this interface.



@defmethod[(get-language-numbers)
           (cons number (listof number))]{
Returns a list of numbers, whose length must be the same as
the result of 
@method[drscheme:language:simple-module-based-language<%> get-language-position]. Each number indicates the sorted order of the
language positions in the language dialog.

}

@defmethod[(get-language-position)
           (cons string (listof string))]{
This method is the same as
@method[drscheme:language:language<%> get-language-position].

}

@defmethod[(get-module)
           s-expression]{
This method specifies the module that defines the language.

This method replaces 
@method[drscheme:language:language<%> front-end/complete-program] and
@method[drscheme:language:language<%> front-end/interaction].


The result is expected to be the 
@scheme[module] (its initial require)
except as value, ie @scheme[quote]d.

}

@defmethod[(get-one-line-summary)
           string?]{
The result of this method is shown in the language dialog
when the user selects this language.

}

@defmethod[(get-reader)
           (->* () (any/c input-port?) (or/c syntax? eof-object?))]{

This method must return a procedure that is used to read
syntax from a port in the same manner as 
@scheme[read-syntax]. It is used as the reader
for this language.



}}


@defclass[drscheme:language:simple-module-based-language% object% (drscheme:language:simple-module-based-language<%>)]{



@defconstructor/make[([module s-expression]
                      [language-position (cons string (listof string))]
                      [language-numbers (cons number (listof number)) (map (lambda (x) 0) language-position)]
                      [one-line-summary string? ""]
                      [documentation-reference (or/c false/c something-else) #f]
                      [reader (->* () (any/c input-port?) (or/c syntax? eof-object?))]
                      [language-id string?])]{

The init args are used as the results of the
@method[drscheme:language:simple-module-based-language% get-module] and
@method[drscheme:language:simple-module-based-language% get-language-position] methods


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


@defmixin[drscheme:language:simple-module-based-language->module-based-language-mixin (drscheme:language:simple-module-based-language<%>) (drscheme:language:module-based-language<%>)]{

@index{drscheme:language:simple-settings}
This mixin uses a struct definition for its settings:
@schemeblock[
(define-struct drscheme:language:simple-settings
  (case-sensitive  (code:comment @#,t{boolean?})
   printing-style  (code:comment @#,t{(symbols 'constructor 'quasiquote 'write 'print)})
   fraction-style  (code:comment @#,t{(symbols 'mixed-fraction 'mixed-fraction-e})
                   (code:comment @#,t{         'repeating-decimal 'repeating-decimal-e)})
   show-sharing    (code:comment @#,t{boolean?})
   insert-newlines (code:comment @#,t{boolean?})
   annotations))   (code:comment @#,t{(symbols 'none 'debug 'debug/profile})
                   (code:comment @#,t{         'test-coverage)})
]

The settings in this structure reflect the settings show in
the language configuration dialog for languages constructed
with this mixin. The first controls the input for the
language. The rest specify printing controls for the
language. The style @scheme['write] is the default style,
used in the MzScheme REPL. The sharing field determines if
cycles and sharing in values are displayed when the value is
rendered. The insert newlines field determines if values in
the repl are formatted with @scheme[write] style-line
printouts, or with @scheme[pretty-print] multi-line printouts.



@defmethod[#:mode override 
           (config-panel)
           (case-> (-> settings) (settings -> void))]{

Constructs a configuration panel that lets the user
configure all of the settings for this language.

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin]
for details of the
simple-settings structure, this mixin's @scheme[settings] type.
}

@defmethod[#:mode override 
           (default-settings)
           settings]{

The defaults for the settings are
@itemize[
@item{@scheme[case-sensitive] is @scheme[#f]}
@item{@scheme[printing-style] is @scheme['write]}
@item{@scheme[show-sharing] is @scheme[#f]}
@item{@scheme[insert-newlines] is @scheme[#t]}
]

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin] for details of the
simple-settings structure, this mixins @scheme[settings] type.


}

@defmethod[#:mode override 
           (default-settings?)
           boolean?]{}

@defmethod[#:mode override 
           (get-init-code [settings settings])
           sexpression]{

Creates an s-expression of a module that sets the
@scheme[current-inspector], @scheme[read-case-sensitive],
and @scheme[error-value->string] parameters. Additionally,
it may load @scheme[errortrace], if debugging is enabled.


}

@defmethod[#:mode override 
           (get-transformer-module)
           s-expression]{

Returns @scheme['mzscheme].


}

@defmethod[#:mode override 
           (marshall-settings)
           writable]{

Constructs a vector from the structure.

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin]
for details of the
simple-settings structure, this mixins @scheme[settings] type.


}

@defmethod[#:mode override 
           (on-execute)
           void?]{

Sets the case sensitivity of the language.

Sets the structure inspector to a new inspector,
saving the original inspector for use during printing.

Sets the
@scheme[global-port-print-handler]
to print based on the settings structure, but without
any newlines.

If debugging is enabled, it sets the
@scheme[current-eval]
handler to one that annotates each evaluated program with 
debugging annotations. Additionally, it sets the 
@scheme[error-display-handler]
to show the debugging annotations when an error is raised.

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin] for details of the
simple-settings structure, this mixin's @scheme[settings] type.


}

@defmethod[#:mode override 
           (render-value)
           void?]{

Translates the value to a string, based on the settings.

Restores a super struct inspector to render structs properly.
(See also
@method[drscheme:language:simple-module-based-language->module-based-language-mixin% on-execute])

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin] for details of the
simple-settings structure, this mixin's @scheme[settings] type.


}

@defmethod[#:mode override 
           (render-value/format)
           void?]{

Translates the value to a string, based on the settings.

Restores a super struct inspector to render structs properly.
(See also
@method[drscheme:language:simple-module-based-language->module-based-language-mixin% on-execute])
     
See also 
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin]
for details of the
simple-settings structure, this mixin's @scheme[settings] type.


}

@defmethod[#:mode override 
           (unmarshall-settings)
           (or/c false/c settings)]{

Builds a settings structure from the vector, or @scheme[#f] if
the vector doesn't match the types of the structure.

See also
@scheme[drscheme:language:simple-module-based-language->module-based-language-mixin] for details of the
simple-settings structure, this mixin's @scheme[settings] type.



}

@defmethod[#:mode override 
           (use-mred-launcher)
           boolean?]{

Returns @scheme[#t].


}}


@definterface[drscheme:language:module-based-language<%> ()]{

This interface is for languages that can be implemented
with MzScheme @scheme[module]s.

Use the
@scheme[drscheme:language:module-based-language->language-mixin]
mixin to construct an implementation of
@scheme[drscheme:language:language<%>] from an implementation of this interface.



@defmethod[(config-panel [parent (is-a?/c panel%)])
           (case-> (-> settings) (settings -> void))]{
This method is the same as
@method[drscheme:language:language<%> config-panel].

}

@defmethod[(default-settings)
           settings]{
This method is the same as
@method[drscheme:language:language<%> default-settings].

}

@defmethod[(default-settings? [settings settings])
           boolean?]{
This method is the same as
@method[drscheme:language:language<%> default-settings?].

}

@defmethod[(get-init-code [settings settings])
           sexp ]{
Returns a module in sexpression form that is used for
creating executables. The module must provide a thunk,
called @scheme[init-code]. 

When either a stand-alone executable or a launcher is
created, the module is required, and @scheme[init-code] is
invoked. This procedure is expected to set up the
environment, based on the settings.

}

@defmethod[(get-language-numbers)
           (cons number (listof number))]{
This method is the same as
@method[drscheme:language:language<%> get-language-numbers].

}

@defmethod[(get-language-position)
           (cons string (listof string))]{
This method is the same as
@method[drscheme:language:language<%> get-language-position].

}

@defmethod[(get-module)
           s-expression]{
This method specifies the module that defines the language.
It is used to initialize the user's namespace.

The result is expected to be the 
@scheme[module] (its initial require)
except as value, ie @scheme[quote]d.

See also
@method[drscheme:language:module-based-language<%> get-transformer-module].


}

@defmethod[(get-one-line-summary)
           string?]{
The result of this method is shown in the language dialog
when the user selects this language.

}

@defmethod[(get-reader)
           (->* () (any/c input-port?) (or/c syntax? eof-object?))]{

This method must return a procedure that is used to read
syntax from a port in the same manner as 
@scheme[read-syntax]. It is used as the reader
for this language.



}

@defmethod[(get-transformer-module)
           (or/c quoted-module-path #f)]{
This method specifies the module that defines the
transformation language. It is used to initialize
the transformer portion of the user's namespace.

The result is expected to be the 
@scheme[module] (its initial require)
except as value, ie @scheme[quote]d or @scheme[#f].

If the result is @scheme[#f], no module is required into the
transformer part of the namespace.

See also
@method[drscheme:language:module-based-language<%> get-module].

}

@defmethod[(marshall-settings [settings settings])
           writable]{
This method is the same as 
@method[drscheme:language:language<%> marshall-settings].

}

@defmethod[(on-execute [settings settings]
                       [run-in-user-thread ((-> void) -> void)])
           vod]{
This method is the same as
@method[drscheme:language:language<%> on-execute].

}

@defmethod[(render-value [value TST]
                         [settings settings]
                         [port port])
           void?]{
This method is the same as
@method[drscheme:language:language<%> render-value].

}

@defmethod[(render-value/format [value TST]
                                [settings settings]
                                [port port]
                                [width (or/c number (symbols 'infinity))])
           void?]{
This method is the same as
@method[drscheme:language:language<%> render-value/format].

}

@defmethod[(unmarshall-settings [input writable])
           (or/c settings false/c)]{
This method is the same as
@method[drscheme:language:language<%> unmarshall-settings].

}

@defmethod[(use-mred-launcher)
           boolean?]{
This method is called when an executable is created to
determine if the executable should use the mred or the
mzscheme binary.

}

@defmethod[(use-namespace-require/copy?)
           boolean?]{
@methspec{

The result of this method controls how the module is
attached to the user's namespace. If 
the method returns @scheme[#t], 
the mzscheme primitive
@scheme[namespace-require/copy]
is used and if it returns @scheme[#f],
@scheme[namespace-require]
is used.

}
@methimpl{

Defaultly returns @scheme[#f].


}}}


@defmixin[drscheme:language:module-based-language->language-mixin (drscheme:language:module-based-language<%>) (drscheme:language:language<%>)]{



@defmethod[#:mode override 
           (front-end/complete-program)
           (-> (or/c sexp/c syntax? eof-object?))]{

Reads a syntax object, from @scheme[input]. Does not use
@scheme[settings].

For languages that use these mixins, there is no difference
between this method and
@method[drscheme:language:module-based-language->language-mixin% front-end/interaction].


}

@defmethod[#:mode override 
           (front-end/interaction)
           (-> (or/c sexp/c syntax? eof-object?))]{

Reads a syntax object, from @scheme[input]. Does not use
@scheme[settings].

For languages that use these mixins, there is no difference
between this method and
@method[drscheme:language:module-based-language->language-mixin% front-end/complete-program].


}

@defmethod[#:mode override 
           (get-language-name)
           string?]{

Returns the last element of the list returned by
@method[drscheme:language:language<%> get-language-position].


}

@defmethod[#:mode override 
           (on-execute)
           void?]{

Calls the super method.

Uses @scheme[namespace-require]
to install the result of
@method[drscheme:language:module-based-language<%> get-module] and
Uses @scheme[namespace-transformer-require]
to install the result of 
@method[drscheme:language:module-based-language<%> get-transformer-module] into the user's namespace.


}}


@definterface[drscheme:language:language<%> ()]{

Implementations of this interface are languages that
DrScheme supports.

See @secref["adding-languages"] for an overview of
adding languages to DrScheme.



@defmethod[(capability-value [key symbol])
           any]{
@methspec{

Returns the language-specific value for some capability. See
also
@scheme[drscheme:language:register-capability].

}
@methimpl{

Defaultly returns the value from:
@scheme[drscheme:language:get-capability-default].


}}

@defmethod[(config-panel [parent (is-a?/c panel%)])
           (case-> (-> settings) (settings -> void))]{
This method used by the language configuration dialog to
construct the "details" panel for this language. It accepts
a parent panel and returns a get/set function that either
updates the GUI to the argument or returns the settings for
the current GUI.
   
}

@defmethod[(create-executable [settings settings]
                              [parent (or/c (is-a?/c dialog%) (is-a?/c frame%))]
                              [program-filename string?])
           void?]{
This method creates an executable in the given language. The
@scheme[program-filename] is the name of the program to store
in the executable and @scheme[executable-filename] is the name
of a file where the executable goes.

See also
@scheme[drscheme:language:create-module-based-stand-alone-executable] and
@scheme[drscheme:language:create-module-based-launcher].

}

@defmethod[(default-settings)
           settings]{
Specifies the default settings for this language.

}

@defmethod[(default-settings? [settings settings])
           boolean?]{
Return @scheme[#t] if the input settings matches the
default settings obtained via
@method[drscheme:language:language<%> default-settings].

}

@defmethod[(first-opened)
           void?]{

This method is called when the language is initialized, but
no program is run. It is called from the user's eventspace's
main thread.

See also
@method[drscheme:rep:text% initialize-console].


}

@defmethod[(front-end/complete-program [port port]
                                       [settings settings])
           (-> (or/c sexp/c syntax? eof-object?))]{
@scheme[front-end/complete-program] method reads, parses,
and optionally compiles a program in the language. The first
argument contains all of the data to be read (until eof) and
the second argument is a value representing the source of
the program (typically an editor, but may also be a string
naming a file or some other value).

The third argument is the current settings
for the language. The @scheme[front-end/complete-program]
method is expected to return a thunk that is called
repeatedly to get all of the expressions in the
program. When all expressions have been read, the thunk is
expected to return @scheme[eof].

This method is only called for programs in the definitions
window. Notably, it is not called for
programs that are @scheme[load]ed or @scheme[eval]ed.
See
@scheme[current-load] and 
@scheme[current-eval]
for those.

This method is expected to raise an appropriate exception if
the program is malformed, eg an @scheme[exn:syntax] or
@scheme[exn:read].

This is called on the user's thread, as is the thunk it
returns.

Implementations of this method should not return fully
expanded expressions, since there are two forms of
expansion, using either
@scheme[expand]
or 
@scheme[expand-top-level-with-compile-time-evals] 
and the use of the expanded code dictates which applies.

See also
@method[drscheme:language:language<%> front-end/interaction]
and
@method[drscheme:language:language<%> front-end/finished-complete-program].
}

@defmethod[(front-end/finished-complete-program [settings settings]) any]{
  This method is called when @onscreen{Run} is clicked, but only after
  @method[drscheme:language:language<%> front-end/complete-program]
  has been called. Specifically, 
  @method[drscheme:language:language<%> front-end/complete-program] is
  first called to get a thunk that reads from the program. That thunk
  is called some number of times, eventually returning @scheme[eof],
  or raising an exception. Then, this method is called.
 
  This method is called on the user's main eventspace thread, and without
  a prompt or other control delimiter. It must return without raising an
  error, or else the DrScheme window will be wedged.
}

@defmethod[(front-end/interaction [port input-port]
                                  [settings settings])
           (-> (or/c sexp/c syntax? eof-object?))]{
This method is just like
@method[drscheme:language:language<%> front-end/complete-program]
except that it is called with program fragments, for example the
expressions entered in the interactions window. It is also used in
other contexts by tools to expand single expressions.

See also
@method[drscheme:language:language<%> front-end/finished-complete-program].
}

@defmethod[(get-comment-character)
           (values string? char?)]{
Returns text to be used for the ``Insert Large Letters''
menu item in DrScheme. The first result is a prefix to be
placed at the beginning of each line and the second result
is a character to be used for each pixel in the letters.

}

@defmethod[(get-language-name)
           string?]{
Returns the name of the language, as shown in the REPL when
executing programs in the language and in the bottom left of
the drscheme window.

}

@defmethod[(get-language-numbers)
           (cons number (listof number))]{
This method is used in a manner analogous to
@method[drscheme:language:language<%> get-language-position].

Each element in the list indicates how the names at that
point in dialog will be sorted. Names with lower numbers
appear first. If two languages are added to DrScheme with
the same strings (as given by the 
@method[drscheme:language:language<%> get-language-position] method) the corresponding numbers returned by this method
must be the same. Additionally, no two languages can have the
same set of numbers.

(Note: this method should always return the same result, for
the same language.)

}

@defmethod[(get-language-position)
           (cons string (listof string))]{
This method returns a list of strings that is used to
organize this language with the other languages. Each entry
in that list is a category or subcategory of the language
and the last entry in the list is the name of the language
itself. In the language dialog, each element in the list
except for the last will be a nested turn down triangle on
the left of the dialog. The final entry will be the name
that the user can choose to select this language. Names that
are the same will be combined into the same turndown entry.

For example, if one language's position is:
@schemeblock[
(list "General Category" "Specific Category" "My Language")
]
and another's is:
@schemeblock[
(list "General Category" "Specific Category" "My Other Language")
]
The language dialog will collapse the first two elements in
the list, resulting in only a pair of nested turn-down
triangles, not parallel pairs of nested turn-down triangles.

}

@defmethod[(get-language-url)
           (or/c string? false/c)]{
@methspec{

Returns a url for the language.

}
@methimpl{

If the result isn't @scheme[#f], the name of the language is
clickable in the interactions window and clicking takes you
to this url.


}}

@defmethod[(get-metadata [modname symbol?] [settings any/c])
           string?]{

This method is only called when
@method[drscheme:language:language<%> get-reader-module] returns an sexp.

It is expected to return a string that contains N lines,
where N is the result of calling
@method[drscheme:language:language<%> get-metadata-lines]. The string is prefixed to the buffer before the file is
saved by DrScheme, and removed from the buffer after it is
opened in DrScheme.

The string is expect to be a prefix to the file that sets up
a reader for files in this language, using @tt{#reader}.

The @scheme[modname] argument's printed form is the same as the file's
name, but without the path, and without an extension. The
@scheme[settings] argument is the current language's settings value.

See also
@method[drscheme:language:language<%> metadata->settings],
@method[drscheme:language:language<%> get-metadata-lines], and
@method[drscheme:language:language<%> get-reader-module].


}

@defmethod[(get-metadata-lines)
           number]{

This method is only called when
@method[drscheme:language:language<%> get-reader-module] returns an sexp.

The result of the method is a count of the number of lines
in the strings that
@method[drscheme:language:language<%> get-metadata] returns. The
@method[drscheme:language:language<%> get-metadata] function does not necessarily return the same string
each time it is called (see
@method[drscheme:language:language<%> metadata->settings]) but it is expected to always return a string with a fixed
number of lines, as indicated by the result of this method.


}

@defmethod[(get-one-line-summary)
           string?]{
@methspec{

The result of this method is shown in the language dialog
when the user selects this language.

}
@methimpl{



}}

@defmethod[(get-reader-module)
           (or/c sexp-representing-a-require-spec false/c)]{

The result of this method is used when saving or loading files.

If the result is a sexp, saved files get a prefix inserted
at the beginning (the prefix is determined by calling
@method[drscheme:language:language<%> get-metadata]). When the file is then loaded, DrScheme recognizes this
prefix and sets the language back to match the saved file.

See also
@method[drscheme:language:language<%> metadata->settings],
@method[drscheme:language:language<%> get-metadata-lines], and
@method[drscheme:language:language<%> get-metadata].


}

@defmethod[(get-style-delta)
           (or/c #f (is-a?/c style-delta%) (listof (list/c (is-a?/c style-delta%) number? number?)))]{
The style delta that this method returns is used in the
language dialog and the DrScheme REPL when the language's
name is printed.

When it is @scheme[#f], no styling is used.

If the result is a list, each element is expected to be a
list of three items, a style-delta, and two numbers. The
style delta will be applied to the corresponding portion of
the name.

}

@defmethod[(extra-repl-information [settings settings] [port output-port?]) void?]{
  This method is called on the DrScheme eventspace main thread to insert extra
  information into the REPL to reflect the state of the program.
  
  It is used, for example, to print out the ``Teachpack'' lines in the HtDP languages.
}
                   

                                                                                                     
@defmethod[(marshall-settings [settings settings])
           writable]{
Translates an instance of the settings type into a scheme
object that can be written out to disk.

}

@defmethod[(metadata->settings [metadata string?])
           settings]{

This method is only called when
@method[drscheme:language:language<%> get-reader-module] returns an sexp.

When a file is opened in DrScheme, if this language's
@method[drscheme:language:language<%> get-reader-module] returns an sexp, the prefix of the file
(the first N lines, where N is the number
returned by 
@method[drscheme:language:language<%> get-metadata-lines]) is scanned for @scheme["#reader"] followed by the
result of
@method[drscheme:language:language<%> get-reader-module]. If that pattern is found, the language is set to this language.
Also, the entire prefix is passed, as a string,
to this method which returns a @scheme[settings] value, used as
the settings for this language.


}

@defmethod[(on-execute [settings settings]
                       [run-in-user-thread ((-> any) -> any)])
           any]{
The @scheme[on-execute] method is called on DrScheme's
eventspace's main thread before any evaluation happens
when the Run button is clicked. It is also called when
a new DrScheme tab (or window) is created to initialize
the empty interactions window.

Use this method to initialize MzScheme's
@secref[#:doc '(lib "scribblings/reference/reference.scrbl") "parameters"]
for the user. When
this function is called, the user's thread has already been
created, as has its custodian. These parameters have been
changed from the defaults in MzScheme:
@itemize[
@item{@scheme[current-custodian] is set to a new custodian.}
@item{@scheme[current-namespace] has been set to a newly
  created empty namespace.This namespace has the following modules 
  copied (with @scheme[namespace-attach-module])
  from DrScheme's original namespace:
  @itemize[
  @item{@scheme['mzscheme]}
  @item{@scheme['(lib "mred.ss" "mred")]}
  ]}
@item{
  @scheme[read-curly-brace-as-paren]
  is @scheme[#t],}
@item{
  @scheme[read-square-bracket-as-paren]
  is @scheme[#t],}
@item{The 
  @scheme[port-write-handler]
  and
  @scheme[port-display-handler]
  have been set to procedures
  that call 
  @scheme[pretty-print]
  and 
  @scheme[pretty-display] instead
  of 
  @scheme[write] and 
  @scheme[display].  When
  @scheme[pretty-print] and 
  @scheme[pretty-display] are
  called by these parameters, the
  @scheme[pretty-print-columns] parameter is set to
  @scheme['infinity], so the output looks just like
  @scheme[write] and 
  @scheme[display]. This is done so that
  special scheme values can be displayed as snips.}
@item{The
  @scheme[current-print-covert-hook] is to a
  procedure so that @scheme[snip%]s are just returned
  directly to be inserted into the interactions
  @scheme[text%] object.} 
@item{The output and input ports are set to point to the
  interactions window with these parameters:
  @scheme[current-input-port],
  @scheme[current-output-port], and
  @scheme[current-error-port].}
@item{The
@scheme[event-dispatch-handler]   is set so that DrScheme can perform some initial setup and
  close down around the user's code.}
@item{The
  @scheme[current-directory] and
  @scheme[current-load-relative-directory]
  are set to the directory where the definitions file is
  saved, or if it isn't saved, to the initial directory where
  DrScheme started up.}
@item{The snip-class-list, returned by
@scheme[get-the-snip-class-list] is initialized with all of the snipclasses in DrScheme's eventspace's snip-class-list.}

@item{
The 
@scheme[error-print-source-location]
parameter is set to @scheme[#f] and the 
@scheme[error-display-handler]
is set to a handler that creates an error message from the
exception record, with font and color information and inserts
that error message into the definitions window.}

]

The @scheme[run-in-user-thread] arguments accepts thunks and
runs them on the user's eventspace's main thread. These
thunks must not raise an exceptions (or drscheme itself will
get stuck). In addition, the output ports are not yet
functioning, so print outs should be directed to the
original drscheme output port, if necessary.

}

@defmethod[(order-manuals [manuals (listof bytes?)])
           (values (listof bytes?) boolean?)]{
Returns a sublist of its input, that specifies the manuals
(and their order) to search in. The boolean result indicates
if doc.txt files should be searched.

}

@defmethod[(render-value [value TST]
                         [settings settings]
                         [port port])
           void?]{
This method is just like
@method[drscheme:language:language<%> render-value/format] except that it is expected to put the entire value on a
single line with no newline after the value.

}

@defmethod[(render-value/format [value TST]
                                [settings settings]
                                [port port]
                                [width (or/c number (symbols 'infinity))])
           void?]{
This method is used to print values into a port, for display
to a user. The final argument is a maximum width to use (in
characters) when formatting the value.

This method is expected to format the value by inserting
newlines in appropriate places and is expected to render a
newline after the value.

See also
@method[drscheme:language:language<%> render-value].

}

@defmethod[(unmarshall-settings [input writable])
           (or/c settings false/c)]{
Translates a Scheme value into a settings, returning
@scheme[#f] if that is not possible.

}}

@(tools-include "language")
