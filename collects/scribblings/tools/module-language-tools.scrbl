#lang scribble/doc
@(require "common.rkt")

@tools-title["module-language-tools"]

@language-info-def[drracket:toolbar-buttons]{
If the result of @racket[read-language] for a language is a function, 
DrRacket will query it to determine if there are any new toolbar
buttons to be used when editing files in this language (when
DrRacket's language is set to the Module language).
}

Specifically, DrRacket will pass @indexed-racket['drracket:toolbar-buttons]
to the function and expect back a value matching this contract:
@racketblock[(or/c (listof (list/c string?
                                   (is-a?/c bitmap%)
                                   (-> (is-a?/c drracket:unit:frame<%>) any)
                                   (or/c real? #f)))
                   #f)]
which is then used to create new toolbar buttons, one for each list in the
first. The string is the label on the button; the bitmap is the icon 
(it should be 16x16); the function is called when the button is clicked;
and the number is passed as the @racket[#:number] argument to
@method[drracket:unit:frame<%> register-toolbar-button].

If the result is @racket[#f], then no toolbar buttons are created.

To implement functionality similar to the Run button, call the
@method[drracket:unit:frame% execute-callback] method. You may also
want to use the @racket[drracket:rep:after-expression] parameter.

If @racket['drracket:toolbar-buttons] is not recognized, DrRacket will also
pass @indexed-racket['drscheme:toolbar-buttons]; this is for backwards
compatibility and new code should not use it.
Similarly, if the fourth element from the list (the argument to @racket[#:number])
is not present, then it is treated as @racket[#f].


@(tools-include "module-language-tools")
