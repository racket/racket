#lang scribble/doc
@(require "common.ss")

@(tools-title "module-language-tools")
@section-index["drscheme:toolbar-buttons"]

If the result of @scheme[read-language] for a language is a function, 
DrScheme will query it to determine if there are any new toolbar
buttons to be used when editing files in this language (when
DrScheme's language is set to the Module language).

Specifically, DrScheme will pass @scheme['drscheme:toolbar-buttons]
to the function and expect back a value matching this contract:
@schemeblock[(listof (list/c string?
                             (is-a?/c bitmap%)
                             (-> (is-a?/c drracket:unit:frame<%>) any)))]
which is then used to create new toolbar buttons, one for each list in the
first. The string is the label on the button; the bitmap is the icon (it should be 16x16),
and the function is called when the button is clicked.

@(tools-include "module-language-tools")
