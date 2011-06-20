#lang scribble/doc
@(require "common.rkt")

@defclass/title[printer-dc% object% (dc<%>)]{

A @scheme[printer-dc%] object is a printer device context. A newly
 created @scheme[printer-dc%] object obtains orientation (portrait
 versus landscape) and scaling information from the current
 @scheme[ps-setup%] object, as determined by the
 @scheme[current-ps-setup] parameter. This information can be
 configured by the user through a dialog shown by
 @scheme[get-page-setup-from-user].

@|PrintNote|

See also @scheme[post-script-dc%].

When a @scheme[printer-dc%] object is created, the user gets
 platform-specific modal dialogs for configuring the output.
 If the user cancels the dialog, the @method[dc<%> ok?] method
 of the object returns @scheme[#f].

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f])]{

If @scheme[parent] is not @scheme[#f], it is used as the parent window
 of the configuration dialog.


}}

