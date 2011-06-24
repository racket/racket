#lang scribble/doc
@(require "common.rkt")

@defclass/title[printer-dc% object% (dc<%>)]{

A @racket[printer-dc%] object is a printer device context. A newly
 created @racket[printer-dc%] object obtains orientation (portrait
 versus landscape) and scaling information from the current
 @racket[ps-setup%] object, as determined by the
 @racket[current-ps-setup] parameter. This information can be
 configured by the user through a dialog shown by
 @racket[get-page-setup-from-user].

@|PrintNote|

See also @racket[post-script-dc%].

When a @racket[printer-dc%] object is created, the user gets
 platform-specific modal dialogs for configuring the output.
 If the user cancels the dialog, the @method[dc<%> ok?] method
 of the object returns @racket[#f].

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f])]{

If @racket[parent] is not @racket[#f], it is used as the parent window
 of the configuration dialog.


}}

