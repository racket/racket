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

When the @method[dc<%> end-doc] method is called on a
 @racket[printer-dc%] instance, the user may receive a dialog
 to determine how the document is printed.

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f])]{

If @racket[parent] is not @racket[#f], it is may be as the parent window
 of the dialog (if any) presented by @method[dc<%> end-doc].


}}

