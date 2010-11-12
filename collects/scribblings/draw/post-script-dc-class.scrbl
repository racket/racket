#lang scribble/doc
@(require "common.ss")

@defclass/title[post-script-dc% object% (dc<%>)]{

A @scheme[post-script-dc%] object is a PostScript device context, that
 can write PostScript files on any platform. See also
 @scheme[ps-setup%].

@|PrintNote|

See also @scheme[printer-dc%].


@defconstructor[([interactive any/c #t]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) false/c) #f]
                 [use-paper-bbox any/c #f]
                 [as-eps any/c #t])]{

If @scheme[interactive] is true, the user is given a dialog for
 setting printing parameters (see @scheme[get-ps-setup-from-user]);
 the resulting configuration is installed as the current
 configuration). If the user chooses to print to a file (the only
 possibility under Windows and Mac OS X), another dialog is given to
 select the filename.  If the user hits cancel in either of these
 dialogs, then @method[dc<%> ok?] returns @scheme[#f].

If @scheme[parent] is not @scheme[#f], it is used as the parent window of
 the configuration dialog.

If @scheme[interactive] is @scheme[#f], then the settings returned by
 @scheme[current-ps-setup] are used. A file dialog is still presented
 to the user if the @method[ps-setup% get-file] method returns
 @scheme[#f], and the user may hit cancel in that case so that
 @method[dc<%> ok?] returns @scheme[#f].

If @scheme[use-paper-bbox] is @scheme[#f], then the PostScript
 bounding box for the output is determined by drawing commands issued
 to the object; such a bounding box encloses all parts of the drawing
 @italic{ignoring} clipping regions (so the bounding box may be
 approximate). If @scheme[use-paper-bbox] is not @scheme[#f], then the
 bounding box is determined by the current paper size (as specified by
 @scheme[current-ps-setup]), and the bounding box does not include the
 margin (also specified by @scheme[current-ps-setup]).

@index["Encapsulated PostScript (EPS)"]{If} @scheme[as-eps] is
 @scheme[#f], then the generated PostScript does not include an
 Encapsulated PostScript (EPS) header, and instead includes a generic
 PostScript header. Otherwise, the generated PostScript includes a
 header that identifiers it as EPS.

See also @scheme[ps-setup%] and @scheme[current-ps-setup]. The
settings for a particular @scheme[post-script-dc%] object are fixed to
the values in the current configuration when the object is created
(after the user has interactively adjusted them when
@scheme[interactive] is true).

}}
