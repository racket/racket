#reader(lib "defreader.ss" "scribble")
@require["common.ss"]

@defclass[grow-box-spacer-pane% pane% ()]{

A @scheme[grow-box-spacer-pane%] object is intended for use as a
 lightweight spacer in the bottom-right corner of a frame, rather than
 as a container. Under Mac OS X, a
 @scheme[grow-box-spacer-pane%] has the same width and height as the
 grow box that is inset into the bottom-right corner of a frame. Under
 Windows and X, a @scheme[grow-box-spacer-pane%] has zero width and
 height. Unlike all other container types, a
 @scheme[grow-box-spacer-pane%] is unstretchable by default.





@defconstructor/auto-super[[#f unknown]]{
Passes all arguments to @scheme[super-init].
}}

