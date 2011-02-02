#lang scribble/doc
@(require "common.ss"
          (for-label framework))

@title{@bold{Embedded GUI}: Widgets within @scheme[editor<%>]}

@author["Mike T. McHenry"]

@defmodule[embedded-gui]

The @schememodname[embedded-gui] library provides a class hierarchy
for creating graphical boxes within @scheme[editor<%>] objects with
geometry management that mirrors that of @scheme[vertical-panel%] and
@scheme[horizontal-panel%].

@table-of-contents[]

@include-section["containers.scrbl"]
@include-section["controls.scrbl"]
@include-section["control-snips.scrbl"]

@; ----------------------------------------------------------------------

@section{Helpers}

@defmixin[stretchable-editor-snip-mixin (editor-snip%) (stretchable-snip<%>)]{

 Extends an editor snip the @scheme[stretchable-snip<%>] interface,
 which allows it to be stretched to fit an
 @scheme[alignment-parent<%>]'s allotted width. Stretchable snips are
 useful as the snip of a @scheme[snip-wrapper%] }


@defclass[stretchable-editor-snip% editor-snip% (stretchable-editor-snip-mixin editor-snip%)]{

 @defconstructor[([stretchable-width boolean? #t]
                  [stretchable-height boolean? #t])]{

 Creates a stretchable snip with the given initial stretchability.}}


@defproc[(fixed-width-label-snip [possible-labels (listof string?)])
         (subclass?/c snip%)]{

 Returns a subclass of @scheme[snip%] that takes a single
 initialization argument. The argument provided when instantiating the
 class must be a member of @scheme[possible-labels]; the given label
 is displayed by the snip, but the snip is sized to match the longest
 of the labels in @scheme[possible-labels].

 In other words, the resulting class helps align multiple GUI elements
t   hat are labeled from a particular set of strings.}


@definterface[tabbable-text<%> ()]{

 An interface for tabbing between embedded @scheme[text%]s.

 @defmethod[(set-caret-owner) void?]{

 Moves the caret into the @scheme[tabbable-text<%>].}

 @defmethod[(set-ahead) void?]{

 Called when tabbing ahead.}

 @defmethod[ (set-back) void?]{

 Called when tabbing backward.}}


@defmixin[tabbable-text-mixin (editor:keymap<%>) (tabbable-text<%>)]{

  Adds the @scheme[tabbable-text<%>] interface to an
  @scheme[editor:text%] class, where instantiation installs key
  bindings to tab ahead and backward}


@defproc[(set-tabbing [a-text (is-a?/c tabbable-text<%>)] ...)
          void?]{

Sets the tabbing order of @scheme[tabbable-text<%>]s by setting each
text's @method[tabbable-text<%> set-ahead] and
@method[tabbable-text<%> set-back] thunks to point to its neighbor in
the argument list.}


@defmixin[grey-editor-snip-mixin (editor-snip%) ()]{

 Gives an @scheme[editor-snip%] a colored background indicating that
 is disabled. The editor is not disabled by the mixin however, and
 must be locked separately.}

@defmixin[grey-editor-mixin (editor<%>) ()]{

 Gives an @scheme[editor<%>] a colored background indicating that is
 disabled. The editor is not disabled by the mixin however, and must be
 locked separately.}


@defmixin[single-line-text-mixin (editor:keymap<%>) ()]{

 Restricts a text to one line by overriding its key bindings to do
 nothing on enter.}


@defmixin[cue-text-mixin (text%) ()]{

 Gives a @scheme[text%] an instantiation argument of a string that is
 displayed in the @scheme[text%] initially in grey; the text
 disappears when the text gets focus. This technique is useful for
 labeling texts without needing to take up space.}


@defclass[cue-text% (cue-text-mixin text%) ()]{

 @defconstructor[([cue-text string? ""]
                  [color string? "gray"]
                  [behavior (listof (one-of/c 'on-focus 'on-char)) '(on-focus)])]{

  Creates an instance with the given initial content, color, and
  behvior for when to clear the text.}

 @defmethod[(clear-cue-text) void?]{

  Clears the cue text, if it's still present.}

}

@; ----------------------------------------------------------------------

@include-section["snip-procs.scrbl"]
