#lang scribble/doc

@(require scribble/manual)
@(defmodule framework/framework)

@title{@bold{Framework}: PLT GUI Application Framework}

The framework provides these libraries:
@itemize{

@item{@bold{Entire Framework}

@itemize{

@item{@scheme[(require framework/framework)]

  This library provides all of the definitions and syntax
  described in this manual.
}
@item{@scheme[(require (lib "framework-sig.ss" "framework"))]
  
  This library provides the signature definitions:
  @scheme[framework^], and
  @scheme[framework-class^].
  The @scheme[framework^] signature contains all of the 
  names of the procedures described in this manual, except
  those that begin with @scheme[test:] and
  @scheme[gui-utils:]. The @scheme[framework-class^]
  signature contains all of the classes defined in this
  manual.
}
@item{@scheme[(require (lib "framework-unit.ss" "framework"))]

  This library provides one
  @scheme[unit/sig]: @scheme[framework@]. It exports the signature
  @scheme[framework^]. It imports the @scheme[mred^] signature.

}
}}
@item{
 @bold{Test Suite Engine}

@scheme[(require (lib "test.ss" "framework"))]

This library provides all of the definitions beginning with
@scheme[test:] described in this manual.
}
@item{ @bold{GUI Utilities}
@scheme[(require (lib "gui-utils.ss" "framework"))]
    
    This libraries provides all of the definitions beginning
    with \scheme{gui-utils:} described in this manual.
}
@item{ @bold{Preferences}
@scheme[(require (lib "preferences.ss" "framework"))]
    
  This library provides a subset of the names of the
  \scheme|framework.ss| library, namely those for
  manipulating preference settings and is designed to be
  used from mzscheme.

The precise set of exported names is:
@scheme[preferences:snapshot?],
@scheme[preferences:restore-prefs-snapshot],
@scheme[preferences:get-prefs-snapshot],
@scheme[exn:make-unknown-preference],
@scheme[exn:unknown-preference?],
@scheme[preferences:low-level-put-preferences],
@scheme[preferences:get],
@scheme[preferences:set],
@scheme[preferences:add-callback],
@scheme[preferences:set-default],
@scheme[preferences:set-un/marshall], and
@scheme[preferences:restore-defaults].
}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
@section{Thanks}

Thanks to Shriram Krishnamurthi, Cormac Flanagan, Matthias
Felleisen, Ian Barland, Gann Bierner, Richard Cobbe, Dan
Grossman, Stephanie Weirich, Paul Steckler, Sebastian Good,
Johnathan Franklin, Mark Krentel, Corky Cartwright, Michael
Ernst, Kennis Koldewyn, Bruce Duba, and many others for
their feedback and help.


@include-section["framework-application.scrbl"]
@include-section["framework-autosave.scrbl"]
@include-section["framework-canvas.scrbl"]
@include-section["framework-color-model.scrbl"]
@include-section["framework-color-prefs.scrbl"]
@include-section["framework-color.scrbl"]
@include-section["framework-comment-box.scrbl"]
@include-section["framework-editor.scrbl"]
@include-section["framework-exit.scrbl"]
@include-section["framework-finder.scrbl"]
@include-section["framework-frame.scrbl"]
@include-section["framework-group.scrbl"]
@include-section["framework-handler.scrbl"]
@include-section["framework-icon.scrbl"]
@include-section["framework-keymap.scrbl"]
@;include-section["framework-main.scrbl"]
@include-section["framework-menu.scrbl"]
@;include-section["framework-mode.scrbl"]
@include-section["framework-number-snip.scrbl"]
@include-section["framework-panel.scrbl"]
@include-section["framework-pasteboard.scrbl"]
@include-section["framework-path-utils.scrbl"]
@include-section["framework-preferences.scrbl"]
@include-section["framework-scheme.scrbl"]
@include-section["framework-text.scrbl"]
@include-section["framework-test.scrbl"]
@include-section["framework-version.scrbl"]

@index-section[]
