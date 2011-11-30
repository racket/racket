#lang scribble/doc

@(require (for-label framework framework/framework-unit framework/framework-sig racket/gui)
          (for-syntax framework/private/scheme racket/base)
          scribble/manual)

@title{Framework: Racket GUI Application Framework}
@(defmodule framework)

@author["Robert Bruce Findler" "Matthew Flatt"]

The framework provides a number of mixins, classes and
functions designed to help you build a complete application
program on top of the @racket[racket/gui] library.


@bold{Thanks}
Thanks to Shriram Krishnamurthi, Cormac Flanagan, Matthias
Felleisen, Ian Barland, Gann Bierner, Richard Cobbe, Dan
Grossman, Stephanie Weirich, Paul Steckler, Sebastian Good,
Johnathan Franklin, Mark Krentel, Corky Cartwright, Michael
Ernst, Kennis Koldewyn, Bruce Duba, and many others for
their feedback and help.


@section{Framework Libraries Overview}

@itemize[
@item{Entire Framework: @racketmodname[framework]

  This library provides all of the definitions and syntax
  described in this manual.
}
 
@item{Test Suite Engine: @racketmodname[framework/test]

This library provides all of the definitions beginning with
@racket[test:] described in this manual.
}

@item{GUI Utilities @racketmodname[framework/gui-utils]

    This libraries provides all of the definitions beginning
    with @racket[gui-utils:] described in this manual.
}
@item{Preferences @racketmodname[framework/preferences]
    
  This library provides a subset of the names of the
  @racketmodname[framework] library, namely those for
  manipulating preference settings and is designed to be
  used from @exec{racket}.
}

@item{Splash Screen @racketmodname[framework/splash]  
       This library provides support for a splash screen. See
       @racketmodname[framework/splash] for more.
}]
        
@include-section["application.scrbl"]
@include-section["autosave.scrbl"]
@include-section["canvas.scrbl"]
@include-section["color-model.scrbl"]
@include-section["color-prefs.scrbl"]
@include-section["color.scrbl"]
@include-section["comment-box.scrbl"]

@section{Decorated Editor Snip}

@defmodule[framework/decorated-editor-snip]

This library is here for backwards compatibility. The
functionality in it has moved into the framework proper, in
the @secref["editor-snip"] section.

 @defidform[decorated-editor-snip%]{
    Use @racket[editor-snip:decorated%] instead.
  }
 @defidform[decorated-editor-snipclass%]{
    Use @racket[editor-snip:decorated-snipclass%] instead.
  }
 @defidform[decorated-editor-snip-mixin]{
    Use @racket[editor-snip:decorated-mixin] instead.
  }
 @defidform[decorated-editor-snip<%>]{
    Use @racket[editor-snip:decorated<%>] instead.
  }


@include-section["editor-snip.scrbl"]
@include-section["editor.scrbl"]
@include-section["exit.scrbl"]
@include-section["finder.scrbl"]
@include-section["frame.scrbl"]
@include-section["group.scrbl"]
@include-section["gui-utils.scrbl"]
@include-section["handler.scrbl"]
@include-section["icon.scrbl"]
@include-section["keymap.scrbl"]
@include-section["menu.scrbl"]
@include-section["mode.scrbl"]
@include-section["number-snip.scrbl"]
@include-section["panel.scrbl"]
@include-section["pasteboard.scrbl"]
@include-section["path-utils.scrbl"]
@include-section["preferences.scrbl"]
@include-section["preferences-text.scrbl"]
@include-section["racket.scrbl"]
@include-section["text.scrbl"]
@include-section["splash.scrbl"]
@include-section["test.scrbl"]
@include-section["version.scrbl"]

@section{Backwards Compatibility}

@(define-syntax (scheme:-docs stx)
   #`(begin
       #,@(for/list ([suffix (in-list racket:ids)])
            (define scheme:id (string->symbol (format "scheme:~a" suffix)))
            (define racket:id (string->symbol (format "racket:~a" suffix)))
            #`@defidform[#,scheme:id]{An alias for @racket[#,racket:id].})))
@(scheme:-docs)

@section{Signatures}

@defmodule[framework/framework-sig]

@defsignature[framework^ ()]{
  Contains of the names of the procedures in this
  manual, except   those that begin with @racket[test:] and
  @racket[gui-utils:]. 
}


@defsignature[framework-class^ ()]{
  Contains all of the classes defined in this
  manual.
}

@section{Unit}

@defmodule[framework/framework-unit]

@defthing[framework@ unit?]{
  Exports the signature
  @racket[framework^] and imports the @racket[mred^] signature.
}

@index-section[]
