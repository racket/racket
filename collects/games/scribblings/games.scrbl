#lang scribble/doc
@(require "common.rkt"
          (for-label scheme/base scheme/contract
                     games/show-help games/show-scribbling))

@title{Games: Fun Examples}

The @exec{PLT Games} executable (or @exec{plt-games} on Unix) lets
you select one of the games distributed by PLT or other games
installed as sub-collections of the @filepath{games} collection (see
@secref["new-games"]).

@table-of-contents[]

@; ----------------------------------------------------------------------

@include-section["std-games.scrbl"]

@; ----------------------------------------------------------------------

@section[#:tag "new-games"]{Implementing New Games}

The game-starting console inspects the sub-collections of the
@filepath{games} collection. If a sub-collection has an
@filepath{info.rkt} module (see @racketmodname[setup/infotab]), the
following fields of the collection's @filepath{info.rkt} file are used:

@itemize[

 @item{@racketidfont{game} [required] : used as a module name in the
    sub-collection to load for the game; the module must provide a
    @racketidfont["game@"] unit (see @racketmodname[scheme/unit]) with
    no particular exports; the unit is invoked with no imports to
    start the game.}

 @item{@racketidfont{name} [defaults to the collection name] : used to
   label the game-starting button in the game console.}

 @item{@racketidfont{game-icon} [defaults to collection name with
   @filepath{.png}] : used as a path to a bitmap file that is used for
   the game button's label; this image should be 32 by 32 pixels and
   have a mask.}

 @item{@racketidfont{game-set} [defaults to @racket["Other Games"]] :
   a label used to group games that declare themselves to be in the
   same set.}

]

To implement card games, see @racketmodname[games/cards]. Card games
typically belong in the @racket["Cards"] game set.


@; ----------------------------------------------------------------------

@section{Showing Scribbled Help}

@defmodule[games/show-scribbling]

@defproc[(show-scribbling [mod-path module-path?]
                          [section-tag string?])
         (-> void?)]{

Returns a thunk for opening a Scribbled section in the user's HTML
browser. The @racket[mod-path] is the document's main source module,
and @racket[section-tag] specifies the section in the document.}

@; ----------------------------------------------------------------------

@section{Showing Text Help}

@defmodule[games/show-help]

@defproc[(show-help [coll-path (listof string?)]
                    [frame-title string?]
                    [verbatim? any/c #f])
         (-> any)]{

Returns a thunk for showing a help window based on plain
text. Multiple invocations of the thunk bring the same window to the
foreground (until the user closes the window).

The help window displays @filepath{doc.txt} from the collection
specified by @racket[coll-path].

The @racket[frame-title] argument is used for the help window title.

If @racket[verbatim?] is true, then @filepath{doc.txt} is displayed
verbatim, otherwise it is formatted as follows:

@itemize[
   
 @item{Any line of the form @litchar{**}....@litchar{**} is omitted.}

 @item{Any line that starts with @litchar{*} after whitespace is indented
       as a bullet point.}

 @item{Any line that contains only @litchar{-}s and is as long as the previous
       line causes the previous line to be formatted as a title.}
 
 @item{Other lines are paragraph-flowed to fit the window.}

]}
