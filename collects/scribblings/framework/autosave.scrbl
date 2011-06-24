#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Autosave}

@definterface[autosave:autosavable<%> ()]{
  Classes that implement this interface can be autosaved.
  @defmethod*[(((do-autosave) void?))]{
    This method is called when the object is registered to be
    autosaved (see @racket[autosave:register]).
  }
}

@(include-previously-extracted "main-extracts.rkt" #rx"^autosave:")
