#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework/framework))
@(require (for-label scheme/gui))
@title{Autosave}

@definterface[autosave:autosavable<%> ()]{
  Classes that implement this interface can be autosaved.
  @defmethod*[(((do-autosave) void))]{
    This method is called when the object is registered to be
    autosaved (see
    @scheme[autosave:register]).


  }
}
@(require framework/framework-docs)
@(def-fw-procs autosave)
