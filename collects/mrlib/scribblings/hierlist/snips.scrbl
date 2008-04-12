#lang scribble/doc
@(require "../common.ss"
          (for-label mrlib/hierlist))

@title{Snips in a @scheme[hierarchical-list%] Instance}

The @xmethod[text% find-snip] method of the editor in a
@scheme[hierarchical-list%] return instances of
@scheme[hierarchical-item-snip%] and @scheme[hierarchical-list-snip%].

@defclass[hierarchical-item-snip% editor-snip% ()]{

 @defmethod[(get-item) (is-a?/c hierarchical-list-item<%>)]{

 Returns the @scheme[hierarchical-list-item<%>] corresponding to the
 snip.}

}


@defclass[hierarchical-list-snip% editor-snip% ()]{

 @defmethod[(get-item) (is-a?/c hierarchical-list-compound-item<%>)]{

 Returns the @scheme[hierarchical-list-compound-item<%>] corresponding to the
 snip.}

 @defmethod[(get-content-buffer) (is-a?/c text%)]{

 Returns the text% that contains the sub-item snips.}

}
