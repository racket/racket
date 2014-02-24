#lang scribble/doc
@(require scribble/manual scribble/extract
          (for-label framework scheme/gui)
          (for-syntax "mode-helpers.rkt"))
@title{Mode}

@(begin
(require framework/private/mode (for-syntax scheme/base))

(define-syntax (docs stx)
  (syntax-case stx ()
   [(_ spec ...)
   #`(begin
    @definterface[mode:surrogate-text<%> ()]{
      @defmethod[(on-enable-surrogate [txt (is-a?/c text%)]) any]{
        Called by @method[mode:host-text<%> set-surrogate] to notify the
        surrogate that it has just become active.
      }
      @defmethod[(on-disable-surrogate [txt (is-a?/c text%)]) any]{
        Called by @method[mode:host-text<%> set-surrogate] to notify the
        surrogate that it has just been disabled.
      }
    }
    @defclass[mode:surrogate-text% object% (mode:surrogate-text<%>)]{
      @#,@(map spec->surrogate-method (syntax->list #'(spec ...)))
    }
    @definterface[mode:host-text<%> ()]{
      @defmethod[(get-surrogate)
                 (or/c false/c (is-a?/c mode:surrogate-text<%>))]{
       Returns the currently active surrogate.
      }
      @defmethod[(set-surrogate
                  [surrogate (or/c false/c (is-a?/c mode:surrogate-text<%>))])
                 void?]{
        Sets the current surrogate to @racket[surrogate].
      }
    }
    @defmixin[mode:host-text-mixin () (mode:host-text<%>)]{
      @#,@(map spec->host-method (syntax->list #'(spec ...)))
    })]))

(surrogate-methods docs)
)

@(include-previously-extracted "main-extracts.rkt" #rx"^mode:")
