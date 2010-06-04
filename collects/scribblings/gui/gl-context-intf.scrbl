#lang scribble/doc
@(require "common.ss")

@definterface/title[gl-context<%> ()]{

A @scheme[gl-context<%>] object represents a context for drawing with
 @as-index{OpenGL} to a specific @scheme[dc<%>] instance. To obtain a
 @scheme[gl-context<%>] object, call @method[dc<%> get-gl-context] of
 the target drawing context.

Only canvas @scheme[dc<%>] and @scheme[bitmap-dc%] objects support
 OpenGL (always under Windows and Mac OS X, sometimes under X), and in
 the case of a @scheme[bitmap-dc%], the context is usable only when
 the target bitmap is non-monochrome. When the target bitmap for a
 @scheme[bitmap-dc%] context is changed via @method[bitmap-dc%
 set-bitmap], the associated OpenGL context is reset, but the
 @scheme[gl-context<%>] keeps its identity. Canvas contexts are double
 buffered, and bitmap contexts are single buffered.

The @schememodname[racket/gui/base] library provides no OpenGL
 routines. Instead, they must be obtained from a separate library,
 such as @schememodname[sgl]. The facilities in
 @schememodname[racket/gui/base] merely manage the current OpenGL
 context, connecting it to windows and bitmaps.

Only one OpenGL context can be active at a time across all threads and
 eventspaces. Except under Mac OS X, OpenGL contexts are not protected
 against interference among threads; that is, if a thread selects one
 of its OpenGL contexts, then other threads can write into the context
 via OpenGL commands. However, if all threads issue OpenGL commands
 only within a thunk passed to @method[gl-context<%> call-as-current],
 then drawing from the separate threads will not interfere, because
 @method[gl-context<%> call-as-current] uses a lock to serialize
 context selection across all threads in Racket.


@defmethod[(call-as-current [thunk (-> any)]
                            [alternate evt? never-evt]
                            [enable-breaks? any/c #f])
           any/c]{

Calls a thunk with this OpenGL context as the current context for
 OpenGL commands.

The method blocks to obtain a lock that protects the global OpenGL
 context, and it releases the lock when the thunk returns or
 escapes. The lock is re-entrant, so a nested use of the method in the
 same thread with the same OpenGL context does not obtain or release
 the lock.

The lock prevents interference among OpenGL-using threads.  If a
 thread is terminated while holding the context lock, the lock is
 released. Continuation jumps into the thunk do not grab the lock or
 set the OpenGL context. See @scheme[gl-context<%>] for more
 information on interference.

The method accepts an alternate @tech[#:doc
 reference-doc]{synchronizable event} for use while blocking for the
 context lock; see also @scheme[sync].

The result of the method call is the result of the thunk if it is
 called, or the result of the alternate event if it is chosen instead
 of the context lock.

If @method[gl-context<%> ok?] returns @scheme[#f] at the time that
 this method is called, then @|MismatchExn|.

If @scheme[enable-breaks?] is true, then the method uses
 @scheme[sync/enable-break] while blocking for the context-setting
 lock instead of @scheme[sync].

}

@defmethod[(ok?)
           boolean?]{

Returns @scheme[#t] if this context is available OpenGL drawing,
 @scheme[#f] otherwise.

A context is unavailable if OpenGL support is disabled at compile time
 or run time, if the context is associated with a
 @scheme[bitmap-dc%] with no selected bitmap or with a monochrome
 selected bitmap, if the context is for a canvas that no longer
 exists, or if there was a low-level error when preparing the context.

}

@defmethod[(swap-buffers)
           void?]{

Swaps the front (visible) and back (OpenGL-drawing) buffer for a
 context associated with a canvas, and has no effect on a bitmap
 context.

This method implicitly uses @method[gl-context<%> call-as-current] to
 obtain the context lock. Since the lock is re-entrant, however, the
 @method[gl-context<%> swap-buffers] method can be safely used within
 a @method[gl-context<%> call-as-current] thunk.

}}
