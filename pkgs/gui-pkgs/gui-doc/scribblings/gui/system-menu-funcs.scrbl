#lang scribble/doc
@(require "common.rkt")

@title{System Menus}


@defproc[(current-eventspace-has-standard-menus?)
         boolean?]{
Returns @racket[#t] for Mac OS X when the current eventspace is the
 initial one, since that eventspace is the target for the standard
 application menus. For any other system or eventspace, the result is
 @racket[#f].

This procedure is intended for use in deciding whether to include a
 @onscreen{Quit}, @onscreen{About}, and @onscreen{Preferences} menu
 item in a frame's menu. On Mac OS X, the application
 @onscreen{Quit} menu triggers a call to a frame's
@method[top-level-window<%> on-exit] method, the @onscreen{About} menu item is controlled by
 @racket[application-about-handler], and the
 @onscreen{Preferences} menu item is controlled by
 @racket[application-preferences-handler].

}

@defproc[(current-eventspace-has-menu-root?)
         boolean?]{
Returns @racket[#t] for Mac OS X when the current eventspace is the
 initial one, since that eventspace can supply a menu bar to be active
 when no frame is visible. For any other system or eventspace, the
 result is @racket[#f].

This procedure is intended for use in deciding whether to create a
 @racket[menu-bar%] instance with @racket['root] as its parent.

}

@defproc*[([(application-about-handler)
            (-> any)]
           [(application-about-handler [handler-thunk (-> any)])
            void?])]{

When the current eventspace is the initial eventspace, this
procedure retrieves or installs a thunk that is called when the
user selects the application @onscreen{About} menu item on Mac OS
X.  The thunk is always called in the initial eventspace's
handler thread (as a callback).

The default handler displays a generic Racket dialog.

If the current eventspace is not the initial eventspace, this
procedure returns @racket[void] (when called with zero arguments)
or has no effect (when called with a handler).

}


@defproc*[([(application-file-handler)
            (path? . -> . any)]
           [(application-file-handler [handler-proc (path? . -> . any)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a procedure that is called on Mac OS X
 and Windows when the application is running and user double-clicks an
 application-handled file or drags a file onto the application's
 icon. The procedure is always called in the initial eventspace's
 handler thread (as a callback), and the argument is a filename.

The default handler queues a callback to the
@method[window<%> on-drop-file] method of the most-recently activated frame in the main eventspace (see
@racket[get-top-level-edit-target-window]), if any such frame exists and if
 drag-and-drop is enabled for that frame. Otherwise, it saves
 the filename and re-queues the handler event when the application
 file handler is later changed or when a frame becomes active.

On Windows, when the application is @italic{not} running and user double-clicks an
 application-handled file or drags a file onto the application's icon,
 the filename is provided as a command-line argument to the
 application.

On Mac OS X, if an application is started @emph{without} files, then
 the @racket[application-start-empty-handler] procedure is called.

If the current eventspace is not the initial eventspace, this
procedure returns @racket[void] (when called with zero arguments)
or has no effect (when called with a handler).
}


@defproc*[([(application-preferences-handler)
            (or/c (-> any) #f)]
           [(application-preferences-handler [handler-thunk (or/c (-> any) #f)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a thunk that is called when the user selects
 the application @onscreen{Preferences} menu item on Mac OS X.  The
 thunk is always called in the initial eventspace's handler thread (as
 a callback). If the handler is set to @racket[#f], the
 @onscreen{Preferences} item is disabled.

The default handler is @racket[#f].

If the current eventspace is not the initial eventspace, this
procedure returns @racket[void] (when called with zero arguments)
or has no effect (when called with a handler).
}

@defproc*[([(application-quit-handler)
            (-> any)]
           [(application-quit-handler [handler-thunk (-> any)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a thunk that is called when the user requests
 that the application quit (e.g., through the @onscreen{Quit} menu
 item on Mac OS X, or when shutting down the machine in Windows). The
 thunk is always called in the initial eventspace's handler thread (as
 a callback). If the result of the thunk is @racket[#f], then the
 operating system is explicitly notified that the application does not
 intend to quit (on Windows).

The default handler queues a call to the
 @method[top-level-window<%> can-exit?] method of the most
 recently active frame in the initial eventspace (and then calls the
 frame's @method[top-level-window<%> on-exit] method if the
 result is true). The result is @racket[#t] if the eventspace is
 left with no open frames after
 @method[top-level-window<%> on-exit] returns, @racket[#f]
 otherwise.


If the current eventspace is not the initial eventspace, this
procedure returns @racket[void] (when called with zero arguments)
or has no effect (when called with a handler).
}


@defproc*[([(application-start-empty-handler)
            (-> any)]
           [(application-start-empty-handler [handler-thunk (-> any)])
            void?])]{
When the current eventspace is the initial eventspace, this procedure
 retrieves or installs a thunk that is called when the user starts
 the application on Mac OS X without supplying any initial files (e.g.,
 by double-clicking the application icon instead of double-clicking
 files that are handled by the application).

The default handler re-queues the handler event when the application
 start-empty handler is later changed. As a result, if an application
 sets both @racket[application-start-empty-handler] and
 @racket[application-file-handler], then one or the other is
 eventually called.

If the current eventspace is not the initial eventspace, this
procedure returns @racket[void] (when called with zero arguments)
or has no effect (when called with a handler).
}
