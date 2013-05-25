#lang scribble/doc
@(require scribble/manual
          (for-label framework/splash
                     racket/gui
                     racket/base))
@title{Splash}
@defmodule[framework/splash]

This module helps support applications with splash screens like the one in DrRacket.

When this module is invoked, it sets the @racket[current-load] parameter to a procedure
that counts how many files are loaded (until @racket[shutdown-splash] is called) and uses
that number to control the gauge along the bottom of the splash screen.

@defproc[(start-splash [draw-spec (or/c path-string?
                                        (is-a?/c bitmap%)
                                        (vector/c (or/c (-> (is-a?/c dc<%>) void?)
                                                        (-> (is-a?/c dc<%>) 
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?
                                                            exact-nonnegative-integer?
                                                            void?))
                                                  exact-nonnegative-integer?
                                                  exact-nonnegative-integer?))]
                       [splash-title string?]
                       [width-default exact-nonnegative-integer?]
                       [#:allow-funny? allow-funny? boolean? #f]
                       [#:frame-icon
                        frame-icon
                        (or/c #f
                              (is-a?/c bitmap%)
                              (cons/c (is-a?/c bitmap%)
                                      (is-a?/c bitmap%)))
                        #f])
         void?]{
  Starts a new splash screen. The splash screen is created in its own, new 
  @tech[#:doc '(lib "scribblings/gui/gui.scrbl") #:key "eventspace"]{eventspace}.
  The progress gauge at the bottom of the window advances as files are loaded
  (monitored via the @racket[current-load] parameter).
  
  The @racket[draw-spec] determines what the splash window contains. 
  The @racket[splash-title] is used as the title of the window and the @racket[width-default] determines
  how many progress steps the gauge in the splash screen should
  contain if there is no preference saved for the splash screen width.
  The splash library uses @racket[get-preference] and @racket[put-preferences]
  to store preferences, using 
  @racketblock[(string->symbol (format "plt:~a-splash-max-width" splash-title))]
  as the key for the preference. Each time the app starts up, the maximum width 
  is reset based on the number of files that were loaded that time.
  
  If the @racket[draw-spec] is a @racket[path-string?], then the path is expected to be a file
  that contains a bitmap that is drawn as the contents of the splash screen. If it is
  a bitmap, then that bitmap is used directly. If @racket[draw-spec]
  is a vector, then the vector's first element is a procedure that is called to draw
  the splash screen and the other two integers are the size of the splash screen, width followed by height.
  If the procedure accepts only one argument, then it is called with a @racket[dc<%>] object where the
  drawing should occur. If it accepts 5 arguments, it is called with the @racket[dc<%>], as well as
  (in order) the current value of the gauge, the maximum value of the gauge, and the width and the height
  of the area to draw.

  The @racket[allow-funny?] argument determines if a special gauge is used on Christmas day.  

  The @racket[frame-icon] is used just like the value of the parameter @racket[frame:current-icon] is used,
  but for the splash screen.
}

@defproc[(shutdown-splash) void?]{
  Stops the splash window's gauge from advancing. Call this after all of the files have been loaded.
}

@defproc[(close-splash) void?]{
  Closes the splash window. Call @racket[shutdown-splash] first. You can leave some time between these two
  if there is more initialization work to be done where you do not want to count loaded files.
}

@defproc[(add-splash-icon [bmp (is-a?/c bitmap%)] [x exact-nonnegative-integer?] [y exact-nonnegative-integer?])
         void?]{
  Adds an icon to the splash screen. (DrRacket uses this function to show the tools as they are loaded.)                
}

@defproc[(get-splash-bitmap) (or/c #f (is-a?/c bitmap%))]{Returns the splash bitmap unless one has not been set.}
@defproc[(set-splash-bitmap [bmp (is-a?/c bitmap%)]) void?]{
  Sets the splash bitmap to @racket[bmp] and triggers a redrawing of the splash screen. Don't use this to set
  the initial bitmap, use @racket[start-splash] instead.
}
@defproc[(get-splash-canvas) (is-a?/c canvas%)]{
  Returns the canvas where the splash screen bitmap is drawn (if there is a bitmap); see @racket[start-splash] for how the splash is drawn.
}
@defproc[(get-splash-eventspace) eventspace?]{
  Returns the splash screen's eventspace.
}
@defproc[(get-splash-paint-callback)
         (-> (is-a?/c dc<%>) 
             exact-nonnegative-integer?
             exact-nonnegative-integer?
             exact-nonnegative-integer?
             exact-nonnegative-integer?
             void?)]{
  Returns the callback that is invoked when redrawing the splash screen.
}
@defproc[(set-splash-paint-callback
          [cb
           (-> (is-a?/c dc<%>) 
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               void?)])
         void?]{
  Sets the callback that is invoked when redrawing the splash screen. See @racket[start-splash] for
  what the arguments are.
}
@defproc[(set-splash-progress-bar?! [b boolean?]) void?]{
  Calling this procedure with @racket[#f] removes the progress bar from the splash screen.
  Useful in conjunction with setting your own paint callback for the splash screen that measures
  progress in its own way, during drawing. DrRacket uses this on King Kamehameha and Prince
  Kuhio day.
}
@defproc[(set-splash-char-observer [obs (-> (is-a?/c key-event%) any)]) void?]{
  Sets a procedure that is called whenever a user types a key with the splash screen as the focus.
}
@defproc[(set-splash-event-callback [obj (-> (is-?/c mouse-event%) any)]) void?]{
  Sets a procedure that is called whenever a mouse event happens in the splash canvas.                                             }
@defproc[(get-splash-event-callback) (-> (is-?/c mouse-event%) any)]{
  Returns the last procedure passed to @racket[set-splash-event-callback] or @racket[void], if
  @racket[set-splash-event-callback] has not been called.
}
@defproc[(set-refresh-splash-on-gauge-change?! [proc (-> exact-nonnegative-integer? 
                                                         exact-nonnegative-integer? 
                                                         any)]) 
         void?]{
  Sets a procedure that is called each time the splash gauge changes. If the procedure returns a true value (i.e., not @racket[#f]),
  then the splash screen is redrawn. The procedure is called with the current value of the gauge and the maximum value.
  
  The default function is @racket[(lambda (curr tot) #f)].
}
@defproc[(get-splash-width) exact-nonnegative-integer?]{Returns the width of the splash drawing area / bitmap. See @racket[start-splash] for the details of the size and how things are drawn.}
@defproc[(get-splash-height) exact-nonnegative-integer?]{Returns the width of the splash drawing area / bitmap. See @racket[start-splash] for the details of the size and how things are drawn.}
@defproc[(refresh-splash) void?]{
  Triggers a refresh of the splash, handling the details of double buffering
  and doing the drawing on the splash's
  @tech[#:doc '(lib "scribblings/gui/gui.scrbl") #:key "eventspace"]{eventspace's}
  main thread.
}
