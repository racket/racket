#lang scribble/doc
@(require "common.rkt" scribble/struct)

@(define (atable . l)
   (make-table #f (map (lambda (i)
                         (map (lambda (e)
                                (make-flow (list (make-paragraph (list e)))))
                              i))
                       l)))
@(define (tline l r)
   (list (hspace 2) l (hspace 1) 'rarr (hspace 1) r))


@title{Miscellaneous}

@defproc[(begin-busy-cursor) void?]{

Changes the cursor to a watch cursor for all windows in the current eventspace.
Use @racket[end-busy-cursor] to revert the cursor back to its previous
state. Calls to @racket[begin-busy-cursor] and @racket[end-busy-cursor] can be
nested arbitrarily.

The cursor installed by @racket[begin-busy-cursor] overrides any
window-specific cursors installed with @method[window<%> set-cursor].

See also @racket[is-busy?].
}

@defproc[(bell) void?]{
Rings the system bell.
}


@defproc[(dimension-integer? [v any/c]) boolean?]{

Equivalent to @racket[(integer-in 0 1000000)].

Beware that certain kinds of windows behave badly when larger than
32,000 or so in either dimension on some platforms. Redraw of the
window may be disabled or clipped, for example.}


@defproc[(end-busy-cursor) void?]{
See @racket[begin-busy-cursor].
}

@defproc*[([(file-creator-and-type [filename path?]
                                   [creator-string (lambda (s) (and (bytes? s)
                                                                    (= 4 (bytes-length s))))]
                                   [type-bytes (lambda (s) (and (bytes? s)
                                                                 (= 4 (bytes-length s))))])
            void?]
           [(file-creator-and-type [filename path?])
            (values (lambda (s) (and (bytes? s)
                                (= 4 (bytes-length s))))
                    (lambda (s) (and (bytes? s)
                                (= 4 (bytes-length s)))))])]{

Gets or sets the creator and type of a file in Mac OS X.

The get operation always returns @racket[#"????"] and @racket[#"????"] for
 Unix or Windows. The set operation has no effect on Unix or
 Windows.
}

@defproc[(find-graphical-system-path [what (or/c 'init-file 'x-display)])
         (or/c path? #f)]{

Finds a platform-specific (and possibly user- or machine-specific)
 standard filename or directory. See also @racket[find-system-path].

The result depends on @racket[what], and a @racket[#f] result is only
 possible when @racket[what] is @racket['x-display]:

@itemize[

 @item{@racket['init-file] returns the ,path to the user-specific
 initialization file (containing Racket code). The directory part of
 the path is the same path as returned for @racket['init-dir] by
 Racket's @racket[find-system-path].  The file name is
 platform-specific:
  @itemize[

  @item{@|AllUnix|: @indexed-file{.gracketrc}}
  @item{Windows: @indexed-file{gracketrc.rktl}}

  ]}

 @item{@racket['x-display] returns a ``path'' whose string identifies
 the X11 display if specified by either the @Flag{display} flag or the
 @envvar{DISPLAY} environment variable when GRacket starts on Unix. For
 other platforms, or when neither @Flag{display} nor @envvar{DISPLAY}
 was specified, the result is @racket[#f].}

]

}

@defproc[(get-default-shortcut-prefix)
         (case (system-type)
           [(windows) (list/c 'ctl)]
           [(macosx)  (list/c 'cmd)]
           [(unix)    (list/c (or/c 'alt 'cmd 'meta 'ctl 'shift 'option))])]{
Returns an immutable list specifying the default prefix for menu
shortcuts. See also
@xmethod[selectable-menu-item<%> get-shortcut-prefix].

On Windows, the default is @racket['(ctl)]. On Mac OS X, the
default is @racket['(cmd)]. On Unix, the default is normally
@racket['(ctl)], but the default can be changed through the
@Resource{defaultMenuPrefix} low-level preference (see
@|mrprefsdiscuss|).}

@defproc[(get-panel-background)
         (is-a?/c color%)]{

Returns a shade of gray.

Historically, the result matched the color of
a @racket[panel%] background, but @racket[panel%] backgrounds can vary
on some platforms (e.g., when nested in a @racket[group-box-panel%]),
so the result is no longer guaranteed to be related to a
@racket[panel%]'s color.
}


@defproc[(get-highlight-background-color) (is-a?/c color%)]{

Returns the color that is drawn behind selected text.}


@defproc[(get-highlight-text-color) (or/c (is-a?/c color%) #f)]{

Returns the color that is used to draw selected text or @racket[#f] if
selected text is drawn with its usual color.}


@defproc[(get-window-text-extent [string string?]
                                 [font (is-a?/c font%)]
                                 [combine? any/c #f])
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{

Returns the pixel size of a string drawn as a window's label or value
when drawn with the given font. The optional @racket[combine?]
argument is as for @xmethod[dc<%> get-text-extent].

See also @xmethod[dc<%> get-text-extent].
}

@defproc[(graphical-read-eval-print-loop [eval-eventspace (or/c eventspace? #f) #f]
                                         [redirect-ports? any/c (not eval-eventspace)])
         void?]{

Similar to @racket[read-eval-print-loop], except that none of
 @racket[read-eval-print-loop]'s configuration parameters are used (such
 as @racket[current-read]) and the interaction occurs in a GUI window
 instead of using the current input and output ports.

Expressions entered into the graphical read-eval-print loop can be
 evaluated in an eventspace (and thread) that is distinct from the one
 implementing the @racket[graphical-read-eval-print-loop]
 window (i.e., the current eventspace when
 @racket[graphical-read-eval-print-loop] is called).

If no eventspace is provided, or if @racket[#f] is provided, an
 evaluation eventspace is created using @racket[(make-eventspace)]
 with a new custodian; the eventspace and its threads are be shut down
 when the user closes the @racket[graphical-read-eval-print-loop]
 window. If an eventspace is provided, closing the window performs no
 shut-down actions on eventspace.

When @racket[redirect-ports?] is true, the following parameters are
 initialized in the created eventspace's handler thread:
@itemize[

 @item{@racket[current-output-port] --- writes to the frame}
 @item{@racket[current-error-port] --- writes to the frame}
 @item{@racket[current-input-port] --- always returns @racket[eof]}

]

The keymap for the read-eval-print loop's editor is initialized by
 calling the current keymap initializer procedure, which is determined
 by the
@racket[current-text-keymap-initializer] parameter.
}

@defproc[(textual-read-eval-print-loop) void?]{

Similar to @racket[read-eval-print-loop], except that evaluation uses
 a newly created eventspace like @racket[graphical-read-eval-print-loop].

The @racket[current-prompt-read] parameter is used in the current
 thread to read input. The result is queued for evaluation and
 printing in the created eventspace's @tech{handler thread}, which
 uses @racket[current-eval] and @racket[current-print]. After printing
 completes for an interaction result, the next expression in read in
 the original thread, and so on.

If an @racket[exn:break] exception is raised in the original thread
during reading, it aborts the current call to @racket[(current-read)]
and a new one is started. If an @racket[exn:break] exception is raised
in the original thread while waiting for an interaction to complete, a
break is sent (via @racket[break-thread]) to the created eventspace's
@tech{handler thread}.}


@defproc[(get-current-mouse-state) (values (is-a?/c point%)
                                           (listof (or/c 'left 'middle 'right
                                                         'shift 'control 'alt 'meta 'caps)))]{

@margin-note{On Mac OS X 10.5 and earlier, mouse-button information is
not available, so the second result includes only symbols for modifier
keys.}

Returns the current location of the mouse in screen coordinates, and
returns a list of symbols for mouse buttons and modifier keys that are
currently pressed.}


@defproc[(hide-cursor-until-moved) void?]{

Hides the cursor until the user moves the mouse or clicks the mouse
 button. (For some platforms, the cursor is not hidden if it is over
 a window in a different eventspace or application.)
}

@defproc[(is-busy?) boolean?]{

Returns @racket[#t] if a busy cursor has been installed with
@racket[begin-busy-cursor] and not removed with
@racket[end-busy-cursor].
}

@defproc[(label->plain-label [label string?]) string?]{

Strips shortcut ampersands from @racket[label], removes parenthesized
 ampersand--character combinations along with any surrounding space,
 and removes anything after a tab. Overall, it returns the label as it would
 appear on a button on a platform without support for mnemonics.

}


@defproc[(make-gl-bitmap [width exact-positive-integer?]
                         [height exact-positive-integer?]
                         [config (is-a?/c gl-config%)])
         (is-a?/c bitmap%)]{

Creates a bitmap that supports both normal @racket[dc<%>] drawing an
OpenGL drawing through a context returned by @xmethod[dc<%> get-gl-context].

For @racket[dc<%>] drawing, an OpenGL-supporting bitmap draws like a
bitmap from @racket[make-screen-bitmap] on some platforms, while it
draws like a bitmap instantiated directly from @racket[bitmap%] on
other platforms.}


@defproc[(make-gui-empty-namespace) namespace?]{

Like @racket[make-base-empty-namespace], but with
@racketmodname[racket/class] and @racketmodname[racket/gui/base] also
attached to the result namespace.}

@defproc[(make-gui-namespace) namespace?]{

Like @racket[make-base-namespace], but with @racketmodname[racket/class] and
@racketmodname[racket/gui/base] also required into the top-level
environment of the result namespace.}


@defproc[(make-screen-bitmap [width exact-positive-integer?]
                             [height exact-positive-integer?]) 
         (is-a?/c bitmap%)]{

Creates a bitmap that draws in a way that is the same as drawing to a
canvas in its default configuration.

In particular, on Mac OS X when the main monitor is in Retina display
mode, a drawing unit corresponds to two pixels, and the bitmap
internally contains four times as many pixels as requested by
@racket[width] and @racket[height]. See also
@racket[get-display-backing-scale].

See also @secref[#:doc '(lib "scribblings/draw/draw.scrbl") "Portability"].}


@defproc[(play-sound [filename path-string?]
                     [async? any/c])
         boolean?]{

Plays a sound file. If @racket[async?] is false, the function does not
 return until the sound completes. Otherwise, it returns immediately.
 The result is @racket[#t] if the sound plays successfully, @racket[#f]
 otherwise.

On Windows, only @filepath{.wav} files are supported.

On Unix, the function invokes an external sound-playing program;
  looking for a few known programs (@exec{aplay}, @exec{play},
  @exec{esdplay}, @exec{sndfile-play}, @exec{audioplay}). In addition, a
  play command can be defined through the @ResourceFirst{playcmd}
  preference (see @|mrprefsdiscuss|). The preference can hold a
  program name, or a format string containing a single @litchar{~a}
  where the filename should be substituted---and used as a shell
  command.  (Don't use @litchar{~s}, since the string that is used
  with the format string will be properly quoted and wrapped in double
  quotes.)  A plain command name is usually better since execution is
  faster.  The command's output is discarded, unless it returns an
  error code---in this case the last part of the error output is
  shown.

On Mac OS X, Quicktime is used to play sounds; most sound
 formats (.wav, .aiff, .mp3) are supported in recent versions of
 Quicktime. In order to play .wav files, Quicktime 3.0 (compatible
 with OS 7.5 and up) is required.}


@defproc[(position-integer? [v any/c]) boolean?]{

Equivalent to @racket[(integer-in -1000000 1000000)].}


@defproc[(positive-dimension-integer? [v any/c]) boolean?]{

Equivalent to @racket[(integer-in 1 1000000)].}


@defproc[(register-collecting-blit [canvas (is-a?/c canvas%)]
                                   [x position-integer?]
                                   [y position-integer?]
                                   [w dimension-integer?]
                                   [h dimension-integer?]
                                   [on (is-a?/c bitmap%)]
                                   [off (is-a?/c bitmap%)]
                                   [on-x real? 0]
                                   [on-y real? 0]
                                   [off-x real? 0]
                                   [off-y real? 0])
         void?]{

Registers a ``blit'' to occur when garbage collection starts and
 ends. When garbage collection starts, @racket[on] is drawn at
 location @racket[x] and @racket[y] within @racket[canvas], if
 @racket[canvas] is shown.  When garbage collection ends, the drawing
 is reverted, possibly by drawing the @racket[off] bitmap.

The background behind @racket[on] is unspecified, so @racket[on]
 should be a solid image, and the canvas's scale or scrolling is not
 applied to the drawing. Only the portion of @racket[on] within
 @racket[w] and @racket[h] pixels is used; if @racket[on-x] and
 @racket[on-y] are specified, they specify an offset within the bitmap
 that is used for drawing, and @racket[off-x] and @racket[off-y]
 similarly specify an offset within @racket[off].

The blit is automatically unregistered if @racket[canvas] becomes
 invisible and inaccessible.  Multiple registrations can be installed
 for the same @racket[canvas].

See also @racket[unregister-collecting-blit].}


@defproc[(unregister-collecting-blit [canvas (is-a?/c canvas%)])
         void?]{

Unregisters all blit requests installed for @racket[canvas] with
 @racket[register-collecting-blit].}


@defproc[(send-message-to-window [x position-integer?]
                                 [y position-integer?]
                                 [message any/c])
         any/c]{

@index['("drag-and-drop")]{Finds} the frontmost top-level window at
 (@racket[x], @racket[y]) in global coordinates. If a window is there,
 this function calls the window's @method[top-level-window<%>
 on-message] method, providing @racket[message] as the method's
 argument; the result of the function call is the result returned by
 the method. If no Racket window is at the given coordinates, or if it
 is covered by a non-Racket window at (@racket[x], @racket[y]),
 @racket[#f] is returned.
}


@defproc[(spacing-integer? [v any/c]) boolean?]{

Equivalent to @racket[(integer-in 0 1000)].}


@defproc[(system-position-ok-before-cancel?) boolean?]{

Returns @racket[#t] on Windows---indicating that a dialog with
@onscreen{OK} and @onscreen{Cancel} buttons should place the
@onscreen{OK} button on to left of the @onscreen{Cancel} button---and
returns @racket[#f] on Mac OS X and Unix.}


@defthing[the-clipboard (is-a?/c clipboard<%>)]{

See @racket[clipboard<%>].


}

@defthing[the-x-selection-clipboard (is-a?/c clipboard<%>)]{

See @racket[clipboard<%>].


}

@defproc[(label-string? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a string whose length is less than or equal to @racket[200].

  This predicate is typically used as the contract for strings that 
  appear in GUI objects. In some cases, such as the label in a @racket[button%]
  or @racket[menu-item%] object, the character @litchar{&} is treated specially
  to indicate that the following character is used in keyboard navigation. See
  @xmethod[labelled-menu-item<%> set-label] for one such example.
  In other cases, such as the label on a @racket[frame%], @litchar{&} is not
  treated specially.
}

@defproc[(key-code-symbol? [v any/c]) boolean?]{
  Returns @racket[#t] if the argument is a symbol that can be returned by
  @racket[@key-event%]'s method @method[key-event% get-key-code].
}
