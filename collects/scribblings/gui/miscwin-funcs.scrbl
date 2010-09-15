#lang scribble/doc
@(require "common.ss"
          scribble/struct)

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
Use 
@racket[end-busy-cursor] to revert the cursor back to its previous state. Calls to
@racket[begin-busy-cursor] and 
@racket[end-busy-cursor] can be nested arbitrarily.

The cursor installed by 
@racket[begin-busy-cursor] overrides any window-specific cursors installed with
@method[window<%> set-cursor].

See also @racket[is-busy?].
}

@defproc[(bell) void?]{
Rings the system bell.
}

@defproc[(end-busy-cursor) void?]{
See @racket[begin-busy-cursor].
}

@defproc*[([(file-creator-and-type [filename path]
                                   [creator-string (lambda (s) (and (bytes? s)
                                                                    (= 4 (bytes-length s))))]
                                   [type-bytes (lambda (s) (and (bytes? s)
                                                                 (= 4 (bytes-length s))))])
            void?]
           [(file-creator-and-type [filename path])
            (values (lambda (s) (and (bytes? s)
                                (= 4 (bytes-length s))))
                    (lambda (s) (and (bytes? s)
                                (= 4 (bytes-length s)))))])]{

Gets or sets the creator and type of a file in Mac OS X.

The get operation always returns @racket[#"????"] and @racket[#"????"] for
 Unix or Windows. The set operation has no effect under Unix or
 Windows.
}

@defproc[(find-graphical-system-path [what (one-of/c 'init-file 'setup-file 'x-display)])
         (or/c path? false/c)]{

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
  @item{Windows: @indexed-file{racketrc.rktl}}

  ]}

 @item{@racket['setup-file] returns the path to the file
 containing resources used by @racket[get-resource]; obsolete.}

 @item{@racket['x-display] returns a ``path'' whose string identifies
 the X display if specified by either the @Flag{display} flag or the
 @envvar{DISPLAY} environment variable when GRacket starts under X. For
 other platforms, or when neither @Flag{display} nor @envvar{DISPLAY}
 was specified, the result is @racket[#f].}

]



}


@defproc[(get-default-shortcut-prefix)
         (listof (one-of/c 'alt 'cmd 'meta 'ctl 'shift 'option))]{
Returns an immutable list specifying the default prefix for menu
shortcuts. See also
@xmethod[selectable-menu-item<%> get-shortcut-prefix].

Under Windows, the default is @racket['(ctl)]. Under Mac OS X, the
default is @racket['(cmd)]. Under X, the default is normally
@racket['(ctl)], but the default can be changed through the
@Resource{defaultMenuPrefix} low-level preference (see
@|mrprefsdiscuss|).}

@defproc[(get-panel-background)
         (is-a?/c color%)]{

Returns the background color of a panel (usually some shade of gray)
 for the current platform.

}


@defproc[(get-highlight-background-color) (is-a?/c color%)]{

Returns the color drawn behind selected text.}


@defproc[(get-highlight-text-color) (or/c (is-a?/c color%) #f)]{

Returns the color used to draw selected text or @racket[#f] if
selected text is drawn with its usual color.}


@defproc[(get-resource [section string?]
                       [entry string?]
                       [value (box/c (or/c string? exact-integer?))]
                       [file (or/c path? false/c) #f])
         boolean?]{

Gets a resource value from the resource database. The resource value
 is keyed on the combination of @racket[section] and @racket[entry].  The
 return value is @racket[#t] if a value is found, @racket[#f] if it is
 not. The type of the value initially in the @racket[value] box
 determines the way that the resource is interpreted, and @racket[value]
 is filled with a new value of the same type if one is found.

If @racket[file] is @racket[#f], platform-specific resource files
 are read, as determined by @racket[find-graphical-system-path]
 with @indexed-racket['setup-file]. (Under X, when @racket[file] is
 @racket[#f], the user's @filepath{.Xdefaults} file is also read, or the
 file specified by the @filepath{XENVIRONMENT} environment variable.)

The format of a resource entry depends on the platform. Windows
 resources use the standard @filepath{.ini} format. X and Mac OS X
 resources use the standard X resource format, where each entry
 consists of a @racket[section].@racket[entry] resource name, a colon, and
 the resource value, terminated by a newline.  Section and entry names are
 case-sensitive.

@index['("registry")]{@index['("Windows registry")]{Under}} Windows, if
 @racket[section] is one of the following strings, then @racket[file]
 is ignored, and @racket[entry] is used as a resource path:

@itemize[

 @item{@indexed-racket["HKEY_CLASSES_ROOT"]}
 @item{@indexed-racket["HKEY_CURRENT_CONFIG"]}
 @item{@indexed-racket["HKEY_CURRENT_USER"]}
 @item{@indexed-racket["HKEY_LOCAL_MACHINE"]}
 @item{@indexed-racket["HKEY_USERS"]}

]

In that case, the @racket[entry] argument is parsed as a resource entry
path, followed by a backslash, followed by a value name. To get the
``default'' value for an entry, use the empty name. For example, the
following expression gets a command line for starting a browser:

@racketblock[
(let ([b (box "")])
  (get-resource "HKEY_CLASSES_ROOT"
                "htmlfile\\shell\\open\\command\\" b)
  (unbox b))
]

See also @racket[write-resource].}

@defproc[(get-window-text-extent [string string]
                                 [font (is-a?/c font%)]
                                 [combine? any/c #f])
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{

Returns the pixel size of a string drawn as a window's label or value
when drawn with the given font. The optional @racket[combine?]
argument is as for @xmethod[dc<%> get-text-extent].

See also @xmethod[dc<%> get-text-extent].
}

@defproc[(graphical-read-eval-print-loop [eval-eventspace eventspace #f]
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
 a newly created eventspace.

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

@defproc[(label->plain-label [label string]) string?]{

Strips shortcut ampersands from @racket[label], removes parenthesized
 ampersand--character combinations along with any surrounding space,
 and removes anything after a tab. Overall, it returns the label as it would
 appear on a button on a platform without support for mnemonics.

}


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
         (is-a/c? bitmap%)]{

Creates a bitmap that draws in a way that is the same as drawing to a
canvas in its default configuration. The bitmap is always in color
with an alpha channel.

A normal @racket[bitmap%] draws in a more platform-independent way and
may use fewer constrained resources, particularly under Windows.}


@defproc[(play-sound [filename path-string?]
                     [async? any/c])
         boolean?]{

Plays a sound file. If @racket[async?] is false, the function does not
 return until the sound completes. Otherwise, it returns immediately.
 The result is @racket[#t] if the sound plays successfully, @racket[#f]
 otherwise.

Under Windows, only @filepath{.wav} files are supported.

Under X, the function invokes an external sound-playing program;
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

Under Mac OS X, Quicktime is used to play sounds; most sound
 formats (.wav, .aiff, .mp3) are supported in recent versions of
 Quicktime. In order to play .wav files, Quicktime 3.0 (compatible
 with OS 7.5 and up) is required.}


@defproc[(register-collecting-blit [canvas (is-a?/c canvas%)]
                                   [x real?]
                                   [y real?]
                                   [w (and/c real? (not/c negative?))]
                                   [h (and/c real? (not/c negative?))]
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

The blit is automatically unregistered if @scheme[canvas] becomes
 invisible and inaccessible.  Multiple registrations can be installed
 for the same @scheme[canvas].

See also @scheme[unregister-collecting-blit].}


@defproc[(unregister-collecting-blit [canvas (is-a?/c canvas%)])
         void?]{

Unregisters all blit requests installed for @racket[canvas] with
 @scheme[register-collecting-blit].}


@defproc[(send-event [receiver-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [event-class-bytes (lambda (s) (and (bytes? s)
                                                         (= 4 (bytes-length s))))]
                     [event-id-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [direct-arg-v any/c (void)]
                     [argument-list list? null])
         any/c]{

Sends an AppleEvent or raises @racket[exn:fail:unsupported].

The @racket[receiver-bytes], @racket[event-class-bytes], and
@racket[event-id-bytes] arguments specify the signature of the
receiving application, the class of the AppleEvent, and the ID of
the AppleEvent.

The @racket[direct-arg-v] value is converted (see below) and passed as
the main argument of the event; if @racket[direct-argument-v] is
@|void-const|, no main argument is sent in the event. The
@racket[argument-list] argument is a list of two-element lists
containing a typestring and value; each typestring is used ad the
keyword name of an AppleEvent argument for the associated converted
value. 

The following types of Racket values can be converted to AppleEvent
values passed to the receiver:

@atable[
(tline @elem{@racket[#t] or @racket[#f]}  @elem{Boolean})
(tline @elem{small integer}   @elem{Long Integer})
(tline @elem{inexact real number} @elem{Double})
(tline @elem{string} @elem{Characters})
(tline @elem{list of convertible values}  @elem{List of converted values})
(tline @racket[#(file _pathname)]   @elem{Alias (file exists) or FSSpec (does not exist)})
(tline @racket[#(record (_typestring _v) ...)]   @elem{Record of keyword-tagged values})
]

If other types of values are passed to @racket[send-event] for
 conversion, the @exnraise[exn:fail:unsupported].

The @racket[send-event] procedure does not return until the receiver
of the AppleEvent replies. The result of @racket[send-event] is the
reverse-converted reply value (see below), or the @exnraise[exn:fail]
if there is an error. If there is no error or return value,
@racket[send-event] returns @|void-const|.

The following types of AppleEvent values can be reverse-converted into
a Racket value returned by @racket[send-event]:

@atable[
(tline @elem{Boolean}  @elem{@racket[#t] or @racket[#f]})
(tline @elem{Signed Integer}  @elem{integer})
(tline @elem{Float, Double, or Extended}  @elem{inexact real number})
(tline @elem{Characters}  @elem{string})
(tline @elem{List of reverse-convertible values}  @elem{list of reverse-converted values})
(tline @elem{Alias or FSSpec}  @racket[#(file _pathname)])
(tline @elem{Record of keyword-tagged values}  @racket[#(record (_typestring _v) ...)])
]

If the AppleEvent reply contains a value that cannot be
 reverse-converted, the @exnraise[exn:fail].

}

@defproc[(send-message-to-window [x (integer-in -10000 10000)]
                                 [y (integer-in -10000 10000)]
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


@defproc[(system-position-ok-before-cancel?) boolean?]{

Returns @racket[#t] under Windows---indicating that a dialog with
@onscreen{OK} and @onscreen{Cancel} buttons should place the
@onscreen{OK} button on to left of the @onscreen{Cancel} button---and
returns @racket[#f] under Mac OS X and X.}


@defthing[the-clipboard (is-a?/c clipboard<%>)]{

See @racket[clipboard<%>].


}

@defthing[the-x-selection-clipboard (is-a?/c clipboard<%>)]{

See @racket[clipboard<%>].


}

@defproc[(write-resource [section string?]
                         [entry string?]
                         [value (or/c string? exact-integer?)]
                         [file (or/c path-string? false/c) #f])
         boolean?]{

Writes a resource value to the specified resource database. The
 resource value is keyed on the combination of @racket[section] and
 @racket[entry], with the same special handling of @racket[entry] for
 under Windows as for @racket[get-resource].

If @racket[file] is @racket[#f], the platform-specific resource
 database is read, as determined by
 @racket[find-graphical-system-path] with
 @indexed-racket['setup-file].

The return value is @racket[#t] if the write succeeds, @racket[#f]
 otherwise. (A failure indicates that the resource file cannot be
 written.)

If @racket[value] is an integer outside a platform-specific range,
 @|MismatchExn|.

See also @racket[get-resource].}

@defproc[(label-string? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a string whose length is less than or equal to @racket[200].                                             
}

@defproc[(key-code-symbol? [v any/c]) boolean?]{
  Returns @racket[#t] if the argument is a symbol that can be returned by
  @racket[@key-event%]'s method @method[key-event% get-key-code].
}
