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

@defproc[(begin-busy-cursor)
         void?]{

Changes the cursor to a watch cursor for all windows in the current eventspace.
Use 
@scheme[end-busy-cursor] to revert the cursor back to its previous state. Calls to
@scheme[begin-busy-cursor] and 
@scheme[end-busy-cursor] can be nested arbitrarily.

The cursor installed by 
@scheme[begin-busy-cursor] overrides any window-specific cursors installed with
@method[window<%> set-cursor].

See also 
@scheme[is-busy?].



}

@defproc[(bell)
         void?]{

Rings the system bell.



}

@defproc[(end-busy-cursor)
         void?]{

See
@scheme[begin-busy-cursor].



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

The get operation always returns @scheme[#"????"] and @scheme[#"????"] for
 Unix or Windows. The set operation has no effect under Unix or
 Windows.
}

@defproc[(find-graphical-system-path [what (one-of/c 'init-file 'setup-file 'x-display)])
         (or/c path? false/c)]{

Finds a platform-specific (and possibly user- or machine-specific)
 standard filename or directory. See also @scheme[find-system-path].

The result depends on @scheme[what], and a @scheme[#f] result is only
 possible when @scheme[what] is @scheme['x-display]:

@itemize[

 @item{@scheme['init-file] returns the path to the user-specific
 initialization file (containing Scheme code). The directory part of
 the path is the same path as returned for @scheme['init-dir] by
 MzScheme's @scheme[find-system-path].  The file name is
 platform-specific:
  @itemize[

  @item{@|AllUnix|: @indexed-file{.mredrc}}
  @item{Windows: @indexed-file{mredrc.ss}}

  ]}

 @item{@scheme['setup-file] returns the path to the file
 containing resources used by @scheme[get-resource]; obsolete.}

 @item{@scheme['x-display] returns a ``path'' whose string identifies
 the X display if specified by either the @Flag{display} flag or the
 @envvar{DISPLAY} environment variable when MrEd starts under X. For
 other platforms, or when neither @Flag{display} nor @envvar{DISPLAY}
 was specified, the result is @scheme[#f].}

]



}


@defproc[(get-default-shortcut-prefix)
         (listof (one-of/c 'alt 'cmd 'meta 'ctl 'shift 'option))]{
Returns an immutable list specifying the default prefix for menu
shortcuts. See also
@xmethod[selectable-menu-item<%> get-shortcut-prefix].

Under Windows, the default is @scheme['(ctl)]. Under Mac OS X, the
default is @scheme['(cmd)]. Under X, the default is normally
@scheme['(ctl)], but the default can be changed through the
@Resource{defaultMenuPrefix} low-level preference (see
@|mrprefsdiscuss|).}

@defproc[(get-panel-background)
         (is-a?/c color%)]{

Returns the background color of a panel (usually some shade of gray)
 for the current platform.

}

@defproc[(get-resource [section string?]
                       [entry string?]
                       [value (box/c (or/c string? exact-integer?))]
                       [file (or/c path? false/c) #f])
         boolean?]{

Gets a resource value from the resource database. The resource value
 is keyed on the combination of @scheme[section] and @scheme[entry].  The
 return value is @scheme[#t] if a value is found, @scheme[#f] if it is
 not. The type of the value initially in the @scheme[value] box
 determines the way that the resource is interpreted, and @scheme[value]
 is filled with a new value of the same type if one is found.

If @scheme[file] is @scheme[#f], platform-specific resource files
 are read, as determined by @scheme[find-graphical-system-path]
 with @indexed-scheme['setup-file]. (Under X, when @scheme[file] is
 @scheme[#f], the user's @filepath{.Xdefaults} file is also read, or the
 file specified by the @filepath{XENVIRONMENT} environment variable.)

The format of a resource entry depends on the platform. Windows
 resources use the standard @filepath{.ini} format. X and Mac OS X
 resources use the standard X resource format, where each entry
 consists of a @scheme[section].@scheme[entry] resource name, a colon, and
 the resource value, terminated by a newline.  Section and entry names are
 case-sensitive.

@index['("registry")]{@index['("Windows registry")]{Under}} Windows, if
 @scheme[section] is one of the following strings, then @scheme[file]
 is ignored, and @scheme[entry] is used as a resource path:

@itemize[

 @item{@indexed-scheme["HKEY_CLASSES_ROOT"]}
 @item{@indexed-scheme["HKEY_CURRENT_CONFIG"]}
 @item{@indexed-scheme["HKEY_CURRENT_USER"]}
 @item{@indexed-scheme["HKEY_LOCAL_MACHINE"]}
 @item{@indexed-scheme["HKEY_USERS"]}

]

In that case, the @scheme[entry] argument is parsed as a resource entry
path, followed by a backslash, followed by a value name. To get the
``default'' value for an entry, use the empty name. For example, the
following expression gets a command line for starting a browser:

@schemeblock[
(let ([b (box "")])
  (get-resource "HKEY_CLASSES_ROOT"
                "htmlfile\\shell\\open\\command\\" b)
  (unbox b))
]

See also @scheme[write-resource].}

@defproc[(get-window-text-extent [string string]
                                 [font (is-a?/c font%)]
                                 [combine? any/c #f])
         (values exact-nonnegative-integer?
                 exact-nonnegative-integer?)]{

Returns the pixel size of a string drawn as a window's label or value
when drawn with the given font. The optional @scheme[combine?]
argument is as for @xmethod[dc<%> get-text-extent].

See also @xmethod[dc<%> get-text-extent].
}

@defproc[(graphical-read-eval-print-loop [eval-eventspace eventspace #f]
                                         [redirect-ports? any/c (not eval-eventspace)])
         void?]{

Similar to @scheme[read-eval-print-loop], except that none of
 @scheme[read-eval-print-loop]'s configuration parameters are used (such
 as @scheme[current-read]) and the interaction occurs in a GUI window
 instead of using the current input and output ports.

Expressions entered into the graphical read-eval-print loop can be
 evaluated in an eventspace (and thread) that is distinct from the one
 implementing the @scheme[graphical-read-eval-print-loop]
 window (i.e., the current eventspace when
 @scheme[graphical-read-eval-print-loop] is called).

If no eventspace is provided, or if @scheme[#f] is provided, an
 evaluation eventspace is created using @scheme[(make-eventspace)]
 with a new custodian; the eventspace and its threads are be shut down
 when the user closes the @scheme[graphical-read-eval-print-loop]
 window. If an eventspace is provided, closing the window performs no
 shut-down actions on eventspace.

When @scheme[redirect-ports?] is true, the following parameters are
 initialized in the created eventspace's handler thread:
@itemize[

 @item{@scheme[current-output-port] --- writes to the frame}
 @item{@scheme[current-error-port] --- writes to the frame}
 @item{@scheme[current-input-port] --- always returns @scheme[eof]}

]

The keymap for the read-eval-print loop's editor is initialized by
 calling the current keymap initializer procedure, which is determined
 by the
@scheme[current-text-keymap-initializer] parameter.


}

@defproc[(textual-read-eval-print-loop)
         void?]{

Similar to @scheme[read-eval-print-loop], except that evaluation uses
 a newly created eventspace.

The @scheme[current-prompt-read] parameter is used in the current
 thread to read input. The result is queued for evaluation and
 printing in the created eventspace's @tech{handler thread}, which
 uses @scheme[current-eval] and @scheme[current-print]. After printing
 completes for an interaction result, the next expression in read in
 the original thread, and so on.

If an @scheme[exn:break] exception is raised in the original thread
during reading, it aborts the current call to @scheme[(current-read)]
and a new one is started. If an @scheme[exn:break] exception is raised
in the original thread while waiting for an interaction to complete, a
break is sent (via @scheme[break-thread]) to the created eventspace's
@tech{handler thread}.}


@defproc[(hide-cursor-until-moved)
         void?]{

Hides the cursor until the user moves the mouse or clicks the mouse
 button. (For some platforms, the cursor is not hidden if it is over
 a window in a different eventspace or application.)



}

@defproc[(is-busy?)
         boolean?]{

Returns @scheme[#t] if a busy cursor has been installed with
@scheme[begin-busy-cursor] and not removed with 
@scheme[end-busy-cursor].



}

@defproc[(label->plain-label [label string])
         string]{

Strips shortcut ampersands from @scheme[label], removes parenthesized
 ampersand--character combinations along with any surrounding space,
 and removes anything after a tab. Overall, it returns the label as it would
 appear on a button on a platform without support for mnemonics.

}


@defproc[(make-gui-empty-namespace)
         namespace?]{

Like @scheme[make-base-empty-namespace], but with
@scheme[scheme/class] and @schememodname[scheme/gui/base] also
attached to the result namespace.}

@defproc[(make-gui-namespace)
         namespace?]{

Like @scheme[make-base-namespace], but with @scheme[scheme/class] and
@schememodname[scheme/gui/base] also required into the top-level
environment of the result namespace.}


@defproc[(play-sound [filename path-string?]
                     [async? any/c])
         boolean?]{

Plays a sound file. If @scheme[async?] is false, the function does not
 return until the sound completes. Otherwise, it returns immediately.
 The result is @scheme[#t] if the sound plays successfully, @scheme[#f]
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
 with OS 7.5 and up) is required.



}


@defproc[(send-event [receiver-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [event-class-bytes (lambda (s) (and (bytes? s)
                                                         (= 4 (bytes-length s))))]
                     [event-id-bytes (lambda (s) (and (bytes? s)
                                                      (= 4 (bytes-length s))))]
                     [direct-arg-v any/c (void)]
                     [argument-list list? null])
         any/c]{

Sends an AppleEvent or raises @scheme[exn:fail:unsupported].

The @scheme[receiver-bytes], @scheme[event-class-bytes], and
@scheme[event-id-bytes] arguments specify the signature of the
receiving application, the class of the AppleEvent, and the ID of
the AppleEvent.

The @scheme[direct-arg-v] value is converted (see below) and passed as
the main argument of the event; if @scheme[direct-argument-v] is
@|void-const|, no main argument is sent in the event. The
@scheme[argument-list] argument is a list of two-element lists
containing a typestring and value; each typestring is used ad the
keyword name of an AppleEvent argument for the associated converted
value. 

The following types of MzScheme values can be converted to AppleEvent
values passed to the receiver:

@atable[
(tline @elem{@scheme[#t] or @scheme[#f]}  @elem{Boolean})
(tline @elem{small integer}   @elem{Long Integer})
(tline @elem{inexact real number} @elem{Double})
(tline @elem{string} @elem{Characters})
(tline @elem{list of convertible values}  @elem{List of converted values})
(tline @scheme[#(file _pathname)]   @elem{Alias (file exists) or FSSpec (does not exist)})
(tline @scheme[#(record (_typestring _v) ...)]   @elem{Record of keyword-tagged values})
]

If other types of values are passed to @scheme[send-event] for
 conversion, the @exnraise[exn:fail:unsupported].

The @scheme[send-event] procedure does not return until the receiver
of the AppleEvent replies. The result of @scheme[send-event] is the
reverse-converted reply value (see below), or the @exnraise[exn:fail]
if there is an error. If there is no error or return value,
@scheme[send-event] returns @|void-const|.

The following types of AppleEvent values can be reverse-converted into
a MzScheme value returned by @scheme[send-event]:

@atable[
(tline @elem{Boolean}  @elem{@scheme[#t] or @scheme[#f]})
(tline @elem{Signed Integer}  @elem{integer})
(tline @elem{Float, Double, or Extended}  @elem{inexact real number})
(tline @elem{Characters}  @elem{string})
(tline @elem{List of reverse-convertible values}  @elem{list of reverse-converted values})
(tline @elem{Alias or FSSpec}  @scheme[#(file _pathname)])
(tline @elem{Record of keyword-tagged values}  @scheme[#(record (_typestring _v) ...)])
]

If the AppleEvent reply contains a value that cannot be
 reverse-converted, the @exnraise[exn:fail].

}

@defproc[(send-message-to-window [x (integer-in -10000 10000)]
                                 [y (integer-in -10000 10000)]
                                 [message any/c])
         any/c]{

@index['("drag-and-drop")]{Finds} the frontmost top-level window at
 (@scheme[x], @scheme[y]) in global coordinates. If a window is there,
 this function calls the window's @method[top-level-window<%>
 on-message] method, providing @scheme[message] as the method's
 argument; the result of the function call is the result returned by
 the method. If no Scheme window is at the given coordinates, or if it
 is covered by a non-Scheme window at (@scheme[x], @scheme[y]),
 @scheme[#f] is returned.
}


@defproc[(system-position-ok-before-cancel?) boolean?]{

Returns @scheme[#t] under Windows---indicating that a dialog with
@onscreen{OK} and @onscreen{Cancel} buttons should place the
@onscreen{OK} button on to left of the @onscreen{Cancel} button---and
returns @scheme[#f] under Mac OS X and X.}


@defthing[the-clipboard (is-a?/c clipboard<%>)]{

See @scheme[clipboard<%>].


}

@defthing[the-x-selection-clipboard (is-a?/c clipboard<%>)]{

See @scheme[clipboard<%>].


}

@defproc[(write-resource [section string?]
                         [entry string?]
                         [value (or/c string? exact-integer?)]
                         [file (or/c path-string? false/c) #f])
         boolean?]{

Writes a resource value to the specified resource database. The
 resource value is keyed on the combination of @scheme[section] and
 @scheme[entry], with the same special handling of @scheme[entry] for
 under Windows as for @scheme[get-resource].

If @scheme[file] is @scheme[#f], the platform-specific resource
 database is read, as determined by
 @scheme[find-graphical-system-path] with
 @indexed-scheme['setup-file].

The return value is @scheme[#t] if the write succeeds, @scheme[#f]
 otherwise. (A failure indicates that the resource file cannot be
 written.)

If @scheme[value] is an integer outside a platform-specific range,
 @|MismatchExn|.

See also @scheme[get-resource].}

