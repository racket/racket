#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "subprocess"]{Processes}

@defproc[(subprocess [stdout (or/c output-port? false/c)]
                     [stdin (or/c input-port? false/c)]
                     [stderr (or/c output-port? false/c)]
                     [command path-string?]
                     [arg string?] ...)
         (values subprocess?
                 (or/c input-port? false/c)
                 (or/c output-port? false/c)
                 (or/c input-port? false/c))]{

Creates a new process in the underlying operating system to execute
@scheme[command] asynchronously. The @scheme[command] argument is a
path to a program executable, and the @scheme[arg]s are command-line
arguments for the program. Under Unix and Mac OS X, command-line
arguments are passed as byte strings using the current locale's
encoding (see @secref["encodings"]).

Under Windows, the first @scheme[arg] can be @indexed-scheme['exact],
which triggers a Windows-specific hack: the second @scheme[arg] is
used exactly as the command-line for the subprocess, and no additional
@scheme[arg]s can be supplied.  Otherwise, a command-line string is
constructed from @scheme[command] and @scheme[arg] so that a typical
Windows console application can parse it back to an array of
arguments. If @scheme['exact] is provided on a non-Windows platform,
the @exnraise[exn:fail:contract].

@margin-note{For information on the Windows command-line conventions,
search for ``command line parsing'' at
@tt{http://msdn.microsoft.com/}.}

Unless it is @scheme[#f], @scheme[stdout] is used for the launched
process's standard output, @scheme[stdin] is used for the process's
standard input, and @scheme[stderr] is used for the process's standard
error.  All provided ports must be file-stream ports. Any of the ports
can be @scheme[#f], in which case a system pipe is created and
returned by @scheme[subprocess]. For each port that is provided, no
pipe is created and the corresponding returned value is @scheme[#f].

The @scheme[subprocess] procedure returns four values:

@itemize{

 @item{a subprocess value representing the created process;}

 @item{an input port piped from the process's standard output, or
 @scheme[#f] if @scheme[stdout-output-port] was a port;}

 @item{an output port piped to the process standard input, or
 @scheme[#f] if @scheme[stdin-input-port] was a port;}

 @item{an input port piped from the process's standard error, or
 @scheme[#f] if @scheme[stderr-output-port] was a port.}

}

@bold{Important:} All ports returned from @scheme[subprocess] must be
explicitly closed with @scheme[close-input-port] or
@scheme[close-output-port].

The returned ports are @tech{file-stream ports} (see
@secref["file-ports"]), and they are placed into the management of
the current custodian (see @secref["custodians"]).  The
@exnraise[exn:fail] when a low-level error prevents the spawning of a
process or the creation of operating system pipes for process
communication.}


@defproc[(subprocess-wait [subproc subprocess?]) void?]{

Blocks until the process represented by @scheme[subproc] terminates.}


@defproc[(subprocess-status [subproc subprocess?]) 
         (or/c (one-of/c 'running)
               nonnegative-exact-integer?)]{

Returns @indexed-scheme['running] if the process represented by
@scheme[subproc] is still running, or its exit code otherwise. The
exit code is an exact integer, and @scheme[0] typically indicates
success. If the process terminated due to a fault or signal, the exit
code is non-zero.}


@defproc[(subprocess-kill [subproc subprocess?][force? any/c]) void?]{

Terminates the subprocess represented by @scheme[subproc] if
@scheme[force?] is true and if the process still running. If an error
occurs during termination, the @exnraise[exn:fail].

If @scheme[force?] is @scheme[#f] under @|AllUnix|, the subprocess is
sent an interrupt signal instead of a kill signal (and the subprocess
might handle the signal without terminating). Under Windows, no action
is taken when @scheme[force?] is @scheme[#f].}


@defproc[(subprocess-pid [subproce subprocess?]) nonnegative-exact-integer?]{

Returns the operating system's numerical ID (if any) for the process
represented by @scheme[subproc], valid only as long as the process is
running.}


@defproc[(subprocess? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a subprocess value, @scheme[#f]
otherwise.}


@defproc[(shell-execute [verb (or/c string? false/c)]
                        [target string?][parameters string?][dir path-string?][show-mode symbol?]) 
         false/c]

@index['("ShellExecute")]{Performs} the action specified by @scheme[verb]
on @scheme[target] in Windows. For platforms other than Windows, the
@exnraise[exn:fail:unsupported].

For example,

@schemeblock[
(shell-execute #f "http://www.plt-scheme.org" "" 
               (current-directory) 'sw_shownormal)
]

Opens the PLT Scheme home page in a browser window.

The @scheme[verb] can be @scheme[#f], in which case the operating
system will use a default verb. Common verbs include @scheme["open"],
@scheme["edit"], @scheme["find"], @scheme["explore"], and
@scheme["print"].

The @scheme[target] is the target for the action, usually a filename
path. The file could be executable, or it could be a file with a
recognized extension that can be handled by an installed application.

The @scheme[parameters] argument is passed on to the system to perform
the action. For example, in the case of opening an executable, the
@scheme[parameters] is used as the command line (after the executable
name).

The @scheme[dir] is used as the current directory when performing the
action.

The @scheme[show-mode] sets the display mode for a Window affected by
the action. It must be one of the following symbols; the description
of each symbol's meaning is taken from the Windows API documentation.

@itemize{

 @item{@indexed-scheme['sw_hide] or @indexed-scheme['SW_HIDE] ---
 Hides the window and activates another window.}

 @item{@indexed-scheme['sw_maximize] or @indexed-scheme['SW_MAXIMIZE]
 --- Maximizes the window.}

 @item{@indexed-scheme['sw_minimize] or @indexed-scheme['SW_MINIMIZE]
 --- Minimizes the window and activates the next top-level window in
 the z-order.}

 @item{@indexed-scheme['sw_restore] or @indexed-scheme['SW_RESTORE]
 --- Activates and displays the window. If the window is minimized or
 maximized, Windows restores it to its original size and position.}

 @item{@indexed-scheme['sw_show] or @indexed-scheme['SW_SHOW] ---
 Activates the window and displays it in its current size and
 position.}

 @item{@indexed-scheme['sw_showdefault] or
 @indexed-scheme['SW_SHOWDEFAULT] --- Uses a default.}

 @item{@indexed-scheme['sw_showmaximized] or
 @indexed-scheme['SW_SHOWMAXIMIZED] --- Activates the window and
 displays it as a maximized window.}

 @item{@indexed-scheme['sw_showminimized] or
 @indexed-scheme['SW_SHOWMINIMIZED] --- Activates the window and
 displays it as a minimized window.}

 @item{@indexed-scheme['sw_showminnoactive] or
 @indexed-scheme['SW_SHOWMINNOACTIVE] --- Displays the window as a
 minimized window. The active window remains active.}

 @item{@indexed-scheme['sw_showna] or @indexed-scheme['SW_SHOWNA] ---
 Displays the window in its current state. The active window remains
 active.}

 @item{@indexed-scheme['sw_shownoactivate] or
 @indexed-scheme['SW_SHOWNOACTIVATE] --- Displays a window in its most
 recent size and position. The active window remains active.}

 @item{@indexed-scheme['sw_shownormal] or
 @indexed-scheme['SW_SHOWNORMAL] --- Activates and displays a
 window. If the window is minimized or maximized, Windows restores it
 to its original size and position.}

 }

If the action fails, the @exnraise[exn:fail]. If the action succeeds,
the result is @scheme[#f].

In future versions of Scheme, the result may be a subprocess value if
the operating system did returns a process handle (but if a subprocess
value is returned, its process ID will be @scheme[0] instead of the
real process ID).
