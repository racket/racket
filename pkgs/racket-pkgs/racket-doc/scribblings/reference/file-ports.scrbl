#lang scribble/doc
@(require "mz.rkt" racket/file)

@(begin
  ;; ignore expressions at the top-level so that they don't print #<void>
  (define-syntax ignore
    (syntax-rules ()
      [(_ expr) (define x expr)]))

  ;; hacky?
  (define file-eval
    (lambda ()
      (let ([the-eval (make-base-eval)])
        (the-eval '(require (for-syntax racket/base)
                            racket/file))
        (the-eval '(define some-file (make-temporary-file)))
        (the-eval '(define some-other-file (make-temporary-file)))
        the-eval)))

  (define-syntax file-examples
    (syntax-rules ()
      [(_ expr ...)
       (let [(my-eval (file-eval))]
         (define (clean)
           (my-eval '(for [(i (list some-file some-other-file))]
                          (when (file-exists? i)
                            (delete-file i)))))
         (clean)
         (begin0
           (defexamples #:eval my-eval
                        expr ...)
           (clean)))]))

  "")

@title[#:tag "file-ports"]{File Ports}

A port created by @racket[open-input-file], @racket[open-output-file],
@racket[subprocess], and related functions is a @deftech{file-stream
port}.  The initial input, output, and error ports in @exec{racket}
are also file-stream ports. The @racket[file-stream-port?]  predicate
recognizes file-stream ports.

When an input or output file-stream port is created, it is placed into
the management of the current custodian (see
@secref["custodians"]). In the case of an output port, a @tech{flush
callback} is registered with the @tech{current plumber} to flush the port.

@defproc[(open-input-file [path path-string?]
                          [#:mode mode-flag (or/c 'binary 'text) 'binary]
                          [#:for-module? for-module? any/c #f])
         input-port?]{

Opens the file specified by @racket[path] for input. The
@racket[mode-flag] argument specifies how the file's bytes are
translated on input:

@itemize[

 @item{@indexed-racket['binary] --- bytes are returned from the port
 exactly as they are read from the file.}

 @item{@indexed-racket['text] --- return and linefeed bytes (10 and
 13) as read from the file are filtered by the port in a platform
 specific manner:

  @itemize[

  @item{@|AllUnix|: no filtering occurs.}

  @item{Windows: a return-linefeed combination from a file is returned
        by the port as a single linefeed; no filtering occurs for
        return bytes that are not followed by a linefeed, or for a
        linefeed that is not preceded by a return.}
  ]}
]

On Windows, @racket['text] mode works only with regular files;
attempting to use @racket['text] with other kinds of files triggers an
@racket[exn:fail:filesystem] exception.

Otherwise, the file specified by @racket[path] need not be a regular
file. It might be a device that is connected through the filesystem, such
as @filepath{aux} on Windows or @filepath{/dev/null} on Unix. In all
cases, the port is buffered by default.

The port produced by @racket[open-input-file] should be explicitly
closed, either though @racket[close-input-port] or indirectly via
@racket[custodian-shutdown-all], to release the OS-level file
handle. The input port will not be closed automatically if it is
otherwise available for garbage collection (see
@secref["gc-model"]); a @tech{will} could be associated with an input port
to close it more automatically (see @secref["willexecutor"]).

A @tech{path} value that is the @tech{cleanse}d version of
@racket[path] is used as the name of the opened port.

If opening the file fails, if @racket[for-module?] is true, and
@racket[current-module-path-for-load] has a non-@racket[#f] value,
then the raised exception is either
@racket[exn:fail:syntax:missing-module] (if the value of
@racket[current-module-path-for-load] is a @tech{syntax object}) or
@racket[exn:fail:filesystem:missing-module] (otherwise).

@history[#:changed "6.0.1.6" @elem{Added @racket[#:for-module?].}]

@file-examples[
;; put some text in a file
(with-output-to-file some-file
  (lambda () (printf "hello world")))
(define in (open-input-file some-file))
(read-string 11 in)
(close-input-port in)
]}

@defproc[(open-output-file [path path-string?]
                           [#:mode mode-flag (or/c 'binary 'text) 'binary]
                           [#:exists exists-flag (or/c 'error 'append 'update 'can-update
                                                       'replace 'truncate 
                                                       'must-truncate 'truncate/replace) 'error])
          output-port?]{

Opens the file specified by @racket[path] for output. The
@racket[mode-flag] argument specifies how bytes written to the port
are translated when written to the file:

@itemize[

 @item{@racket['binary] --- bytes are written to the file exactly
 as written to the port.}

 @item{@racket['text] --- on Windows, a linefeed byte (10) written
 to the port is translated to a return-linefeed combination in the
 file; no filtering occurs for returns.}

]

On Windows, @racket['text] mode works only with regular files;
attempting to use @racket['text] with other kinds of files triggers an
@racket[exn:fail:filesystem] exception.

The @racket[exists-flag] argument specifies how to handle/require
files that already exist:

@itemize[

 @item{@indexed-racket['error] --- raise @racket[exn:fail:filesystem]
       if the file exists.}

 @item{@indexed-racket['replace] --- remove the old file, if it
       exists, and write a new one.}

 @item{@indexed-racket['truncate] --- remove all old data, if the file
       exists.}

 @item{@indexed-racket['must-truncate] --- remove all old data in an
       existing file; if the file does not exist, the
       @exnraise[exn:fail:filesystem].}

 @item{@indexed-racket['truncate/replace] --- try @racket['truncate];
       if it fails (perhaps due to file permissions), try
       @racket['replace].}

 @item{@indexed-racket['update] --- open an existing file without
       truncating it; if the file does not exist, the
       @exnraise[exn:fail:filesystem]. Use @racket[file-position]
       to change the current read/write position.}

 @item{@indexed-racket['can-update] --- open an existing file without
       truncating it, or create the file if it does not exist.}

 @item{@indexed-racket['append] --- append to the end of the file,
       whether it already exists or not; on Windows,
       @racket['append] is equivalent to @racket['update], except that
       the file is not required to exist, and the file position is
       immediately set to the end of the file after opening it.}

]

The file specified by @racket[path] need not be a regular file. It
might be a device that is connected through the filesystem, such as
@filepath{aux} on Windows or @filepath{/dev/null} on Unix. The output
port is block-buffered by default, unless the file corresponds to a
terminal, in which case it is line-buffered by default.

The port produced by @racket[open-output-file] should be explicitly
closed, either though @racket[close-output-port] or indirectly via
@racket[custodian-shutdown-all], to release the OS-level file
handle. The output port will not be closed automatically if it is
otherwise available for garbage collection (see
@secref["gc-model"]); a @tech{will} could be associated with an output port
to close it more automatically (see @secref["willexecutor"]).

A @tech{path} value that is the @tech{cleanse}d version of
@racket[path] is used as the name of the opened port.

@file-examples[
(define out (open-output-file some-file))
(write "hello world" out)
(close-output-port out)
]}

@defproc[(open-input-output-file [path path-string?]
                           [#:mode mode-flag (or/c 'binary 'text) 'binary]
                           [#:exists exists-flag (or/c 'error 'append 'update 'can-update
                                                       'replace 'truncate 'truncate/replace) 'error])
          (values input-port? output-port?)]{

Like @racket[open-output-file], but producing two values: an input
port and an output port. The two ports are connected in that they
share the underlying file descriptor. This procedure is intended for use
with special devices that can be opened by only one process, such as
@filepath{COM1} in Windows. For regular files, sharing the file descriptor can be
confusing. For example, using one port does not automatically flush
the other port's buffer, and reading or writing in one port moves the
file position (if any) for the other port. For regular files, use
separate @racket[open-input-file] and @racket[open-output-file] calls
to avoid confusion.}

@defproc[(call-with-input-file [path path-string?]
                               [proc (input-port? . -> . any)]
                               [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{
Calls @racket[open-input-file] with the @racket[path] and
@racket[mode-flag] arguments, and passes the resulting port
to @racket[proc]. The result of @racket[proc] is the result of the
@racket[call-with-input-file] call, but the newly opened port is closed
when @racket[proc] returns.

@file-examples[
(with-output-to-file some-file
  (lambda () (printf "text in a file")))
(call-with-input-file some-file
  (lambda (in) (read-string 14 in)))
]}

@defproc[(call-with-output-file [path path-string?]
                                [proc (output-port? . -> . any)]
                                [#:mode mode-flag (or/c 'binary 'text) 'binary]
                                [#:exists exists-flag (or/c 'error 'append 'update
                                                            'replace 'truncate 'truncate/replace) 'error])
         any]{
Analogous to @racket[call-with-input-file], but passing @racket[path],
@racket[mode-flag] and @racket[exists-flag] to
@racket[open-output-file].

@file-examples[
(call-with-output-file some-file
  (lambda (out)
    (write 'hello out)))
(call-with-input-file some-file
  (lambda (in)
    (read-string 5 in)))
]}

@defproc[(call-with-input-file* [path path-string?]
                                [proc (input-port? . -> . any)]
                                [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{
Like @racket[call-with-input-file], but the newly opened port is
closed whenever control escapes the dynamic extent of the
@racket[call-with-input-file*] call, whether through @racket[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(call-with-output-file* [path path-string?]
                                 [proc (output-port? . -> . any)]
                                 [#:mode mode-flag (or/c 'binary 'text) 'binary]
                                 [#:exists exists-flag (or/c 'error 'append 'update
                                                             'replace 'truncate 'truncate/replace) 'error])
         any]{
Like @racket[call-with-output-file], but the newly opened port is
closed whenever control escapes the dynamic extent of the
@racket[call-with-output-file*] call, whether through @racket[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(with-input-from-file [path path-string?]
                               [thunk (-> any)]
                               [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{
Like @racket[call-with-input-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current input port (see @racket[current-input-port]) using
@racket[parameterize] around the call to @racket[thunk].

@file-examples[
(with-output-to-file some-file
  (lambda () (printf "hello")))
(with-input-from-file some-file
  (lambda () (read-string 5)))
]}

@defproc[(with-output-to-file [path path-string?]
                              [thunk (-> any)]
                              [#:mode mode-flag (or/c 'binary 'text) 'binary]
                              [#:exists exists-flag (or/c 'error 'append 'update
                                                          'replace 'truncate 'truncate/replace) 'error])
         any]{
Like @racket[call-with-output-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current output port (see @racket[current-output-port]) using
@racket[parameterize] around the call to @racket[thunk].

@file-examples[
(with-output-to-file some-file
  (lambda () (printf "hello")))
(with-input-from-file some-file
  (lambda () (read-string 5)))
]}


@defproc[(port-try-file-lock? [port file-stream-port?]
                              [mode (or/c 'shared 'exclusive)])
         boolean?]{

Attempts to acquire a lock on the file using the current platform's
facilities for file locking. Multiple processes can acquire a
@racket['shared] lock on a file, but at most one process can hold an
@racket['exclusive] lock, and @racket['shared] and @racket['exclusive]
locks are mutually exclusive. When @racket[mode] is @racket['shared],
then @racket[port] must be an input port; when @racket[mode] is
@racket['exclusive], then @racket[port] must be an output port.

The result is @racket[#t] if the requested lock is acquired,
@racket[#f] otherwise. When a lock is acquired, it is held until
either it is released with @racket[port-file-unlock] or the port is closed
(perhaps because the process terminates).

Depending on the platform, locks may be merely advisory (i.e., locks
affect only the ability of processes to acquire locks) or they may
correspond to mandatory locks that prevent reads and writes to the
locked file. Specifically, locks are mandatory on Windows and advisory
on other platforms. Multiple tries for a @racket['shared] lock on a
single port can succeed; on Unix and Mac OS X, a single
@racket[port-file-unlock] release the lock, while on other Windows, a
@racket[port-file-unlock] is needed for each successful
@racket[port-try-file-lock?]. On Unix and Mac OS X, multiple tries for
a @racket['exclusive] lock can succeed and a single
@racket[port-file-unlock] releases the lock, while on Windows, a try
for an @racket['exclusive] lock fails for a given port if the port
already holds the lock.

A lock acquired for an input port from @racket[open-input-output-file]
can be released through @racket[port-file-unlock] on the corresponding
output port, and vice versa. If the output port from
@racket[open-input-output-file] holds an @racket['exclusive] lock, the
corresponding input port can still acquire a @racket['shared] lock,
even multiple times; on Windows, a @racket[port-file-unlock] is needed
for each successful lock try, while a single @racket[port-file-unlock]
balances the lock tries on Unix and Mac OS X. A @racket['shared] lock on
an input port can be upgraded to an @racket['exclusive] lock through the
corresponding output port on Unix and Mac OS X, in which case a single
@racket[port-file-unlock] (on either port) releases the lock, while
such upgrades are not allowed on Windows.

Locking is normally supported only for file ports, and attempting to
acquire a lock with other kinds of file-stream ports raises an
@racket[exn:fail:filesystem] exception.}


@defproc[(port-file-unlock [port file-stream-port?])
         void?]{

Releases a lock held by the current process on the file of
@racket[port].}


@defproc[(port-file-identity [port file-stream-port?]) exact-positive-integer?]{

@index['("inode")]{Returns} a number that represents
the identity of the device and file read or written by
@racket[port]. For two ports whose open times overlap, the
result of @racket[port-file-identity] is the same for both ports if
and only if the ports access the same device and file. For ports whose
open times do not overlap, no guarantee can be provided for the port
identities (even if the ports actually access the same file)---except
as can be inferred through relationships with other ports. If
@racket[port] is closed, the @exnraise[exn:fail].  On
Windows 95, 98, and Me, if @racket[port] is connected to a
pipe instead of a file, the @exnraise[exn:fail:filesystem].

@file-examples[
(define file1 (open-output-file some-file))
(define file2 (open-output-file some-other-file))
(port-file-identity file1)
(port-file-identity file2)
(close-output-port file1)
(close-output-port file2)
]}
