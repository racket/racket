#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:file-ports"]{File Ports}

A port created by @scheme[open-input-file], @scheme[open-output-file],
@scheme[subprocess], and related functions is a @deftech{file-stream
port}.  The initial input, output, and error ports in stand-alone
MzScheme are also file-stream ports. The @scheme[file-stream-port?]
predicate recognizes file-stream ports.

When an input or output file-stream port is created, it is placed into
the management of the current custodian (see
@secref["mz:custodians"]).

@defproc[(open-input-file [path path-string?]
                          [#:mode mode-flag (one-of/c 'binary 'text) 'binary])
         input-port?]{

Opens the file specified by @scheme[path] for input. The
@scheme[mode-flag] argument specifies how the file's bytes are
translated on input:

@itemize{

 @item{@indexed-scheme['binary] --- bytes are returned from the port
 exactly as they are read from the file.}

 @item{@indexed-scheme['text] --- return and linefeed bytes (10 and
 13) as read from the file are filtered by the port in a platform
 specific manner:

  @itemize{

  @item{@|AllUnix|: no filtering occurs.}

  @item{Windows: a return-linefeed combination from a file is returned
        by the port as a single linefeed; no filtering occurs for
        return bytes that are not followed by a linefeed, or for a
        linefeed that is not preceded by a return.}
  }}
}

Under Windows, @scheme['text] mode works only with regular files;
attempting to use @scheme['text] with other kinds of files triggers an
@scheme[exn:fail:filesystem] exception.

Otherwise, the file specified by @scheme[path] need not be a regular
file. It might a device that is connected through the filesystem, such
as @file{aux} under Windows or @file{/dev/null} under Unix. In all
cases, the port is buffered by default.

The port produced by @scheme[open-input-port] should be explicitly
closed, either though @scheme[close-input-port] or indirectly via
@scheme[custodian-shutdown-all], to release the OS-level file
handle. The input port will not closed automatically if it is
otherwise available for garbage collection (see
@secref["mz:gc-model"]); a @tech{will} could be associated input port
to close it more automatically (see @secref["mz:willexecutor"]).

A @tech{path} value that is the expanded version of @scheme[path] is
used as the name of the opened port.}

@defproc[(open-output-file [path path-string?]
                           [#:mode mode-flag (one-of/c 'binary 'text) 'binary]
                           [#:exists exists-flag (one-of/c 'error 'append 'update
                                                           'replace 'truncate 'truncate/replace) 'error])
          output-port?]{

Opens the file specified by @scheme[path] for output. The
@scheme[mode-flag] argument specifies how bytes written to the port
are translated when written to the file:

@itemize{

 @item{@scheme['binary] --- bytes are written to the file exactly
 as written to the port.}

 @item{@scheme['text] --- under Windows, a linefeed byte (10) written
 to the port is translated to a return-linefeed combination in the
 file; no filtering occurs for returns.}

}

Under Windows, @scheme['text] mode works only with regular files;
attempting to use @scheme['text] with other kinds of files triggers an
@scheme[exn:fail:filesystem] exception.

The @scheme[exists-flag] argument specifies how to handle the case
that the file already exists. 

@itemize{

 @item{@indexed-scheme['error] --- raise @scheme[exn:fail:filesystem].}

 @item{@indexed-scheme['replace] --- remove the old file and write a new one.}

 @item{@indexed-scheme['truncate] --- removed all old data.}

 @item{@indexed-scheme['truncate/replace] --- try @scheme['truncate];
       if it fails (perhaps due to file permissions), try
       @scheme['replace].}

 @item{@indexed-scheme['update] --- open an existing file without
       truncating it; if the file does not exist, the
       @exnraise[exn:fail:filesystem].}

 @item{@indexed-scheme['append] --- append to the end of the file
       under @|AllUnix|; under Windows, @scheme['append] is equivalent
       to @scheme['update], except that the file position is
       immediately set to the end of the file after opening it.}

}

The file specified by @scheme[path] need not be a regular file. It
might a device that is connected through the filesystem, such as
@file{aux} under Windows or @file{/dev/null} under Unix. The output
port is block-buffered by default, unless the file corresponds to a
terminal, in which case is it line buffered bu default.

The port produced by @scheme[open-output-port] should be explicitly
closed, either though @scheme[close-output-port] or indirectly via
@scheme[custodian-shutdown-all], to release the OS-level file
handle. The output port will not closed automatically if it is
otherwise available for garbage collection (see
@secref["mz:gc-model"]); a @tech{will} could be associated input port
to close it more automatically (see @secref["mz:willexecutor"]).

A @tech{path} value that is the expanded version of @scheme[path] is
used as the name of the opened port.}

@defproc[(open-input-output-file [path path-string?]
                           [#:mode mode-flag (one-of/c 'binary 'text) 'binary]
                           [#:exists exists-flag (one-of/c 'error 'append 'update
                                                           'replace 'truncate 'truncate/replace) 'error])
          (values input-port? output-port?)]{

Like @scheme[open-output-file], but producing two values: an input
port and an output port. The two ports are connected in that they
share the underlying file device. This procedure is intended for use
with special devices that can be opened by only one process, such as
@file{COM1} in Windows. For regular files, sharing the device can be
confusing. For example, using one port does not automatically flush
the other port's buffer, and reading or writing in one port moves the
file position (if any) for the other port. For regular files, use
separate @scheme[open-input-file] and @scheme[open-output-file] calls
to avoid confusion.}

@defproc[(call-with-input-file [path path-string?]
                               [proc (input-port? . -> . any)]
                               [#:mode mode-flag (one-of/c 'binary 'text) 'binary])
         any]{
Calls @scheme[open-input-port] with the @scheme[path] and
@scheme[mode-flag] arguments, and passes the resulting port
to @scheme[proc]. The result of @scheme[proc] is the result of the
@scheme[call-with-input-file] call, but the newly opened port is closed
when @scheme[thunk] return.}

@defproc[(call-with-output-file [path path-string?]
                                [proc (output-port? . -> . any)]
                                [#:mode mode-flag (one-of/c 'binary 'text) 'binary]
                                [#:exists exists-flag (one-of/c 'error 'append 'update
                                                                'replace 'truncate 'truncate/replace) 'error])
         any]{
Analogous to @scheme[call-with-input-file], but passing @scheme[path],
@scheme[mode-flag] and @scheme[exists-flag] to
@scheme[open-output-file].}

@defproc[(call-with-input-file* [path path-string?]
                                [proc (input-port? . -> . any)]
                                [#:mode mode-flag (one-of/c 'binary 'text) 'binary])
         any]{
Like @scheme[call-with-input-file], but the newly opened port is
closed whenever control escapes the the dynamic extent of the
@scheme[call-with-input-file*] call, whether through @scheme[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(call-with-output-file* [path path-string?]
                                 [proc (output-port? . -> . any)]
                                 [#:mode mode-flag (one-of/c 'binary 'text) 'binary]
                                 [#:exists exists-flag (one-of/c 'error 'append 'update
                                                                 'replace 'truncate 'truncate/replace) 'error])
         any]{
Like @scheme[call-with-output-file], but the newly opened port is
closed whenever control escapes the the dynamic extent of the
@scheme[call-with-output-file*] call, whether through @scheme[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(with-input-from-file [path path-string?]
                               [thunk (-> any)]
                               [#:mode mode-flag (one-of/c 'binary 'text) 'binary])
         any]{
Like @scheme[call-with-input-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current input port (see @scheme[current-input-port]) using
@scheme[parameterize] around the call to @scheme[thunk].}

@defproc[(with-output-to-file [path path-string?]
                              [thunk (-> any)]
                              [#:mode mode-flag (one-of/c 'binary 'text) 'binary]
                              [#:exists exists-flag (one-of/c 'error 'append 'update
                                                              'replace 'truncate 'truncate/replace) 'error])
         any]{
Like @scheme[call-with-output-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current output port (see @scheme[current-output-port]) using
@scheme[parameterize] around the call to @scheme[thunk].}
