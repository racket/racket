#lang scribble/doc
@(require "mz.ss"
          racket/file)

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

A port created by @scheme[open-input-file], @scheme[open-output-file],
@scheme[subprocess], and related functions is a @deftech{file-stream
port}.  The initial input, output, and error ports in stand-alone
MzScheme are also file-stream ports. The @scheme[file-stream-port?]
predicate recognizes file-stream ports.

When an input or output file-stream port is created, it is placed into
the management of the current custodian (see
@secref["custodians"]).

@defproc[(open-input-file [path path-string?]
                          [#:mode mode-flag (or/c 'binary 'text) 'binary])
         input-port?]{

Opens the file specified by @scheme[path] for input. The
@scheme[mode-flag] argument specifies how the file's bytes are
translated on input:

@itemize[

 @item{@indexed-scheme['binary] --- bytes are returned from the port
 exactly as they are read from the file.}

 @item{@indexed-scheme['text] --- return and linefeed bytes (10 and
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

Under Windows, @scheme['text] mode works only with regular files;
attempting to use @scheme['text] with other kinds of files triggers an
@scheme[exn:fail:filesystem] exception.

Otherwise, the file specified by @scheme[path] need not be a regular
file. It might a device that is connected through the filesystem, such
as @filepath{aux} under Windows or @filepath{/dev/null} under Unix. In all
cases, the port is buffered by default.

The port produced by @scheme[open-input-file] should be explicitly
closed, either though @scheme[close-input-port] or indirectly via
@scheme[custodian-shutdown-all], to release the OS-level file
handle. The input port will not be closed automatically if it is
otherwise available for garbage collection (see
@secref["gc-model"]); a @tech{will} could be associated input port
to close it more automatically (see @secref["willexecutor"]).

A @tech{path} value that is the @tech{cleanse}d version of
@scheme[path] is used as the name of the opened port.

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

Opens the file specified by @scheme[path] for output. The
@scheme[mode-flag] argument specifies how bytes written to the port
are translated when written to the file:

@itemize[

 @item{@scheme['binary] --- bytes are written to the file exactly
 as written to the port.}

 @item{@scheme['text] --- under Windows, a linefeed byte (10) written
 to the port is translated to a return-linefeed combination in the
 file; no filtering occurs for returns.}

]

Under Windows, @scheme['text] mode works only with regular files;
attempting to use @scheme['text] with other kinds of files triggers an
@scheme[exn:fail:filesystem] exception.

The @scheme[exists-flag] argument specifies how to handle/require
files that already exist:

@itemize[

 @item{@indexed-scheme['error] --- raise @scheme[exn:fail:filesystem]
       if the file exists.}

 @item{@indexed-scheme['replace] --- remove the old file, if it
       exists, and write a new one.}

 @item{@indexed-scheme['truncate] --- remove all old data, if the file
       exists.}

 @item{@indexed-scheme['must-truncate] --- remove all old data in an
       existing file; if the file does not exist, the
       @exnraise[exn:fail:filesystem].}

 @item{@indexed-scheme['truncate/replace] --- try @scheme['truncate];
       if it fails (perhaps due to file permissions), try
       @scheme['replace].}

 @item{@indexed-scheme['update] --- open an existing file without
       truncating it; if the file does not exist, the
       @exnraise[exn:fail:filesystem]. Use @scheme[file-position]
       to change the current read/write position.}

 @item{@indexed-scheme['can-update] --- open an existing file without
       truncating it, or create the file if it does not exist.}

 @item{@indexed-scheme['append] --- append to the end of the file,
       whether it already exists or not; under Windows,
       @scheme['append] is equivalent to @scheme['update], except that
       the file is not required to exist, and the file position is
       immediately set to the end of the file after opening it.}

]

The file specified by @scheme[path] need not be a regular file. It
might a device that is connected through the filesystem, such as
@filepath{aux} under Windows or @filepath{/dev/null} under Unix. The output
port is block-buffered by default, unless the file corresponds to a
terminal, in which case is it line buffered bu default.

The port produced by @scheme[open-output-port] should be explicitly
closed, either though @scheme[close-output-port] or indirectly via
@scheme[custodian-shutdown-all], to release the OS-level file
handle. The output port will not be closed automatically if it is
otherwise available for garbage collection (see
@secref["gc-model"]); a @tech{will} could be associated input port
to close it more automatically (see @secref["willexecutor"]).

A @tech{path} value that is the @tech{cleanse}d version of
@scheme[path] is used as the name of the opened port.

@file-examples[
(define out (open-output-file some-file))
(write "hello world" out)
(close-output-port out)
]}

@defproc[(open-input-output-file [path path-string?]
                           [#:mode mode-flag (or/c 'binary 'text) 'binary]
                           [#:exists exists-flag (or/c 'error 'append 'update
                                                       'replace 'truncate 'truncate/replace) 'error])
          (values input-port? output-port?)]{

Like @scheme[open-output-file], but producing two values: an input
port and an output port. The two ports are connected in that they
share the underlying file device. This procedure is intended for use
with special devices that can be opened by only one process, such as
@filepath{COM1} in Windows. For regular files, sharing the device can be
confusing. For example, using one port does not automatically flush
the other port's buffer, and reading or writing in one port moves the
file position (if any) for the other port. For regular files, use
separate @scheme[open-input-file] and @scheme[open-output-file] calls
to avoid confusion.}

@defproc[(call-with-input-file [path path-string?]
                               [proc (input-port? . -> . any)]
                               [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{
Calls @scheme[open-input-file] with the @scheme[path] and
@scheme[mode-flag] arguments, and passes the resulting port
to @scheme[proc]. The result of @scheme[proc] is the result of the
@scheme[call-with-input-file] call, but the newly opened port is closed
when @scheme[thunk] return.

@file-examples[
(with-output-to-file some-file
  (lambda () (printf "text in a file")))
(call-with-input-file some-file
  (lambda (in) (read-string 15 in)))
]}

@defproc[(call-with-output-file [path path-string?]
                                [proc (output-port? . -> . any)]
                                [#:mode mode-flag (or/c 'binary 'text) 'binary]
                                [#:exists exists-flag (or/c 'error 'append 'update
                                                            'replace 'truncate 'truncate/replace) 'error])
         any]{
Analogous to @scheme[call-with-input-file], but passing @scheme[path],
@scheme[mode-flag] and @scheme[exists-flag] to
@scheme[open-output-file].

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
Like @scheme[call-with-input-file], but the newly opened port is
closed whenever control escapes the dynamic extent of the
@scheme[call-with-input-file*] call, whether through @scheme[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(call-with-output-file* [path path-string?]
                                 [proc (output-port? . -> . any)]
                                 [#:mode mode-flag (or/c 'binary 'text) 'binary]
                                 [#:exists exists-flag (or/c 'error 'append 'update
                                                             'replace 'truncate 'truncate/replace) 'error])
         any]{
Like @scheme[call-with-output-file], but the newly opened port is
closed whenever control escapes the dynamic extent of the
@scheme[call-with-output-file*] call, whether through @scheme[proc]'s
return, a continuation application, or a prompt-based abort.}

@defproc[(with-input-from-file [path path-string?]
                               [thunk (-> any)]
                               [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{
Like @scheme[call-with-input-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current input port (see @scheme[current-input-port]) using
@scheme[parameterize] around the call to @scheme[thunk].

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
Like @scheme[call-with-output-file*], but instead of passing the newly
opened port to the given procedure argument, the port is installed as
the current output port (see @scheme[current-output-port]) using
@scheme[parameterize] around the call to @scheme[thunk].

@file-examples[
(with-output-to-file some-file
  (lambda () (printf "hello")))
(with-input-from-file some-file
  (lambda () (read-string 5)))
]}

@defproc[(port-file-identity [port file-stream-port?]) exact-positive-integer?]{

@index['("inode")]{Returns} a number that represents
the identity of the device and file read or written by
@scheme[port]. For two ports whose open times overlap, the
result of @scheme[port-file-identity] is the same for both ports if
and only if the ports access the same device and file. For ports whose
open times do not overlap, no guarantee can be provided for the port
identities (even if the ports actually access the same file)---except
as can be inferred through relationships with other ports. If
@scheme[port] is closed, the @exnraise[exn:fail].  Under
Windows 95, 98, and Me, if @scheme[port] is connected to a
pipe instead of a file, the @exnraise[exn:fail:filesystem].

@file-examples[
(define file1 (open-output-file some-file))
(define file2 (open-output-file some-other-file))
(port-file-identity file1)
(port-file-identity file2)
(close-output-port file1)
(close-output-port file2)
]}
