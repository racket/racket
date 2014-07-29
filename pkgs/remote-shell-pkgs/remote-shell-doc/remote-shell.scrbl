#lang scribble/manual
@(require (for-label racket/base
                     racket/contract
                     remote-shell/ssh
                     remote-shell/vbox))

@title{Remote Shells and Virtual Machines}

The @filepath{remote-shell} collection provides tools for running
shell commands on a remote or virtual machine, including tools for
starting, stopping, and managing VirtualBox virtual-machine instances.

@table-of-contents[]

@; ----------------------------------------

@section{Remote Shells}

@defmodule[remote-shell/ssh]

@defproc[(remote? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a remote-host representation
produced by @racket[remote], @racket[#f] otherwise.}

@defproc[(remote [#:host host string?]
                 [#:user user string? ""]
                 [#:env env (listof (cons/c string? string?)) '()]
                 [#:remote-tunnels remote-tunnels (listof (cons/c (integer-in 1 65535)
                                                                  (integer-in 1 65535)))
                                   null]
                 [#:timeout timeout-secs real? 600])
         remote?]{

Creates a representation of a remote host. The @racket[host] argument
specifies the host for an @exec{ssh} connection. If @racket[user] is
empty, then the current user name is used for the remote host.

The @racket[env] argument specifies environment variables to set
before running any command on the remote host.

The @racket[remote-tunnels] argument specifies ports to tunnel from
the remote host back to the local host. The first port number in each
pair is the port number on the remote host, and the second port number
is the port that it tunnels to on the local host.

The @racket[timeout] argument specifies a timeout after which a remote
command will be considered failed.}


@defproc[(ssh [remote remote?]
              [command (or/c string? path-string?)]
              [#:mode mode (or/c 'error 'result 'output) 'error]
              [#:failure-log failure-dest (or/c #f path-string?) #f]
              [#:success-log success-dest (or/c #f path-string?) #f]
              [#:show-time? show-time? any/c #f])
           (or/c void? boolean? (cons/c boolean? bytes?))]{

Runs a shell command at @racket[remote], were the @racket[command]s
are concatenated (with no additional spaces) to specify the remote
shell command. The remote command is implemented with @exec{ssh} as
found by @racket[find-system-path].

If @racket[mode] is @racket['error], then the result is
@racket[(void)] or an exception is raised if the remote command fails
with an connection error, an error exit code, or by timing out. If
@racket[mode] is @racket['result], then the result is @racket[#t] for
success or @racket[#f] for failure. If @racket[mode] is
@racket['cons], then the result is a pair containing whether the
command succeeded and a byte string for the command's output
(including error output).

If @racket[failure-dest] is not @racket[#f], then if the command
fails, the remote output (including error output) is recorded to the
specified file. If @racket[success-dest] is not @racket[#f], then if
the command fails, the remote output (including error output) is
recorded to the specified file.}

@defproc[(scp [remote remote?]
              [source path-string?]
              [dest path-string?]
              [#:mode mode (or/c 'error 'result 'output) 'error])
          (or/c void? boolean?)]{

Copies a file to/from a remote host. Use @racket[at-remote] to form
either the @racket[source] or @racket[dest] argument. The remote
command is implemented with @exec{scp} as found by
@racket[find-system-path].

If @racket[mode] is @racket['error], then the result is
@racket[(void)] or an exception is raised if the remote command
fails. If @racket[mode] is @racket['result], then the result is
@racket[#t] for success or @racket[#f] for failure.}


@defproc[(at-remote [remote remote?]
                    [path path-string?])
         string?]{

Combines @racket[remote] and @racket[path] to form an argument for
@racket[scp] to specify a path at the remote host.}


@defproc[(make-sure-remote-is-ready [remote remote?]
                                    [#:tries tries exact-nonnegative-integer? 3])
         void?]{

Runs a simple command at @racket[remote] to check that it receives
connections, trying up to @racket[tries] times.}


@; ----------------------------------------

@section{Managing VirtualBox Machines}

@defmodule[remote-shell/vbox]

@defproc[(start-vbox-vm [name string?]
                        [#:max-vms max-vms real? 1]
                        [#:log-status log-status (string? #:rest any/c . -> . any) printf]
                        [#:pause-seconds pause-seconds real? 3]
                        [#:dry-run? dry-run? any/c #f])
          void?]{

Starts a VirtualBox virtual machine @racket[name] that is in a saved,
powered off, or running state (where a running machine continues to
run).

The start will fail if @racket[max-vms] virtual machines are already
currently running. This limit is a precaution against starting too
many virtual-machine instances, which can overwhelm the host operating
system.

The @racket[log-status] argument is used to report actions and status
information.

After the machine is started, @racket[start-vbox-vm] pauses for the
amount of time specified by @racket[pause-seconds], which gives the
virtual machine time to find its bearings.

If @racket[dry-run] is @racket[#t], then the machine is not actually
started, but status information is written using @racket[log-status]
to report the action that would have been taken.}


@defproc[(stop-vbox-vm [name string?]
                       [#:save-state? save-state? any/c #t]
                       [#:log-status log-status (string? #:rest any/c . -> . any) printf]
                       [#:dry-run? dry-run? any/c #f])
         void?]{

Stops a VirtualBox virtual machine @racket[name] that is in a running
state. If @racket[save-state?] is true, then the machine is put into
saved state, otherwise the current machine state is discarded and the
machine is powered off.

The @racket[log-status] argument is used to report actions and status
information.

If @racket[dry-run] is @racket[#t], then the machine is not actually
started, but status information is written using @racket[log-status]
to report the action that would have been taken.}


@defproc[(take-vbox-snapshot [name string?]
                             [snapshot-name string?])
         void?]{

Takes a snapshot of a virtual machine (which may be running), creating
the snapshot named @racket[snapshot-name].}


@defproc[(restore-vbox-snapshot [name string?]
                                [snapshot-name string?])
         void?]{

Changes the current state of a virtual machine to be the one recorded
as @racket[snapshot-name]. The virtual machine must not be running.}

@defproc[(delete-vbox-snapshot [name string?]
                               [snapshot-name string?])
         void?]{

Deletes @racket[snapshot-name] for the virtual machine @racket[name].}


@defproc[(exists-vbox-snapshot? [name string?]
                                [snapshot-name string?])
         boolean?]{

Reports whether @racket[snapshot-name] exists for the virtual machine
@racket[name].}
