#lang scribble/doc
@(require "common.rkt")

@title[#:tag "server-setup"]{Server Setup}

@declare-exporting[#:use-sources (handin-server/scribblings/hook-dummy)]

You must prepare a special directory to host the handin server.  To run
the server, you should either be in this directory, or you should set
the @envvar{PLT_HANDINSERVER_DIR} environment variable.

This directory contains the following files and sub-directories:
@itemize[
@item{@filepath{server-cert.pem}: the server's certificate.  To create a
  certificate and key with openssl:
  @commandline{openssl req -new -nodes -x509 -days 365
                 -out server-cert.pem -keyout private-key.pem}}

@item{@filepath{private-key.pem}: the private key to go with
  @filepath{server-cert.pem}.  Whereas @filepath{server-cert.pem} gets
  distributed to students with the handin client,
  @filepath{private-key.pem} is kept private.}

@item{@filepath{config.rktd}: configuration options.  The file format is
  @verbatim[#:indent 2]{((<key> <val>) ...)}

  The following keys can be used:

  @itemize[
  @item{@indexed-racket[active-dirs] --- a list of directories that
    are active submissions, relative to the current directory or
    absolute; the last path element for each of these (and
    @racketid[inactive-dirs] below) should be unique, and is used to
    identify the submission (for example, in the client's submission
    dialog and in the status servlet).  If a specified directory does
    not exist, it will be created.}

  @item{@indexed-racket[inactive-dirs] --- a list of inactive
    submission directories (see above for details).}

  @item{@indexed-racket[port-number] --- the port for the main handin
    server; the default is 7979.}

  @item{@indexed-racket[use-https] --- determines whether to start an
    embedded web server for handin status reports; the default is
    @racket[#t].}

  @item{@indexed-racket[session-timeout] --- number of seconds before
    the session times-out.  The client is given this many seconds for
    the login stage and then starts again so the same number of
    seconds is given for the submit-validation process; the default is
    300.}

  @item{@indexed-racket[session-memory-limit] --- maximum size in
    bytes of memory allowed for per-session computation, if
    per-session limits are supported (i.e., when using GRacket and
    Racket with the (default) exact garbage collector and memory
    accounting); the default is 40000000.}

  @item{@indexed-racket[allow-web-upload] --- either @racket[#f] (to
    disable upload via the HTTPS status server) or a non-empty list of
    suffix strings (to enable uploads for active assignments and force
    the uploaded file to have one of the suffixes); the default is
    @racket[#f]. The suffix strings should include a @litchar{.}, as
    in @racket[".rkt"].}

  @item{@indexed-racket[default-file-name] --- the default filename
    that will be saved with the submission contents.  The default is
    @filepath{handin.rkt}.}

  @item{@indexed-racket[max-upload] --- maximum size in bytes of an
    acceptable submission; the default is 500000.}

  @item{@indexed-racket[max-upload-keep] --- maximum index of
    submissions to keep; the most recent submission is
    @filepath{handin.rkt} (by default), the next oldest is in
    @filepath{BACKUP-0/handin.rkt}, next oldest is
    @filepath{BACKUP-1/handin.rkt}, etc.  The default is 9.}

  @item{@indexed-racket[user-regexp] --- a regular expression that is
    used to validate usernames; alternatively, this can be @racket[#f]
    meaning no restriction, or a list of permitted strings.  Young
    students often choose exotic usernames that are impossible to
    remember, and forget capitalization, so the default is fairly
    strict--- @racket[#rx"^[a-z][a-z0-9]+$"]; a @litchar{+} is always
    disallowed in a username, since it is used in a submission username
    to specify joint work.}

  @item{@indexed-racket[user-desc] --- a plain-words description of
    the acceptable username format (according to user-regexp above);
    @racket[#f] stands for no description; the default is
    @racket["alphanumeric string"] which matches the default
    user-regexp.}

  @item{@indexed-racket[username-case-sensitive] --- a boolean; when
    @racket[#f], usernames are case-folded for all purposes; defaults
    to @racket[#f] (note that you should not set this to @racket[#t]
    on Windows or when using other case-insensitive filesystems, since
    usernames are used as directory names).}

  @item{@indexed-racket[allow-new-users] --- a boolean indicating
    whether to allow new-user requests from a client tool; the default
    is @racket[#f].}

  @item{@indexed-racket[allow-change-info] --- a boolean indicating
    whether to allow changing user information from a client tool
    (changing passwords is always possible); the default is
    @racket[#f].}

  @item{@indexed-racket[master-password] --- a string for an MD5 hash
    for a password that allows login as any user; the default is
    @racket[#f], which disables the password.}

  @item{@indexed-racket[log-output] --- a boolean that controls
    whether the handin server log is written on the standard output;
    defaults to @racket[#t].}

  @item{@indexed-racket[log-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename for the
    handin server log (possibly combined with the @racketid[log-output]
    option), or @racket[#f] for no log file; defaults to
    @filepath{log}.}

  @item{@indexed-racket[web-log-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename for
    logging the internal HTTPS status web server; or @racket[#f] (the
    default) to disable this log.}

  @item{@indexed-racket[extra-fields] --- a list that describes extra
    string fields of information for student records; each element in
    this list is a list of three values: the name of the field, the
    regexp (or @racket[#f], or a list of permitted string values), and
    a string describing acceptable strings.  The default is
    @verbatim[#:indent 2]|{
      (("Full Name" #f #f)
       ("ID#" #f #f)
       ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
        "a valid email address"))
    }|
    You can set this to a list of fields that you are interested in
    keeping, for example:
    @verbatim[#:indent 2]|{
      (("Full Name"
        #rx"^[A-Z][a-zA-Z-]+(?: [A-Z][a-zA-Z-]+)+$"
        "full name, no punctuation, properly capitalized")
       ("Utah ID Number"
        #rx"^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"
        "Utah ID Number with exactly nine digits")
       ("Email"
        #rx"^[^@<>\"`',]+@cs\\.utah\\.edu$"
        "A Utah CS email address"))}|
    The order of these fields will be used both on the client GUI side
    and in the @filepath{users.rktd} file (see below).

    @; JBC: a hyperlink here for users.rktd?

    The second item in a field description can also be the symbol
    @racketid[-], which marks this field as one that is hidden from the
    user interface: students will not see it and will not be able to
    provide or modify it; when a new student creates an account, such
    fields will be left empty.  This is useful for adding information
    that you have on students from another source, for example, adding
    information from a course roster.  You should manually edit the
    @filepath{users.rktd} file and fill in such information.  (The third
    element for such descriptors is ignored.)}

  @item{@indexed-racket[hook-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename that
    contains a `hook' module.  This is useful as a general device for
    customizing the server through Racket code.  The file is expected
    to contain a module that provides a @racket[hook] function, which
    should be receiving three arguments:

    @defproc[(hook [operation symbol?]
                   [connection-context (or/c number? symbol? false?)]
                   [relevant-info (listof (list/c symbol? any))])
             void?]{

      The @racket[operation] argument indicates the operation that is
      now taking place.  It can be one of the following:
      @indexed-racket['server-start], @indexed-racket['server-connect],
      @indexed-racket['user-create], @indexed-racket['user-change],
      @indexed-racket['login], @indexed-racket['submission-received],
      @indexed-racket['submission-committed],
      @indexed-racket['submission-retrieved],
      @indexed-racket['status-login], or
      @indexed-racket['status-file-get].

      The @racket[connection-context] argument is a datum that specifies
      the connection context (a number for handin connections, a
      @racketid[wN] symbol for servlet connections, and @racket[#f] for
      other server operations).

      The @racket[relevant-info] contains an alist of information
      relevant to this operation.  Currently, the hook is used in
      several places after an operation has completed.

      For example, here is a simple hook module that sends
      notification messages when users are created or their
      information has changed:

      @racketmod[
        racket/base
        (provide hook)
        (require net/sendmail)
        (define (hook what session alist)
          (when (memq what '(user-create user-change))
            (send-mail-message
             "course-staff@university.edu"
             (format "[server] ~a (~a)" what session)
             '("course-staff@university.edu") '() '()
             (map (lambda (key+val)
                    (apply format "~a: ~s" key+val))
                  alist))))]}}]

  In addition, you can add your own keys --- see @racket[get-conf] for
  details.

  Changes to @filepath{config.rktd} are detected, the file will be
  re-read, and options will be reloaded.  A few options are fixed at startup
  time: port numbers and log file specs are fixed as configured at
  startup.  All other options will change the behavior of the running
  server (but for things like @racketid[username-case-sensitive?] it would
  be unwise to do so).  (For safety, options are not reloaded until the
  file parses correctly, but make sure that you don't save a copy that
  has inconsistent options: it is best to create a new configuration
  file and move it over the old one, or use an editor that does so and
  avoid saving until the new contents is ready.)  This is most useful for
  closing & opening submissions directories.}

@item{@filepath{users.rktd} (created if not present when a user is added):
  keeps the list of user accounts, along with the associated password
  (actually the MD5 hash of the password), and extra string fields as
  specified by the @racketid[extra-fields] configuration entry (in the
  same order).  The file format is
  @verbatim[#:indent 2]{
    ((<username-sym> (<pw-md5-str> <extra-field> ...))
     ...)}

  For example, the default @racketid[extra-field] setting will make
  this:
  @verbatim[#:indent 2]{
    ((<username-sym> (<pw-md5-str> <full-name> <id> <email>))
      ...)}

  Usernames that begin with ``solution'' are special.  They are used by
  the HTTPS status server.  Independent of the @racketid[user-regexp]
  and @racketid[username-case-sensitive?] configuration items, usernames
  are not allowed to contain characters that are illegal in Windows
  pathnames, and they cannot end or begin in spaces or periods.

  If the @racketid[allow-new-users] configuration allows new users, the
  @filepath{users.rktd} file can be updated by the server with new users.
  It can always be updated by the server to change passwords.

  If you have access to a standard Unix password file (from
  @filepath{/etc/passwd} or @filepath{/etc/shadow}), then you can
  construct a @filepath{users.rktd} file that will allow users to use
  their normal passwords.  To achieve this, use a list with
  @racketid[unix] as the first element and the system's encrypted
  password string as the second element.  Such passwords can be used,
  but when users change them, a plain md5 hash will be used.

  You can combine this with other fields from the password file to
  create your @filepath{users.rktd}, but make sure you have information
  that matches your @racketid[extra-fields] specification.  For example,
  given this system file:
  @verbatim[#:indent 2]|{
    foo:wRzN1u5q2SqRD:1203:1203:L.E. Foo        :/home/foo:/bin/tcsh
    bar:$1$dKlU0OkJ$t63TzKz:1205:1205:Bar Z. Lie:/home/bar:/bin/bash}|
  you can create this @filepath{users.rktd} file:
  @verbatim[#:indent 2]|{
    ((foo ((unix "wRzN1u5q2SqRD") "L.E. Foo" "?"))
     (bar ((unix "$1$dKlU0OkJ$t63TzKz") "Bar Z. Lie" "?")))}|
  which can be combined with this setting for @racketid[extra-fields] in
  your @filepath{config.rktd}:
  @verbatim[#:indent 2]{
    ...
    (extra-fields (("Full Name" #f #f)
                   ("TA" ("Alice" "Bob") "Your TA")))
    ...}
  and you can tell your students to use their department username and
  password, and use the @onscreen{Manage ...} dialog to properly set
  their TA name.

  Finally, a password value can be a list that begins with a
  @racketid[plaintext] symbol, which will be used without encryption.
  This may be useful for manually resetting a forgotten passwords.}

@item{@filepath{log} (or any other name that the @racketid[log-file]
  configuration option specifies (if any), created if not present,
  appended otherwise): records connections and actions, where each entry
  is of the form
  @verbatim{[<id>|<time>] <msg>}
  where @racketid[<id>] is an integer representing the connection
  (numbered consecutively from 1 when the server starts), ``@tt{-}'' for
  a message without a connection, and ``@tt{wN}'' for a message from the
  status servlet.}

@item{Active and inactive assignment directories (which you can put in
  a nested directory for convenience, or specify a different absolute
  directory), as specified by the configuration file using the
  @racketid[active-dirs] and @racketid[inactive-dirs].  A list of active
  assignment directories (the last path element in each specified path
  is used as a label) is sent to the client tool when a student clicks
  @onscreen{Handin}.  The assignment labels are ordered in the student's
  menu using @racket[string<?], and the first assignment is the default
  selection.

  Within each assignment directory, the student id is used for a
  sub-directory name.  Within each student sub-directory are
  directories for handin attempts and successes.  If a directory
  @filepath{ATTEMPT} exists, it contains the most recent (unsuccessful
  or currently-in-submission) handin attempt.  Directories
  @filepath{SUCCESS-n} (where n counts from 0) contain successful
  handins; the lowest numbered such directory represents the latest
  handin.

  A cleanup process in the server copies successful submissions to the
  student directory, one level up from the corresponding
  @filepath{SUCCESS-n} directory.  This is done only for files and
  directories that are newer in @filepath{SUCCESS-n} than in the
  submission root, other files and directories are left intact.  If
  external tools add new content to the student directory (e.g., a
  @filepath{grade} file, as described below) it will stay there.  If
  the machine crashes or the server is stopped, the cleanup process
  might not finish.  When the server is started, it automatically runs
  the cleanup process for each student directory.

  Within a student directory, a @filepath{handin.rkt} file (or some
  other name if the @racketid[default-file-name] option is set) contains
  the actual submission.  A @racket[checker] procedure can change this
  default file name, and it can create additional files in an
  @filepath{ATTEMPT} directory (to be copied by the cleanup process);
  see below for more details on @racketmodname[handin-server/checker].

  For submissions from a normal DrRacket frame, a submission file
  contains a copy of the student's definitions and interactions
  windows.  The file is in a binary format (to support non-text code),
  and opening the file directly in DrRacket shows the definitions
  part.  To get both the definitions and interactions parts, the file
  can be parsed with @racket[unpack-submission] from
  @racketmodname[handin-server/utils].

  To submit an assignment as a group, students use a concatenation of
  usernames separated by ``@tt{+}'' and any number of spaces (e.g.,
  ``@tt{user1+user2}'').  The same syntax (``@tt{user1+user2}'') is
  used for the directory for shared submissions, where the usernames
  are always sorted so that directory names are deterministic.
  Multiple submissions for a particular user in different groups will
  be rejected.

  Inactive assignment directories are used by the HTTPS status web
  server.}

@item{@filepath{<active-assignment>/checker.rkt} (optional): a module
  that exports a @racket[checker] function.  This function receives
  two
  @; JBC: use defproc here?
  arguments: a username list and a submission as a byte string.  (See
  also @racket[unpack-submission], etc. from
  @racketmodname[handin-server/utils].)  To
    reject the submission, the @racket[checker] function can raise an
    exception; the exception message will be relayed back to the
    student.  The module is loaded when the current directory is the
    main server directory, so it can read files from there (but note
    that to read values from @filepath{config.rktd} it is better to use
    @racket[get-conf]).  Also, the module will be reloaded if the
    checker file is modified; there's no need to restart the server,
    but make sure that you do not save a broken checker (i.e., do not
    save in mid-edit).

    The first argument is a list of usernames with at least one
    username, and more than one if this is a joint submission (where
    the submission username was a concatenation of usernames separated
    by ``@tt{+}'').

    The @racket[checker] function is called with the current directory
    as @filepath{<active-assignment>/<username(s)>/ATTEMPT}, and the
    submission is saved in the file @filepath{handin}, and the timeout
    clock is reset to the value of the @racket[session-timeout]
    configuration.  The checker function can change @filepath{handin},
    and it can create additional files in this directory.  (Extra
    files in the current directory will be preserved as it is later
    renamed to @filepath{SUCCESS-0}, and copied to the submission's
    root (@filepath{<active-assignment>/<username(s)>/}), etc.)  To
    hide generated files from the HTTPS status web server interface,
    put the files in a subdirectory, it is preserved but hidden from
    the status interface.

    The checker should return a string, such as @filepath{handin.rkt},
    to use in naming the submission file, or @racket[#f] to indicate
    that he file should be deleted (e.g., when the checker alrady
    created the submission file(s) in a different place).

    Alternatively, the module can bind @racket[checker] to a list of
    three procedures: a pre-checker, a checker, and a post-checker.
    All three are applied in exactly the same way as the checker (same
    arguments, and always within the submission directory), except
    that:
    @itemize[

    @item{If there was an error during the pre-checker, and the
      submission directory does not have a @filepath{SUCCESS-*}
      directory, then the whole submission directory is removed.  This
      is useful for checking that the user/s are valid; if you allow a
      submission only when @racket[users] is @racket['("foo" "bar")],
      and ``@tt{foo}'' tries to submit alone, then the submission
      directory for ``@tt{foo}'' should be removed to allow a proper
      submission later.  Note that the timeout clock is reset only
      once, before the pre-checker is used.}

    @item{The post-checker is used at the end of the process, after
      the @filepath{ATTEMPT} directory was renamed to
      @filepath{SUCCESS-0}.  At this stage, the submission is
      considered successful, so this function should avoid throwing an
      exception (it can, but the submission will still be in place).
      This is useful for things like notifying the user of the
      successful submission (see @racket[message]), or sending a
      ``receipt'' email.}]

    To specify only pre/post-checker, use @racket[#f] for the one you
    want to omit.}

@item{@filepath{<[in]active-assignment>/<user(s)>/<filename>} (if
  submitted): the most recent submission for
  @tt{<[in]active-assignment>} by @tt{<user(s)>} where <filename> was
  returned by the checker (or the value of the
  @racketid[default-file-name] configuration option if there's no
  checker).  If the submission is from multiple users, then
  ``@tt{<user(s)>}'' is actually ``@tt{<user1>+<user2>}'' etc.  Also, if
  the cleanup process was interrupted (by a machine failure, etc.), the
  submission may actually be in @filepath{SUCCESS-n} as described above,
  but will move up when the server performs a cleanup (or when
  restarted).}

@item{@filepath{<[in]active-assignment>/<user(s)>/grade} (optional):
  the @tt{<user(s)>}'s grade for @tt{<[in]active-assignment>}, to be
  reported by the HTTPS status web server}

@item{@filepath{<[in]active-assignment>/solution*}: the solution to
  the assignment, made available by the status server to any user who
  logs in.  The solution can be either a file or a directory with a
  name that begins with @filepath{solution}.  In the first case, the
  status web server will have a ``Solution'' link to the file, and in
  the second case, all files in the @filepath{solution*} directory
  will be listed and accessible.}
]

Remember that if you're not using the (default) 3m variant of Racket,
you don't get memory accounting.

The server currently provides no mechanism for a graceful shutdown,
but terminating the server is no worse than a network outage.  (In
particular, no data should be lost.)  The server reloads the
configuration file, checker modules etc, so there should not be any
need to restart it for reconfigurations.

The client and server are designed to be robust against network
problems and timeouts.  The client-side tool always provides a
@onscreen{cancel} button for any network transaction.  For handins,
@onscreen{cancel} is guaranteed to work up to the point that the
client sends a ``commit'' command; this command is sent only after the
server is ready to record the submission (having run it through the
checker, if any), but before renaming @filepath{ATTEMPT}.  Also, the
server responds to a commit with @onscreen{ok} only after it has
written the file.  Thus, when the client-side tool reports that the
handin was successful, the report is reliable.  Meanwhile, the tool
can also report successful cancels most of the time.  In the (normally
brief) time between a commit and an @onscreen{ok} response, the tool
gives the student a suitable warning that the cancel is unreliable.

To minimize human error, the number of active assignments should be
limited to one whenever possible.  When multiple assignments are
active, design a checker to help ensure that the student has selected
the correct assignment in the handin dialog.

A student can download his/her own submissions through the handin
dialog.  This can also be done through a web server that runs
concurrently with the handin server (on the same port) if you set the
@racket[use-https] option in the configuration file to @racket[#t] (the
default).  The starting URL is

@commandline{https://SERVER:PORT/}

to obtain a list of all assignments, or

@commandline{https://SERVER:PORT/?handin=ASSIGNMENT}

to start with a specific assignment (named ASSIGNMENT).
