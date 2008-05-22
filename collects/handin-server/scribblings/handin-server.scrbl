#lang scribble/doc
@(require scribble/manual
          (for-label scheme)
          #;(for-label scheme/sandbox)
          (for-label handin-server/sandbox
                     handin-server/utils
                     (only-in handin-server/checker
                              pre: post: submission-eval user-data
                              user-substs pairs-or-singles-with-warning
                              teams-in-file add-header-line! procedure/arity?
                              !defined !procedure !procedure* !integer !integer*
                              check:
                              !test !all-covered)
                     mred))

@(require (for-label handin-server/scribblings/hook-dummy))

@(define (comment . args) "")

@comment{Is there an existing mechanism for comments?}
@comment{There's no enumerate?}
@comment{commandline _and_ exec?}
@comment{using commandline for stand-alone URLs?}
@comment{TO-DO: sandbox docs, create index, TOC?}

@title{@bold{Handin Server}}


@section{Handin-Server and Client}

The @filepath{handin-server} directory contains a server to be run by a
course instructor for accepting homework assignments and reporting on
submitted assignments.

The @filepath{handin-client} directory contains a client to be
customized then re-distributed to students in the course.  The
customized client will embed a particular hostname and port where the
server is running, as well as a server certificate.

With a customized client, students simply install a @filepath{.plt}
file---so there's no futzing with configuration dialogs and
certificates.  A student can install any number of clients at once
(assuming that the clients are properly customized, as described
below).

The result, on the student's side, is a @onscreen{Handin} button in
DrScheme's toolbar.  Clicking the @onscreen{Handin} button allows the
student to type a password and upload the current content of the
definitions and interactions window to the course instructor's server.
The @onscreen{File} menu is also extended with a @onscreen{Manage...}
menu item for managing a handin account (i.e., changing the password
and other information, or creating a new account if the instructor
configures the server to allow new accounts).  Students can submit
joint work by submitting with a concatenation of usernames separated
by a ``@tt{+}''.

On the instructor's side, the handin server can be configured to check
the student's submission before accepting it.

The handin process uses SSL, so it is effectively as secure as the
server and each user's password.


@section{Quick Start for a Test Drive}

@itemize{
@item{Create a new directory.}

@item{Copy @filepath{server-cert.pem} from the
  @filepath{handin-client} collection to the new directory.

  NOTE: For real use, you need a new certificate.

  NOTE: See also @secref{wheres-the-collection}.}

@item{Copy @filepath{private-key.pem} from the
  @filepath{handin-server} collection to the new directory.

  NOTE: For real use, you need a new key.}

@item{Create a file @filepath{users.ss} with the following content:
  @schemeblock[
    ((tester ("8fe4c11451281c094a6578e6ddbf5eed"
              "Tester" "1" "test@cs")))]}

@item{Make a @filepath{test} subdirectory in your new directory.}

@item{Create a file @filepath{config.ss} with the following content:
  @schemeblock[((active-dirs ("test")))]}

@item{In your new directory, run @commandline{mred -l handin-server}}

@item{In the @filepath{handin-client} collection, edit
  @filepath{info.ss} and uncomment the lines that define
  @scheme[server:port], @scheme[tools], @scheme[tool-names], and
  @scheme[tool-icons].}

@item{Run @commandline{setup-plt -l handin-client}

  NOTE: Under Windows, the executable is ``@tt{Setup PLT}'' instead of
  ``@tt{setup-plt}''.

  NOTE: The command line arguments are optional.}

@item{Start DrScheme, click @onscreen{Handin} to run the client,
  submit with username ``@tt{tester}'' and password ``@tt{pw}''.

  The submitted file will be @filepath{.../test/tester/handin.scm}.}

@item{Check the status of your submission by pointing a web browser at
  @tt{https://localhost:7980/servlets/status.ss}.  Note the ``s'' in
  ``https''. Use the ``@tt{tester}'' username and ``@tt{pw}''
  password, as before.}
}


@section[#:tag "wheres-the-collection"]{Where is the collection?}

If you obtained the server and client by installing a @filepath{.plt}
file, then the @filepath{handin-server} and @filepath{handin-client}
directories might be in your PLT addon space.  Start MzScheme, and
enter
@schemeblock[(collection-path "handin-server")]
@schemeblock[(collection-path "handin-client")]
to find out where these collections are.


@section{Client Customization}

@itemize{
@item{Rename (or make a copy of) the @filepath{handin-client}
collection directory.  The new name should describe your class
uniquely.  For example, @filepath{uu-cpsc2010} is a good name for CPSC
2010 at the University of Utah.}

@item{Edit the first three definitions of @filepath{info.ss} in your
  renamed client collection:
  @itemize{
  @item{For @scheme[name], choose a name for the handin tool as it
    will appear in DrScheme's interface (e.g., the @onscreen{XXX} for
    the @onscreen{Manage XXX Handin Account...}  menu item).  Again,
    make the name specific to the course, in case a student installs
    multiple handin tools.  Do not use @onscreen{Handin} as the last
    part of the name, since @onscreen{Handin} is always added for
    button and menu names.}

  @item{Uncomment the definitions of @scheme[tools],
    @scheme[tool-names], and @scheme[tool-icons].}

  @item{For @scheme[server:port], uncomment the line, and use the
    hostname and port where the server will be running to accept
    handin submissions.}}

  Optionally uncomment and edit the next two definitions,
  @scheme[web-menu-name] and @scheme[web-address], to add an item to
  the @onscreen{Help} menu that opens a (course-specific) web page.}

@item{Replace @filepath{icon.png} in your renamed directory with a new
  32x32 icon.  This icon is displayed on startup with DrScheme's
  splash screen, and it is included at half size on the
  @onscreen{Handin} button.  Again, choose a distinct icon for the
  benefit of students who install multiple handin tools.  A school
  logo is typically useful, as it provides a recognizably local visual
  cue.}

@item{Replace @filepath{server-cert.pem} in your renamed directory
  with a server certificate.  The file @filepath{server-cert.pem} in
  @filepath{handin-client} collection is ok for testing, but the point
  of this certificate is to make handins secure, so you should
  generate a new (self-certifying) certificate and keep its key
  private.  (See @secref{server-setup}.)}

@item{Run @commandline{mzc --collection-plt <name>.plt <name>} where
  @tt{<name>} is the name that you chose for your directory (i.e.,
  whatever you changed @filepath{handin-client} to).}

@item{Distribute @filepath{<name>.plt} to students for installation
  into their copies of DrScheme.  The students need not have access to
  the DrScheme installation directory; the tool will be installed on
  the filesystem in the student's personal space.  If you want to
  install it once on a shared installation, use setup-plt with the
  @DFlag{all-users} flag.}

}

@section{Bogus Section}


@section[#:tag "server-setup"]{Server Setup}

@declare-exporting[#:use-sources (handin-server/scribblings/hook-dummy)]


You must prepare a special directory to host the handin server.  To
run the server, you should either be in this directory, or you should
set the @envvar{PLT_HANDINSERVER_DIR} environment variable.

This directory contains the following files and sub-directories:
@itemize{
@item{@filepath{server-cert.pem}: the server's certificate.  To create
  a certificate and key with openssl:
  @commandline{openssl req -new -nodes -x509 -days 365
                 -out server-cert.pem -keyout private-key.pem}}

@item{@filepath{private-key.pem}: the private key to go with
  @filepath{server-cert.pem}.  Whereas @filepath{server-cert.pem} gets
  distributed to students with the handin client,
  @filepath{private-key.pem} is kept private.}

@item{@filepath{config.ss}: configuration options.  The file format is
  @schemeblock[((<key> <val>) ...)]

  The following keys can be used:

  @itemize{
  @item{@indexed-scheme[active-dirs] --- a list of directories that
    are active submissions, relative to the current directory or
    absolute; the last path element for each of these (and
    @scheme[inactive-dirs] below) should be unique, and is used to
    identify the submission (for example, in the client's submission
    dialog and in the status servlet).}

  @item{@indexed-scheme[inactive-dirs] --- a list of inactive
    submission directories (see above for details).}

  @item{@indexed-scheme[port-number] --- the port for the main handin
    server; the default is 7979.}

  @item{@indexed-scheme[https-port-number] --- the port number for the
    handin-status HTTPS server; the default is @scheme[#f] which
    indicates that no HTTPS server is started.}

  @item{@indexed-scheme[session-timeout] --- number of seconds before
    the session times-out.  The client is given this many seconds for
    the login stage and then starts again so the same number of
    seconds is given for the submit-validation process; the default is
    300.}

  @item{@indexed-scheme[session-memory-limit] --- maximum size in
    bytes of memory allowed for per-session computation, if
    per-session limits are supported (i.e., when using MrEd and
    MzScheme with the (default) exact garbage collector and memory
    accounting); the default is 40000000.}

  @item{@indexed-scheme[default-file-name] --- the default filename
    that will be saved with the submission contents.  The default is
    @filepath{handin.scm}.}

  @item{@indexed-scheme[max-upload] --- maximum size in bytes of an
    acceptable submission; the default is 500000.}

  @item{@indexed-scheme[max-upload-keep] --- maximum index of
    submissions to keep; the most recent submission is
    @filepath{handin.scm} (by default), the next oldest is in
    @filepath{BACKUP-0/handin.scm}, next oldest is
    @filepath{BACKUP-1/handin.scm}, etc.  The default is 9.}

  @item{@indexed-scheme[user-regexp] --- a regular expression that is
    used to validate usernames; alternatively, this can be @scheme[#f]
    meaning no restriction, or a list of permitted strings.  Young
    students often choose exotic usernames that are impossible to
    remember, and forget capitalization, so the default is fairly
    strict--- @scheme[#rx"^[a-z][a-z0-9]+$"]; a @scheme["+"] is always
    disallowed in a username, since it is used in a submission
    username to specify joint work.}

  @item{@indexed-scheme[user-desc] --- a plain-words description of
    the acceptable username format (according to user-regexp above);
    @scheme[#f] stands for no description; the default is
    @scheme["alphanumeric string"] which matches the default
    user-regexp.}

  @item{@indexed-scheme[username-case-sensitive] --- a boolean; when
    @scheme[#f], usernames are case-folded for all purposes; defaults
    to @scheme[#f] (note that you should not set this to @scheme[#t]
    on Windows or when using other case-insensitive filesystems, since
    usernames are used as directory names).}

  @item{@indexed-scheme[allow-new-users] --- a boolean indicating
    whether to allow new-user requests from a client tool; the default
    is @scheme[#f].}

  @item{@indexed-scheme[allow-change-info] --- a boolean indicating
    whether to allow changing user information from a client tool
    (changing passwords is always possible); the default is
    @scheme[#f].}

  @item{@indexed-scheme[master-password] --- a string for an MD5 hash
    for a password that allows login as any user; the default is
    @scheme[#f], which disables the password.}

  @item{@indexed-scheme[log-output] --- a boolean that controls
    whether the handin server log is written on the standard output;
    defaults to @scheme[#t].}

  @item{@indexed-scheme[log-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename for the
    handin server log (possibly combined with the @scheme[log-output]
    option), or @scheme[#f] for no log file; defaults to
    @filepath{log}.}

  @item{@indexed-scheme[web-base-dir] --- if @scheme[#f] (the
    default), the built-in web server will use the
    @filepath{status-web-root} in this collection for its
    configuration; to have complete control over the built in server,
    you can copy and edit @filepath{status-web-root}, and add this
    configuration entry with the name of your new copy (relative to
    the handin server directory, or absolute).}

  @item{@indexed-scheme[web-log-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename for
    logging the internal HTTPS status web server; or @scheme[#f] (the
    default) to disable this log.}

  @item{@indexed-scheme[extra-fields] --- a list that describes extra
    string fields of information for student records; each element in
    this list is a list of three values: the name of the field, the
    regexp (or @scheme[#f], or a list of permitted string values), and
    a string describing acceptable strings.  The default is
    @schemeblock[
      '(("Full Name" #f #f)
        ("ID#" #f #f)
        ("Email" #rx"^[^@<>\"`',]+@[a-zA-Z0-9_.-]+[.][a-zA-Z]+$"
         "a valid email address"))]
    You can set this to a list of fields that you are interested in
    keeping, for example:
    @schemeblock[
      '(("Full Name"
         #rx"^[A-Z][a-zA-Z]+(?: [A-Z][a-zA-Z]+)+$"
         "full name, no punctuations, properly capitalized")
        ("Utah ID Number"
         #rx"^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"
         "Utah ID Number with exactly nine digits")
        ("Email"
         #rx"^[^@<>\"`',]+@cs\\.utah\\.edu$"
         "A Utah CS email address"))]
    The order of these fields will be used both on the client GUI side
    and in the @filepath{users.ss} file (see below).

    @comment{a hyperlink here for users.ss?}

    The second item in a field description can also be the symbol
    @scheme['-], which marks this field as one that is hidden from the
    user interface: students will not see it and will not be able to
    provide or modify it; when a new student creates an account, such
    fields will be left empty.  This is useful for adding information
    that you have on students from another source, for example, adding
    information from a course roster.  You should manually edit the
    @filepath{users.ss} file and fill in such information.  (The third
    element for such descriptors is ignored.)}

  @item{@indexed-scheme[hook-file] --- a path (relative to handin
    server directory or absolute) that specifies a filename that
    contains a `hook' module.  This is useful as a general device for
    customizing the server through Scheme code.  The file is expected
    to contain a module that provides a @scheme[hook] function, which
    should be receiving three arguments:

    @defproc[(hook [operation symbol?]
                   [connection-context (or/c number? symbol? false?)]
                   [relevant-info (listof (list/c symbol? any))])
             void?]{

      The @scheme[operation] argument indicates the operation that is
      now taking place.  It can be one of the following:
      @indexed-scheme['server-start],
      @indexed-scheme['server-connect], @indexed-scheme['user-create],
      @indexed-scheme['user-change], @indexed-scheme['login],
      @indexed-scheme['submission-received],
      @indexed-scheme['submission-committed],
      @indexed-scheme['submission-retrieved],
      @indexed-scheme['status-login], or
      @indexed-scheme['status-file-get].

      The @scheme[connection-context] argument is a datum that
      specifies the connection context (a number for handin
      connections, a @scheme['wN] symbol for servlet connections, and
      @scheme[#f] for other server operations).
    
      The @scheme[relevant-info] contains an alist of information
      relevant to this operation.  Currently, the hook is used in
      several places after an operation has completed.

      For example, here is a simple hook module that sends
      notification messages when users are created or their
      information has changed:

      @schememod[
        mzscheme
        (provide hook)
        (require net/sendmail)
        (define (hook what session alist)
          (when (memq what '(user-create user-change))
            (send-mail-message
             "course-staff@university.edu"
             (format "[server] ~a (~a)" what session)
             '("course-staff@university.edu") '() '()
             (map (lambda (key+val) (apply format "~a: ~s" key+val))
                  alist))))]}}}

  Changes to @filepath{config.ss} are detected, the file will be
  re-read, and options are reloaded.  A few options are fixed at
  startup time: port numbers, log file specs, and the
  @scheme[web-base-dir] are as configured at startup.  All other
  options will change the behavior of the running server (but things
  like @scheme[username-case-sensitive?]  it would be unwise to do
  so).  (For safety, options are not reloaded until the file parses
  correctly, but make sure that you don't save a copy that has
  inconsistent options: it is best to create a new configuration file
  and move it over the old one, or use an editor that does so and not
  save until the new contents is ready.)  This is most useful for
  closing & opening submissions directories.}

@item{@filepath{users.ss} (created if not present if a user is added):
  keeps the list of user accounts, along with the associated password
  (actually the MD5 hash of the password), and extra string fields as
  specified by the 'extra-fields configuration entry (in the same
  order).  The file format is
  @schemeblock{
    ((<username-sym> (<pw-md5-str> <extra-field> ...))
     ...)}

  For example, the default @scheme['extra-field] setting will make this:
  @schemeblock{
    ((<username-sym> (<pw-md5-str> <full-name> <id> <email>))
      ...)}

  Usernames that begin with ``solution'' are special.  They are used
  by the HTTPS status server.  Independent of the
  @scheme['user-regexp] and @scheme['username-case-sensitive?]
  configuration items, usernames are not allowed to contain characters
  that are illegal in Windows pathnames, and they cannot end or begin
  in spaces or periods.

  If the @scheme['allow-new-users] configuration allows new users, the
  @filepath{users.ss} file can be updated by the server with new
  users.  It can always be updated by the server to change passwords.

  If you have access to a standard Unix password file (from
  @filepath{/etc/passwd} or @filepath{/etc/shadow}), then you can
  construct a @filepath{users.ss} file that will allow users to use
  their normal passwords.  To achieve this, use a list with 'unix as
  the first element and the system's encrypted password string as the
  second element.  Such passwords can be used, but when users change
  them, a plain md5 hash will be used.

  You can combine this with other fields from the password file to
  create your @filepath{users.ss}, but make sure you have information
  that matches your 'extra-fields specification.  For example, given
  this system file:
  @verbatim[#:indent 2]{
    foo:wRzN1u5q2SqRD:1203:1203:L.E. Foo        :/home/foo:/bin/tcsh
    bar:$1$dKlU0OkJ$t63TzKz:1205:1205:Bar Z. Lie:/home/bar:/bin/bash}
  you can create this @filepath{users.ss} file:
  @schemeblock[
    ((foo ((unix "wRzN1u5q2SqRD") "L.E. Foo" "?"))
     (bar ((unix "$1$dKlU0OkJ$t63TzKz") "Bar Z. Lie" "?")))]
  which can be combined with this setting for @scheme['extra-fields]
  in your @filepath{config.ss}:
  @schemeblock[
    ...
    (extra-fields (("Full Name" #f #f)
                   ("TA" '("Alice" "Bob") "Your TA")))
    ...]
  and you can tell your students to use their department username and
  password, and use the @onscreen{Manage ...} dialog to properly set
  their TA name.

  Finally, a password value can be a list that begins with a
  @scheme['plaintext] symbol, which will be used without encryption.
  This may be useful for manually resetting a forgotten passwords.}

@item{@filepath{log} (or any other name that the @scheme['log-file]
  configuration option specifies (if any), created if not present,
  appended otherwise): records connections and actions, where each
  entry is of the form
  @verbatim{[<id>|<time>] <msg>}
  where @scheme[<id>] is an integer representing the connection
  (numbered consecutively from 1 when the server starts), ``@tt{-}''
  for a message without a connection, and ``@tt{wN}'' for a message
  from the status servlet.}

@item{Active and inactive assignment directories (which you can put in
  a nested directory for convenience, or specify a different absolute
  directory), as specified by the configuration file using the
  @scheme['active-dirs] and @scheme['inactive-dirs].  A list of active
  assignment directories (the last path element in each specified path
  is used as a label) is sent to the client tool when a student clicks
  @onscreen{Handin}.  The assignment labels are ordered in the
  student's menu using @scheme[string<?], and the first assignment is
  the default selection.

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

  Within a student directory, a @filepath{handin.scm} file (or some
  other name if the @scheme[default-file-name] option is set) contains
  the actual submission.  A @scheme[checker] procedure can change this
  default file name, and it can create additional files in an
  @filepath{ATTEMPT} directory (to be copied by the cleanup process);
  see below for more details on @schememodname[handin-server/checker].

  For submissions from a normal DrScheme frame, a submission file
  contains a copy of the student's definitions and interactions
  windows.  The file is in a binary format (to support non-text code),
  and opening the file directly in DrScheme shows the definitions
  part.  To get both the definitions and interactions parts, the file
  can be parsed with @scheme[unpack-submission] from
  @schememodname[handin-server/utils].

  To submit an assignment as a group, students use a concatenation of
  usernames separated by ``@tt{+}'' and any number of spaces (e.g.,
  ``@tt{user1+user2}'').  The same syntax (``@tt{user1+user2}'') is
  used for the directory for shared submissions, where the usernames
  are always sorted so that directory names are deterministic.
  Multiple submissions for a particular user in different groups will
  be rejected.

  Inactive assignment directories are used by the the HTTPS status web
  server.}

@item{@filepath{<active-assignment>/checker.ss} (optional): a module
  that exports a @scheme[checker] function.  This function receives
  two
  @; use defproc here?
  arguments: a username list and a submission as a byte string.  (See
  also @scheme[unpack-submission], etc. from
  @schememodname[handin-server/utils].)  To
    reject the submission, the @scheme[checker] function can raise an
    exception; the exception message will be relayed back to the
    student.  The module is loaded when the current directory is the
    main server directory, so it can read files from there (but note
    that to read values from @filepath{config.ss} it is better to use
    @scheme[get-conf]).  Also, the module will be reloaded if the
    checker file is modified; there's no need to restart the server,
    but make sure that you do not save a broken checker (i.e., do not
    save in mid-edit).

    The first argument is a list of usernames with at least one
    username, and more than one if this is a joint submission (where
    the submission username was a concatenation of usernames separated
    by ``@tt{+}'').

    The @scheme[checker] function is called with the current directory
    as @filepath{<active-assignment>/<username(s)>/ATTEMPT}, and the
    submission is saved in the file @filepath{handin}, and the timeout
    clock is reset to the value of the @scheme[session-timeout]
    configuration.  The checker function can change @filepath{handin},
    and it can create additional files in this directory.  (Extra
    files in the current directory will be preserved as it is later
    renamed to @filepath{SUCCESS-0}, and copied to the submission's
    root (@filepath{<active-assignment>/<username(s)>/}), etc.)  To
    hide generated files from the HTTPS status web server interface,
    put the files in a subdirectory, it is preserved but hidden from
    the status interface.

    The checker should return a string, such as @filepath{handin.scm},
    to use in naming the submission file, or @scheme[#f] to indicate
    that he file should be deleted (e.g., when the checker alrady
    created the submission file(s) in a different place).

    Alternatively, the module can bind @scheme[checker] to a list of
    three procedures: a pre-checker, a checker, and a post-checker.
    All three are applied in exactly the same way as the checker (same
    arguments, and always within the submission directory), except
    that:
    @itemize{

    @item{If there was an error during the pre-checker, and the
      submission directory does not have a @filepath{SUCCESS-*}
      directory, then the whole submission directory is removed.  This
      is useful for checking that the user/s are valid; if you allow a
      submission only when @scheme[users] is @scheme['("foo" "bar")],
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
      successful submission (see @scheme[message]), or sending a
      ``receipt'' email.}}

    To specify only pre/post-checker, use @scheme[#f] for the one you
    want to omit.}

@item{@filepath{<[in]active-assignment>/<user(s)>/<filename>} (if
  submitted): the most recent submission for
  @tt{<[in]active-assignment>} by @tt{<user(s)>} where <filename> was
  returned by the checker (or the value of the
  @scheme[default-file-name] configuration option if there's no
  checker).  If the submission is from multiple users, then
  ``@tt{<user(s)>}'' is actually ``@tt{<user1>+<user2>}'' etc.  Also,
  if the cleanup process was interrupted (by a machine failure, etc.),
  the submission may actually be in @filepath{SUCCESS-n} as described
  above, but will move up when the server performs a cleanup (or when
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
}

The server can be run within either MzScheme or MrEd, but
@schememodname[handin-server/utils] requires MrEd (which means that
@scheme[checker] modules will likely require the server to run under
MrEd).  Remember that if you're not using the (default) 3m garbage
collector you don't get memory accounting.

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

A student can download his/her own submissions through a web server
that runs concurrently with the handin server.  The starting URL is

@commandline{https://SERVER:PORT/servlets/status.ss}

to obtain a list of all assignments, or

@commandline{https://SERVER:PORT/servlets/status.ss?handin=ASSIGNMENT}

to start with a specific assignment (named ASSIGNMENT).  The default
PORT is 7980.


@section{Checker Utilities}

The checker utilities are provided to make writing checker functions.
They are provided in a few layers, each layer provides new
functionality in addition to the lower one.  These modules are (in
order):

@itemize{

@comment{scheme/sandbox or mzlib/sandbox?}

@item{@schememodname[scheme/sandbox]: contains basic sandbox
  evaluation utilities.  This is in MzLib since it can be used
  independently.}

@item{@schememodname[handin-server/sandbox]: contains a wrapper that
  configures MzLib's sandbox for the handin server.}

@item{@schememodname[handin-server/utils]: contains additional
  utilities for dealing with handin submissions, as well as a few
  helpers for testing code.}

@item{@schememodname[handin-server/checker]: automates the task of
  creating a checker function (in
  @filepath{<active-assignment>/checker.ss} modules) to cope with
  common submission situations.}}

The following sections describe each of these modules.


@section{Sandbox}

@defmodule[handin-server/sandbox]

This is just a wrapper around the sandbox engine from MzLib.  It
configures it for use with the handin server.


@section{Utils}

@defmodule[handin-server/utils]

@comment{have eli verify these contracts?}

@defproc[(get-conf [key symbol?]) any/c]{

  Returns a value from the configuration file (useful for reading
  things like field names, etc.).}

@defproc[(unpack-submission [submission bytes?])
         (values (is-a?/c text%) (is-a?/c text%))]{

  Returns two @scheme[text%] objects corresponding to the submitted
  definitions and interactions windows.}

@defproc[(make-evaluator/submission
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (cons/c (one-of/c 'begin) list?))]
          [teachpack-paths (listof path-string?)]
          [content bytes?])
         (any/c . -> . any)]{

  Like @scheme[make-evaluator], but the definitions content is
  supplied as a submission byte string.  The byte string is opened for
  reading, with line-counting enabled.}

@defproc[(call-with-evaluator
          [language (or/c module-path?
                          (list/c (one-of/c 'special) symbol?)
                          (cons/c (one-of/c 'begin) list?))]
          [teachpack-paths (listof path-string?)]
          [input-program any/c]
          [proc (any/c . -> . any)])
         any]{

  Calls @scheme[proc] with an evaluator for the given language,
  teachpack paths, and initial definition content as supplied by
  @scheme[input-program] (see @scheme[make-evaluator]).  It also sets
  the current error-value print handler to print values in a way
  suitable for @scheme[language], it initializes
  @scheme[set-run-status] with @scheme["executing your code"], and it
  catches all exceptions to re-raise them in a form suitable as a
  submission error.}

@defproc[(call-with-evaluator/submission [language
          (or/c module-path?
                (list/c (one-of/c 'special) symbol?)
                (cons/c (one-of/c 'begin) list?))]
          [teachpack-paths (listof path-string?)]
          [submission bytes?]
          [proc (any/c . -> . any)])
         any]{

  Like @scheme[call-with-evaluator], but the definitions content is
  supplied as a byte string.  The byte string is opened for reading,
  with line-counting enabled.}

@comment{this contract is probably wrong}
@comment{does this eval accept an optional namespace?}
@defproc[(evaluate-all [source any]
                       [input-port port?]
                       [eval (any/c . -> . any)]) any]{
  Like @scheme[load] on an input port.}

@defproc[(evaluate-submission [submission bytes?]
                              [eval (any/c . -> . any)])
         any]{

  Like @scheme[load] on a submission byte string.}

@defproc[(check-proc [eval (any/c . -> . any)]
                     [expect-v any/c]
                     [compare-proc (any/c any/c . -> . any)]
                     [proc-name symbol?]
                     [arg any/c] ...)
         any]{

  Calls the function named @scheme[proc-name] using the evaluator
  @scheme[eval], giving it the (unquoted) arguments @scheme[arg ...]
  Let @scheme[result-v] be the result of the call; unless
  @scheme[(compare-proc result-v expect-v)] is true, an exception is
  raised.}

Every exception or result mismatch during the call to
@scheme[compare-proc] is phrased suitably for the handin client.

@defproc[(check-defined [eval (any/c . -> . any)]
                        [name symbol?])
         any]{

  Checks whether @scheme[name] is defined in the evaluator
  @scheme[eval], and raises an error if not (suitably phrased for the
  handin client).  If it is defined as non-syntax, its value is
  returned.  Warning: in the beginner language level, procedure
  definitions are bound as syntax.}

@comment{returns what? signals error?}

@defproc[(look-for-tests [text (is-a?/c text%)] [name symbol?] [n number?])
         any]{

  Inspects the given @scheme[text%] object to determine whether it
  contains at least @scheme[n] tests for the function @scheme[name].
  The tests must be top-level expressions.}

@defproc[(user-construct [eval (any/c . -> . any)]
                         [name symbol?]
                         [arg any/c] ...)
         any]{

  Like @scheme[check-proc], but with no result checking.  This
  function is often useful for calling a student-defined constructor.}

@defparam[test-history-enabled on? any/c]{

  Controls how run-time errors are reported to the handin client.  If
  the parameter's value is true, then the complete sequence of tested
  expressions is reported to the handin client for any test failure.
  Set this parameter to true when testing programs that use state.}

@defproc*[([(message [string string?]) void?]
           [(message [string string?]
                     [styles (or/c (symbols 'final)
                                   (listof (one-of/c 'ok 'ok-cancel
                                                     'yes-no 'caution 'stop)))])
            any])]{
  If given only a string, this string will be shown on the client's
  submission dialog; if @scheme[styles] is also given, it can be the
  symbol @scheme['final], which will be used as the text on the handin
  dialog after a successful submission instead of ``Handin
  successful.'' (useful for submissions that were saved, but had
  problems); finally, @scheme[styles] can be used as a list of styles
  for a @scheme[message-box] dialog on the client side, and the
  resulting value is returned as the result of @scheme[message].  You
  can use this to send warnings to the student or ask confirmation.}

@defproc[(set-run-status [status (or/c false? string?)]) void?]{
  Registers information about the current actions of the checker, in
  case the session is terminated due to excessive memory consumption
  or a timeout.  For example, a checker might set the status to
  indicate which instructor-supplied test was being executed when the
  session aborted.}

@defparam[current-value-printer proc (any/c . -> . string?)]{
  Controls how values are printed. The @scheme[proc] must be a
  procedure that expects a Scheme value and returns a string
  representation for it.  The default value printer uses
  @scheme[pretty-print], with DrScheme-like settings.}

@defproc[(reraise-exn-as-submission-problem [thunk (-> any)]) any]{

  Calls @scheme[thunk] in a context that catches exceptions and
  re-raises them in a form suitable as a submission error.  It returns
  the value returned by @scheme[thunk] if no exception occurs.}

@defproc[(log-line [fmt string?] [args any/c] ...) void?]{
  Produces a line in the server log file, using the given format
  string and arguments.  This function arranges to print the line fast
  (to avoid mixing lines from different threads) to the error port,
  and flush it.  (The log port will prefix all lines with a time stamp
  and a connection identifier.)}

@defproc[(timeout-control [msg string?]) void?]{

  Controls the timeout for this session.  The timeout is initialized
  by the value of the @scheme[session-timeout] configuration entry,
  and the checker can use this procedure to further control it: if
  @scheme[msg] is @scheme['reset] the timeout is reset to
  @scheme[session-timeout] seconds; if @scheme[msg] is a number the
  timeout will be set to that many seconds in the future.  The timeout
  can be completely disabled by @scheme[(timeout-control #f)].  (Note
  that before the checker is used (after the pre-checker, if
  specified), the timer will be reset to the @scheme['session-timeout]
  value.)}


@section{checker}

@defmodulelang[handin-server/checker]{

The @schememodname[handin-server/checker] module provides a
higher-level of utilities, helpful in implementing `checker' functions
that are intended for a more automated system.  This module is a
language module---a typical checker that uses it looks like this:

@schemeblock[
   (module checker (lib "checker.ss" "handin-server")
     (check: :language  'intermediate
             :users     pairs-or-singles-with-warning
             :coverage? #t
       (!procedure Fahrenheit->Celsius 1)
       (!test (Fahrenheit->Celsius  32)   0)
       (!test (Fahrenheit->Celsius 212) 100)
       (!test (Fahrenheit->Celsius  -4) -20)
       ...))]

}

@defform/subs[(check: keys-n-vals body ...)
             ([keys-n-vals code:blank
                           (code:line :key val keys-n-vals)])]{

Constructs (and provides) an appropriate checker function, using
keywords for features that you want, the body of the checker can
contain arbitrary code, using all utilities from
@schememodname[handin-server/utils], as well as additional ones (see
below).}

Keywords for configuring @scheme[check:]:

@itemize{

@item{@indexed-scheme[:users]---specification of users that are
  acceptable for submission.  Can be either a list of user lists, each
  representing a known team, or procedure which will accept a list of
  users and throw an exception if they are unacceptable.  The default
  is to accept only single-user submissions.  The
  @scheme[pairs-or-singles-with-warning] procedure is a useful value
  for pair submission where the pairs are unknown.}

@item{@indexed-scheme[:eval?]---whether submissions should be
  evaluated.  Defaults to @scheme[#t].  Note that if it is specified
  as @scheme[#f], then the checker body will not be able to run any
  tests on the code, unless it contains code that performs some
  evaluation (e.g., using the facilities of
  @schememodname[handin-server/utils]).}

@item{@indexed-scheme[:language]---the language that is used for
  evaluating submissions, same as the @scheme[_language] argument for
  @scheme[make-evaluator] (see @schememodname[handin-server/sandbox]).
  There is no default for this, so it must be set or an error is
  raised.}

@item{@indexed-scheme[:teachpacks]---teachpacks for evaluating
  submissions, same as the @scheme[_teachpacks] argument for
  @scheme[make-evaluator] (see @schememodname[handin-server/sandbox]).
  This defaults to null---no teachpacks.}

@item{@indexed-scheme[:create-text?]---if true, then a textual version
  of the submission is saved as @filepath{text.scm} in a
  @filepath{grading} subdirectory (or any suffix that is specified by
  @scheme[:output] below, for example @filepath{hw.java} is converted
  into a textual @filepath{grading/text.java}).  This is intended for
  printouts and grading, and is in a subdirectory so students will not
  see it on the status web server.  Defaults to @scheme[#t].}

@item{@indexed-scheme[:untabify?]---if true, then tabs are converted
  to spaces, assuming a standard tab width of 8 places.  This is
  needed for a correct computation of line lengths, but note that
  DrScheme does not insert tabs in Scheme mode.  Defaults to
  @scheme[#t].}

@item{@indexed-scheme[:textualize?]---if true, then all submissions
  are converted to text, trying to convert objects like comment boxes
  and test cases to some form of text.  Defaults to @scheme[#f],
  meaning that an exception is raised for submissions that are not all
  text.}

@item{@indexed-scheme[:maxwidth]---a number that specifies maximum
  line lengths for submissions (a helpful feature for reading student
  code).  Defaults to 79.  This feature can be disabled if set to
  @scheme[#f].}

@item{@indexed-scheme[:output]---the name of the original handin file
  (unrelated to the text-converted files).  Defaults to
  @filepath{hw.scm}.  (The suffix changes the defaults of
  @scheme[:markup-prefix] and @scheme[:prefix-re].)  Can be
  @scheme[#f] for removing the original file after processing.}

@item{@indexed-scheme[:multi-file]---by default, this is set to
  @scheme[#f], which means that only DrScheme is used to send
  submissions as usual.  See @secref{multi-file} for setting up
  multi-file submissions.}

@item{@indexed-scheme[:names-checker]---used for multi-file
  submissions; see @secref{multi-file} for details.}

@item{@indexed-scheme[:markup-prefix]---used as the prefix for
  @scheme[:student-lines] and @scheme[:extra-lines] below.  The
  default is @scheme[";;> "] or @scheme["//> "], depending on the
  suffix of @scheme[:output] above.  (Note: if you change this, make
  sure to change @scheme[:prefix-re] too.)}

@item{@indexed-scheme[:prefix-re]---used to identify lines with markup
  (@scheme[";>"] or @scheme["//>"] etc), so students cannot fool the
  system by writing marked-up code.  The default is @scheme[";>"] or
  @scheme["//>"], depending on the suffix of :output above.}

@item{@indexed-scheme[:student-line]---when a submission is converted
  to text, it begins with lines describing the students that have
  submitted it; this is used to specify the format of these lines.  It
  is a string with holes that that @scheme[user-substs] fills out.
  The default is @scheme["Student: {username} ({Full Name} <{Email}>)"],
  which requires @scheme["Full Name"] and @scheme["Email"] entries in
  the server's extra-fields configuration.  These lines are prefixed
  with @scheme[";;> "] or the prefix specified by
  @scheme[:makup-prefix] above.}

@item{@indexed-scheme[:extra-lines]---a list of lines to add after the
  student lines, all with a @scheme[";;> "] or :markup-prefix too.
  Defaults to a single line:
  @scheme["Maximum points for this assignment: <+100>"].  (Can use
  @scheme["{submission}"] for the submission directory.)  See also
  @scheme[add-header-line!].}

@item{@indexed-scheme[:user-error-message]---a string that is used to
  report an error that occurred during evaluation of the submitted
  code (not during additional tests).  It can be a plain string which
  will be used as the error message, or a string with single a
  @scheme["~a"] (or @scheme["~e"], @scheme["~s"], @scheme["~v"]) that
  will be used as a format string with the actual error message.  The
  default is @scheme["Error in your code --\n~a"].  Useful examples of
  these messages:

    @scheme["There is an error in your program, hit \"Run\" to debug"]

    @scheme["There is an error in your program:\n----\n~a\n----\nHit \"Run\" and debug your code."]

  Alternatively, the value can be a procedure that will be invoked
  with the error message.  The procedure can do anything it wants, and
  if it does not raise an exception, then the checker will proceed as
  usual.  For example:

  @schemeblock{
    (lambda (msg)
      (add-header-line! "Erroneous submission!")
      (add-header-line! (format "  --> ~a" msg))
      (message (string-append
                "You have an error in your program -- please hit"
                " \"Run\" and debug your code.\n"
                "Email the course staff if you think your code is"
                " fine.\n"
                "(The submission has been saved but marked as"
                " erroneous.)")
               '(ok))
      (message "Handin saved as erroneous." 'final))}

  (Note that if you do this, then additional tests should be adjusted
  to not raise an exception too.)}

@item{@indexed-scheme[:value-printer]---if specified, this will be
  used for @scheme[current-value-printer].}

@item{@indexed-scheme[:coverage?]---collect coverage information when
  evaluating the submission.  This will cause an error if some input
  is not covered.  This check happens after checker tests are run, but
  the information is collected and stored before, so checker tests do
  not change the result.  Also, you can use the @scheme[!all-covered]
  procedure in the checker before other tests, if you want that
  feedback earlier.}}

Within the body of @scheme[check:], @scheme[users] and
@scheme[submission] will be bound to the checker arguments---a
(sorted) list of usernames and the submission as a byte string.  In
addition to the functionality below, you can use
@scheme[((submission-eval) expr)] to evaluate expressions in the
submitted code context, and you can use
@scheme[(with-submission-bindings (id ...) body ...)] to evaluate the
body when @scheme[id]'s are bound to their values from the submission
code.}

@deftogether[(@defform[(pre: body ...)]
              @defform[(post: body ...)])]{

  These two macros define a pre- and a post-checker.  In their bodies,
  @scheme[_users] and @scheme[_submission] are bound as in
  @scheme[check:], but there is nothing else special about these.  See
  the description of the @scheme[pre-checker] and
  @scheme[post-checker] values for what can be done with these, and
  note that the check for valid users is always first.  An example for
  a sophisticated @scheme[post:] block is below---it will first
  disable timeouts for this session, then it will send a email with a
  submission receipt, with CC to the TA (assuming a single TA), and
  pop-up a message telling the student about it:

  @schemeblock[
    (require net/sendmail)
    (post:
      (define info
        (format "hw.scm: ~a ~a"
                (file-size "hw.scm")
                (file-or-directory-modify-seconds "hw.scm")))
      (timeout-control 'disable)
      (log-line "Sending a receipt: ~a" info)
      (send-mail-message
       "course-staff@university.edu"
       "Submission Receipt"
       (map (lambda (user) (user-substs user "{Full Name} <{Email}>"))
            users)
       (list (user-substs (car users) "{TA Name} <{TA Email}>"))
       null
       `("Your submission was received" ,info))
      (message (string-append
                "Your submission was successfully saved."
                "  You will get an email receipt within 30 minutes;"
                " if not, please contact the course staff.")
               '(ok)))]}

@defparam[submission-eval eval (any/c . -> . any)]{

  Holds an evaluation procedure for evaluating code in the submission
  context.}

@comment{is this always just a list of strings?}
@defproc[(user-data [user string?]) (listof string?)]{

  Returns a user information given a username.  The returned
  information is a list of strings that corresponds to the configured
  @scheme[extra-fields].}

@defproc[(user-substs [user string?] [fmt string?]) string]{

  Uses the mappings in @scheme[user-data] to substitute user
  information for substrings of the form ``@tt{{some-field-name}}'' in
  @scheme[fmt].  This procedure signals an error if a field name is
  missing in the user data.  Also, ``@tt{{username}}'' will always be
  replaced by the username and ``@tt{{submission}}'' by the current
  submission directory.

  This is used to process the @scheme[:student-line] value in the
  checker, but it is provided for additional uses.  See the above
  sample code for @scheme[post:] for using this procedure.}

@defproc[(pairs-or-singles-with-warning [users (listof string?)])
         any]{

  Intended for use as the @scheme[:users] entry in a checker.  It will
  do nothing if there are two users, and throw an error if there are
  more.  If there is a single user, then the user will be asked to
  verify a single submission. If the student cancels, then an
  exception is raised so the submission directory is retracted.  If
  the student approves this, the question is not repeated (this is
  marked by creating a directory with a known name).  This is useful
  for cases where you want to allow free pair submissions---students
  will often try to submit their work alone, and later on re-submit
  with a partner.}

@defproc[(teams-in-file [team-file path-string?])
         ((listof string?) . -> . void?)]{

  @italic{Returns} a procedure that can be used for the :users entry
  in a checker.  The team file (relative from the server's main
  directory) is expected to have user entries---a sequence of
  s-expressions, each one a string or a list of strings.  The
  resulting procedure will allow submission only by teams that are
  specified in this file.  Furthermore, if this file is modified, the
  new contents will be used immediately, so there is no need to
  restart the server of you want to change student teams.  (But
  remember that if you change @scheme[("foo" "bar")] to
  @scheme[("foo" "baz")], and there is already a @filepath{bar+foo}
  submission directory, then the system will not allow ``@tt{foo}'' to
  submit with ``@tt{bar}''.)}

@defproc[(add-header-line! [line string?]) void?]{
  During the checker operation, can be used to add header lines to the
  text version of the submitted file (in addition to the
  @scheme[:extra-lines] setting).  It will not have an effect if
  @scheme[:create-text?] is false.}

@defproc[(procedure/arity? [proc procedure?] [arity number?])
         boolean?]{
  Returns @scheme[#t] if @scheme[proc] is a procedure that accepts
  @scheme[arity] arguments.}

@defform[(!defined id ...)]{
  Checks that the given identifiers are defined in the (evaluated)
  submission, and throws an error otherwise.}

@defform[(!procedure id arity)]{

  Checks that @scheme[id] is defined, and is bound to a procedure.}

@defform[(!procedure* expr arity)]{

  Similar to @scheme[!procedure] but omits the defined check, making
  it usable with any expression, which is then evaluated in the
  submission context.}

@deftogether[(@defform[(!integer id)]
              @defform[(!integer* expr)])]{

  Similar to @scheme[!procedure] and @scheme[!procedure*] for
  integers.}

@defform*[((!test expr)
           (!test expr result)
           (!test expr result equal?))]{

  The first form checks that the given expression evaluates to a
  non-@scheme[#f] value in the submission context, throwing an error
  otherwise.  The second form compares the result of evaluation,
  requiring it to be equal to @scheme[result]. The third allows
  specifying an equality procedure.  Note that the @scheme[result] and
  @scheme[equal?] forms are @italic{not} evaluated in the submission
  context.}

@defproc*[([(!all-covered) void?]
           [(!all-covered [proc (string? . -> . any)]) void?])]{

  When coverage information is enabled (see @scheme[:coverage?]
  above), checks the collected coverage information and throws an
  error with source information if some code is left uncovered.  If
  @scheme[proc] is provided, it is applied to a string argument that
  describes the location of the uncovered expression
  (@scheme["<line>:<col>"], @scheme["#<char-pos>"], or
  @scheme["(unknown position)"]) instead of throwing an error.  The
  collected information includes only execution coverage by submission
  code, excluding additional checker tests.  You do not have to call
  this explicitly---it is called at the end of the process
  automatically when @scheme[:coverage?] is enabled.  It is made
  available so you can call it earlier (e.g., before testing) to show
  clients a coverage error first, or if you want to avoid an error.
  For example, you can do this:

  @schemeblock[
    (!all-covered
     (lambda (where)
       (case (message (string-append
                       "Incomplete coverage at "where", do you want"
                       " to save this submission with 10% penalty?"))
         [(yes) (add-header-line! "No full coverage <*90%>")
                (message "Handin saved with penalty.")]
         [else (error "aborting submission")])))]}


@section[#:tag "multi-file"]{Multiple-File Submissions}

By default, the system is set up for submissions of single a single
file, straight fom DrScheme using the handin-client.  There is some
support for multi-file submissions in
@schememodname[handin-server/checker] and in the handin-client. It is
possible to submit multiple files, and have the system generate a
single file that is the concatenation of all submission files (used
only with text files).  To set up multi-file submissions, do the
following:

@itemize{
@item{Add a @scheme[:multi-file] keyword in @scheme[check:], and as a
  value, use the suffix that should be used for the single
  concatenated output file.}

@item{You can also add a @scheme[:names-checker] keyword--the value
  can be a regexp that all submitted files must follow (e.g.,
  @scheme[".*[.]scm$"]), or a list of expected file names.
  Alternatively, it can be a 1-argument procedure that will receive
  the (sorted) list of submitted files and can throw an error if some
  files are missing or some files are forbidden.}

@item{In the @filepath{info.ss} file of the handin-client you need to
  set @scheme[enable-multifile-handin] to @scheme[#t], and adjust
  @scheme[selection-default] to patterns that are common to your
  course.  (It can be a single pattern, or a list of them.)}}

On the server side, each submission is saved in a file called
@filepath{raw}, which contains all submitted files.  In the
@filepath{grading} directory, you will get a @filepath{text.<sfx>}
file (@filepath{<sfx>} is the suffix that is used as a value for
@scheme[:multi-file]) that contains all submitted files with clear
separators.  A possible confusion is that every submission is a
complete set of files that overwrites any existing submission, whereas
students may think that the server accumulates incoming files.  To
avoid such confusion, when a submission arrives an there is already an
existing previous submission, the contents is compared, and if there
are files that existed in the old submission but not in the new ones,
the student will see a warning pop-up that allows aborting the
submission.

On the client side, students will have an additional file-menu entry
for submitting multiple files, which pops up a dialog that can be used
to submit multiple files.  In this dialog, students choose their
working directory, and the @scheme[selection-default] entry from the
@filepath{handin-client/info.ss} file specifies a few patterns that
can be used to automatically select files.  The dialog provides all
handin-related functionality that is available in DrScheme.  For
further convenience, it can be used as a standalone application: in
the account management dialog, the @onscreen{Un/Install} tab has a
button that will ask for a directory where it will create an
executable for the multi-file submission utility---the resulting
executable can be used outside of DrScheme (but PLT Scheme is still
required, so it cannot be uninstalled).


@section{Auto-Updater}

The handin-client has code that can be used for automatic updating of
clients.  This can be useful for courses where you distribute some
additional functionality (collections, teachpacks, language-levels
etc), and this functionality can change (or expected to change, for
example, distributing per-homework teachpacks).

To enable this, uncomment the relevant part of the @filepath{info.ss}
file in the client code. It has the following three keys:
@indexed-scheme[enable-auto-update] that turns this facility on, and
@indexed-scheme[version-filename] and
@indexed-scheme[package-filename] which are the expected file names of
the version file and the @filepath{.plt} file relative to the course
web address (the value of the @scheme[web-address] key).  Also,
include in your client collection a @filepath{version} file that
contains a single number that is its version. Use a big integer that
holds the time of this collection in a @tt{YYYYMMDDHHMM} format.

When students install the client, every time DrScheme starts, it will
automatically check the version from the web page (as specified by the
@scheme[web-address] and @scheme[version-filename] keys), and if that
contains a bigger number, it will offer the students to download and
install the new version.  So, every time you want to distribute a new
version, you build a new @filepath{.plt} file that contains a new
version file, then copy these version and @filepath{.plt} files to
your web page, and students will be notified automatically.  Note: to
get this to work, you need to create your @filepath{.plt} file using
mzc's @DFlag{--replace} flag, so it will be possible to overwrite
existing files.  (Also note that there is no way to delete files when
a new @filepath{.plt} is installed.)
