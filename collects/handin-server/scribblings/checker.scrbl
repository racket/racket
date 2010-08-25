#lang scribble/doc
@(require "common.ss")

@(define textoption
   @t{(Effective only when saving a textual version of
      the submission files: when @scheme[:create-text?] is on.)})

@title{Checker}

@defmodulelang[handin-server/checker]{

The @schememodname[handin-server/checker] module provides a
higher-level of utilities, helpful in implementing `checker' functions
that are intended for a more automated system.  This module is a
language module---a typical checker that uses it looks like this:

@schemeblock[
   (module checker handin-server/checker
     (check: :language  '(special intermediate)
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
keywords to customize features that you want it to have.  The body of
the checker (following the keywords) can contain arbitrary code, using
utility functions from @schememodname[handin-server/utils], as well as
additional ones that are defined below.  Submission files are arriving
to the handin server in binary form (in the GRacket format that is used
to store text and other objects like images), and a number of these
options involve genrating a textual version of this file.  The purpose
of these options is to have these text files integrate easily into a
course framework for grading, based on these text files.}

Keywords for configuring @scheme[check:]:

@itemize[

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
  @scheme[make-evaluator] (see @schememodname[handin-server/sandbox]),
  except that if the value ls @racket[(list 'module _spec)],
  then @racket[make-module-evaluator] is used with @racket[_spec] 
  as its @racket[#:language] argument.
  There is no default for this, so it must be set or an error is
  raised.  (See @scheme[call-with-evaluator/submission] for further
  details.)}

@item{@indexed-scheme[:requires]---paths for additional libraries to
  require for evaluating the submission, same as the
  @scheme[_requires] argument for @scheme[make-evaluator] (see
  @schememodname[handin-server/sandbox]).  This defaults to null---no
  teachpacks.  Note: if a module language is used (See
  @scheme[call-with-evaluator/submission] for further details), it is
  passed as the @scheme[_allow-read] argument.}

@item{@indexed-scheme[:teachpacks]---an alternative name for
  @scheme[:requires], kept for legacy checkers.}

@item{@indexed-scheme[:create-text?]---if true, then a textual version
  of the submission is saved as @filepath{text.scm} in a
  @filepath{grading} subdirectory (or any suffix that is specified by
  @scheme[:output] below, for example @filepath{hw.java} is converted
  into a textual @filepath{grading/text.java}).  This is intended for
  printouts and grading, and is in a subdirectory so students will not
  see it on the status web server.  Defaults to @scheme[#t].}

@item{@indexed-scheme[:textualize?]---if true, then all submissions
  are converted to text, trying to convert objects like images and
  comment boxes to some form of text.  Defaults to @scheme[#f],
  meaning that an exception is raised for submissions that are not all
  text. @textoption

  This flag is effective only when saving a textual version of the
  submission files --- when @scheme[:create-text?] is on.  The
  possible configurations are:
  @itemize[
  @item{@scheme[:create-text?] is on and @scheme[:textualize?] is off
    (the default) --- in this case a text version of submissions is
    created, and submissions must contain only plain text.  The text
    file has the same semantics of the submission and can be used to
    run student code.}
  @item{@scheme[:create-text?] is off --- allowing submissions that
    contain non-textual objects, but no text file is created so
    grading and testing must be done using DrRacket (because the saved
    submission is always in binary format).}
  @item{Both flags are on --- allowing submission with non-textual
    objects and generating text files, but these files will not be
    usable as code since objects like images cannot be represented in
    plain text.}]}

@item{@indexed-scheme[:untabify?]---if true, then tabs are converted
  to spaces, assuming a standard tab width of 8 places.  This is
  needed for a correct computation of line lengths, but note that
  DrRacket does not insert tabs in Scheme mode.  Defaults to
  @scheme[#t].  @textoption}

@item{@indexed-scheme[:maxwidth]---a number that specifies maximum
  line lengths for submissions (a helpful feature for reading student
  code).  Defaults to 79.  This feature can be disabled if set to
  @scheme[#f].  @textoption}

@item{@indexed-scheme[:output]---the name of the original handin file
  (unrelated to the text-converted files).  Defaults to
  @filepath{hw.rkt}.  (The suffix changes the defaults of
  @scheme[:markup-prefix] and @scheme[:prefix-re].)  Can be
  @scheme[#f] for removing the original file after processing.  The
  file is always stored in GRacket's binary format.}

@item{@indexed-scheme[:multi-file]---by default, this is set to
  @scheme[#f], which means that only DrRacket is used to send
  submissions as usual.  See @secref{multi-file} for setting up
  multi-file submissions.}

@item{@indexed-scheme[:names-checker]---used for multi-file
  submissions; see @secref{multi-file} for details.}

@item{@indexed-scheme[:markup-prefix]---used as the prefix for
  @scheme[:student-lines] and @scheme[:extra-lines] below.  The
  default is @scheme[";;> "] or @scheme["//> "], depending on the
  suffix of @scheme[:output] above.  Note: if you change this, make
  sure to change @scheme[:prefix-re] too.  @textoption}

@item{@indexed-scheme[:prefix-re]---used to identify lines with markup
  (@scheme[";>"] or @scheme["//>"] etc), so students cannot fool the
  system by writing marked-up code.  The default is @scheme[";>"] or
  @scheme["//>"], depending on the suffix of :output above.
  @textoption}

@item{@indexed-scheme[:student-line]---when a submission is converted
  to text, it begins with lines describing the students that have
  submitted it; this is used to specify the format of these lines.  It
  is a string with holes that @scheme[user-substs] fills out.
  The default is @scheme["Student: {username} ({Full Name} <{Email}>)"],
  which requires @scheme["Full Name"] and @scheme["Email"] entries in
  the server's extra-fields configuration.  These lines are prefixed
  with @scheme[";;> "] or the prefix specified by
  @scheme[:makup-prefix] above.  @textoption}

@item{@indexed-scheme[:extra-lines]---a list of lines to add after the
  student lines, all with a @scheme[";;> "] or :markup-prefix too.
  Defaults to a single line:
  @scheme["Maximum points for this assignment: <+100>"].  (Can use
  @scheme["{submission}"] for the submission directory.)  See also
  @scheme[add-header-line!].  @textoption}

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

  @schemeblock[
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
      (message "Handin saved as erroneous." 'final))]

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
  feedback earlier.}]

Within the body of @scheme[check:], @scheme[users] and
@scheme[submission] will be bound to the checker arguments---a
(sorted) list of usernames and the submission as a byte string.  In
addition to the functionality below, you can use
@scheme[(!eval _expr)] (or @scheme[((submission-eval) '_expr)]) to
evaluate expressions in the submitted code context, and you can use
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
        (format "hw.rkt: ~a ~a"
                (file-size "hw.rkt")
                (file-or-directory-modify-seconds "hw.rkt")))
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

@; JBC: is this always just a list of strings?
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
  submission, and throws an error otherwise.  The identifiers can be
  bound as either a plain value or as a syntax.}

@defform[(!bound id ...)]{
  Checks that the given identifiers are defined in the (evaluated)
  submission as a plain value.  Throws an error if not, or if an
  identifier is bound to a syntax.}

@defform[(!syntax id arity)]{
  Checks that @scheme[id] is defined, and is bound as a macro.}

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

@deftogether[(
@defform*[((!test expr)
           (!test expr result)
           (!test expr result equal?))]
@defform[(!test/exn expr)]
)]{

  The first @scheme[!test] form checks that the given expression
  evaluates to a non-@scheme[#f] value in the submission context,
  throwing an error otherwise.  The second form compares the result of
  evaluation, requiring it to be equal to @scheme[result]. The third
  allows specifying an equality procedure. The @scheme[!test/exn] form
  checks that the given expression throws an @scheme[exn:fail?] error,
  throwing an error otherwise.

  For the latter two @scheme[!test] forms, note that the
  @scheme[result] and @scheme[equal?] forms are @italic{not} evaluated
  in the submission context.}

@defform[(!eval expr)]{

  Evaluate an arbitrary expession in the submission context.  This is
  a simple shorthand for @scheme[((submission-eval) `expr)].}

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
                       " to save this submission with 10% penalty?")
                      '(yes-no))
         [(yes) (add-header-line! "No full coverage <*90%>")
                (message "Handin saved with penalty.")]
         [else (error "aborting submission")])))]}
