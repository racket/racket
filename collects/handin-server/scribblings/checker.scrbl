#lang scribble/doc
@(require "common.rkt")

@(define textoption
   @t{(Effective only when saving a textual version of
      the submission files: when @racket[:create-text?] is on.)})

@title{Checker}

@defmodulelang[handin-server/checker]{

The @racketmodname[handin-server/checker] module provides a
higher-level of utilities, helpful in implementing `checker' functions
that are intended for a more automated system.  This module is a
language module---a typical checker that uses it looks like this:

@racketblock[
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
utility functions from @racketmodname[handin-server/utils], as well as
additional ones that are defined below.  Submission files are arriving
to the handin server in binary form (in the GRacket format that is used
to store text and other objects like images), and a number of these
options involve genrating a textual version of this file.  The purpose
of these options is to have these text files integrate easily into a
course framework for grading, based on these text files.}

Keywords for configuring @racket[check:]:

@itemize[

@item{@indexed-racket[:users]---specification of users that are
  acceptable for submission.  Can be either a list of user lists, each
  representing a known team, or procedure which will accept a list of
  users and throw an exception if they are unacceptable.  The default
  is to accept only single-user submissions.  The
  @racket[pairs-or-singles-with-warning] procedure is a useful value
  for pair submission where the pairs are unknown.}

@item{@indexed-racket[:eval?]---whether submissions should be
  evaluated.  Defaults to @racket[#t].  Note that if it is specified
  as @racket[#f], then the checker body will not be able to run any
  tests on the code, unless it contains code that performs some
  evaluation (e.g., using the facilities of
  @racketmodname[handin-server/utils]).}

@item{@indexed-racket[:language]---the language that is used for
  evaluating submissions. If the value is of the form 
  @racket[(list 'module _spec)], then @racket[make-module-evaluator] 
  is used with @racket[_spec] as its @racket[#:language] argument to
  generate an evaluator. Otherwise, the value is passed as the 
  @racket[_language] argument to @racket[make-evaluator].
  There is no default for this, so it must be set or an error is
  raised.  (See @racket[call-with-evaluator/submission] for further
  details.)}

@item{@indexed-racket[:requires]---paths for additional libraries to
  require for evaluating the submission, same as the
  @racket[_requires] argument for @racket[make-evaluator] (see
  @racketmodname[handin-server/sandbox]).  This defaults to null---no
  teachpacks.  Note: if a module language is used (See
  @racket[call-with-evaluator/submission] for further details), it is
  passed as the @racket[_allow-read] argument.}

@item{@indexed-racket[:teachpacks]---an alternative name for
  @racket[:requires], kept for legacy checkers.}

@item{@indexed-racket[:create-text?]---if true, then a textual version
  of the submission is saved as @filepath{text.rkt} in a
  @filepath{grading} subdirectory (or any suffix that is specified by
  @racket[:output] below, for example @filepath{hw.java} is converted
  into a textual @filepath{grading/text.java}).  This is intended for
  printouts and grading, and is in a subdirectory so students will not
  see it on the status web server.  Defaults to @racket[#t].}

@item{@indexed-racket[:textualize?]---if true, then all submissions
  are converted to text, trying to convert objects like images and
  comment boxes to some form of text.  Defaults to @racket[#f],
  meaning that an exception is raised for submissions that are not all
  text. @textoption

  This flag is effective only when saving a textual version of the
  submission files --- when @racket[:create-text?] is on.  The
  possible configurations are:
  @itemize[
  @item{@racket[:create-text?] is on and @racket[:textualize?] is off
    (the default) --- in this case a text version of submissions is
    created, and submissions must contain only plain text.  The text
    file has the same semantics of the submission and can be used to
    run student code.}
  @item{@racket[:create-text?] is off --- allowing submissions that
    contain non-textual objects, but no text file is created so
    grading and testing must be done using DrRacket (because the saved
    submission is always in binary format).}
  @item{Both flags are on --- allowing submission with non-textual
    objects and generating text files, but these files will not be
    usable as code since objects like images cannot be represented in
    plain text.}]}

@item{@indexed-racket[:untabify?]---if true, then tabs are converted
  to spaces, assuming a standard tab width of 8 places.  This is
  needed for a correct computation of line lengths, but note that
  DrRacket does not insert tabs in Scheme mode.  Defaults to
  @racket[#t].  @textoption}

@item{@indexed-racket[:maxwidth]---a number that specifies maximum
  line lengths for submissions (a helpful feature for reading student
  code).  Defaults to 79.  This feature can be disabled if set to
  @racket[#f].  @textoption}

@item{@indexed-racket[:output]---the name of the original handin file
  (unrelated to the text-converted files).  Defaults to
  @filepath{hw.rkt}.  (The suffix changes the defaults of
  @racket[:markup-prefix] and @racket[:prefix-re].)  Can be
  @racket[#f] for removing the original file after processing.  The
  file is always stored in GRacket's binary format.}

@item{@indexed-racket[:multi-file]---by default, this is set to
  @racket[#f], which means that only DrRacket is used to send
  submissions as usual.  See @secref{multi-file} for setting up
  multi-file submissions.}

@item{@indexed-racket[:names-checker]---used for multi-file
  submissions; see @secref{multi-file} for details.}

@item{@indexed-racket[:markup-prefix]---used as the prefix for
  @racket[:student-lines] and @racket[:extra-lines] below.  The
  default is @racket[";;> "] or @racket["//> "], depending on the
  suffix of @racket[:output] above.  Note: if you change this, make
  sure to change @racket[:prefix-re] too.  @textoption}

@item{@indexed-racket[:prefix-re]---used to identify lines with markup
  (@racket[";>"] or @racket["//>"] etc), so students cannot fool the
  system by writing marked-up code.  The default is @racket[";>"] or
  @racket["//>"], depending on the suffix of :output above.
  @textoption}

@item{@indexed-racket[:student-line]---when a submission is converted
  to text, it begins with lines describing the students that have
  submitted it; this is used to specify the format of these lines.  It
  is a string with holes that @racket[user-substs] fills out.
  The default is @racket["Student: {username} ({Full Name} <{Email}>)"],
  which requires @racket["Full Name"] and @racket["Email"] entries in
  the server's extra-fields configuration.  These lines are prefixed
  with @racket[";;> "] or the prefix specified by
  @racket[:makup-prefix] above.  @textoption}

@item{@indexed-racket[:extra-lines]---a list of lines to add after the
  student lines, all with a @racket[";;> "] or :markup-prefix too.
  Defaults to a single line:
  @racket["Maximum points for this assignment: <+100>"].  (Can use
  @racket["{submission}"] for the submission directory.)  See also
  @racket[add-header-line!].  @textoption}

@item{@indexed-racket[:user-error-message]---a string that is used to
  report an error that occurred during evaluation of the submitted
  code (not during additional tests).  It can be a plain string which
  will be used as the error message, or a string with single a
  @racket["~a"] (or @racket["~s"], @racket["~v"], @racket["~e"],
  or @racket["~.a"] etc) that
  will be used as a format string with the actual error message.  The
  default is @racket["Error in your code --\n~a"].  Useful examples of
  these messages:

    @racket["There is an error in your program, hit \"Run\" to debug"]

    @racket["There is an error in your program:\n----\n~a\n----\nHit \"Run\" and debug your code."]

  Alternatively, the value can be a procedure that will be invoked
  with the error message.  The procedure can do anything it wants, and
  if it does not raise an exception, then the checker will proceed as
  usual.  For example:

  @racketblock[
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

@item{@indexed-racket[:value-printer]---if specified, this will be
  used for @racket[current-value-printer].}

@item{@indexed-racket[:coverage?]---collect coverage information when
  evaluating the submission.  This will cause an error if some input
  is not covered.  This check happens after checker tests are run, but
  the information is collected and stored before, so checker tests do
  not change the result.  Also, you can use the @racket[!all-covered]
  procedure in the checker before other tests, if you want that
  feedback earlier.}]

Within the body of @racket[check:], @racket[users] and
@racket[submission] will be bound to the checker arguments---a
(sorted) list of usernames and the submission as a byte string.  In
addition to the functionality below, you can use
@racket[(!eval _expr)] (or @racket[((submission-eval) '_expr)]) to
evaluate expressions in the submitted code context, and you can use
@racket[(with-submission-bindings (id ...) body ...)] to evaluate the
body when @racket[id]'s are bound to their values from the submission
code.}

@deftogether[(@defform[(pre: body ...)]
              @defform[(post: body ...)])]{

  These two macros define a pre- and a post-checker.  In their bodies,
  @racket[_users] and @racket[_submission] are bound as in
  @racket[check:], but there is nothing else special about these.  See
  the description of the @racket[pre-checker] and
  @racket[post-checker] values for what can be done with these, and
  note that the check for valid users is always first.  An example for
  a sophisticated @racket[post:] block is below---it will first set a
  very long timeout for this session, then it will send an email with
  a submission receipt, with a CC to the TA (assuming a single TA),
  and pop-up a message telling the student about it:

  @racketblock[
    (require net/sendmail)
    (post:
      (define info
        (format "hw.rkt: ~a ~a"
                (file-size "hw.rkt")
                (file-or-directory-modify-seconds "hw.rkt")))
      (timeout-control 300)
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
  @racket[extra-fields].}

@defproc[(user-substs [user string?] [fmt string?]) string]{

  Uses the mappings in @racket[user-data] to substitute user
  information for substrings of the form ``@tt{{some-field-name}}'' in
  @racket[fmt].  This procedure signals an error if a field name is
  missing in the user data.  Also, ``@tt{{username}}'' will always be
  replaced by the username and ``@tt{{submission}}'' by the current
  submission directory.

  This is used to process the @racket[:student-line] value in the
  checker, but it is provided for additional uses.  See the above
  sample code for @racket[post:] for using this procedure.}

@defproc[(pairs-or-singles-with-warning [users (listof string?)])
         any]{

  Intended for use as the @racket[:users] entry in a checker.  It will
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
  remember that if you change @racket[("foo" "bar")] to
  @racket[("foo" "baz")], and there is already a @filepath{bar+foo}
  submission directory, then the system will not allow ``@tt{foo}'' to
  submit with ``@tt{bar}''.)}

@defproc[(add-header-line! [line string?]) void?]{
  During the checker operation, can be used to add header lines to the
  text version of the submitted file (in addition to the
  @racket[:extra-lines] setting).  It will not have an effect if
  @racket[:create-text?] is false.}

@defproc[(procedure/arity? [proc procedure?] [arity number?])
         boolean?]{
  Returns @racket[#t] if @racket[proc] is a procedure that accepts
  @racket[arity] arguments.}

@defform[(!defined id ...)]{
  Checks that the given identifiers are defined in the (evaluated)
  submission, and throws an error otherwise.  The identifiers can be
  bound as either a plain value or as a syntax.}

@defform[(!bound id ...)]{
  Checks that the given identifiers are defined in the (evaluated)
  submission as a plain value.  Throws an error if not, or if an
  identifier is bound to a syntax.}

@defform[(!syntax id arity)]{
  Checks that @racket[id] is defined, and is bound as a macro.}

@defform[(!procedure id arity)]{
  Checks that @racket[id] is defined, and is bound to a procedure.}

@defform[(!procedure* expr arity)]{

  Similar to @racket[!procedure] but omits the defined check, making
  it usable with any expression, which is then evaluated in the
  submission context.}

@deftogether[(@defform[(!integer id)]
              @defform[(!integer* expr)])]{

  Similar to @racket[!procedure] and @racket[!procedure*] for
  integers.}

@deftogether[(
@defform*[((!test expr)
           (!test expr result)
           (!test expr result equal?))]
@defform[(!test/exn expr)]
)]{

  The first @racket[!test] form checks that the given expression
  evaluates to a non-@racket[#f] value in the submission context,
  throwing an error otherwise.  The second form compares the result of
  evaluation, requiring it to be equal to @racket[result]. The third
  allows specifying an equality procedure. The @racket[!test/exn] form
  checks that the given expression throws an @racket[exn:fail?] error,
  throwing an error otherwise.

  For the latter two @racket[!test] forms, note that the
  @racket[result] and @racket[equal?] forms are @italic{not} evaluated
  in the submission context.}

@defform[(!eval expr)]{

  Evaluate an arbitrary expression in the submission context.  This is
  a simple shorthand for @racket[((submission-eval) `expr)].}

@defproc*[([(!all-covered) void?]
           [(!all-covered [proc (string? . -> . any)]) void?])]{

  When coverage information is enabled (see @racket[:coverage?]
  above), checks the collected coverage information and throws an
  error with source information if some code is left uncovered.  If
  @racket[proc] is provided, it is applied to a string argument that
  describes the location of the uncovered expression
  (@racket["<line>:<col>"], @racket["#<char-pos>"], or
  @racket["(unknown position)"]) instead of throwing an error.  The
  collected information includes only execution coverage by submission
  code, excluding additional checker tests.  You do not have to call
  this explicitly---it is called at the end of the process
  automatically when @racket[:coverage?] is enabled.  It is made
  available so you can call it earlier (e.g., before testing) to show
  clients a coverage error first, or if you want to avoid an error.
  For example, you can do this:

  @racketblock[
    (!all-covered
     (lambda (where)
       (case (message (string-append
                       "Incomplete coverage at "where", do you want"
                       " to save this submission with 10% penalty?")
                      '(yes-no))
         [(yes) (add-header-line! "No full coverage <*90%>")
                (message "Handin saved with penalty.")]
         [else (error "aborting submission")])))]}
