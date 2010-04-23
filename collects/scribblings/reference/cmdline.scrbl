#lang scribble/doc
@(require "mz.ss"
          (for-label racket/cmdline))

@title{Command-Line Parsing}

@note-lib[racket/cmdline]

@defform/subs[#:literals (multi once-each once-any final jelp-labels args 
                          help-labels =>)
              (command-line optional-name-expr optional-argv-expr 
                            flag-clause ... 
                            finish-clause)
              ([optional-name-expr code:blank
                                   (code:line #:program name-expr)]
               [optional-argv-expr code:blank
                                   (code:line #:argv argv-expr)]
               [flag-clause (code:line #:multi flag-spec ...)
                            (code:line #:once-each flag-spec ...)
                            (code:line #:once-any flag-spec ...)
                            (code:line #:final flag-spec ...)
                            (code:line #:help-labels string ...)]
                [flag-spec (flags id ... help-spec body ...+) 
                           (flags => handler-expr help-expr)]
                [flags flag-string
                       (flag-string ...+)]
                [help-spec string
                           (string-expr ...+)]
                [finish-clause code:blank
                               (code:line #:args arg-formals body ...+)
                               (code:line #:handlers handlers-exprs)]
                [arg-formals id
                             (id ...)
                             (id ...+ . id)]
                [handlers-exprs (code:line finish-expr arg-strings-expr)
                                (code:line finish-expr arg-strings-expr help-expr)
                                (code:line finish-expr arg-strings-expr help-expr
                                           unknown-expr)])]{

Parses a command line according to the specification in the
@scheme[flag-clause]s.

The @scheme[name-expr], if provided, should produce a path or string
to be used as the program name for reporting errors when the
command-line is ill-formed. It defaults to @scheme[(find-system-path
'run-file)]. When a path is provided, only the last element of the
path is used to report an error.

The @scheme[argv-expr], if provided, must evaluate to a list or a
vector of strings. It defaults to
@scheme[(current-command-line-arguments)].

The command-line is disassembled into flags, each possibly with
flag-specific arguments, followed by (non-flag)
arguments. Command-line strings starting with @litchar{-} or
@litchar{+} are parsed as flags, but arguments to flags are never
parsed as flags, and integers and decimal numbers that start with
@litchar{-} or @litchar{+} are not treated as flags. Non-flag
arguments in the command-line must appear after all flags and the
flags' arguments. No command-line string past the first non-flag
argument is parsed as a flag. The built-in @Flag{-} flag signals the
end of command-line flags; any command-line string past the @Flag{-}
flag is parsed as a non-flag argument.

A @scheme[#:multi], @scheme[#:once-each], @scheme[#:once-any], or
@scheme[#:final] clause introduces a set of command-line flag
specifications. The clause tag indicates how many times the flag can
appear on the command line:

@itemize[

 @item{@scheme[#:multi] --- Each flag specified in the set can be
 represented any number of times on the command line; i.e., the flags
 in the set are independent and each flag can be used multiple times.}

 @item{@scheme[#:once-each] --- Each flag specified in the set can be
 represented once on the command line; i.e., the flags in the set are
 independent, but each flag should be specified at most once. If a
 flag specification is represented in the command line more than once,
 the @exnraise[exn:fail].}

 @item{@scheme[#:once-any] --- Only one flag specified in the set can
 be represented on the command line; i.e., the flags in the set are
 mutually exclusive. If the set is represented in the command line
 more than once, the @exnraise[exn:fail].}

 @item{@scheme[#:final] --- Like @scheme[#:multi], except that no
 argument after the flag is treated as a flag. Note that multiple
 @scheme[#:final] flags can be specified if they have short names; for
 example, if @Flag{a} is a @scheme[#:final] flag, then @Flag{aa} combines
 two instances of @Flag{a} in a single command-line argument.}

]

A normal flag specification has four parts:

@itemize[

 @item{@scheme[flags] --- a flag string, or a set of flag strings.  If
 a set of flags is provided, all of the flags are equivalent.  Each
 flag string must be of the form
 @schemevalfont{"-}@schemevarfont{x}@schemevalfont{"} or
 @schemevalfont{"+}@schemevarfont{x}@schemevalfont{"} for some
 character @schemevarfont{x}, or
 @schemevalfont{"--}@schemevarfont{x}@schemevalfont{"} or
 @schemevalfont{"++}@schemevarfont{x}@schemevalfont{"} for some
 sequence of characters @schemevarfont{x}. An @schemevarfont{x} cannot
 contain only digits or digits plus a single decimal point, since
 simple (signed) numbers are not treated as flags. In addition, the
 flags @scheme["--"], @scheme["-h"], and @scheme["--help"] are
 predefined and cannot be changed.}

 @item{@scheme[id]s --- identifier that are bound to the flag's
 arguments. The number of identifiers determines how many arguments
 can be provided on the command line with the flag, and the names of
 these identifiers will appear in the help message describing the
 flag. The @scheme[id]s are bound to string values in the
 @scheme[body]s for handling the flag.}

 @item{@scheme[help-spec] --- a string or sequence of strings that
 describes the flag. This string is used in the help message generated
 by the handler for the built-in @Flag{h} (or @DFlag{help}) flag. A
 single literal string can be provided, or any number of expressions
 that produce strings; in the latter case, strings after the first one
 are displayed on subsequent lines.}

 @item{@scheme[body]s --- expressions that are evaluated when one of
 the @scheme[flags] appears on the command line. The flags are parsed
 left-to-right, and each sequence of @scheme[body]s is evaluated as
 the corresponding flag is encountered. When the @scheme[body]s are
 evaluated, the preceding @scheme[id]s are bound to the arguments
 provided for the flag on the command line.}

]

A flag specification using @scheme[=>] escapes to a more general
method of specifying the handler and help strings. In this case, the
handler procedure and help string list returned by
@scheme[handler-expr] and @scheme[help-expr] are used as in the
@scheme[_table] argument of @scheme[parse-command-line].

A @scheme[#:help-labels] clause inserts text lines into the help table
of command-line flags. Each string in the clause provides a separate
line of text.

After the flag clauses, a final clause handles command-line arguments
that are not parsed as flags:

@itemize[

 @item{Supplying no finish clause is the same as suppling
 @scheme[#:args () (void)].}

 @item{For an @scheme[#:args] finish clause, identifiers in
 @scheme[arg-formals] are bound to the leftover command-line strings
 in the same way that identifiers are bound for a @scheme[lambda]
 expression. Thus, specifying a single @scheme[id] (without
 parentheses) collects all of the leftover arguments into a list. The
 effective arity of the @scheme[arg-formals] specification determines
 the number of extra command-line arguments that the user can provide,
 and the names of the identifiers in @scheme[arg-formals] are used in
 the help string. When the command-line is parsed, if the number of
 provided arguments cannot be matched to identifiers in
 @scheme[arg-formals], the @exnraise[exn:fail]. Otherwise,
 @scheme[args] clause's @scheme[body]s are evaluated to handle the
 leftover arguments, and the result of the last @scheme[body] is the
 result of the @scheme[command-line] expression.}

 @item{A @scheme[#:handlers] finish clause escapes to a more general
 method of handling the leftover arguments. In this case, the values
 of the expressions are used like the last two to four arguments
 @scheme[parse-command-line].}

]

Example:

@schemeblock[
(define verbose-mode (make-parameter #f))
(define profiling-on (make-parameter #f))
(define optimize-level (make-parameter 0))
(define link-flags (make-parameter null))

(define file-to-compile
  (command-line
   #:program "compiler"
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages"
                       (verbose-mode #t)]
   [("-p" "--profile") "Compile with profiling"
                       (profiling-on #t)]
   #:once-any
   [("-o" "--optimize-1") "Compile with optimization level 1"
                          (optimize-level 1)]
   ["--optimize-2"        ((code:comment @#,t{show help on separate lines})
                           "Compile with optimization level 2,"
                           "which includes all of level 1")
                          (optimize-level 2)]
   #:multi
   [("-l" "--link-flags") lf (code:comment @#,t{flag takes one argument})
                          "Add a flag <lf> for the linker"
                          (link-flags (cons lf (link-flags)))]
   #:args (filename) (code:comment @#,t{expect one command-line argument: <filename>})
   (code:comment @#,t{return the argument as a filename to compile})
   filename))
]}

@; ----------------------------------------------------------------------

@defproc[(parse-command-line [name (or/c string? path?)]
                             [argv (or/c (listof string?) (vectorof string?))]
                             [table (listof (cons/c symbol? list?))]
                             [finish-proc ((list?) () #:rest list? . ->* . any)]
                             [arg-help-strs (listof string?)]
                             [help-proc (string? . -> . any) (lambda (str) ....)]
                             [unknown-proc (string? . -> . any) (lambda (str) ...)])
         any]{

Parses a command-line using the specification in @scheme[table]. For
an overview of command-line parsing, see the @scheme[command-line]
form, which provides a more convenient notation for most purposes.

The @scheme[table] argument to this procedural form encodes the
information in @scheme[command-line]'s clauses, except for the
@scheme[args] clause.  Instead, arguments are handled by the
@scheme[finish-proc] procedure, and help information about non-flag
arguments is provided in @scheme[arg-help-strs]. In addition, the
@scheme[finish-proc] procedure receives information accumulated while
parsing flags. The @scheme[help-proc] and @scheme[unknown-proc]
arguments allow customization that is not possible with
@scheme[command-line].

When there are no more flags, @scheme[finish-proc] is called with a
list of information accumulated for command-line flags (see below) and
the remaining non-flag arguments from the command-line. The arity of
@scheme[finish-proc] determines the number of non-flag arguments
accepted and required from the command-line. For example, if
@scheme[finish-proc] accepts either two or three arguments, then
either one or two non-flag arguments must be provided on the
command-line. The @scheme[finish-proc] procedure can have any arity
(see @scheme[procedure-arity]) except @scheme[0] or a list of
@scheme[0]s (i.e., the procedure must at least accept one or more
arguments).

The @scheme[arg-help-strs] argument is a list of strings identifying
the expected (non-flag) command-line arguments, one for each
argument. If an arbitrary number of arguments are allowed, the last
string in @scheme[arg-help-strs] represents all of them.

The @scheme[help-proc] procedure is called with a help string if the
@Flag{h} or @DFlag{help} flag is included on the command line.  If an
unknown flag is encountered, the @scheme[unknown-proc] procedure is
called just like a flag-handling procedure (as described below); it
must at least accept one argument (the unknown flag), but it may also
accept more arguments. The default @scheme[help-proc] displays the
string and exits and the default @scheme[unknown-proc] raises the
@scheme[exn:fail] exception.

A @scheme[table] is a list of flag specification sets. Each set is
represented as a pair of two items: a mode symbol and a list of either
help strings or flag specifications.  A mode symbol is one of
@scheme['once-each], @scheme['once-any], @scheme['multi],
@scheme['final], or @scheme['help-labels], with the same meanings as
the corresponding clause tags in @scheme[command-line]. For the
@scheme['help-labels] mode, a list of help string is provided. For the
other modes, a list of flag specifications is provided, where each
specification maps a number of flags to a single handler procedure. A
specification is a list of three items:

@itemize[

 @item{A list of strings for the flags defined by the spec. See
 @scheme[command-line] for information about the format of flag
 strings.}

 @item{A procedure to handle the flag and its arguments when one of
 the flags is found on the command line. The arity of this handler
 procedure determines the number of arguments consumed by the flag:
 the handler procedure is called with a flag string plus the next few
 arguments from the command line to match the arity of the handler
 procedure. The handler procedure must accept at least one argument to
 receive the flag. If the handler accepts arbitrarily many arguments,
 all of the remaining arguments are passed to the handler.  A handler
 procedure's arity must either be a number or an
 @scheme[arity-at-least] value.

 The return value from the handler is added to a list that is
 eventually passed to @scheme[finish-proc]. If the handler returns
 @|void-const|, no value is added onto this list. For all
 non-@|void-const| values returned by handlers, the order of the
 values in the list is the same as the order of the arguments on the
 command-line.}

 @item{A non-empty list for constructing help information for the
 spec. The first element of the list describes the flag; it can be a
 string or a non-empty list of strings, and in the latter case, each
 string is shown on its own line. Additional elements of the main
 list must be strings to name the expected arguments for the flag. The
 number of extra help strings provided for a spec must match the
 number of arguments accepted by the spec's handler procedure.}

]

The following example is the same as the core example for
@scheme[command-line], translated to the procedural form:

@schemeblock[
(parse-command-line "compile" (current-command-line-arguments)
  `((once-each
     [("-v" "--verbose")
      ,(lambda (flag) (verbose-mode #t))
      ("Compile with verbose messages")]
     [("-p" "--profile")
      ,(lambda (flag) (profiling-on #t))
      ("Compile with profiling")])
    (once-any
     [("-o" "--optimize-1")
      ,(lambda (flag) (optimize-level 1))
      ("Compile with optimization level 1")]
     [("--optimize-2")
      ,(lambda (flag) (optimize-level 2))
      (("Compile with optimization level 2,"
        "which implies all optimizations of level 1"))])
    (multi
     [("-l" "--link-flags")
      ,(lambda (flag lf) (link-flags (cons lf (link-flags))))
      ("Add a flag <lf> for the linker" "lf")]))
   (lambda (flag-accum file) file)
   '("filename"))
]}
