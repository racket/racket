#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     scheme/port))

@title{@exec{mzpp} and @exec{mztext}: Preprocessors}

@author["Eli Barzilay"]

The @filepath{preprocessor} collection defines two Racket-based
preprocessors for texts that can have embedded Racket code.  The two
processors share a few features, like several command-line flags and
the fact that embedded Racket code is case-sensitive by default.

Note that these processors are @bold{not} intended as preprocessors for
Racket code, since you have macros to do that.

@table-of-contents[]

@; ----------------------------------------

@section[#:tag "overview"]{Overview}

@defmodule[preprocessor/pp-run]

The preprocessors can be invoked from Racket programs, but the main
usage should be through the launchers.  Both launchers use code from
@racketmodname[preprocessor/pp-run] that allows a special invocation
mode through the @DFlag{run} flag.

The @DFlag{run} is a convenient way of making the preprocessors cooperate
with some other command, making it possible to use preprocessed text
without an additional glue script or a makefile.  The following examples
use @exec{mzpp}, but they work with @exec{mztext} too.  @DFlag{run} uses a single
argument which is a string specifying a command to run:

@itemize[

@item{1. In its simplest form, the command string specifies some shell command
   which will be executed with its standard input piped in from the
   preprocessor's output.  For example, @exec{mzpp --run pr foo} is the same
   as @exec{mzpp foo | pr}.  An error is raised if an output file is
   specified with such an argument.}

@item{2. If the command string contains a @racket[*] and an output file is
   specified, then the command will be executed on this output file after it is
   generated.  For example, @exec{mzpp --run 'pr *' -o foo x y z} is the same
   as @exec{mzpp -o foo x y z; pr foo}.}

@item{3. If the command string contains a @racket[*], and no output file is
   specified, and there is exactly one input file, then a temporary file
   will be used to save the original while the command is running.  For
   example, @exec{mzpp --run 'pr *' foo} is the same as @exec{mv foo
   foo-mzpp-temporary; mzpp -o foo foo-mzpp-temporary; pr foo; rm foo;
   mv foo-mzpp-temporary foo}.  If there is an error while @exec{mzpp} is
   running, the working file will be erased and the original will be
   renamed back.}

@item{4. Any other cases where the command string contains a @litchar{*} are invalid.}

]

If an executed command fails with a return status different than 0, the
preprocessor execution will signal a failure by returning 1.


@; ----------------------------------------
@include-section["mzpp.scrbl"]

@; ----------------------------------------
@include-section["mztext.scrbl"]

@; ----------------------------------------
