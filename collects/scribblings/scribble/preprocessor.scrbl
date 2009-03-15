#lang scribble/doc
@(require scribble/manual scribble/struct "utils.ss"
          (for-label scheme/base))
@initialize-tests

@title[#:tag "preprocessor"]{Text Preprocessor}

@defmodulelang[scribble/text]{The @schememodname[scribble/text]
language provides everything from @scheme[scheme/base] with a few
changes that make it suitable as a preprocessor language:

@itemize{

  @item{It uses @scheme[read-syntax-inside] to read the body of the
        module, similar to @secref["docreader"].  This means that by
        default, all text is read in as Scheme strings; and
        @seclink["reader"]|{@-forms}| can be used to use Scheme
        functions and expression escapes.}

  @item{Values of expressions are printed with a custom
        @scheme[output] function.  This function displays most values
        in a similar way to @scheme[display], except that it is more
        convenient for a preprocessor output.}}

}

@;--------------------------------------------------------------------
@section{Writing Preprocessor Files}

The combination of the two features makes text in files in the
@scheme[scribble/text] language be read as strings, which get printed
out when the module is @scheme[require]d, for example, when a file is
given as an argument to @exec{mzscheme}.  (In these example the left
part shows the source input, and the right part the printed result.)

@example|-{#lang scribble/text
           Programming languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.
           ---***---
           Programming languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.}-|

Using @seclink["reader"]|{@-forms}| we can define and use Scheme
functions.

@example|-{#lang scribble/text
           @(require scheme/list)
           @(define Foo "Preprocessing")
           @(define (3x . x)
              (add-between (list x x x) " "))
           @Foo languages should
           be designed not by piling
           feature on top of feature, but
           @3x{blah}.
           ---***---
           Preprocessing languages should
           be designed not by piling
           feature on top of feature, but
           blah blah blah.}-|

As demonstrated in this case, the @scheme[output] function simply
scans nested list structures recursively, which makes them convenient
for function results.  In addition, @scheme[output] prints most values
similarly to @scheme[display] \- a notable exception are void and
false values which cause no output to appear.  This can be used for
convenient conditional output.

@example|-{#lang scribble/text
           @(define (errors n)
              (list n
                    " error"
                    (and (not (= n 1)) "s")))
           You have @errors[3] in your code,
           I fixed @errors[1].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

Using the scribble @seclink["reader"]|{@-forms}| syntax, you can write
functions more conveniently too.

@example|-{#lang scribble/text
           @(define (errors n)
              @list{@n error@;
                    @and[(not (= n 1))]{s}})
           You have @errors[3] in your code,
           I fixed @errors[1].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

Following the details of the scribble reader, you may notice that in
these examples there are newline strings after each definition, yet
they do not show in the output.  To make it easier to write
definitions, newlines after definitions and indentation spaces before
them are ignored.

@example|-{#lang scribble/text

           @(define (plural n)
              (unless (= n 1) "s"))

           @(define (errors n)
              @list{@n error@plural[n]})

           You have @errors[3] in your code,
           I fixed @errors[1].
           ---***---
           You have 3 errors in your code,
           I fixed 1 error.}-|

@;--------------------------------------------------------------------
@section{Using External Files}

Using additional files that contain code for your preprocessing is
trivial: the preprocessor source is a plain Scheme file, so you can
@scheme[require] additional files as usual.

However, things can become tricky if you want to include an external
file that should also be preprocessed.  Using @scheme[require] with a
text file (that uses the @scheme[scribble/text] language) almost
works, but when a module is required, it is invoked before the current
module, which means that the required file will be preprocessed before
the current file regardless of where the @scheme[require] expression
happens to be.  Alternatively, you can use @scheme[dynamic-require]
with @scheme[#f] for the last argument (which makes it similar to a
plain @scheme[load])---but remember that the path will be relative to
the current directory, not to the source file.

Finally, there is a convenient syntax for including text files to be
processed:

@defform[(include filename)]{

Preprocess the @scheme[filename] using the same syntax as
@scheme[scribble/text].  This is similar to using @scheme[load] in a
namespace that can access names bound in the current file so included
code can refer to bindings from the including module.  Note, however,
that the including module cannot refer to names that are bound the
included file because it is still a plain scheme module---for such
uses you should still use @scheme[require] as usual.}
