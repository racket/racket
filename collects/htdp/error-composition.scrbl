#lang scribble/doc

@(require scribble/manual 
          (for-label [only-in lang/htdp-advanced set!]
                     [only-in lang/htdp-intermediate let]  
                     [only-in lang/htdp-beginner define]
                     [only-in racket/base syntax-local-expand-expression]))

@(require scribble/decode)

@(require [only-in scribble/core table style])


@title[#:tag "error-guidelines"]{Error Message Composition Guidelines}

This section lists some guidelines for writing good error messages for
novices, as informed by @seclink["research"]{our research}.  Please
follow these guidelines when you write code that is intended for
beginners, including libraries and teachpacks.  It ensures that error
messages from your code fits messages from the student languages and
from other teachpacks.

@(define (samp . text) @splice[@list{@emph{``@splice[text]''}}])
@(define (word . text) @splice[@list{‘@splice[text]’}])

@section{General Guidelines}

@itemize[
  @item{Frustrated students will peer at the error message for clues on
    how to proceed.  Avoid offering hints, and avoid proposing any
    specific modification. Students will follow well-meaning-but-wrong
    advice uncritically, if only because they have no reason to doubt
    the authoritative voice of the tool.}

  @item{Be concise and clear.  Students give up reading error messages
    if the text is too long, uses obscure words, or employs difficult
    grammar.}]

@section{Message Structure and Form}

@itemize[
  @item{Start the message with the name of the construct whose
    constraint is being violated, followed by a colon.}

  @item{State the constraint that was violated (@samp{expected a...}),
    then contrast with what was found.  For example, @samp{this function
    expects two arguments, but found only one}.  If needed, explain how
    what was found fails to satisfy the constraint.  Write somewhat
    anthropomorphically with an objective voice that is neither friendly
    nor antagonistic.}

  @item{If an expression contains multiple errors, report the leftmost
    error first.  E.g., the error in @racket[(define 1 2 3)] is
    @samp{expected the variable name, but found a number}, not
    @samp{expected 2 parts after define, but found 3}.  Before raising
    an error about a sub-part of a macro, call
    @racket[syntax-local-expand-expression] on sub-expressions to its
    left, so that such errors are shown first.}

  @item{State the number of parts instead of saying @samp{found too many
    parts}.  Write the code necessary to make plurals agree.}]

@section{Words For Describing Code}

Use only the following vocabulary words to describe code:

@table[(style 'boxed '())
@list[@list[@para{function} @para{variable} @para{argument} @para{function body}]
      @list[@para{expression} @para{part} @para{clause} @para{top level}]
      @list[@para{structure name} @para{type name} @para{field name} @para{binding}]]]

@itemize[
  @item{Use binding for the square-braced pair in a @racket[let]
    and similar binding forms.}

  @item{Use @word{argument} for actual arguments and @word{variable} for
    formal arguments in the header and body of a definition.}

  @item{Use @word{part} when speaking about an s-expression that is not
    an expression, either because it is malformed, because it occurs in
    a non-expression position, or because it is a valid piece of syntax
    for a macro invocation.  A well-formed and well-placed call to a
    function, primitive, or macro is not a @word{part}, it is an
    @word{expression}.}]

@section{Words For Describing Runtime Behavior}

Use the following vocabulary words to describe how code runs:

@itemize[
  @item{When specifying a function's behavior, say @samp{the function
    takes ... and produces ...}}

  @item{When describing a contract violation, say @samp{the function
    expects ... but received ...}}

  @item{As much as possible, identify expressions and the value they evaluate
    to, e.g. @samp{the value of @racket[(f x)] is 5}.  If it is necessary to
    mention evaluation order, such as when the context discusses mutable state,
    say that the expression @samp{evaluates to} a value.  Function calls
    are a special case of expression.  Prefer @samp{the function call produces ...}
    to @samp{the function call evaluates to ...}, except when trying to draw attention to
    the evaluation of the arguments.}

  @item{@racket[set!] and
    @racketidfont{set-}@racket[_structure-name]@racketidfont{-}@racket[_field-name]@racketidfont{!}
    @word{mutate} variables and structure instances, respectively. Avoid using
    the verb @word{sets} when discussing mutation, and reserve the verbs
    @word{changes} and @word{updates} for functional updates.}]


@section{Prohibited Words}

These guidelines use few terms intentionally, emphasizing commonality
among concepts rather than technical precision (which most students do
not appreciate anyway).

@table[(style 'boxed '())
@list[
  @list[@para{@bold{Instead of}}
        @para{@bold{Use}}]
  @list[@para{procedure, primitive name, primitive operator, predicate,
          selector, constructor}
        @para{@samp{function}''}]
  @list[@para{s-expression}
        @para{@samp{expression}}]
  @list[@para{identifier}
        @para{@samp{argument} or @samp{variable}, depending on the use
          in the program}]
  @list[@para{defined name}
        @para{@samp{function} or @samp{variable}}]
  @list[@para{sequence}
        @para{@samp{at least one (in parentheses)}}]
  @list[@para{function header}
        @para{@samp{after define}, @samp{after the name},
          @samp{after the first argument}, ...}]
  @list[@para{keyword}
        @para{mention the construct directly by name, such as
          @samp{expected a variable but found a cond}}]
  @list[@para{built-in}  @para{Nothing --- avoid this term}]
  @list[@para{macro} @para{Nothing --- avoid this term}]]]

@section{General Vocabulary Guidelines}

@itemize[
  @item{Avoid modifiers that are not necessary to disambiguate. Write
    @word{variable} instead of @word{local variable}, @word{defined
    variable}, or @word{input variable}.  Write @word{clause} instead of
    @word{question-answer clause}.  If they appear necessary for
    disambiguation, try to find some other way to achieve this (and drop
    the modifier).}

  @item{When introducing macros with sub-parts, reuse existing
    vocabulary words, such as @word{clause} or @word{binding} (if
    appropriate), or just @word{part}, instead of defining new terms.}

  @item{Use @word{name} only when describing the syntax of a definition
    form.  For example, the define form in BSL should say @samp{expected
    at least one variable after the function name}.  Outside of the
    definition form, simply use the word @word{function} rather than
    distinguish between (1) a function, (2) the variable that binds the
    function, and (3) the name of that variable.

    [Rationale: Students learn this distinction when they learn about
    lambda.  The first is the lambda implicit in the definition, the
    second is the variable introduced by the definition that can appear
    as the first argument to @racket[set!], the third is the particular
    sequence of letters.  But BSL should avoid this complexity, and
    ASL’s error messages should maintain consistency with BSL.]}

  @item{Avoid introducing technical vocabulary, even if well-known to a
    mathematician.}]

@section{Punctuation}

@itemize[
  @item{Do not use any punctuation beyond those of the normal English
    language.  Do not write @litchar{<>} around type names, and do not
    write quotes around keywords.}]

@section[#:tag "research"]{Supporting Research}

These guidelines arose from a collections of research studies held at
the Worcester Polytechnic Institute, Brown University, and Northeastern
University.  Further experiment details and results are described in:
@itemize[
  @item{@hyperlink["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mfk-mind-lang-novice-inter-error-msg/"]{
          Mind Your Language: On Novices' Interactions with Error
          Messages}

    This paper reports on a series of studies that explore beginning
    students' interactions with the vocabulary and source-expression
    highlighting in DrRacket.  Our findings demonstrate that the error
    message DrRacket's old error messages significantly failed to convey
    information accurately to students.}

  @item{@hyperlink["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mfk-measur-effect-error-msg-novice-sigcse/"]{
          Measuring the Effectiveness of Error Messages Designed for
          Novice Programmers}

    This paper presents a fine-grained grading rubric for evaluating the
    performance of individual error messages.  We applied the rubric to
    a course worth of student work, which allowed us to characterize
    some ways error messages fail.}]

@; -----------------------------------------------------------------------------
