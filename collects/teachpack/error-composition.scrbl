#lang scribble/doc
@(require scribble/manual
          (for-label racket/base)
          )

@title[#:tag "error-guidelines"]{Error Message Composition Guidelines for *SL}

These guidelines distill our current thoughts on writing good error messages for
novices, as informed by @seclink["research"]{our research}.  Please apply these to all code you write
that's intended for beginners, including libraries and Teachpacks. It will
ensure your error messages harmonizes with those of the teaching languages.

@section{General Guidelines}

@itemize[
@item{
Frustrated students will peer at the error message for clues on how to
proceed. Avoid offering hints, and avoid pro-posing any specific
modification. Students will follow well-meaning-but-wrong advice uncritically,
if only because they have no reason to doubt the authoritative voice of the
tool.}

@item{Be concise and clear. Students give up reading error messages if the text is
too long, uses obscure words, or employs difficult grammar.}]

@section{Message Structure and Form}
@itemize[
@item{Start the message with the name of the construct whose constraint is being
violated, followed by a colon.}

@item{State the constraint that was violated (``expected a...''), then contrast with what
was found. For example, @emph{``this function expects two arguments, but found only
one.''} If needed, explain how what was found failed to satisfy the constraint.
Write somewhat anthropomorphically with an objective voice that is neither
friendly nor antagonistic.}

@item{If an expression contains multiple errors, report the leftmost error first:
e.g., the error in (define 1 2 3) is @emph{``expected the variable name, but found a
number''}, not @emph{``expected 2 parts after define, but found 3''}. Before raising an
error about a sub-part, call @racket[syntax-local-expand-expression] on all sub-expressions to
the left to trigger their errors.}

@item{State the number of parts instead of saying ``found too many parts.'' Write the
code necessary to make plurals agree.}]

@section{Vocabulary}

@subsection{Permitted Words}

Use only the following vocabulary words to describe code:

@nested{@tt{function, variable, argument, function body, expression, part, clause,
top level, structure name, type name, field name, binding.}}

@itemize[
@item{Use ‘binding’ for the square-braced pair in a let and other similar binding
forms.}

@item{Use ‘argument’ for actual arguments and ‘variable’ for formal arguments and in
the body of the definition.}

@item{Use ‘part’ when speaking about an s-expression that is not an expression,
either because it is malformed, because it occurs in a non-expression position,
or because it is a valid piece of syntax for a macro invocation. A well-formed
and well-placed call to a function, primitive, or macro is not a ‘part’, it is
an ‘expression’.   }]

@subsection{Prohibited Words}

These guidelines use fewer terms than the prior implementations of *SL,
emphasizing commonality among concepts rather than technical precision (which
most students do not appreciate anyway).

@tabular[
@list[
@list[@para{@bold{Instead of}} @para{@bold{Use}}]

@list[@para{procedure, primitive name, primitive operator, predicate, selector,
constructor} @para{``function''}]

@list[@para{s-expression} @para{``expression''}]

@list[@para{identifier} @para{``argument'' or ``variable'', depending on the use in the
program}]

@list[@para{defined name} @para{``function'' or ``variable''}]

@list[@para{sequence}  @para{``at least one (in parentheses)''}]

@list[@para{function header} @para{``after define'', ``after the name'', ``after the first
argument'', ...}]

@list[@para{keyword} @para{mention the construct directly by name, such as ``expected a
variable but found a cond''}]

@list[@para{built-in}  @para{Nothing – avoid this term}]

@list[@para{macro} @para{Nothing – avoid this term}]]]
 

@subsection{General Vocabulary Guidelines}
@itemize[
@item{Avoid modifiers that are not necessary to disambiguate. Write ‘variable’
instead of ``local variable'', ``defined variable'', or ``input variable''. Write
‘clause’ instead of ``question-answer clause''.  If they appear necessary for
disambiguation, try to find some other way to achieve this (and drop the
modifier).}

@item{When introducing macros with sub-parts, reuse existing vocabulary words, such
as ‘clause’ or ‘binding’ (if appropriate), or just ‘part’, instead of defining
new terms.}

@item{Use ‘name’ only when describing the syntax of a definition form. For example,
the define form in BSL should say ``expected at least one variable after the
function name.''  Outside of the definition form, simply use the word ‘function’
rather than distinguish between (1) a function, (2) the variable that binds the
function, and (3) the name of that variable. [Rationale: Students learn this
distinction when they learn about lambdathe first is the lambda implicit in the
definition, the second is the variable introduced by the definition that can
appear as the first argument to set!, the third is the particular sequence of
lettersbut BSL should avoid this complexity, and ASL’s error messages should
maintain consistency with BSL.]}

@item{Avoid introducing technical vocabulary, even if well-known to a mathematician.}]

@section{Punctuation}
@itemize[
@item{Do not use any punctuation beyond those of the normal English language. Do not
write <> around type names, and do not write ` ‘ around keywords.}]

@section{Runtime Behavior}
@itemize[

@item{When specifying a function's behavior, say ``the function takes ... and returns ...''}

@item{When describing a contract violation, say ``the function expects ... but received ...''}

@item{As much as possible, identify expressions to the value they evaluate to,
e.g. ``the value of @tt{(f x)} is 5''. If it is necessary to explicate
evaluation times, the context discusses mutable state or order of evaluation,
then say that the expressions ``evaluate to'' a value. Function calls are a
special case of expression. Prefer ``the function call returns'', instead of
``it evaluates to'', except when trying to draw attention to you evaluation of
the arguments.}]


@section[#:tag "research"]{Supporting Research}

These guidelines arose from the collections of research studies held at
Worcester Polytechnic Institute, Brown University, and Northeastern
University. People who are interested in learning the details of our
experiments, our results should read the following two papers.

@itemize[
@item{@hyperlink["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mfk-mind-lang-novice-inter-error-msg/"]{Mind Your Language: On Novices’ Interactions with Error Messages}

This paper reports on a series of studies that explore beginning students’
interactions with the vocabulary and source-expression highlighting in
DrRacket. Our findings demonstrate that the error message DrRacket's old error
messages significantly failed to convey information accurately to students.}

@item{@hyperlink["http://www.cs.brown.edu/~sk/Publications/Papers/Published/mfk-measur-effect-error-msg-novice-sigcse/"]{Measuring
the Effectiveness of Error Messages Designed for Novice Programmers} 

This paper presents a finegrained grading rubric for evaluating the performance
of individual error messages. We applied the rubric to a courseworth of student
work, which allowed us to characterize some ways error messages fail.}]


