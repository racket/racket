#lang scribble/doc
@(require scribble/manual
          (for-label racket/base)
          )

@title[#:style '(toc)]{Error Message Composition Guidelines for *SL}

These guidelines distill our current thoughts on writing good error messages
for novices, as informed by our research.  Please apply these to all code you
write, including libraries and Teachpacks.  Inform Kathi, Shriram, and
Guillaume of any cases that appear awkward under these guidelines.

@section{General Guidelines}

@itemize[
@item{
Frustrated students will peer at the error message for clues on how to
proceed. Avoid offering hints, and avoid pro-posing any specific
modification. Students will follow well-meaning-but-wrong advice uncritically,
if only because they have no reason to doubt the authoritative voice of the
tool.}

@item{Be concise and clear. Students give up reading error messages if the text is
too long, uses obscure words, or employs difficult grammar.
}]

@section{Message Structure and Form}
@itemize[
@item{Start the message with the name of the construct whose constraint is being
violated, followed by a colon.
}

@item{State the constraint that was violated (``expected a...''), then contrast with what
was found. For example, @emph{``this function expects two arguments, but found only
one.''} If needed, explain how what was found failed to satisfy the constraint.
Write somewhat anthropomorphically with an objective voice that is neither
friendly nor antagonistic.
}

@item{If an expression contains multiple errors, report the leftmost error first:
e.g., the error in (define 1 2 3) is @emph{``expected the variable name, but found a
number''}, not @emph{``expected 2 parts after define, but found 3''}. Before raising an
error about a sub-part, call @racket[local-syntax-expand] on all sub-expressions to
the left to trigger their errors.
}

@item{State the number of parts instead of saying ``found too many parts.'' Write the
code necessary to make plurals agree.
}]

@section{Vocabulary}

@subsection{Permitted Words}

Use only the following vocabulary words to describe code:

@nested{@tt{function, variable, argument, function body, expression, part, clause,
top level, structure name, type name, field name, binding.}}

@itemize[
@item{Use ‘binding’ for the square-braced pair in a let and other similar binding
forms.
}

@item{Use ‘argument’ for actual arguments and ‘variable’ for formal arguments and in
the body of the definition.
}

@item{Use ‘part’ when speaking about an s-expression that is not an expression,
either because it is malformed, because it occurs in a non-expression position,
or because it is a valid piece of syntax for a macro invocation. A well-formed
and well-placed call to a function, primitive, or macro is not a ‘part’, it is
an ‘expression’.   
}]

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
modifier).
}

@item{When introducing macros with sub-parts, reuse existing vocabulary words, such
as ‘clause’ or ‘binding’ (if appropriate), or just ‘part’, instead of defining
new terms.
}

@item{Use ‘name’ only when describing the syntax of a definition form. For example,
the define form in BSL should say ``expected at least one variable after the
function name.''  Outside of the definition form, simply use the word ‘function’
rather than distinguish between (1) a function, (2) the variable that binds the
function, and (3) the name of that variable. [Rationale: Students learn this
distinction when they learn about lambdathe first is the lambda implicit in the
definition, the second is the variable introduced by the definition that can
appear as the first argument to set!, the third is the particular sequence of
lettersbut BSL should avoid this complexity, and ASL’s error messages should
maintain consistency with BSL.]
}

@item{Avoid introducing technical vocabulary, even if well-known to a mathematician.
}]

@section{Punctuation}

@itemize[

@item{Do not use any punctuation beyond those of the normal English language. Do not
write <> around type names, and do not write ` ‘ around keywords.
}]
 

