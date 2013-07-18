#lang scribble/manual

@title{Hello}

@table-of-contents[]

@section[#:style 'grouper]{One}

@subsection{A Section}

@subsubsection{A Subsection}

This is some prose.

@subsubsection{A Subsection, Revisited}

This is also some prose.


@section[#:style 'grouper]{Two}

@subsection{Another Section}

@subsubsection{Another Subsection}

More prose.

@subsubsection[#:style 'unnumbered]{>> Unnumbered Subsection}

Nothing to see here.

@subsubsection{Another Subsection, Revisited}

More prose, also.

@subsubsection[#:style 'hidden-number]{>> Hidden Number}

Nothing to see here, either.

@subsubsection{Last Subsection}

The last subsection has some prose.

@subsection[#:style '(hidden toc-hidden)]{}

This is actually in a hidden section.
