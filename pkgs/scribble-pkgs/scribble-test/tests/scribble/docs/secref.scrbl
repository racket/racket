#lang scribble/base

@title[#:tag "top"]{Title}

@section[#:tag "1" #:style 'grouper]{Section One}

This is @secref["1"] in @secref["top"].

See @secref["2"] and @secref["2.1"].

See @secref["3"], @secref["3.1"], @secref["3.2"], @secref["3.2.1"], and
@secref["3.3"].


@section[#:tag "2" #:style 'grouper]{Section Two}

@subsection[#:tag "2.1"]{Section Two.One}

[content a]


@section[#:style '(grouper unnumbered) #:tag "3"]{Section Three}

@subsection[#:tag "3.1"]{Section Three.One}

[content b]

@subsection[#:tag "3.2"]{Section Three.Two}

@subsubsection[#:tag "3.2.1"]{Section Three.Two.One}

[content c]

@subsection[#:tag "3.3" #:style 'unnumbered]{Section Three.Three}

[content d]
