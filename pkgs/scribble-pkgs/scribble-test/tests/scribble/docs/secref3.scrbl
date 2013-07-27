#lang scribble/base

@title[#:tag "top"]{Title}

@table-of-contents[]

@section[#:tag "1"]{Section One}

This is @secref["1"] in @secref["top"].

See @secref["2"] and @secref["2.1"].

See @secref["3"], @secref["3.1"], @secref["3.1.1"], @secref["3.2"],
@secref["3.2.1"], and @secref["3.3"].

See @secref["4"].

See @secref["5"] and @secref["5.1"].

@section[#:tag "2"]{Section Two}

@subsection[#:tag "2.1"]{Section Two.One}

[content a]


@section[#:tag "3"]{Section Three}

@subsection[#:tag "3.1" #:style 'grouper]{Section Three.One}

@subsubsection[#:tag "3.1.1"]{Section Three.One.One}

[content b]

@subsection[#:tag "3.2" #:style 'grouper]{Section Three.Two}

@subsubsection[#:tag "3.2.1"]{Section Three.Two.One}

[content c]

@subsection[#:tag "3.3" #:style '(unnumbered grouper)]{Section Three.Three}

[content d]


@section[#:style '(unnumbered) #:tag "4"]{Section Four}

[content e]


@section[#:tag "5"]{Section Five}

@subsection[#:tag "5.1"]{Section Five.One}

[content f]
