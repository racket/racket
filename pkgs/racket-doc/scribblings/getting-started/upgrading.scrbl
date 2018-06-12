#lang scribble/manual

@title{Upgrading from earlier 6.x and 7.x versions of Racket}

@section{Linux}

@subsection{Upgrading from a release version to a newer release versions, or from a nightly to a newer nightly}

@itemlist[
  @item{Download the newer release from @url{https://download.racket-lang.org/}.}
  @item{@code{chmod +x racket-X.Y-PLATFORM.sh}}
  @item{@code{./racket-X.Y-PLATFORM.sh} and answer the questions to choose where the new version should be installed.}
  @item{@code{raco setup}}]

@subsection{Upgrading from a release version to a nightly}

@itemlist[
  @item{Download the newer release from @url{https://download.racket-lang.org/}.}
  @item{@code{chmod +x racket-X.Y-PLATFORM.sh}}
  @item{@code{./racket-X.Y-PLATFORM.sh} and answer the questions to choose where the new version should be installed.}
  @item{@code{mv ~/.racket/snapshot ~/.racket/snapshot.bak} remove and make a backup of the old "snapshot" folder if one existed}
  @item{@code{mv ~/.racket/OLD_VERSION ~/.racket/snapshot}}
  @item{@code{raco setup}}]

@subsection{Upgrading from a nightly to a release version}

@itemlist[
  @item{Download the newer release from @url{https://download.racket-lang.org/}.}
  @item{@code{chmod +x racket-X.Y-PLATFORM.sh}}
  @item{@code{./racket-X.Y-PLATFORM.sh} and answer the questions to choose where the new version should be installed.}
  @item{@code{mv ~/.racket/SOME_VERSION} remove and make a backup of the old "snapshot" folder if one existed}
  @item{@code{mv ~/.racket/snapshot ~/.racket/SOME_VERSION}}
  @item{@code{raco setup}}]

@section{Upgrading from earlier versions}

If I'm not mistaken, @code{raco} was introduced at some point durign the 5.x series?