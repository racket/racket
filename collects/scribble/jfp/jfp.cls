%%
%% This is file `jfp.cls'
%%
%% CUP Journal of Functional Programming document class
%% Copyright 2001 Cambridge University Press
%%
%% by Mark A. Reed
%%   based on JFP.STY v1.24.
%%
%% Incorporating parts of authordate.sty
%%    by David Rhead, Cripps Computing Centre (Feb 1990).
%%
%% Bugs (in the case of unchanged files) should be reported to
%% texline@cambridge.org
%%
%% This software may only be used in the preparation of journal articles
%% or books or parts of books to be published by Cambridge University Press.
%% Any other use constitutes an infringement of copyright.
%%
%% \CharacterTable
%%  {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%   Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%   Digits        \0\1\2\3\4\5\6\7\8\9
%%   Exclamation   \!     Double quote  \"     Hash (number) \#
%%   Dollar        \$     Percent       \%     Ampersand     \&
%%   Acute accent  \'     Left paren    \(     Right paren   \)
%%   Asterisk      \*     Plus          \+     Comma         \,
%%   Minus         \-     Point         \.     Solidus       \/
%%   Colon         \:     Semicolon     \;     Less than     \<
%%   Equals        \=     Greater than  \>     Question mark \?
%%   Commercial at \@     Left bracket  \[     Backslash     \\
%%   Right bracket \]     Circumflex    \^     Underscore    \_
%%   Grave accent  \`     Left brace    \{     Vertical bar  \|
%%   Right brace   \}     Tilde         \~}
%%

\NeedsTeXFormat{LaTeX2e}[1997/12/01]
\ProvidesClass{jfp}[2001/09/27 v1.02 Journal of Functional Programming
  ^^Jdocument class]

\newif\ifprodtf

\DeclareOption{oneside}{\relax}
\DeclareOption{twoside}{\@twosidetrue \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5\p@}}
\DeclareOption{final}{\setlength\overfullrule{\z@}}
\DeclareOption{onecolumn}{\@twocolumnfalse}
\DeclareOption{twocolumn}{\relax}
\DeclareOption{titlepage}{\relax}
\DeclareOption{notitlepage}{\relax}
\DeclareOption{leqno}{\relax}
\DeclareOption{fleqn}{\relax}
\DeclareOption{prodtf}{\prodtftrue}

\ExecuteOptions{twoside,final,onecolumn}
\ProcessOptions\relax

\newif\ifCUPmtlplainloaded
\ifprodtf
  \CUPmtlplainloadedtrue
  \RequirePackage{CUPTimes,jfp2esym}
\fi

\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\renewcommand\baselinestretch{}

\renewcommand\normalsize{%
  \@setfontsize\normalsize\@xpt{13}%
  \abovedisplayskip 6.5\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 3\p@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \let\@listi\@listI}

\normalsize

\newcommand\small{%
  \@setfontsize\small\@ixpt{11}%
  \abovedisplayskip 6\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip 3\p@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \def\@listi{\leftmargin\leftmargini
    \topsep 6\p@ \@plus 1\p@ \@minus 1\p@
    \parsep \z@ \itemsep \parsep}%
}

\newcommand\footnotesize{%
  \@setfontsize\footnotesize\@viiipt\@ixpt
  \abovedisplayskip 5\p@ \@plus 1\p@ \@minus 1\p@
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip \z@ \@plus 1\p@
  \belowdisplayshortskip \abovedisplayshortskip
  \def\@listi{\leftmargin\leftmargini
    \topsep 4.5\p@ \@plus 1\p@ \@minus 1\p@
    \parsep \z@ \itemsep \parsep}%
}

\newcommand\scriptsize{\@setfontsize\scriptsize\@viipt\@viiipt}
\newcommand\tiny{\@setfontsize\tiny\@vpt\@vipt}
\newcommand\large{\@setfontsize\large\@xiipt{14}}
\newcommand\Large{\@setfontsize\Large\@xivpt{18}}
\ifprodtf
  \newcommand\LARGE{\@setfontsize\LARGE{18}{21}}%
\else
  \newcommand\LARGE{\@setfontsize\LARGE\@xviipt{21}}%
\fi
\newcommand\huge{\@setfontsize\huge\@xxpt{25}}
\newcommand\Huge{\@setfontsize\Huge\@xxvpt{30}}

\newcommand\affilsize{\@setfontsize\affilsize\@viiipt\@xpt}
\let\authorsize\normalsize

\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}

\ifprodtf \else
  \setlength\oddsidemargin{3.75pc}
  \setlength\evensidemargin{3.75pc}
  \setlength\topmargin{3.25pc}
\fi

\setlength\marginparwidth{2.0cm}
\setlength\marginparsep{10\p@}

\setlength\headheight{13\p@}
\setlength\headsep{11\p@}
\setlength\topskip{13\p@}
\setlength\footskip{26\p@}

\setlength\textheight{44\baselineskip}
\addtolength\textheight{\topskip}
\setlength\textwidth{30pc}
\setlength\columnsep{10\p@}
\setlength\columnseprule{\z@}

\setlength\footnotesep{\z@}
\setlength{\skip\footins}{19.5\p@ \@plus 12\p@ \@minus 1\p@}

\setlength\floatsep{13\p@ \@plus 6.5\p@ \@minus 1\p@}
\setlength\textfloatsep{15\p@ \@plus 4.5\p@ \@minus 3\p@}
\setlength\intextsep{13\p@ \@plus 6.5\p@ \@minus 2\p@}
\setlength\dblfloatsep{13\p@ \@plus 6.5\p@ \@minus 2\p@}
\setlength\dbltextfloatsep{15\p@ \@plus 4.5\p@ \@minus 3\p@}
\setlength\@fptop{\z@ \@plus 0fil}
\setlength\@fpsep{13\p@ \@plus 0fil}
\setlength\@fpbot{\z@ \@plus 3fil}
\setlength\@dblfptop{\z@ \@plus 0fil}
\setlength\@dblfpsep{13\p@ \@plus 0fil}
\setlength\@dblfpbot{\z@ \@plus 3fil}
\setlength\marginparpush{5\p@}

\setlength\parskip{\z@ \@plus .3\p@}
\setlength\parindent{1em}
\setlength\partopsep{\z@ \@plus 1\p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\@beginparpenalty -\@lowpenalty
\@endparpenalty -\@lowpenalty
\@itempenalty -\@lowpenalty
\clubpenalty\z@
\widowpenalty\@M

\newcommand\partname{Part}
\newcommand\part{\par\addvspace{4ex}\@afterindentfalse \secdef\@part\@spart}

\def\@part[#1]#2{%
  \ifnum \c@secnumdepth >\m@ne
    \refstepcounter{part}%
    \addcontentsline{toc}{part}{\partname\ \thepart: #1}%
  \else
    \addcontentsline{toc}{part}{#1}%
  \fi
  {\parindent \z@ \centering
   \ifnum \c@secnumdepth >\m@ne
     \normalfont\large\rmfamily \MakeUppercase{\partname}\ %
     \ifcase\thepart \or ONE \or TWO \or THREE \or FOUR \or FIVE
     \or SIX \or SEVEN \or EIGHT \or NINE \or TEN \else \fi
     \par \nobreak
   \fi
   \normalfont\LARGE\rmfamily #2 \markboth{}{}\par}%
   \nobreak \vskip 3ex \@afterheading
}

\def\@spart#1{%
  {\parindent \z@ \centering\normalfont\LARGE\rmfamily #1\par}%
  \nobreak \vskip 3ex \@afterheading
}

\newcommand\section{%
  \@startsection {section}{1}{\z@}
    {-19.5\p@ \@plus -6.5\p@ \@minus -3.25\p@}
    {6.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\bfseries\centering}%
}

\newcommand\subsection{%
  \@startsection{subsection}{2}{\z@}
    {-19.5\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {6.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\bfseries\itshape\centering}%
}

\newcommand\subsubsection{%
  \@startsection{subsubsection}{3}{\z@}
    {-19.5\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {6.5\p@ \@plus \z@ \@minus 1\p@}
    {\normalfont\normalsize\itshape\centering}%
}

\newcommand\paragraph{%
  \@startsection{paragraph}{4}{\z@}
    {-13\p@ \@plus -1.5\p@ \@minus -1.5\p@}
    {-0.5em}
    {\normalfont\normalsize\itshape\raggedright}%
}

\newcommand\subparagraph{%
  \@startsection{subparagraph}{4}{\parindent}
    {-13\p@ \@plus -3.25\p@ \@minus -3.25\p@}
    {-0.5em}
    {\normalfont\normalsize\rmfamily\raggedright}%
}

\def\@seccntformat#1{\csname the#1\endcsname\enskip}% FROM LATEX.LTX

\newcommand\appendixname{Appendix}

\newcommand\appendix{\par
  \@addtoreset{equation}{section}%
  \@addtoreset{figure}{section}%
  \@addtoreset{table}{section}%
  \setcounter{section}\z@
  \renewcommand\thesection{\@Alph\c@section}%
  \renewcommand\theequation{\thesection\,\@arabic\c@equation}%
  \renewcommand\thefigure{\thesection\,\@arabic\c@figure}%
  \renewcommand\thetable{\thesection\,\@arabic\c@table}%
}

\setcounter{secnumdepth}{3}

\newcounter{part}
\newcounter{section}
\newcounter{subsection}[section]
\newcounter{subsubsection}[subsection]
\newcounter{paragraph}[subsubsection]
\newcounter{subparagraph}[paragraph]
\renewcommand\thepart          {\@arabic\c@part}
\renewcommand\thesection       {\@arabic\c@section}
\renewcommand\thesubsection    {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection {\thesubsection.\@arabic\c@subsubsection}
\renewcommand\theparagraph     {\thesubsubsection.\@arabic\c@paragraph}
\renewcommand\thesubparagraph  {\theparagraph.\@arabic\c@subparagraph}

\newskip\@leftskip  \@leftskip=\z@

\setlength\leftmargini   {2.5em}
\setlength\leftmarginii  {1.5em}
\setlength\leftmarginiii {1.5em}
\setlength\leftmarginiv  {1.5em}
\setlength\leftmarginv   {1em}
\setlength\leftmarginvi  {1em}
\setlength\leftmargin    {\leftmargini}
\setlength\labelsep      {5\p@}
\setlength\labelwidth    {\leftmargini}
\addtolength\labelwidth  {-\labelsep}

\newcommand\makeRLlabel[1]{\rlap{#1}\hss}
\newcommand\makeRRlabel[1]{\hss\llap{#1}}

\def\@listI{\leftmargin\leftmargini
  \parsep \z@ \topsep 6.5\p@ \@plus 3\p@ \@minus 3\p@
  \itemsep \z@ \@plus 1\p@ \@minus 1\p@
  \let\makelabel\makeRLlabel}

\def\@listii{\leftmargin\leftmarginii
  \labelwidth\leftmarginii
  \advance\labelwidth-\labelsep
  \topsep 3\p@ \@plus 1\p@ \@minus 1\p@
  \parsep \z@ \itemsep \parsep
  \let\makelabel\makeRLlabel}

\def\@listiii{\leftmargin\leftmarginiii
  \labelwidth\leftmarginiii
  \advance\labelwidth-\labelsep
  \topsep 3\p@ \@plus 1\p@ \@minus 1\p@
  \parsep \z@ \partopsep \z@
  \itemsep \topsep
  \let\makelabel\makeRLlabel}

\def\@listiv{\leftmargin\leftmarginiv
  \labelwidth\leftmarginiv
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\def\@listv{\leftmargin\leftmarginv
  \labelwidth\leftmarginv
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\def\@listvi{\leftmargin\leftmarginvi
  \labelwidth\leftmarginvi
  \advance\labelwidth-\labelsep
  \let\makelabel\makeRLlabel}

\let\@listi\@listI
\@listi

\def\itemize{% FROM LATEX.LTX
  \ifnum \@itemdepth >\thr@@ \@toodeep\else
     \advance\@itemdepth \@ne
     \edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
     \expandafter
     \list
       \csname\@itemitem\endcsname
       {\let\makelabel\makeRRlabel}%
  \fi
}

\newcommand\labelitemi{$\m@th\bullet$}
\newcommand\labelitemii{\normalfont\bfseries ---}
\newcommand\labelitemiii{\normalfont\bfseries --}
\newcommand\labelitemiv{$\m@th\cdot$}

\def\enumerate{% FROM LATEX.LTX
  \ifnum \@enumdepth >\thr@@ \@toodeep\else
    \advance\@enumdepth \@ne
    \edef\@enumctr{enum\romannumeral\the\@enumdepth}%
  \fi
  \@ifnextchar [{\@enumeratetwo}{\@enumerateone}%
}

\def\@enumerateone{%
  \expandafter
   \list
     \csname label\@enumctr\endcsname
     {\usecounter{\@enumctr}%
      \let\makelabel\makeRRlabel}%
}

\def\@enumeratetwo[#1]{%
  \expandafter
  \list
    \csname label\@enumctr\endcsname
    {\usecounter{\@enumctr}%
     \settowidth\labelwidth{\normalfont\rmfamily #1}%
     \leftmargin\labelwidth \advance\leftmargin\labelsep
     \let\makelabel\makeRRlabel}%
}

\newcommand\labelenumi  {{\normalfont\rmfamily\theenumi.}}
\newcommand\labelenumii {{\normalfont\rmfamily(\theenumii)}}
\newcommand\labelenumiii{{\normalfont\rmfamily\theenumiii}}
\newcommand\labelenumiv {{\normalfont\rmfamily\theenumiv}}

\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}

\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}

\newcommand*\descriptionlabel[1]{\hspace\labelsep \normalfont\bfseries #1}

\newenvironment{description}
  {\list{}{\leftmargin 1em \labelwidth\z@ \itemindent-\leftmargin
   \let\makelabel\descriptionlabel}}
  {\endlist}

\newenvironment{verse}
  {\let\\=\@centercr
   \list{}{\itemsep\z@
   \itemindent -2.5em%
   \listparindent \itemindent
   \rightmargin\leftmargin
   \advance\leftmargin 2.5em}\item[]}
  {\endlist}

\newenvironment{quotation}
  {\list{}{\listparindent\parindent
   \itemindent\listparindent
   \leftmargin\z@ \rightmargin\leftmargin
   \parsep \z@ \@plus 1\p@}\item[]%
   \normalfont\small\rmfamily}
  {\endlist}

\let\quote\quotation
\let\endquote\endquotation

\newif\ifrembrks
\newcommand\removebrackets{\rembrkstrue}

\def\@begintheorem#1#2{% FROM LATEX.LTX
  \normalfont\rmfamily
  \trivlist
    \item[\hskip \labelsep{\normalfont\itshape #1\ #2}]%
    \item[]%
}

\def\@opargbegintheorem#1#2#3{% FROM LATEX.LTX
  \normalfont\rmfamily
  \trivlist
    \item[\hskip \labelsep{\normalfont\itshape #1\ #2\ %
    \ifrembrks #3\/\global\rembrksfalse\else {\upshape(}#3\/{\upshape)}\fi}]%
    \item[]%
}

\newsavebox{\proofsavebox}

\ifprodtf
  \sbox{\proofsavebox}{$\CUPproofbox$}
  \newcommand\proofbox{\hbox{$\CUPproofbox$}}
\else
  \sbox{\proofsavebox}
    {\unitlength 1pt\begin{picture}(6.5,6.5)%
     \put(0,0){\framebox(6.5,6.5){}}\end{picture}}
  \newcommand\proofbox{\usebox{\proofsavebox}\relax}
\fi

\newcommand\mathproofbox{\rlap{\quad\proofbox}}

\def\@nprf{\normalfont\rmfamily \trivlist
  \item[\hskip \labelsep {\normalfont\itshape Proof}]%
  \item[]}

\def\@oprf[#1]{\normalfont\rmfamily \trivlist
  \item[\hskip \labelsep {\normalfont\itshape #1\ }]%
  \item[]}

\newenvironment{proof}
  {\@ifnextchar[{\@oprf}{\@nprf}}
  {\hspace*{1em}\hbox{\proofbox}\endtrivlist}

\newenvironment{proof*}
  {\proof}
  {\endtrivlist}

\renewcommand\theequation{\@arabic\c@equation}

\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{3\p@}
\setlength\arrayrulewidth{.5\p@}
\setlength\doublerulesep{1.5\p@}
\setlength\tabbingsep{\labelsep}
\setlength{\skip\@mpfootins}{\skip\footins}
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.5\p@}

\newcommand\maketitle{\@ifnextchar [{\m@ketitleone}{\m@ketitleone[n]}}

\def\m@ketitleone[#1]{\par
  \begingroup
    \newpage
    \global\@topnum\z@
    \titlefntrue
    \def\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\hbox{$\@thefnmark$}}%
    \@maketitle{#1}%
    \thispagestyle{titlepage}\@thanks
  \endgroup
  \global\let\@maketitle\relax
  \global\let\@thanks\@empty
  \global\let\@title\@empty
  \global\let\@author\@empty
  \global\let\maketitle\relax
  \global\let\thanks\relax
  \setcounter{footnote}\z@
}

\def\pe@rl#1{%
  \if t#1 {\tpe@rl}\else
    \if T#1 {\Tpe@rl}\else
      \if f#1 {\fpe@rl}\else
        \if F#1 {\Fpe@rl}\else
          \if o#1 {\otherpearl}\else
            \vspace*{32\p@}%
          \fi
        \fi
      \fi
    \fi
  \fi
}

\def\spe@rl{\vspace*{32\p@}\normalfont\LARGE\rmfamily}
\def\epe@rl#1{\par\vspace*{6.5\p@}\gdef\@shorttitle{#1}}

\def\tpe@rl{\spe@rl T\ls H\ls E\ls O\ls R\ls E\ls T\ls
  I\ls C\ls A\ls L\ns  P\ls E\ls A\ls R\ls L\ls S%
  \epe@rl{Theoretical pearls}%
}

\def\Tpe@rl{\spe@rl T\ls H\ls E\ls O\ls R\ls E\ls T\ls
  I\ls C\ls A\ls L\ns  P\ls E\ls A\ls R\ls L%
  \epe@rl{Theoretical pearl}%
}

\def\fpe@rl{\spe@rl F\ls U\ls N\ls C\ls T\ls I\ls O\ls
  N\ls A\ls L\ns  P\ls E\ls A\ls R\ls L\ls S%
  \epe@rl{Functional pearls}%
}

\def\Fpe@rl{\spe@rl F\ls U\ls N\ls C\ls T\ls I\ls O\ls
  N\ls A\ls L\ns  P\ls E\ls A\ls R\ls L%
  \epe@rl{Functional pearl}%
}

\def\otherpearl{\spe@rl
  \@ifundefined{othrpearl}
    {Please define {\normalfont\ttfamily\char92 othrpearl} to obtain\\ the correct title!}
    {\othrpearl}%
  \epe@rl{Short title--please redefine with {\normalfont\ttfamily\char92 shorttitle}}%
}

\def\b@at{\begin{author@tabular}[t]{@{}c@{}}}

\renewcommand\and{and }
\newcommand\@nd{\end{author@tabular}\vskip 6\p@\par\b@at}

\let\authorbreak\relax
\newcommand\auth@rbreak{\end{author@tabular}\\[0pt]\b@at}

\def\@maketitle#1{%
  \newpage
  \vspace*{-15\p@}%
  {\centering \sloppy
   \pe@rl{#1}%
   {\normalfont\LARGE\itshape \@title\par}%
   \vskip 16\p@
   {\normalfont\normalsize\rmfamily
    \let\authorbreak\auth@rbreak
    \let\and\@nd
    \b@at
      \@author
    \end{author@tabular}%
   \par}%
  }%
  \vskip 18\p@ \@plus 2\p@ \@minus 1\p@
}

\def\abs@header#1{%
  \vbox{\hrule \@width\hsize
  \vskip 8\p@ \@plus 3\p@ \@minus 1\p@
  \centerline{\normalfont\normalsize\bfseries #1}}%
}

\def\abs@body{%
  \list{}{\leftmargin\z@ \rightmargin\leftmargin
    \listparindent 1em \parsep \z@ \@plus 1\p@
    \topsep 6.5\p@ \@plus 3\p@ \@minus 1\p@}%
  \item[]\normalfont\small\rmfamily
}

\newcommand\abstractname{Abstract}
\newenvironment{abstract}
  {\abs@header{\abstractname}\abs@body}
  {\endlist\vbox{\hrule \@width \hsize}%
   \gdef\abs@header##1{\vskip 2\p@
     \centerline{\normalfont\normalsize\bfseries ##1}}}

\newcommand\capsulename{Capsule Review}
\newenvironment{capsule}
  {\abs@header{\capsulename}\abs@body}
  {\endabstract}

\def\author@tabular{\normalfont\authorsize\rmfamily
  \def\@halignto{}\@authortable}
\let\endauthor@tabular\endtabular

\def\author@tabcrone{{\ifnum0=`}\fi\@xtabularcr\normalfont\affilsize\itshape
  \let\\\author@tabcrtwo\ignorespaces}

\def\author@tabcrtwo{{\ifnum0=`}\fi\@xtabularcr[-3\p@]\normalfont\affilsize\itshape
  \let\\\author@tabcrtwo\ignorespaces}

\def\@authortable{\leavevmode \hbox \bgroup $\let\@acol\@tabacol
  \let\@classz\@tabclassz \let\@classiv\@tabclassiv
  \let\\\author@tabcrone \ignorespaces \@tabarray}

\mark{{}{}}
\renewcommand\author{\@ifnextchar [{\@authortwo}{\@authorone}}
\def\@authorone#1{\gdef\@author{#1}\gdef\@shortauthor{#1}}
\def\@authortwo[#1]#2{\gdef\@author{#2}\gdef\@shortauthor{#1}}
\gdef\@author{\mbox{}}

\newcommand\shortauthor[1]{\gdef\@shortauthor{#1}}
\gdef\@shortauthor{}

\renewcommand\title{\@ifnextchar [{\@titletwo}{\@titleone}}
\def\@titleone#1{\gdef\@title{#1}\gdef\@shorttitle{#1}}
\def\@titletwo[#1]#2{\gdef\@title{#2}\gdef\@shorttitle{#1}}
\gdef\@title{\mbox{}}

\newcommand\shorttitle[1]{\gdef\@shorttitle{#1}}
\gdef\@shorttitle{}

\newcommand\volume[1]{\gdef\@volume{#1}}
\gdef\@volume{{\normalfont\bfseries 1} (1):}

\newcommand\pagerange[1]{\gdef\@pagerange{#1}}
\gdef\@pagerange{1--000}

\newcommand\pubyear[1]{\gdef\@year{#1}}
\gdef\@year{19XX}

\newcommand\jdate[1]{\gdef\@jdate{#1}}
\gdef\@jdate{January \@year}

\newcommand\doi[1]{\gdef\@doi{10.1017/#1}}
\gdef\@doi{10.1017/S0000000000000000}

\newcommand\journal[1]{\gdef\@journal{#1}}
\def\@journal{%
  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\affilsize\rmfamily
    {\itshape JFP\/}\ \@volume\ \@pagerange, \@jdate.\quad \copyright\ 
      \@year\ Cambridge University Press\\[2.5\p@]
    DOI: \@doi\quad {Printed in the United Kingdom}}%
  \vss}%
}

\def\@underjournal{%
  \vbox to 5.6\p@{\noindent\parbox[t]{4.8in}{\normalfont\affilsize\rmfamily
    {\itshape Under consideration for publication in
     J.\ Functional Programming\/}\\[2.5\p@]
     \ }%
  \vss}%
}

\def\ps@headings{\let\@mkboth\markboth
  \def\@oddhead{\hfil{\itshape\@shorttitle}\hfil \llap{\thepage}}%
  \def\@evenhead{\rlap{\thepage}\hfil\itshape\@shortauthor\hfil}%
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \def\sectionmark##1{\markboth{##1}{}}%
  \def\subsectionmark##1{\markright{##1}}%
}

\def\ps@myheadings{\let\@mkboth\@gobbletwo
  \def\@oddhead{\hfil{\itshape\rightmark}\hfil \llap{\thepage}}%
  \def\@evenhead{\rlap{\thepage}\hfil\itshape\leftmark\hfil}%
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \let\sectionmark\@gobble
  \let\subsectionmark\@gobble
}

\ifprodtf
  \let\@j@urnal\@journal
\else
  \let\@j@urnal\@underjournal
\fi

\def\ps@titlepage{\leftskip\z@ \let\@mkboth\@gobbletwo
  \def\@oddhead{\@j@urnal \hfil\llap{\thepage}}%
  \let\@evenhead\@oddhead
  \let\@oddfoot\@empty
  \let\@evenfoot\@oddfoot
  \let\sectionmark\@gobble
  \let\subsectionmark\@gobble
}

\def\@pnumwidth{1.55em}
\def\@tocrmarg {2.55em}
\def\@dotsep{4.5}
\setcounter{tocdepth}{2}

\def\@dottedtocline#1#2#3#4#5{% FROM LATEX.LTX
  \ifnum #1>\c@tocdepth \else
    \vskip \z@ \@plus.2\p@
    {\leftskip #2\relax \rightskip \@tocrmarg \parfillskip -\rightskip
     \parindent #2\relax\@afterindenttrue
     \interlinepenalty\@M
     \leavevmode
     \@tempdima #3\relax
     \advance\leftskip \@tempdima \null\hskip -\leftskip
     {#4}\nobreak
     \leaders\hbox{$\m@th
        \mkern \@dotsep mu\hbox{\phantom{.}}\mkern \@dotsep
        mu$}\hfill
     \nobreak
     \hb@xt@\@pnumwidth{\hfil\normalfont \normalcolor #5}%
     \par}%
  \fi}

\newcommand\contentsname{Contents}

\newcommand\tableofcontents{%
  \section*{\contentsname}%
  \@starttoc{toc}\par
  \vspace{13\p@}%
}

\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{.5\baselineskip \@plus 1\p@}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \normalfont\rmfamily
       #1\hfil \hb@xt@\@pnumwidth{\hfil}}\par
       \nobreak
       \if@compatibility
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
      \fi
    \endgroup
    \vskip .25\baselineskip \@plus 1\p@
  \fi
}

\newcommand*\l@section[2]{%
  \ifnum \c@tocdepth >\z@
    \addpenalty\@secpenalty
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \normalfont\rmfamily
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      {\bfseries #1}\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
    \endgroup
  \fi}

\newcommand*\l@subsection{\@dottedtocline{2}{1.5em}{2.3em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{3.8em}{3.2em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{7.0em}{4.1em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{10em}{5em}}

\newcommand\listfigurename{List of Figures}
\newcommand\listoffigures{%
  \section*{\listfigurename}%
  \@starttoc{lof}\par
  \vskip 13\p@
}

\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}

\newcommand\listtablename{List of Tables}
\newcommand\listoftables{%
  \section*{\listtablename}%
  \@starttoc{lot}\par
  \vskip 13\p@
}

\let\l@table\l@figure

\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule \@width .4\columnwidth height \z@
  \kern 3\p@}

\newif\iftitlefn
\newcommand\@makefntext[1]{%
  \@setpar{\@@par\@tempdima \hsize
  \advance\@tempdima-1em
  \parshape \@ne 1em \@tempdima}\par
  \noindent \hb@xt@ \z@{\hss$\iftitlefn\else^\fi{\@thefnmark}$\ }#1}

\ifprodtf \else \let\highast\ast\fi

\def\@fnsymbol#1{\ensuremath{% FROM LATEX.LTX
  \ifcase#1\or \hbox{$\highast$}\or \dagger\or \ddagger\or
    \mathchar "278\or \mathchar "27B\or \|\or \hbox{$\highast\highast$}\or
    \dagger\dagger\or \ddagger\ddagger\or \mathchar "278\mathchar "278\or
    \mathchar "27B\mathchar "27B\or \|\|\or \else\@ctrerr\fi}%
}

\renewcommand\@makefnmark{\hbox{$^{\@thefnmark}$}}% FROM LATEX.LTX
\renewcommand\thefootnote{\@arabic\c@footnote}% FROM LATEX.LTX
\renewcommand\thempfootnote{\mathit{\@alph\c@mpfootnote}}% FROM LATEX.LTX

\setcounter{topnumber}{2}
\renewcommand\topfraction{.9}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.9}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.1}
\renewcommand\floatpagefraction{.9}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.9}
\renewcommand\dblfloatpagefraction{.5}

\newcounter{table}
\renewcommand\thetable{\@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{1}
\def\ext@table{lot}
\newcommand\tablename{Table}
\def\fnum@table{\tablename~\thetable}

\newenvironment{table}
  {\@float{table}}
  {\end@float}

\newenvironment{table*}
  {\@dblfloat{table}}
  {\end@dblfloat}

\def\fstyle@table{\normalfont\small\rmfamily}
\def\fjust@table{\centering}
\def\fcapjust@table{\centering}
\def\fcapsize@table{\normalfont\normalsize\rmfamily}
\def\fcapstyle@table{\normalfont\normalsize\itshape}

\newcommand\contname{(cont.)}
\newcommand\continuedfigure{%
  \addtocounter{figure}\m@ne
  \let\curr@thefigure\thefigure
  \def\thefigure{\curr@thefigure\ \contname}%
}

\newcommand\continuedtable{%
  \addtocounter{table}\m@ne
  \let\curr@thetable\thetable
  \def\thetable{\curr@thetable\ \contname}%
}

\newif\ifrem@fullpt
\newcommand\removefullpoint{\global\rem@fullpttrue}

\newif\ifbot@fig
\newenvironment{bottomfigure}{\def\fps@figure{b}%
  \setcounter{bottomnumber}{1}%
  \global\bot@figtrue
  \@float{figure}\fstyle@figure}
  {\end@float}

\newbox\@tempboxb

\long\def\@makecaption#1#2{%
  \ifbot@fig \rule{\textwidth}{.25\p@}\fi
  \vskip 6.5\p@ \@plus .4\p@ \@minus .4\p@
  \begingroup
  \setbox\@tempboxb\hbox{#2}%
  \def\@xtra{\ifdim\wd\@tempboxb>\z@ \ifrem@fullpt\else .\enskip\fi\fi}%
  \setbox\@tempboxa\hbox{#1\@xtra #2}%
  \ifdim\wd\@tempboxa>\tw@\textwidth
    {\let\centering\relax #1\@xtra #2\par}%
  \else
    #1\@xtra #2\par
  \fi
  \endgroup
  \global\bot@figfalse
  \global\rem@fullptfalse
}

\newcounter{figure}
\renewcommand\thefigure{\@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{2}
\def\ext@figure{lof}
\newcommand\figurename{Fig.}
\def\fnum@figure{\figurename~\thefigure}

\newenvironment{figure}
  {\@float{figure}}
  {\end@float}

\newenvironment{figure*}
  {\@dblfloat{figure}}
  {\end@dblfloat}

\def\fstyle@figure{\normalfont\small\rmfamily}
\def\fjust@figure{\centering}
\def\fcapjust@figure{\centering}
\def\fcapsize@figure{\normalfont\small\rmfamily}
\def\fcapstyle@figure{\normalfont\small\rmfamily}

\long\def\@caption#1[#2]#3{% FROM LATEX.LTX
  \par
  \addcontentsline{\csname ext@#1\endcsname}{#1}%
    {\protect\numberline{\csname the#1\endcsname}{\ignorespaces #2}}%
  \begingroup
    \@parboxrestore
    \if@minipage
      \@setminipage
    \fi
    \normalsize
    \@makecaption{\csname fcapjust@#1\endcsname
                  \csname fcapsize@#1\endcsname
                  \csname fnum@#1\endcsname}%
                 {\csname fcapstyle@#1\endcsname \ignorespaces #3}\par
  \endgroup
}

\def\@xfloat #1[#2]{% FROM LATEX.LTX
  \@nodocument
  \def\@captype {#1}%
  \def\@fps {#2}%
  \@onelevel@sanitize \@fps
  \def \reserved@b {!}%
  \ifx \reserved@b \@fps
    \@fpsadddefault
  \else
    \ifx \@fps \@empty
      \@fpsadddefault
    \fi
  \fi
  \ifhmode
    \@bsphack
    \@floatpenalty-\@Mii
  \else
    \@floatpenalty-\@Miii
  \fi
  \ifinner
    \@parmoderr\@floatpenalty\z@
  \else
    \@next\@currbox\@freelist
      {%
       \@tempcnta \sixt@@n
       \expandafter \@tfor \expandafter \reserved@a
         \expandafter :\expandafter =\@fps
         \do
          {%
           \if \reserved@a h%
             \ifodd \@tempcnta
             \else
               \advance \@tempcnta \@ne
             \fi
           \fi
           \if \reserved@a t%
             \@setfpsbit \tw@
           \fi
           \if \reserved@a b%
             \@setfpsbit 4%
           \fi
           \if \reserved@a p%
             \@setfpsbit 8%
           \fi
           \if \reserved@a !%
             \ifnum \@tempcnta>15
               \advance\@tempcnta -\sixt@@n\relax
             \fi
           \fi
           }%
       \@tempcntb \csname ftype@\@captype \endcsname
       \multiply \@tempcntb \@xxxii
       \advance \@tempcnta \@tempcntb
       \global \count\@currbox \@tempcnta
       }%
    \@fltovf
  \fi
  \global \setbox\@currbox
    \color@vbox
      \normalcolor
      \vbox \bgroup
        \hsize\columnwidth
        \@parboxrestore
        \@floatboxreset
        \csname fstyle@\@captype\endcsname
        \csname fjust@\@captype\endcsname
}

\let\oldtabular\tabular
\let\endoldtabular\endtabular

\def\tabular{% FROM LATEX.LTX
  \def\@halignto{to \textwidth}\tabskip\tabcolsep \@plus 1fil\@ttabular
}

\def\@ttabular{\leavevmode \hbox \bgroup $\let\@acol\@tabacol
   \let\@classz\@tabclassz
   \let\@classiv\@tabclassiv \let\\\@tabularcr\@ttabarray}

\def\@ttabarray{\m@th\@ifnextchar[\@tarray{\@ttarray}}

\def\@tarray[#1]#2{\t@barray[#1]{@{\tabskip\tw@\tabcolsep \@plus 3\p@}#2}}
\def\@ttarray#1{\t@barray[c]{@{\tabskip\tw@\tabcolsep \@plus 3\p@}#1}}

\def\t@barray[#1]#2{%
  \if #1t\vtop \else \if#1b\vbox \else \vcenter \fi\fi
  \bgroup
  \setbox\@arstrutbox\hbox{%
    \vrule \@height\arraystretch\ht\strutbox
           \@depth\arraystretch \dp\strutbox
           \@width\z@}%
  \@mkpream{#2}%
  \edef\@preamble{%
    \halign \noexpand\@halignto
      \bgroup \tabskip\z@skip \@arstrut \@preamble \tabskip\tabcolsep \@plus 1fil\cr}%
  \let\@startpbox\@@startpbox \let\@endpbox\@@endpbox
  \let\tabularnewline\\%
  \let\par\@empty
  \let\@sharp##%
  \set@typeset@protect
  \lineskip\z@skip\baselineskip\z@skip
  \@preamble}

\newcommand\ls{\kern.15em\relax}
\newcommand\ns{\kern.55em\relax}

\def\hline{% FROM LATEX.LTX
  \noalign{\ifnum0=`}\fi \vskip 6\p@
  \hrule \@height \arrayrulewidth \vskip 6\p@
  \futurelet \reserved@a\@xhline}

\def\@xhline{% FROM LATEX.LTX
  \ifx\reserved@a\hline
    \vskip -12\p@
    \vskip\doublerulesep
  \fi
  \ifnum0=`{\fi}}

\newcommand\today{}
\edef\today{\number\day\ \ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December
 \fi \ \number\year}

\renewcommand\@biblabel[1]{}% FROM LATEX.LTX
\newcommand\newblock{\hskip .11em \@plus .33em \@minus .07em}

\newcommand\refname{References}
\newenvironment{thebibliography}[1]
  {\section*{\refname}%
   \normalfont\small\rmfamily
   \addcontentsline{toc}{section}{\refname}%
   \list{}{\labelwidth\z@ \leftmargin 1em \itemindent -1em}%
   \parindent\z@
   \parskip 2\p@ \@plus .1\p@
   \sloppy\clubpenalty\z@ \widowpenalty\@M
   \sfcode`\.\@m\relax}
  {\def\@noitemerr
   {\@latex@warning{Empty `thebibliography' environment}}%
   \endlist}

\def\@citex[#1]#2{% FROM LATEX.LTX
  \let\@citea\@empty
  \@cite{\@for\@citeb:=#2\do
    {\@citea\def\@citea{; }%
     \edef\@citeb{\expandafter\@firstofone\@citeb}%
     \if@filesw\immediate\write\@auxout{\string\citation{\@citeb}}\fi
     \@ifundefined{b@\@citeb}{\mbox{\reset@font\bfseries ?}%
       \G@refundefinedtrue
       \@latex@warning
         {Citation `\@citeb' on page \thepage \space undefined}}%
       {\csname b@\@citeb\endcsname}}}{#1}}

\def\@cite#1#2{{\if@tempswa #2\else (#1)\fi}}% FROM LATEX.LTX

\let\@internalcite\cite
\def\cite{\def\citename##1{##1}\@internalcite}
\DeclareRobustCommand\shortcite{\def\citename##1{}\@internalcite}

\newif\iffontfound
\newcommand\checkfont[1]{%
  \batchmode
  \font\test=#1\relax
  \errorstopmode
  \fontfoundfalse
  \ifx\test\nullfont \else \fontfoundtrue\fi
}

\newcommand\email[1]{{\normalfont\rmfamily
  \itshape\textup{(}e-mail: \textup{\texttt{#1})}}}

\edef\r@{\ifprodtf mtr\else cmr\fi}

\let\real@font@warning\@font@warning
\DeclareMathVersion{program}
\let\@font@warning\@gobble
\SetSymbolFont{letters}{program}{OT1}{\r@}{m}{sl}
\let\@font@warning\real@font@warning
\SetMathAlphabet{\mathnormal}{program}{OT1}{\r@}{m}{sl}

\newcommand{\programmath}{\mathversion{program}}
\newcommand{\unprogrammath}{\mathversion{normal}}
\newcommand{\figrule}{\begin{center}\hrule\end{center}}

\DeclareRobustCommand\dplus{\mathbin{+\!\!+}}
\DeclareRobustCommand\dequals{\mathbin{==}}
\DeclareRobustCommand\dcolon{\mathbin{::}}
\DeclareRobustCommand\dcolonequals{\mathbin{::=}}

\pagestyle{headings}
\pagenumbering{arabic}
\frenchspacing
\flushbottom

\endinput

% end of file jfp.cls
