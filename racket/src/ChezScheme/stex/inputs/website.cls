%%% website.cls
%%% Kent Dybvig and Oscar Waddell
%%% August 2003

\ProvidesClass{website}
\def\ProvidesClass#1[#2]{\relax}
\LoadClass{article}

\def\ps@plain{\let\@mkboth\@gobbletwo
     \let\@oddhead\@empty\def\@oddfoot{\reset@font
     \footer}\let\@evenhead\@empty\let\@evenfoot\@oddfoot}
\def\footer{\hfil\ifnum\c@page =1 \copyrightnotice\else\thepage\fi\hfil}

\newcommand{\webpage}[2][]{
\def\thetitle{#2}
}

\newcommand{\documenttitle}[2][]{
\def\thetitle{#2}
}

\def\maketitle{
\begin{flushleft}
\LARGE\textbf{\thetitle}
\end{flushleft}}

\newcommand{\hr}[1][]{\bigskip\hrule}

\newcommand{\copyrightnotice}{}

%%% to support hypertext index entries
\def\hindex#1{\index} % ignore the label here---no links in printed version

\newcommand{\hyperlink}[3][]{#3}
\newcommand{\href}[3][]{#3}
\newcommand{\hpageref}[3][]{#3}
\let\true@ref=\ref\renewcommand{\ref}[2][]{\true@ref{#2}}
\let\true@pageref=\pageref\renewcommand{\pageref}[2][]{\true@pageref{#2}}

%%% \mailto{id}{name}
\newcommand{\mailto}[3][]{#3}

%%% forms
\newenvironment{form}[1]{}{}
\newcommand{\formtextarea}[1]{\strut}
\newcommand{\forminput}[1]{\strut}
\newcommand{\formselect}[2]{\strut}
\newcommand{\formoption}[2][]{\strut}

\newif\iflatex\latextrue
\newif\ifhtml\htmlfalse

\newenvironment{DIV}[1]
  {\relax}
  {\relax}

\def\span#1#2{#2}
\newcommand{\img}[3][]{#2}

%%% \parheader{title}
\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

\newcounter{alphacount}
\def\alphalabel{\textit{\alph{alphacount}}.}
\newenvironment{alphalist}
  {\begingroup\let\beforeschemedisplay=\relax\let\afterschemedisplay=\relax
   \begin{list}{\alphalabel}{\usecounter{alphacount}\itemsep=0pt\parsep=0pt%
        \topsep=0pt}}
  {\end{list}\endgroup}

%%% final set up
\renewcommand{\contentsname}{Contents}
\newcommand{\bibname}{Bibliography}
\renewcommand{\indexname}{Index}
\renewcommand{\today}{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{headings}
\pagenumbering{arabic}
\parindent=0pt
\parskip=4pt

\if@twoside\else\raggedbottom\fi
\endinput
%%% end of website.cls
