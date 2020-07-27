%%% assignment.cls

\ProvidesClass{assignment}

\def\ProvidesClass#1[#2]{\relax}

% \input article.cls
\LoadClass{article}

\def\copyrightnotice{\ifauthor\copyright~\number\year~\theauthor\fi}
\def\ps@plain{\let\@mkboth\@gobbletwo
     \let\@oddhead\@empty\def\@oddfoot{\reset@font
     \footer}\let\@evenhead\@empty\let\@evenfoot\@oddfoot}
\def\footer{\hfil\ifnum\c@page =1 \copyrightnotice\else\thepage\fi\hfil}
\let\trueenddocument=\enddocument
\def\enddocument{
 \theselfstudy
 \trueenddocument}

\newif\iftitle\titlefalse
\newif\ifauthor\authorfalse
\newif\ifdate\datefalse
\def\title#1{\def\thetitle{#1}\titletrue}
\def\author#1{\def\theauthor{#1}\authortrue}
\def\date#1{\def\thedate{#1}\datetrue}
\def\maketitle{
\begin{flushleft}
\iftitle{\LARGE\textbf{\thetitle}} \\[6pt]\fi
\ifauthor{\Large\textbf{\theauthor}} \\\fi
\ifdate{\vskip 3pt\Large\textbf{\thedate}} \\\fi
\end{flushleft}}

%%% to support hypertext index entries
\def\hindex#1{\index} % ignore the label here---no links in printed version

\newcommand{\hyperlink}[3][]{#3}
\newcommand{\href}[3][]{#3}
\newcommand{\hpageref}[3][]{#3}
\let\true@ref=\ref\renewcommand{\ref}[2][]{\true@ref{#2}}
\let\true@pageref=\pageref\renewcommand{\pageref}[2][]{\true@pageref{#2}}

\newif\iflatex\latextrue
\newif\ifhtml\htmlfalse

%%% table of contents
\renewcommand{\@pnumwidth}{1.55em}
\renewcommand{\@tocrmarg} {2.55em}
\renewcommand{\@dotsep}{4.5}
\setcounter{tocdepth}{2}
\newcommand*{\l@chapter}[2]{%
  \ifnum \c@tocdepth >\m@ne
    \addpenalty{-\@highpenalty}%
    \vskip 1.0em \@plus\p@
    \setlength\@tempdima{1.5em}%
    \begingroup
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      \leavevmode \bfseries
      \advance\leftskip\@tempdima
      \hskip -\leftskip
      #1\nobreak\hfil \nobreak\hbox to\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\renewcommand*{\l@section}[2]{\expandafter\l@xection#1\hobbes{#2}}
\renewcommand*{\l@subsection}[2]{\expandafter\l@subxection#1\hobbes{#2}}
\renewcommand*{\l@subsubsection}[2]{\expandafter\l@subsubxection#1\hobbes{#2}}
\def\lead{\leaders\hbox to .3em{{\footnotesize \hss.\hss}}\hfil}
\def\l@xection#1\calvin#2\hobbes#3{%
\strut#1.&\multispan{3}{#2~}\lead&~#3\cr
&\multispan{3}{\hphantom{#2~\hspace{2em}}}\hfil&\cr
}
\def\l@subxection#1\calvin#2\hobbes#3{%
\strut&#1.&\multispan{2}{#2~}\lead&~#3\cr
&&\multispan{2}{\hphantom{#2~\hspace{2em}}}\hfil&\cr
}
\def\l@subsubxection#1\calvin#2\hobbes#3{%
\strut&&#1.&\multispan{1}{#2~}\lead&~#3\cr
&&&\multispan{1}{\hphantom{#2~\hspace{2em}}}\hfil&\cr
}
\def\inserttoc{
\bigskip{\bfseries Contents}\medskip
\begingroup\offinterlineskip
\halign{\hfil##\hspace{.5em}&##\hfil\hspace{.5em}&##\hfil\hspace{.5em}&##\hfil\hspace{.5em}&\hfil##\cr
\input{\jobname.toc}
}\endgroup}
\renewcommand{\tableofcontents}{%
\def\numberline ##1{##1\calvin}
\IfFileExists{\jobname.toc}{\inserttoc}{\typeout{No file \jobname.toc}}
\begingroup
\def\contentsline##1##2##3{\relax}
\@starttoc{toc}
\endgroup
}

\newcounter{exercise}
\renewcommand{\theexercise}{\arabic{exercise}}
\def\@startexercise{\par\bigskip
  \refstepcounter{exercise}\goodbreak
  \par\hrule width \hsize{\hfil}\\\nobreak
  \textbf{Exercise \theexercise. }\ignorespaces}
\newenvironment{exercise}
  {\begingroup
   \def\afterschemedisplay{\removelastskip\penalty10000\vskip5pt}
   \let\newexercise=\@startexercise\newexercise}
  {\par\removelastskip\penalty10000\kern6pt\hrule width \hsize{\hfil}\endgroup}
\newcounter{subexercise}[exercise]
\renewcommand{\thesubexercise}{\theexercise\alph{subexercise}}
\newcommand{\subexercise}{\par\refstepcounter{subexercise}\medskip\textbf{\thesubexercise. }}

%%% self study questions
%%% we'd like to create the self study file and load it all on the same
%%% pass, but this is not TeXnically feasible if we want to include page
%%% numbers.  TeX delays file writing and closing until its output
%%% routines run so that it can get the page numbers correct, so the file
%%% may not be complete by the time we input it.  If we use \immediate
%%% to get the writing and closing done earlier, the page numbers won't
%%% be correct.  so we create \jobname.rawsst and count on the make file to
%%% copy it to \jobname.sst before the next run.  We will need to do something
%%% even more elaborate once we start generating a book with self-study
%%% questions in each chapter.
\newwrite\sstudy
\newcounter{selfstudycounter}
\renewcommand{\theselfstudycounter}{\arabic{selfstudycounter}}
\openout\sstudy=\jobname.rawsst
\newif\ifselfstudy\selfstudyfalse
\def\selfstudy{\stepcounter{selfstudycounter}\selfstudytrue
  \begingroup\@sanitize\addselfstudy}
\def\addselfstudy#1#2{\endgroup
  \edef\sstudyhead{\write\sstudy}%
  \edef\sstudyentry{{\string\sstudyentry{#1}{#2}{\theselfstudycounter}}}%
  \expandafter\sstudyhead\sstudyentry}
\def\theselfstudy{\ifselfstudy\realselfstudy\fi}
\def\realselfstudy{\begingroup
  \def\sstudyentry##1##2##3{\par\bigskip\textbf{##3. } ##1 [Page \pageref{##2}]}
  \section{Questions for self study}\label{section:selfstudy}
  \input \jobname.sst
  \endgroup}

%%% bibliography
\newdimen\bibindent \bibindent=1.5em
\renewcommand{\newblock}{}
\renewenvironment{thebibliography}[1]
     {\chapter{Bibliography}
      \list{\@biblabel{\arabic{enumiv}}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand{\theenumiv}{\arabic{enumiv}}}%
      \renewcommand{\newblock}{\hskip .11em \@plus.33em \@minus.07em}%
      \sloppy\clubpenalty4000\widowpenalty4000%
      \sfcode`\.=\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

%%% index
\def\notsupposedtobehereindexname{\indexname}
\renewenvironment{theindex}
               {\if@openright\cleardoublepage\else\clearpage\fi
                \raggedright
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\plainchapterhead{\indexname}]%
                \addcontentsline{toc}{chapter}{\indexname}%
                \chaptermark{\indexname}{\notsupposedtobehereindexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\clearpage}
\renewcommand{\@idxitem}  {\par\hangindent 40\p@}
\renewcommand{\subitem}   {\par\hangindent 40\p@ \hspace*{20\p@}}
\renewcommand{\subsubitem}{\par\hangindent 40\p@ \hspace*{30\p@}}
\renewcommand{\indexspace}{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}

%%% latex.ltx redefinitions
%% leave padding page blank (no header)
\def\cleardoublepage{\clearpage\if@twoside \ifodd\c@page\else
    {\pagestyle{empty}\hbox{}\newpage\if@twocolumn\hbox{}\newpage\fi}\fi\fi}
%% change section headers to "1.1. foo" instead of "1.1  foo"
\def\@seccntformat#1{\csname the#1\endcsname. }
%% change numbered table of contents lines to "1.1. foo" instead of "1.1  foo"
\def\numberline#1{#1. }

\renewenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\renewcommand*\descriptionlabel[1]{\hspace\labelsep #1}

\def\itemvdots{\item[] \mbox{\vdots}}

\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

%%% grammar support
\newenvironment{grammar}
  {\begingroup
    \def\orbar{&\bar&}
    \def\nobar{&&}
    \def\longis{&$\longrightarrow$&}
   \penalty-100\vskip 4pt\begin{tabular}{lcl}}
  {\end{tabular}\penalty-200\vskip 4pt\endgroup}
\def\bar{$\vert$}
\def\ang#1{$\langle${\small\rm{}#1}$\rangle$}
\def\kstar{\raise.5ex\hbox{\scheme{*}}}
\def\kplus{\raise.5ex\hbox{\scheme{+}}}

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

\if@twoside\else\raggedbottom\fi
\endinput
%%% end of assignment.cls
