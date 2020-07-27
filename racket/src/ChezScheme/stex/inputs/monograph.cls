%%% monograph.cls

\ProvidesClass{monograph}

\def\ProvidesClass#1[#2]{\relax}

\input article.cls

\def\copyrightnotice{\ifauthor\copyright~\number\year~\theauthor\fi}
\def\ps@plain{\let\@mkboth\@gobbletwo
     \let\@oddhead\@empty\def\@oddfoot{\reset@font
     \footer}\let\@evenhead\@empty\let\@evenfoot\@oddfoot}
\def\footer{\hfil\ifnum\c@page =1 \copyrightnotice\else\thepage\fi\hfil}

\newif\iftitle\titlefalse
\newif\ifauthor\authorfalse
\newif\ifdate\datefalse
\def\title#1{\def\thetitle{#1}\titletrue}
\def\author#1{\def\theauthor{#1}\authortrue}
\def\date#1{\def\thedate{#1}\datetrue}
\def\maketitle{
\begin{flushleft}
\iftitle{\LARGE\textbf{\thetitle} \\[6pt]}\fi
\ifauthor{\Large\textbf{\theauthor} \\[6pt]}\fi
\ifdate{\large\textbf{\thedate} \\}\fi
\end{flushleft}}

%%% to support hypertext index entries
\def\hindex#1{\index} % ignore the label here---no links in printed version

\newcommand{\hyperlink}[3][]{#3}
\newcommand{\href}[3][]{#3}
\newcommand{\hpageref}[3][]{#3}

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
\renewcommand*{\l@section}[2] {\vspace{4pt}\hbox to \hsize{\bf #1\hfil#2}}
\renewcommand*{\l@subsection}[2] {\vspace{2pt}\hbox to \hsize{\rm #1\hfill#2}}
\def\numberline#1{#1}
\renewcommand{\tableofcontents}{%
  \thispagestyle{plain}%
  \global\@topnum\z@
  \section*{\contentsname}%
  \@starttoc{toc}}

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

%%% bibliography
\newdimen\bibindent \bibindent=1.5em
\renewcommand{\newblock}{}
\renewenvironment{thebibliography}[1]
     {\section{\bibname}
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

\newcommand{\parheader}[2][.~\ignorespaces]{\medskip\noindent{\bf #2#1}}

\newcommand{\sectionstar}[1]{\section*{#1}}
\newcommand{\subsectionstar}[1]{\subsection*{#1}}
\newcommand{\subsubsectionstar}[1]{\subsubsection*{#1}}

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
%%% end of monograph.cls
