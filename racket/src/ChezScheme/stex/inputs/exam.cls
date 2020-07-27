%%% exam.cls

\ProvidesClass{exam}

\def\ProvidesClass#1[#2]{\relax}

\input article.cls

\newif\iftitle\titlefalse
\newif\ifauthor\authorfalse
\def\title#1{\def\thetitle{#1}\titletrue}
\def\author#1{\def\theauthor{#1}\authortrue}
\def\maketitle{
\begin{flushleft}
\iftitle{\LARGE\textbf{\thetitle}} \\[6pt]\fi
\ifauthor{\Large\textbf{\theauthor}} \\\fi
\end{flushleft}}

\newcounter{quiz}
\renewcommand{\thequiz}{\arabic{quiz}}
\newenvironment{quiz}
  {\refstepcounter{quiz}
   \ifnum\thequiz>1\pagebreak\fi
   \centerline{\baselineskip=18pt
     \hbox to 3in{\vtop{\noindent \quizname\\\quizdate}}
     \hfill
     \hbox to 3in{\vtop{\noindent Name:\\Network ID:\\Lab section:\\Lecture section:}}}
     \bigskip}
  {}

\newcounter{problem}[quiz]
\renewcommand{\theproblem}{\arabic{problem}}
\newenvironment{problem}
  {\par\bigskip\refstepcounter{problem}
   \goodbreak
   \par\nobreak
   \textbf{Problem \theproblem. }}
  {\removelastskip\bigskip\vfill}
\newcounter{subproblem}[problem]
\renewcommand{\thesubproblem}{\alph{subproblem}}
\newcommand{\subproblem}{\par\refstepcounter{subproblem}\medskip\textbf{\theproblem\thesubproblem. }}

%%% latex.ltx redefinitions
%% change section headers to "1.1. foo" instead of "1.1  foo"
\def\@seccntformat#1{\csname the#1\endcsname. }

\renewenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\renewcommand*\descriptionlabel[1]{\hspace\labelsep #1}

\def\itemvdots{\item[] \mbox{\vdots}}

\def\parheader#1 {\medskip\noindent{\bf #1.}~~}

\newenvironment{grammar}
  {\begingroup
    \def\orbar{\hbox to 2em{\hfil}$\vert$\hbox to .75em{\hfil}}
    \def\longis{ $\longrightarrow$\hbox to .75em{\hfil}}
   \penalty-100\vskip 6pt plus 1pt\parindent=0pt\interlinepenalty=5000}
  {\penalty-200\vskip6pt plus 1pt\endgroup}

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
