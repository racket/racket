%%% quiz.cls

\ProvidesClass{quiz}

\def\ProvidesClass#1[#2]{\relax}

\input article.cls

\newif\iflatex\latextrue
\newif\ifhtml\htmlfalse

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
\newenvironment{quiz}[1]
  {\refstepcounter{quiz}
   \ifnum\thequiz>1\pagebreak\fi
   \hbox to \hsize{\quizname \hfill
     \hbox to 3in{\vtop{\noindent Name:\\Lab section: #1\\Lecture section:}}}
     \bigskip}
  {}

\newcounter{problem}[quiz]
\renewcommand{\theproblem}{\arabic{problem}}
\newenvironment{problem}[1][\relax]
  {\par\bigskip\refstepcounter{problem}
   \goodbreak
   \par\nobreak
   \textbf{Problem \theproblem\ifx#1\relax\else~[#1 minutes].\fi~\ignorespaces}}
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
