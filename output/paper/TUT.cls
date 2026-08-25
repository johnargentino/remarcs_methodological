%% =================================================================
%%
%% This is file `TUT.cls', Tulane University Thesis document class
%%               by Dmitri Alexeev, alexeev@member.ams.org
%%                         Version 1.2
%%
%% Modernized and patched in 2026 by Sam Punshon-Smith to natively 
%% inject Tulane SSE guidelines, electronic signatures, and modern 
%% CTAN package compatibility, while preserving the original architecture.
%%
%% This document class is based on the standard book document class
%% and a few more packages, which have been incorporated into this
%% single file. You do not need editing this file.
%%
%% =================================================================
%%
%% MODIFICATION ADVICE:
%% 
%% If you want to customize this file, it is best to make a copy of
%% the source file(s) from which it was produced.  Use a different
%% name for your copy(ies) and modify the copy(ies); this will ensure
%% that your modifications do not get overwritten when you install a
%% new release of the standard system.  You should also ensure that
%% your modified source file does not generate any modified file with
%% the same name as a standard file.
%% 
%% You can then easily distribute your modifications by distributing
%% the modified and renamed copy of the source file; 
%% this will ensure that other
%% users can safely use your modifications.
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
\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{TUT}
              [1999/03/31 v1.2
 Tulane University Thesis document class]
\newcommand\@ptsize{}
\newif\if@restonecol
\newif\if@titlepage
\@titlepagetrue
\newif\if@openright
\newif\if@mainmatter \@mainmattertrue
\if@compatibility\else
\DeclareOption{a4paper}
   {\setlength\paperheight {297mm}%
    \setlength\paperwidth  {210mm}}
\DeclareOption{a5paper}
   {\setlength\paperheight {210mm}%
    \setlength\paperwidth  {148mm}}
\DeclareOption{b5paper}
   {\setlength\paperheight {250mm}%
    \setlength\paperwidth  {176mm}}
\DeclareOption{letterpaper}
   {\setlength\paperheight {11in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{legalpaper}
   {\setlength\paperheight {14in}%
    \setlength\paperwidth  {8.5in}}
\DeclareOption{executivepaper}
   {\setlength\paperheight {10.5in}%
    \setlength\paperwidth  {7.25in}}
\DeclareOption{landscape}
   {\setlength\@tempdima   {\paperheight}%
    \setlength\paperheight {\paperwidth}%
    \setlength\paperwidth  {\@tempdima}}
\fi
\if@compatibility
  \renewcommand\@ptsize{0}
\else
\DeclareOption{10pt}{\renewcommand\@ptsize{0}}
\fi
\DeclareOption{11pt}{\renewcommand\@ptsize{1}}
\DeclareOption{12pt}{\renewcommand\@ptsize{2}}
\if@compatibility\else
\@twosidefalse \@mparswitchfalse
\fi
\DeclareOption{twoside}{\@twosidetrue  \@mparswitchtrue}
\DeclareOption{draft}{\setlength\overfullrule{5pt}}
\if@compatibility\else
\DeclareOption{final}{\setlength\overfullrule{0pt}}
\fi
\DeclareOption{titlepage}{\@titlepagetrue}
\if@compatibility\else
\DeclareOption{notitlepage}{\@titlepagefalse}
\fi
\@openrighttrue
\if@compatibility\else
\DeclareOption{onecolumn}{\@twocolumnfalse}
\fi
\DeclareOption{twocolumn}{\@twocolumntrue}
\DeclareOption{leqno}{\input{leqno.clo}}
\DeclareOption{fleqn}{\input{fleqn.clo}}
\DeclareOption{openbib}{%
  \AtEndOfPackage{%
   \renewcommand\@openbib@code{%
      \advance\leftmargin\bibindent
      \itemindent -\bibindent
      \listparindent \itemindent
      \parsep \z@
      }%
   \renewcommand\newblock{\par}}%
}
\ExecuteOptions{letterpaper,12pt,onecolumn,final,openbib}
\ProcessOptions
\input{bk1\@ptsize.clo}
% Margins are set to satisfy the requirements. You don't need to 
% change them unless your printer gives too much of an offset.
\voffset=0in
\hoffset=0in
\topmargin=-0.25in
\textwidth=6in
\textheight=8.75in
\oddsidemargin=0.5in
\evensidemargin=0.5in
% Title of your thesis or dissertation
\def\title#1{\gdef\@title{#1}}
% Is it thesis or dissertation?
\def\thesisform#1{\gdef\@thesisform{#1}}
% Name of the degree (e.g. Doctor of Philosophy)
\def\degree#1{\gdef\@degree{#1}}
% The year the degree will be officially conferred
\def\degreeyear#1{\gdef\@degreeyear{#1}}
% Copyright year
\def\copyrightyear#1{\gdef\@copyrightyear{#1}}
\copyrightyear{}
% The date degree will be officially conferred 
\def\degreedate#1{\gdef\@degreedate{#1}}
% Name of the department
\def\department#1{\gdef\@department{#1}}
% The full name (unabbreviated) of the school (School of Liberal Arts)
\def\school#1{\gdef\@school{#1}}
% The full (unabbreviated) name of the degree
\def\degree#1{\gdef\@degree{#1}}
% The name of your committee's chair
\def\chair#1{\gdef\@chair{#1}}
% The names of your other committee members (up to six)
\def\membera#1{\gdef\@membera{#1}}
\def\memberb#1{\gdef\@memberb{#1}}
\def\memberc#1{\gdef\@memberc{#1}}
\def\memberd#1{\gdef\@memberd{#1}}
\def\membere#1{\gdef\@membere{#1}}
\def\memberf#1{\gdef\@memberf{#1}}
\membera{}
\memberb{}
\memberc{}
\memberd{}
\membere{}
\memberf{}
% This is how much vertical space is allowed for signatures
\newcommand{\approvalspace}{\vfill}
% This is how much horizontal space is allowed for names and signatures
\newlength{\approvallength}
\setlength\approvallength{2.5in}
% The resf of definitions follows
\setlength\lineskip{1\p@}
\setlength\normallineskip{1\p@}
\setlength{\skip\footins}{3.5ex plus 4pt minus 4pt}
\setlength\footnotesep{3.5ex}
\setlength\parindent{.5in}
\renewcommand\baselinestretch{}
\setlength\parskip{0\p@ \@plus \p@}
\@lowpenalty   51
\@medpenalty  151
\@highpenalty 301
\setcounter{topnumber}{2}
\renewcommand\topfraction{.7}
\setcounter{bottomnumber}{1}
\renewcommand\bottomfraction{.3}
\setcounter{totalnumber}{3}
\renewcommand\textfraction{.2}
\renewcommand\floatpagefraction{.5}
\setcounter{dbltopnumber}{2}
\renewcommand\dbltopfraction{.7}
\renewcommand\dblfloatpagefraction{.5}
  \def\ps@headings{%
    \let\@oddfoot\@empty
    \def\@oddhead{\hfil\thepage}%
    \let\@mkboth\markboth
    \def\chaptermark##1{%
      \markright {}}
    \def\sectionmark##1{%
      \markright {}}}
\def\ps@myheadings{%
    \let\@oddfoot\@empty\let\@evenfoot\@empty
    \def\@evenhead{\thepage\hfil\slshape\leftmark}%
    \def\@oddhead{{\slshape\rightmark}\hfil\thepage}%
    \let\@mkboth\@gobbletwo \let\chaptermark\@gobble
    \let\sectionmark\@gobble } 
\newenvironment{abstract}{%
  \pagestyle{empty}
  \begin{doublespace}
    {\sc
      \begin{center}
        \@title\\
        \vspace{.5in}
        An Abstract\\
        Submitted On \@degreedate, \@degreeyear\\
        To The \@department\\
        In Partial Fulfillment Of The Requirements\\
        Of The \@school\\
        Of Tulane University\\
        For The Degree Of\\
        \@degree\\
        By\\
        \vspace{.3in} \singlespace
        \parbox[c]{\approvallength}{\hrulefill \\ 
          \makebox[\approvallength]{\@author}}
    \end{center}
    
    % NOTE: As of May 2022, Tulane SSE accepts electronic signatures for the committee members below.
    
    \singlespace \hfill Approved: \makebox[\approvallength]{\ }
    
    \hfill \parbox{\approvallength}{ \hrulefill \\ \@chair\\ Chairman}
    \approvalspace
    
    \ifthenelse{\equal{\@membera}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@membera}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberb}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberb}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberc}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberc}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberd}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberd}
      \approvalspace}
    
    \ifthenelse{\equal{\@membere}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@membere}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberf}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberf}
      \approvalspace} }
  \end{doublespace}
  \newpage
  \chapter*{Abstract}}
  {\clearpage} 
\def\maketitle{ \newpage
  \thispagestyle{empty}
  \begin{doublespace}
    {\sc
      \begin{center}
        \@title\\
        \vspace{.5in}
        A \@thesisform\\
        Submitted On \@degreedate, \@degreeyear\\
        To The \@department\\
        In Partial Fulfillment Of The Requirements\\
        Of The \@school\\
        Of Tulane University\\
        For The Degree Of\\
        \@degree\\
        By\\
        \vspace{.3in} \singlespace
        \parbox[c]{\approvallength}{\hrulefill \\ 
          \makebox[\approvallength]{\@author}}
    \end{center}
    
    % NOTE: As of May 2022, Tulane SSE accepts electronic signatures for the committee members below.
    
    \singlespace \hfill Approved: \makebox[\approvallength]{\ }
    
    \hfill \parbox{\approvallength}{ \hrulefill \\ \@chair\\ Chairman}
    \approvalspace
    
    \ifthenelse{\equal{\@membera}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@membera}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberb}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberb}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberc}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberc}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberd}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberd}
      \approvalspace}
    
    \ifthenelse{\equal{\@membere}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@membere}
      \approvalspace}
    
    \ifthenelse{\equal{\@memberf}{}}{}{ \hfill
      \parbox{\approvallength}{ \hrulefill \\ \@memberf}
      \approvalspace} }
  \end{doublespace}
  \newpage \thispagestyle{empty} \ \vspace{.4\textheight}
  \begin{center}
    \ifthenelse{\equal{\@copyrightyear}{}}{}{\copyright\ Copyright by
    \@author, \@copyrightyear\\{\sl All Rights Reserved}}
  \end{center}
  } 
\newcommand*\chaptermark[1]{} \setcounter{secnumdepth}{2}
\newcounter {part} \newcounter {chapter} \newcounter
{section}[chapter] \newcounter {subsection}[section] \newcounter
{subsubsection}[subsection] \newcounter {paragraph}[subsubsection]
\newcounter {subparagraph}[paragraph] \renewcommand \thepart
{\@Roman\c@part} \renewcommand \thechapter {\@arabic\c@chapter}
\renewcommand \thesection {\thechapter.\@arabic\c@section}
\renewcommand\thesubsection {\thesection.\@arabic\c@subsection}
\renewcommand\thesubsubsection{\thesubsection
  .\@arabic\c@subsubsection} \renewcommand\theparagraph
{\thesubsubsection.\@arabic\c@paragraph} \renewcommand\thesubparagraph
{\theparagraph.\@arabic\c@subparagraph}
\newcommand\@chapapp{\chaptername}
\newcommand\frontmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmatterfalse
  \pagenumbering{roman}}
\newcommand\mainmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmattertrue
  \pagenumbering{arabic}}
\newcommand\backmatter{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
  \@mainmatterfalse}
\newcommand\part{%
  \if@openright
    \cleardoublepage
  \else
    \clearpage
  \fi
%  \thispagestyle{plain}%
  \if@twocolumn
    \onecolumn
    \@tempswatrue
  \else
    \@tempswafalse
  \fi
  \null\vfil
  \secdef\@part\@spart}
\def\@part[#1]#2{%
    \ifnum \c@secnumdepth >-2\relax
      \refstepcounter{part}%
      \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
    \else
      \addcontentsline{toc}{part}{#1}%
    \fi
    \markboth{}{}%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \ifnum \c@secnumdepth >-2\relax
       \huge\bfseries \partname~\thepart
       \par
       \vskip 20\p@
     \fi
     \Huge \bfseries #2\par}%
    \@endpart}
\def\@spart#1{%
    {\centering
     \interlinepenalty \@M
     \normalfont
     \Huge \bfseries #1\par}%
    \@endpart}
\def\@endpart{\vfil\newpage
              \if@twoside
                \null
                \thispagestyle{empty}%
                \newpage
              \fi
              \if@tempswa
                \twocolumn
              \fi}
\newcommand\chapter{\if@openright\cleardoublepage\else\clearpage\fi
                    %\thispagestyle{headings}%
                    \global\@topnum\z@
                    \@afterindenttrue
                    \secdef\@chapter\@schapter}
\def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                       \if@mainmatter
                         \refstepcounter{chapter}%
                         \typeout{\@chapapp\space\thechapter.}%
                         \addcontentsline{toc}{chapter}%
                                   {\protect\numberline{\thechapter}#1}%
                       \else
                         \addcontentsline{toc}{chapter}{#1}%
                       \fi
                    \else
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterhead{#2}]%
                    \else
                      \@makechapterhead{#2}%
                      \@afterheading
                    \fi}
\def\@makechapterhead#1{%
  \vspace*{0.75in}%
  {\parindent \z@ \raggedright \normalfont
    \ifnum \c@secnumdepth >\m@ne
      \if@mainmatter
        \huge\bfseries \@chapapp\space \thechapter
        \par\nobreak
        \vskip 15\p@
      \fi
    \fi
    \interlinepenalty\@M
    \Huge \bfseries #1\par\nobreak
    \vskip 40\p@
  }}
\def\@schapter#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}
\def\@makeschapterhead#1{%
  \vspace*{0.75in}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Huge \bfseries  #1\par\nobreak
    \vskip 40\p@
  }}
\newcommand\chapterbib{\if@openright\cleardoublepage\else\clearpage\fi
                    \thispagestyle{headings}%
                    \global\@topnum\z@
                    \@afterindenttrue
                    \secdef\@chapterbib\@schapterbib}
\def\@chapterbib[#1]#2{\ifnum \c@secnumdepth >\m@ne
                      \addcontentsline{toc}{chapter}{#1}%
                    \fi
                    \chaptermark{#1}%
                    \addtocontents{lof}{\protect\addvspace{10\p@}}%
                    \addtocontents{lot}{\protect\addvspace{10\p@}}%
                    \if@twocolumn
                      \@topnewpage[\@makechapterbibhead{#2}]%
                    \else
                      \@makechapterbibhead{#2}%
                      \@afterheading
                    \fi}
\def\@makechapterbibhead#1{%
  \vspace*{0.25in}%
  {\parindent \z@ \raggedright \normalfont
    \interlinepenalty\@M
    \Huge \bfseries #1\par\nobreak
    \vskip 40\p@
  }}
\def\@schapterbib#1{\if@twocolumn
                   \@topnewpage[\@makeschapterhead{#1}]%
                 \else
                   \@makeschapterhead{#1}%
                   \@afterheading
                 \fi}
\def\@makeschapterbibhead#1{%
  \vspace*{0.25in}%
  {\parindent \z@ \raggedright
    \normalfont
    \interlinepenalty\@M
    \Huge \bfseries  #1\par\nobreak
    \vskip 40\p@
  }}
\def\@startsection#1#2#3#4#5#6{%
 \if@noskipsec \leavevmode \fi
 \par \@tempskipa #4\relax
 \@afterindenttrue
 \if@nobreak \everypar{}\else
     \addpenalty\@secpenalty\addvspace\@tempskipa\fi
 \@ifstar{\@dblarg{\@sect{#1}{\@m}{#3}{#4}{#5}{#6}}}%
         {\@dblarg{\@sect{#1}{#2}{#3}{#4}{#5}{#6}}}%
}
\newcommand\section{\@startsection{section}{1}{\z@}%
                                   {0ex \@plus .1ex \@minus .2ex}%
                                   {2.3ex \@plus.2ex}%
                                   {\normalfont\Large\bfseries}}
\newcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                     {0ex\@plus .1ex \@minus .2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\large\bfseries}}
\newcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                     {0ex\@plus .1ex \@minus .2ex}%
                                     {1.5ex \@plus .2ex}%
                                     {\normalfont\normalsize\bfseries}}
\newcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
                                    {0ex \@plus .1ex \@minus .2ex}%
                                    {-1em}%
                                    {\normalfont\normalsize\bfseries}}
\newcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
                                       {0ex \@plus .1ex \@minus .2ex}%
                                       {-1em}%
                                      {\normalfont\normalsize\bfseries}}

\setlength\leftmargini  {2.5em}
\leftmargin  \leftmargini
\setlength\leftmarginii  {2.2em}
\setlength\leftmarginiii {1.87em}
\setlength\leftmarginiv  {1.7em}
\setlength\leftmarginv  {1em}
\setlength\leftmarginvi {1em}
\setlength  \labelsep  {.5em}
\setlength  \labelwidth{\leftmargini}
\addtolength\labelwidth{-\labelsep}
\@beginparpenalty -\@lowpenalty
\@endparpenalty   -\@lowpenalty
\@itempenalty     -\@lowpenalty
\renewcommand\theenumi{\@arabic\c@enumi}
\renewcommand\theenumii{\@alph\c@enumii}
\renewcommand\theenumiii{\@roman\c@enumiii}
\renewcommand\theenumiv{\@Alph\c@enumiv}
\newcommand\labelenumi{\theenumi.}
\newcommand\labelenumii{(\theenumii)}
\newcommand\labelenumiii{\theenumiii.}
\newcommand\labelenumiv{\theenumiv.}
\renewcommand\p@enumii{\theenumi}
\renewcommand\p@enumiii{\theenumi(\theenumii)}
\renewcommand\p@enumiv{\p@enumiii\theenumiii}
\newcommand\labelitemi{\textbullet}
\newcommand\labelitemii{\normalfont\bfseries \textendash}
\newcommand\labelitemiii{\textasteriskcentered}
\newcommand\labelitemiv{\textperiodcentered}
\newenvironment{description}
               {\list{}{\labelwidth\z@ \itemindent-\leftmargin
                        \let\makelabel\descriptionlabel}}
               {\endlist}
\newcommand*\descriptionlabel[1]{\hspace\labelsep
                                \normalfont\bfseries #1}
\newenvironment{verse}
               {\let\\\@centercr
                \list{}{\itemsep      \z@
                        \itemindent   -1.5em%
                        \listparindent\itemindent
                        \rightmargin  \leftmargin
                        \singlespacing
                        \advance\leftmargin 2em}%
                \item\relax}
               {\endlist}
\newenvironment{quotation}
               {\list{}{\listparindent .25in%
                        \leftmargin .25in
                        \singlespacing
                        \itemindent    \listparindent
                        \rightmargin   \leftmargin
                        \parsep        \z@ \@plus\p@}%
                \item\relax}
               {\endlist}
\newenvironment{quote}
               {\list{}{\rightmargin\leftmargin}%
                \item\relax}
               {\endlist}
\newcommand\appendix{\par
  \setcounter{chapter}{0}%
  \setcounter{section}{0}%
  \renewcommand\@chapapp{\appendixname}%
  \renewcommand\thechapter{\@Alph\c@chapter}}
\setlength\arraycolsep{5\p@}
\setlength\tabcolsep{6\p@}
\setlength\arrayrulewidth{.4\p@}
\setlength\doublerulesep{2\p@}
\setlength\tabbingsep{\labelsep}
\skip\@mpfootins = \skip\footins
\setlength\fboxsep{3\p@}
\setlength\fboxrule{.4\p@}
\@addtoreset {equation}{chapter}
\renewcommand\theequation
  {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@equation}
\newcounter{figure}[chapter]
\renewcommand \thefigure
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@figure}
\def\fps@figure{tbp}
\def\ftype@figure{1}
\def\ext@figure{lof}
\def\fnum@figure{\figurename~\thefigure}
\newenvironment{figure}
               {\@float{figure}}
               {\end@float}
\newenvironment{figure*}
               {\@dblfloat{figure}}
               {\end@dblfloat}
\newcounter{table}[chapter]
\renewcommand \thetable
     {\ifnum \c@chapter>\z@ \thechapter.\fi \@arabic\c@table}
\def\fps@table{tbp}
\def\ftype@table{2}
\def\ext@table{lot}
\def\fnum@table{\tablename~\thetable}
\newenvironment{table}
               {\@float{table}}
               {\end@float}
\newenvironment{table*}
               {\@dblfloat{table}}
               {\end@dblfloat}
\newlength\abovecaptionskip
\newlength\belowcaptionskip
\setlength\abovecaptionskip{10\p@}
\setlength\belowcaptionskip{0\p@}
\newsavebox{\@tempboxb}
\long\def\@makecaption#1#2{%
  \vskip\abovecaptionskip
  \sbox{\@tempboxa}{\parbox[l]{5in}{\hspace{-.5in} #1: #2}}%
  \sbox{\@tempboxb}{#1: #2}%
  \ifdim \wd\@tempboxb >\hsize
    \hb@xt@\hsize{\hfil\box\@tempboxa\hfil}%
  \else
    \global \@minipagefalse
    \hb@xt@\hsize{\hfil\box\@tempboxb\hfil}%
  \fi
  \vskip\belowcaptionskip}
\DeclareOldFontCommand{\rm}{\normalfont\rmfamily}{\mathrm}
\DeclareOldFontCommand{\sf}{\normalfont\sffamily}{\mathsf}
\DeclareOldFontCommand{\tt}{\normalfont\ttfamily}{\mathtt}
\DeclareOldFontCommand{\bf}{\normalfont\bfseries}{\mathbf}
\DeclareOldFontCommand{\it}{\normalfont\itshape}{\mathit}
\DeclareOldFontCommand{\sl}{\normalfont\slshape}{\@nomath\sl}
\DeclareOldFontCommand{\sc}{\normalfont\scshape}{\@nomath\sc}
\DeclareRobustCommand*\cal{\@fontswitch\relax\mathcal}
\DeclareRobustCommand*\mit{\@fontswitch\relax\mathnormal}
\newcommand\@pnumwidth{1.55em}
\newcommand\@tocrmarg{2.55em}
\newcommand\@dotsep{4.5}
\setcounter{tocdepth}{2}
\newcommand\tableofcontents{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*\l@part[2]{%
  \ifnum \c@tocdepth >-2\relax
    \addpenalty{-\@highpenalty}%
    \addvspace{2.25em \@plus\p@}%
    \begingroup
      \setlength\@tempdima{3em}%
      \parindent \z@ \rightskip \@pnumwidth
      \parfillskip -\@pnumwidth
      {\leavevmode
       \large \bfseries #1\hfil \hb@xt@\@pnumwidth{\hss #2}}\par
       \nobreak
         \global\@nobreaktrue
         \everypar{\global\@nobreakfalse\everypar{}}%
    \endgroup
  \fi}
\newcommand*\l@chapter[2]{%
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
      #1\nobreak\hfil \nobreak\hb@xt@\@pnumwidth{\hss #2}\par
      \penalty\@highpenalty
    \endgroup
  \fi}
\newcommand*\l@section{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand*\l@subsection{\@dottedtocline{2}{3.8em}{3.2em}}
\newcommand*\l@subsubsection{\@dottedtocline{3}{7.0em}{4.1em}}
\newcommand*\l@paragraph{\@dottedtocline{4}{10em}{5em}}
\newcommand*\l@subparagraph{\@dottedtocline{5}{12em}{6em}}
\newcommand\listoffigures{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listfigurename
      \@mkboth{\MakeUppercase\listfigurename}%
              {\MakeUppercase\listfigurename}}%
    \@starttoc{lof}%
    \if@restonecol\twocolumn\fi
    }
\newcommand*\l@figure{\@dottedtocline{1}{1.5em}{2.3em}}
\newcommand\listoftables{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapter*{\listtablename
      \@mkboth{%
          \MakeUppercase\listtablename}{\MakeUppercase\listtablename}}%
    \@starttoc{lot}%
    \if@restonecol\twocolumn\fi
    }
\let\l@table\l@figure
\newdimen\bibindent
\setlength\bibindent{.5in}
\newenvironment{thebibliography}[1]
      {\chapterbib{\bibname}{
        \@mkboth{}{}}%    
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \itemsep 2.5ex plus .2ex minus .2ex
            \singlespacing
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}
\newcommand\newblock{\hskip .11em\@plus.33em\@minus.07em}
\let\@openbib@code\@empty
\newenvironment{theindex}
               {\if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{headings}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}
\newcommand\@idxitem{\par\hangindent 40\p@}
\newcommand\subitem{\@idxitem \hspace*{20\p@}}
\newcommand\subsubitem{\@idxitem \hspace*{30\p@}}
\newcommand\indexspace{\par \vskip 10\p@ \@plus5\p@ \@minus3\p@\relax}
\renewcommand\footnoterule{%
  \kern-3\p@
  \hrule\@width\columnwidth
  \kern2.6\p@}
\@addtoreset{footnote}{chapter}
\newcommand\@makefntext[1]{%
    \parindent 1em%
    \noindent
    \hb@xt@.5in{\hss\@makefnmark}#1}
\newcommand\contentsname{Contents}
\newcommand\listfigurename{List of Figures}
\newcommand\listtablename{List of Tables}
\newcommand\bibname{References}
\newcommand\indexname{Index}
\newcommand\figurename{Figure}
\newcommand\tablename{Table}
\newcommand\partname{Part}
\newcommand\chaptername{Chapter}
\newcommand\appendixname{Appendix}
\newcommand\today{}
\edef\today{\ifcase\month\or
  January\or February\or March\or April\or May\or June\or
  July\or August\or September\or October\or November\or December\fi
  \space\number\day, \number\year}
\setlength\columnsep{10\p@}
\setlength\columnseprule{0\p@}
\pagestyle{headings}
\pagenumbering{arabic}
\if@twoside
\else
  \raggedbottom
\fi
\if@twocolumn
  \twocolumn
  \sloppy
  \flushbottom
\else
  \onecolumn
\fi
\def\setspace@size{%
  \ifx \@currsize \normalsize
    \@normalsize
  \else
    \@currsize
  \fi
}
\def\doublespacing{%
  \ifcase \@ptsize \relax % 10pt
    \def \baselinestretch {1.667}%
  \or % 11pt
    \def \baselinestretch {1.618}%
  \or % 12pt
    \def \baselinestretch {1.655}%
  \fi
  \setspace@size
}
\def\onehalfspacing{%
  \ifcase \@ptsize \relax % 10pt
    \def \baselinestretch {1.25}%
  \or % 11pt
    \def \baselinestretch {1.213}%
  \or % 12pt
    \def \baselinestretch {1.241}%
  \fi
  \setspace@size
}
\def\singlespacing{%
  \def \baselinestretch {1}%
  \setspace@size
  \vskip \baselineskip  % Correction for coming into singlespace
}
\def\setstretch#1{%
  \renewcommand{\baselinestretch}{#1}}
%---Stretch the baseline BEFORE calculating the strut size. This improves
%   spacing below tabular environments etc., probably...
%   Comments are welcomed.
\def\@setsize#1#2#3#4{%
  % Modified 1993.04.07--GDG per KPC
  \@nomath#1%
  \let\@currsize#1%
  \baselineskip #2%
  \baselineskip \baselinestretch\baselineskip
  \parskip \baselinestretch\parskip
  \setbox\strutbox \hbox{%
    \vrule height.7\baselineskip depth.3\baselineskip width\z@}%
  \skip\footins \baselinestretch\skip\footins
  \normalbaselineskip\baselineskip#3#4}
%---Reset baselinestretch within floats and footnotes.
% GT:  This is where the conflict with the combination of the color
% package and the figure environment used to occur.
\let\latex@xfloat=\@xfloat
\def\@xfloat #1[#2]{%
  \latex@xfloat #1[#2]%
  \def \baselinestretch {1}\@normalsize  %%  GT: need \@normalsize?
  \normalsize
}
\long\def\@footnotetext#1{%
  \insert\footins{%
    \def\baselinestretch {1}%
    \reset@font\footnotesize
    \interlinepenalty\interfootnotelinepenalty
    \splittopskip\footnotesep
    \splitmaxdepth \dp\strutbox \floatingpenalty \@MM
    \hsize\columnwidth
    \@parboxrestore
    \protected@edef\@currentlabel{%
      \csname p@footnote\endcsname\@thefnmark}%
    \color@begingroup
      \@makefntext{%
        \rule\z@\footnotesep\ignorespaces#1\@finalstrut\strutbox}%
    \color@endgroup}}
\long\def\@mpfootnotetext#1{%
  \global\setbox\@mpfootins\vbox{%
    \unvbox \@mpfootins
    \def\baselinestretch {1}%
    \reset@font\footnotesize
    \hsize\columnwidth
    \@parboxrestore
    \protected@edef\@currentlabel{%
      \csname p@mpfootnote\endcsname\@thefnmark}%
    \color@begingroup
      \@makefntext{%
       \rule\z@\footnotesep\ignorespaces#1\@finalstrut\strutbox}%
   \color@endgroup}}
% A single spaced quote (say) is done by surrounding singlespace with quote.
%
\def\singlespace{%
  \vskip \baselineskip
  \def\baselinestretch {1}%
  \setspace@size
  \vskip -\baselineskip
}
\def\endsinglespace{\par}
%  spacing, doublespace and onehalfspace all are meant to INCREASE the
%  spacing (i.e. calling onehalfspace from within doublespace will not
%  produce a graceful transition between spacings)
%
% Next two definitions fixed for consistency with TeX 3.x
\def\spacing#1{%
  \par
  \begingroup             % moved from \endspacing by PGBR 29-1-91
    \def\baselinestretch {#1}%
    \setspace@size
}
\def\endspacing{%
    \par
    \vskip \parskip
    \vskip \baselineskip
  \endgroup
  \vskip -\parskip
  \vskip -\baselineskip
}
% one and a half spacing is 1.5 x pt size
\def\onehalfspace{%
  \ifcase \@ptsize \relax  % 10pt
    \spacing{1.25}%
  \or % 11pt
    \spacing{1.213}%
  \or % 12pt
    \spacing{1.241}%
  \fi
}
\let\endonehalfspace=\endspacing
% double spacing is 2 x pt size
\def\doublespace{%
  \ifcase \@ptsize \relax % 10pt
    \spacing{1.667}%
  \or % 11pt
    \spacing{1.618}%
  \or % 12pt
    \spacing{1.655}%
  \fi
}
\let\enddoublespace=\endspacing
\everydisplay\expandafter{%
  \the\everydisplay
  \abovedisplayskip \baselinestretch\abovedisplayskip
  \belowdisplayskip \abovedisplayskip
  \abovedisplayshortskip \baselinestretch\abovedisplayshortskip
  \belowdisplayshortskip \baselinestretch\belowdisplayshortskip
}
%%%%%
\long\def\ifthenelse#1#2#3{\setbox\@tempboxa
  \vbox{\let\protect\noexpand
        \let\or\@or \let\and\@and \let\not\@neg
        \let\(\@lparen \let\)\@rparen
        {\let\if\relax\let\ifx\relax\let\ifnum\relax\let\fi\relax
        \let\else\relax \xdef\@gtempa{#1}}\expandafter
        \@eval \ifnum \@gtempa\relax \def\@term{T}\else \def\@term{F}\fi
            \@endeval\global\let\@gtempa\@val}\if\@gtempa T#2\else#3\fi}
\def\@eval{\def\@val{F}\def\@orop{T}\def\@negate{F}}
\def\@endeval{\if\@negate T\def\@negate{F}\if\@term T\def\@term{F}\else
               \def\@term{T}\fi\fi
       \if\@orop T\if\@val F\let\@val\@term\fi
             \else \if\@val T\let\@val\@term\fi\fi}
\def\@or{\relax\def\noexpand\@term{T}\else \def\noexpand\@term{F}\fi
         \noexpand\@endeval \def\noexpand\@orop{T}\ifnum}
\def\@and{\relax\def\noexpand\@term{T}\else \def\noexpand\@term{F}\fi
          \noexpand\@endeval\def\noexpand\@orop{F}\ifnum}
\def\@neg{1=1 \fi \def\noexpand\@negate{T}\ifnum}
\def\@lparen{1=1 \fi \begingroup \noexpand\@eval \ifnum}
\def\@rparen{\relax\def\noexpand\@term{T}\else \def\noexpand\@term{F}\fi
             \noexpand\@endeval
             \if\noexpand\@val T\gdef\noexpand\@gtempb{1=1}\else
            \gdef\noexpand\@gtempb{1=0}\fi
            \endgroup \ifnum\noexpand\@gtempb}
\def\equal#1#2{1=1 \fi \edef\noexpand\@tempa{#1}%
               \edef\noexpand\@tempe{#2}\ifx
                \noexpand\@tempa\noexpand\@tempe
                \def\noexpand\@tempa{1=1}\else
                \def\noexpand\@tempa{1=0}\fi \ifnum \noexpand\@tempa}
\long\def\whiledo#1#2{\ifthenelse{#1}{\@whiledotrue \@whilesw\if@whiledo
              \fi{#2\ifthenelse{#1}{\@whiledotrue}{\@whiledofalse}}}{}}
\newif\if@whiledo
\endinput
%%
%% End of file `TUT.cls'.
