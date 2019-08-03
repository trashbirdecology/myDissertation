%%
%% This is file `nuthesis.cls',
%% generated with the docstrip utility.
%%
%% The original source files were:
%%
%% nuthesis.dtx  (with options: `class')
%% This is a generated file.
%% 
%% Copyright (C) 2008 by Ned W. Hummel nhummel@gmail.com
%% 
%% This file may be distributed and/or modified under the conditions of
%% the LaTeX Project Public License, either version 1.3c of this license
%% or (at your option) any later version.  The latest version of this
%% license is in:
%% 
%%    http://www.latex-project.org/lppl.txt
%% 
%% and version 1.3c or later is part of all distributions of LaTeX version
%% 2006/05/20 or later.
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{nuthesis}
    [2017/10/12 v0.7.2 University of Nebraska Thesis class]
%% \CharacterTable
%% {Upper-case    \A\B\C\D\E\F\G\H\I\J\K\L\M\N\O\P\Q\R\S\T\U\V\W\X\Y\Z
%%  Lower-case    \a\b\c\d\e\f\g\h\i\j\k\l\m\n\o\p\q\r\s\t\u\v\w\x\y\z
%%  Digits        \0\1\2\3\4\5\6\7\8\9
%%  Exclamation   \!     Double quote  \"     Hash (number) \#
%%  Dollar        \$     Percent       \%     Ampersand     \&
%%  Acute accent  \'     Left paren    \(     Right paren   \)
%%  Asterisk      \*     Plus          \+     Comma         \,
%%  Minus         \-     Point         \.     Solidus       \/
%%  Colon         \:     Semicolon     \;     Less than     \<
%%  Equals        \=     Greater than  \>     Question mark \?
%%  Commercial at \@     Left bracket  \[     Backslash     \\
%%  Right bracket \]     Circumflex    \^     Underscore    \_
%%  Grave accent  \`     Left brace    \{     Vertical bar  \|
%%  Right brace   \}     Tilde         \~}

%% The ToC is required
%% Uncomment these if need be

\RequirePackage{ifthen}
\RequirePackage{keyval}
\RequirePackage{setspace}
\RequirePackage{scrtime}
\newboolean{nu@print}
\newboolean{nu@single}
\newcommand{\doctype}[1]{%
  \gdef\nu@doctype{#1}}
\newcommand{\nu@doctype}{}
\newcommand{\degree}[1]{%
  \gdef\nu@degree{#1}}
\newcommand{\nu@degree}{}
\newcommand{\degreeabbreviation}[1]{%
  \gdef\nu@degreeabbreviation{#1}}
\newcommand{\nu@degreeabbreviation}{}
\newcommand{\major}[1]{%
  \gdef\nu@major{#1}}
\newcommand{\nu@major}{}
\newcommand{\college}[1]{%
  \gdef\nu@college{#1}}
\newcommand{\nu@college}{Graduate College}
\newcommand{\university}[1]{%
  \gdef\nu@university{#1}}
\newcommand{\nu@university}{University of Nebraska}
\newcommand{\city}[1]{%
  \gdef\nu@city{#1}}
\newcommand{\nu@city}{Lincoln}
\newcommand{\state}[1]{%
  \gdef\nu@state{#1}}
\newcommand{\nu@state}{Nebraska}
\newcommand{\adviser}[1]{%
  \gdef\nu@adviser{#1}}
\newcommand{\adviserAbstract}[1]{%
  \gdef\nu@adviserAbstract{#1}}
\newcommand{\nu@adviser}{}
\newcommand{\nu@adviserAbstract}{}
\newcommand{\degreemonth}[1]{%
  \gdef\nu@month{#1}}
\newcommand{\nu@month}{}
\newcommand{\degreeyear}[1]{%
  \gdef\nu@year{#1}}
\newcommand{\nu@year}{}
\DeclareOption{testing}{%
  \PassOptionsToPackage{showframe}{geometry}
}
\DeclareOption{single}{%
  \setboolean{nu@single}{true}
}

\DeclareOption{double}{%
  \setboolean{nu@single}{false}
}
\DeclareOption{print}{%
  \setboolean{nu@print}{true}
  \PassOptionsToPackage{bindingoffset=0.5in}{geometry}
}
\DeclareOption{electronic}{%
\relax
}
\DeclareOption{phd}{%
  \doctype{Dissertation}
  \degree{Doctor of Philosophy}
  \degreeabbreviation{Ph.D.}
}

\DeclareOption{edd}{%
  \doctype{Dissertation}
  \degree{Doctor of Education}
  \degreeabbreviation{Ed.D.}
}

\DeclareOption{ms}{%
  \doctype{Thesis}
  \degree{Master of Science}
  \degreeabbreviation{M.S.}
}

\DeclareOption{ma}{%
  \doctype{Thesis}
  \degree{Master of Art}
  \degreeabbreviation{M.A.}
}
\DeclareOption*{%
  \PassOptionsToClass{\CurrentOption}{memoir}
}
\ExecuteOptions{double,electronic,phd}
\ProcessOptions
\LoadClass[12pt,oneside]{memoir}
\renewcommand{\chapnamefont}{\normalfont\large\bfseries}
\renewcommand{\chapnumfont}{\normalfont\large\bfseries}
\renewcommand{\chaptitlefont}{\large\bfseries}
\renewcommand{\partnamefont}{\normalfont\large\bfseries}
\renewcommand{\partnumfont}{\normalfont\large\bfseries}
\renewcommand{\parttitlefont}{\normalfont\large\bfseries}
\renewcommand{\secheadstyle}{\large\bfseries}
\renewcommand{\subsecheadstyle}{\normalsize\bfseries}
\renewcommand{\paraheadstyle}{\normalsize\bfseries}
\renewcommand{\subparaheadstyle}{\normalsize\bfseries}
\RequirePackage[letterpaper,margin=1in,includehead,nofoot]{geometry}
\makepagestyle{nu@pagestyle}
\makeoddhead{nu@pagestyle}%
  {}{}{\normalfont\thepage}
\makeevenhead{nu@pagestyle}%
  {\normalfont\thepage}{}{}

\makepagestyle{nu@pagestyleDraft}
\makeoddhead{nu@pagestyleDraft}%
  {\itshape\LaTeX{}ed at \thistime{} on \today}{}{\normalfont\thepage}
\makeevenhead{nu@pagestyleDraft}%
  {\normalfont\thepage}{}{\itshape\LaTeX{}ed at \thistime{} on \today}
\ifdraftdoc
  \aliaspagestyle{plain}{nu@pagestyleDraft}
  \aliaspagestyle{empty}{nu@pagestyleDraft}
%%  \aliaspagestyle{cleared}{nu@pagestyleDraft}
  \pagestyle{nu@pagestyleDraft}
\else
  \aliaspagestyle{plain}{nu@pagestyle}
%%  \aliaspagestyle{cleared}{nu@pagestyle}
  \pagestyle{nu@pagestyle}
\fi
\renewcommand{\maketitle}{%
\begin{center}%
\MakeUppercase{\thetitle}\\[1\baselineskip]
\singlespacing
by\\[2\baselineskip]
\theauthor\\[4\baselineskip]
\MakeUppercase{A \nu@doctype}\\[4\baselineskip]
Presented to the Faculty of\\[\baselineskip]
The \nu@college{} at the \nu@university\\[\baselineskip]
In Partial Fulfilment of Requirements\\[\baselineskip]
For the Degree of \nu@degree\\[3\baselineskip]
Major: \nu@major\\[3\baselineskip]
Under the Supervision of \nu@adviser\\[3\baselineskip]
\nu@city, \nu@state\\[1\baselineskip]
\nu@month, \nu@year
\end{center}
\thispagestyle{empty}
\cleardoublepage
}
\renewenvironment{abstract}{%
  \pagestyle{empty}%
  \begin{doublespacing}%
    \begin{center}%
      \MakeUppercase{\thetitle}\\[.5\baselineskip]
      \theauthor, \nu@degreeabbreviation\\
      \nu@university, \nu@year%
    \end{center}%
    \begin{flushleft}%
      Adviser: \nu@adviserAbstract%
    \end{flushleft}%
  }{\par\end{doublespacing}\cleardoublepage}

\newenvironment{copyrightpage}[1][Copyright]{%
  \begin{center}%
    \MakeUppercase{#1}\\
    \copyright{} \nu@year, \theauthor%
  \end{center}%
  \ifthenelse{\boolean{nu@single}}{\begin{singlespacing}}{\begin{doublespacing}}%
    }{\par%
      \ifthenelse{\boolean{nu@single}}{\end{singlespacing}}{\end{doublespacing}}%
  \cleardoublepage}

\newenvironment{dedication}[1][Dedication]{%
  \begin{center}%
    \MakeUppercase{#1}%
  \end{center}%
  \ifthenelse{\boolean{nu@single}}{\begin{singlespacing}}{\begin{doublespacing}}%
    }{\par%
      \ifthenelse{\boolean{nu@single}}{\end{singlespacing}}{\end{doublespacing}}%
  \cleardoublepage}

\newenvironment{acknowledgments}[1][Acknowledgments]{%
  \begin{center}
    \MakeUppercase{#1}
  \end{center}
  \ifthenelse{\boolean{nu@single}}{\begin{singlespacing}}{\begin{doublespacing}}%
    }{\par%
      \ifthenelse{\boolean{nu@single}}{\end{singlespacing}}{\end{doublespacing}}%
  \cleardoublepage}

\newenvironment{grantinfo}[1][Grant Information]{%
  \begin{center}
    \MakeUppercase{#1}
  \end{center}
  \ifthenelse{\boolean{nu@single}}{\begin{singlespacing}}{\begin{doublespacing}}%
    }{\par%
      \ifthenelse{\boolean{nu@single}}{\end{singlespacing}}{\end{doublespacing}}%
  \cleardoublepage}


\let\nu@ToC\tableofcontents
\renewcommand{\contentsname}{Table of Contents}
\renewcommand{\tableofcontents}{\cleardoublepage\nu@ToC*}

\let\nu@LoF\listoffigures
\renewcommand{\listoffigures}{\cleardoublepage\nu@LoF}

\let\nu@LoT\listoftables
\renewcommand{\listoftables}{\cleardoublepage\nu@LoT}
\maxsecnumdepth{subsubsection}
\setsecnumdepth{subsubsection}
\maxtocdepth{subsubsection}
\let\backmatter\relax
\AtBeginDocument{%
\ifthenelse{\boolean{nu@single}}{\singlespacing}{\doublespacing}%
}
\endinput
%%
%% End of file `nuthesis.cls'.
