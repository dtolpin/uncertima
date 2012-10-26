sed "s/@@@/$1/" > $1.tex <<END
\documentclass{article}

\usepackage{tikz}
\usetikzlibrary{positioning}
\usetikzlibrary{shapes.geometric}
\usetikzlibrary{shapes.symbols}
\usetikzlibrary{shadows}
\usetikzlibrary{arrows}

\begin{document}

\input{../thesis/@@@}

\end{document}
END
