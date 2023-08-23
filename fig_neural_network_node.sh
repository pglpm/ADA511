#! /bin/bash

FILENAME="fig_neural_network_node"

read -r -d '' texstring <<-"EOC"
\documentclass[border=3pt,tikz]{standalone}
\usepackage{tikz}
\usepackage{xcolor}
\usetikzlibrary{positioning}
\definecolor{pink}{HTML}{edc6f5}
\usepackage{listofitems} % for \readlist to create arrays
\tikzstyle{mynode}=[thick,draw=black,fill=pink!80,circle,minimum size=22]
\begin{document}
\begin{tikzpicture}[x=2.2cm,y=1.4cm]
    \begin{scope}[node distance=20mm and 20mm]
        \node[mynode] (a) at (1,1) {$h\left(b + \displaystyle\sum_i^3 w_i x_i\right)$};
        \node (x0) [above left=of a.west] {$x_0$};
        \node (x1) [left=of a.west] {$x_1$};       
        \node (x2) [below left=of a.west] {$x_2$};
    \node (z) [right=of a] {$z$};
    \end{scope}
    \draw[->, thick] (x0) -- (a);
    \draw[->, thick] (x1) -- (a);
    \draw[->, thick] (x2) -- (a);
    \draw[->, thick] (a) -- (z);
\end{tikzpicture}
\end{document}
EOC

# Write and compile tex file
tempfile=$(mktemp --suff=".tex")
echo "Writing to Tex file: ${tempfile}"
echo "$texstring" > ${tempfile}
pdflatex -jobname ${FILENAME} ${tempfile} > /dev/null

exitcode=$?
if [ $exitcode -ne 0 ]; then
    echo "pdflatex failed, see ${FILENAME}.log"
    exit 1
fi

# Convert to png
gs -sDEVICE=pngalpha -dNOPAUSE -r300 -o ${FILENAME}.png ${FILENAME}.pdf > /dev/null

# Clean up files
rm ${FILENAME}.aux ${FILENAME}.log  ${tempfile}
echo "Output file: ${FILENAME}.pdf / .png"
