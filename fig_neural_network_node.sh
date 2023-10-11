#! /bin/bash

# Print output to screen
debug=true

if [ "$debug" = "true" ]; then
    outstream=/dev/stdout
else
    outstream=/dev/null
fi



function compile_figure () {
    local texcode=$1
    local outname=$2

    echo $texcode
    exit 1

    # Write and compile tex file
    local tempfile=$(mktemp --suff=".tex")
    echo "Writing to Tex file: ${tempfile}" > ${outstream}
    echo ${texcode} > ${tempfile}
    pdflatex -jobname ${outname} ${tempfile} > ${outstream}

    exitcode=$?
    if [ $exitcode -ne 0 ]; then
        echo "pdflatex failed, see ${outname}.log" > /dev/stderr
        exit 1
    fi

    # Convert to png
    gs -sDEVICE=pngalpha -dNOPAUSE -r300 -o ${outname}.png ${outname}.pdf > ${outstream}

    # Clean up files
    echo "Cleaning files" > ${outstream}
    rm ${outname}.aux ${outname}.log ${outname}.pdf ${tempfile}
    echo "Output file: ${outname}.png"
}



FILENAME="fig_neural_network_node"

# -----------------------------------------------------------------------------
# Fig 1: Single node
read -r -d '' singlenode <<-"EOC"
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

compile_figure "${singlenode}" "${fig_neural_network_node}"


# -----------------------------------------------------------------------------
# Fig 2: Single layer



