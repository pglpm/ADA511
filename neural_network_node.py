##https://tikz.net/neural_networks/

from os import remove
import tempfile
import subprocess

def create_png(texcode, figname):

    texfile = ''
    with tempfile.NamedTemporaryFile(mode='w') as fp:
        texfile = fp.name
        fp.write(texcode)
        fp.flush()

        try:
            subprocess.check_call(['pdflatex', '-j', figname, texfile])
        except subprocess.CalledProcessError as exc:
            print(f'pdflatex failed -- check {figname}.log')
            raise exc

    subprocess.check_call(['gs', '-sDEVICE=pngalpha', '-dNOPAUSE', '-r300', '-o', f'{figname}.png', f'{figname}.pdf'])

    # Clean remaining files
    remove(f'{figname}.log')
    remove(f'{figname}.aux')
    remove(f'{figname}.pdf')

# -----------------------------------------------------------------------------
# Latex preamble
preamble = r"""
\documentclass[border=3pt,tikz]{standalone}
\usepackage{amsmath} % for aligned
\usepackage{tikz}
\usepackage{listofitems} % for \readlist to create arrays
\usetikzlibrary{arrows.meta} % for arrow size
\usetikzlibrary{positioning}
\contourlength{1.4pt}
\tikzset{>=latex} % for LaTeX arrow head
\usepackage{xcolor}
\definecolor{pink}{HTML}{edc6f5}
\colorlet{mydarkgreen}{green!30!black}
\colorlet{myblue}{blue!80!black}
\tikzstyle{node}=[thick,circle,draw=myblue,minimum size=22,inner sep=0.5,outer sep=0.6]
\tikzstyle{node in}=[node,green!20!black,draw=mygreen!30!black,fill=mygreen!25]
\tikzstyle{node hidden}=[node,blue!20!black,draw=myblue!30!black,fill=myblue!20]
\tikzstyle{node out}=[node,red!20!black,draw=myred!30!black,fill=myred!20]
\tikzstyle{node nostyle}=[node,red!20!black,draw=white,fill=white]
\tikzstyle{connect}=[thick,black] %,line cap=round
\tikzstyle{connect arrow}=[-{Latex[length=4,width=3.5]},thick,black,shorten <=0.5,shorten >=1]
\tikzset{ % node styles, numbered for easy mapping with \nstyle
  node 1/.style={node in},
  node 2/.style={node hidden},
  node 3/.style={node out},
}
\def\nstyle{int(\lay<\Nnodlen?min(2,\lay):3)} % map layer number onto 1, 2, or 3
"""


# -----------------------------------------------------------------------------
# Single network node
singlenode = r"""
\begin{document}
\begin{tikzpicture}[x=2.2cm,y=1.4cm]
    \begin{scope}[node distance=20mm and 20mm]
        \node[node] (a) at (1,1) {$h\left(b + \displaystyle\sum_i^3 w_i x_i\right)$};
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
"""

create_png(singlenode, 'fig_neural_network_node')


# -----------------------------------------------------------------------------
