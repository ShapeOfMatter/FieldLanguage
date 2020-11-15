---
title: "Some Fieldy Language"
author: "Mako Bates"
date: 2020-11-11

geometry: margin=2cm

header-includes:
  - \usepackage{amsmath}

---

\newcommand{\define}{%
  \mathbin{{:}\mspace{-2mu}{:}\mspace{-1mu}{=}}%
}
\newcommand{\booleq}{%
  \mathbin{{=}\mspace{-1.5mu}{=}}%
}
\newcommand{\reduce}{%
  \mathbin{\;\leadsto\;}%
}
\newcommand{\freduce}{%
  \mathbin{{\reduce}\mspace{-20mu}{\,^*}\;\,}%
}
\newcommand{\OR}{\;|\;}
\newcommand{\MOD}{\;\%\;}
\newcommand{\GroupOf}{\mathrm{GroupOf\;}}
\newcommand{\Group}{\mathrm{Group}}
\newcommand{\Type}{\mathrm{Type}}
\newcommand{\Perhaps}{\mathrm{Perhaps\;}}
\newcommand{\Actually}{\mathrm{Actually\;}}
\newcommand{\Nope}{\mathrm{Nope}}
\newcommand{\Default}{\mathrm{Default\;}}
\newcommand{\TypeOf}{\mathrm{TypeOf\;}}
\newcommand{\Let}{\mathrm{Let\;}}
\newcommand{\In}{\mathrm{\;In\;}}
\newcommand{\If}{\mathrm{If\;}}
\newcommand{\Then}{\mathrm{\;Then\;}}
\newcommand{\Else}{\mathrm{\;Else\;}}


# Syntax

The concrete syntax has the following context-free grammar:

$$
\begin{array}{c c l l l}
  x & \in & \mathrm{Variables} && \text{variables}  \\[2mm]
  n & \in & \mathbb{N} && \text{(not used directly)} \\[2mm]
  g & \define & n_n &&  \text{n as an element of the cyclic group of order n.} \\[2mm]
  \tau & \define & \GroupOf n \OR \Group \OR \Perhaps \tau \OR \tau \to \tau \OR \Type && \text{types} \\[2mm]
  v & \define & \tau \OR g \OR \Actually v \OR \Nope \OR \lambda x : e .\;e \ && \text{values} \\[2mm]
  e & \define & x \OR v \OR \GroupOf e \OR \Perhaps e \OR e \to e \OR \Actually e && \text{expressions} \\[1mm]
    &         & \OR e \booleq e \OR e + e \OR -e \OR e \MOD e \OR \Default e\;e \OR \TypeOf e \\[1mm]
    &         & \OR \Let x:e=e\In e \OR \If e \Then e \Else e
\end{array}
$$

# Operations on expressions

We'll leave substitution as self-explainatory for now. $$(x + x + y)[v/x] = (v + v + y)$$

To avoid confusion between function types and reduction steps, we'll use "$\to$" for the function-type constructor, and "$\reduce$" ("leads to") for a single reduction step. "$\freduce$" will indicate transitive reduction.

Equality of expressions ("$=$", not to be confused with the equality-operator "$\booleq$") should always be understoond as applying to a reduced state. Making this work for divergent expressions may not be pracical though.
$$ e_1 = e_2 \iff \exists e_3 : e_1 \freduce e_3 \land e_2 \freduce e_3$$

# Computational Rules

All of these take precidence over the contextual rules
$$
\begin{array}{c}
\mathsf{EQUALITYT}\,\dfrac{v_1 = v_2}{v_1 \booleq v_2 \reduce 1_2}  \\[4mm]
\mathsf{EQUALITYF}\,\dfrac{v_1 \ne v_2}{v_1 \booleq v_2 \reduce 0_2}  \\[4mm]
\mathsf{PLUSE}\,\dfrac{ }{n}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\end{array}
$$

# Contextual Rules

$$
\begin{array}{c}
\mathsf{GROUPOF}\,\dfrac{e \reduce e'}{\GroupOf e \reduce \GroupOf e'}  \\[4mm]
\mathsf{PERHAPS}\,\dfrac{e \reduce e'}{\Perhaps e \reduce \Perhaps e'}  \\[4mm]
\mathsf{FUNCTIONTYPER}\,\dfrac{e_2 \reduce e'}{e_1 \to e_2 \reduce e_1 \to e'}  \\[4mm]
\mathsf{FUNCTIONTYPEL}\,\dfrac{e \reduce e'}{e \to v \reduce e' \to v}  \\[4mm]
\mathsf{ACTUALLY}\,\dfrac{e \reduce e'}{\Actually e \reduce \Actually e'}  \\[4mm]
\mathsf{EQAULITYL}\,\dfrac{e_1 \reduce e'}{e_1 \booleq e_2 \reduce e' \booleq e_2}  \\[4mm]
\mathsf{EQAULITYR}\,\dfrac{e \reduce e'}{v \booleq e \reduce v \booleq e'}  \\[4mm]
\mathsf{PLUSL}\,\dfrac{e_1 \reduce e'}{e_1 + e_2 \reduce e' + e_2}  \\[4mm]
\mathsf{PLUSR}\,\dfrac{e \reduce e'}{v + e \reduce v + e'}  \\[4mm]
\mathsf{NEGATION}\,\dfrac{e \reduce e'}{-e \reduce -e'}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\mathsf{A}\,\dfrac{ }{a}  \\[4mm]
\end{array}
$$


