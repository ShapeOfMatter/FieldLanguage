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
\newcommand{\CONS}{{;}\mspace{-0mu}}
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
  g & \define & n[n] &&  \text{n as an element of the cyclic group of order n.} \\[2mm]
  \tau & \define & \GroupOf n \OR \Group \OR \Perhaps \tau \OR \tau \to \tau \OR \Type && \text{types} \\[2mm]
  \Gamma & \define & [] \OR (x,e,\tau,\Gamma) \OR \Gamma\CONS\Gamma && \text{contexts, also written } \gamma \text{ when nested} \\[2mm]
  v & \define & \tau \OR g \OR \Actually v \OR \Nope : e \OR \lambda\,x:e\,.\;e && \text{values} \\[2mm]
  e & \define & x \OR v \OR \GroupOf e \OR \Perhaps e \OR e \to e \OR \Actually e && \text{expressions} \\[1mm]
    &         & \OR e \booleq e \OR e + e \OR -e \OR e \MOD e \OR \Default e\;e \OR \TypeOf e \\[1mm]
    &         & \OR \Let x:e=e\In e \OR \If e \Then e \Else e \OR e\; e \OR \Gamma\vdash e
\end{array}
$$

# Operations on expressions

We'll leave substitution as self-explainatory for now. $$(x + x + y)[v/x] = (v + v + y)$$

To avoid confusion between function types and reduction steps, we'll use "$\to$" for the function-type constructor, and "$\reduce$" ("leads to") for a single reduction step. "$\freduce$" will indicate transitive (and reflexive) reduction.

Equality of expressions ("$=$", not to be confused with the equality-operator "$\booleq$") should always be understoond as applying to a reduced state. Making this work for divergent expressions may not be pracical though.
$$ e_1 = e_2 \iff \exists e_3 : e_1 \freduce e_3 \land e_2 \freduce e_3$$


# Computational Rules

All of these take precidence over the contextual rules
$$
\begin{array}{c}
\mathsf{VARIABLE}\,\dfrac{ }{\Gamma;(x,e,\tau,\gamma)\vdash x \reduce e}  \\[4mm]
\mathsf{GROUPOF}\,\dfrac{ }{\GroupOf n_1[n_2] \reduce \GroupOf n_1}  \\[4mm]
\mathsf{EQUALITYT}\,\dfrac{v_1 = v_2}{v_1 \booleq v_2 \reduce 1[2]}  \\[4mm]
\mathsf{EQUALITYF}\,\dfrac{v_1 \ne v_2}{v_1 \booleq v_2 \reduce 0[2]}  \\[4mm]
\mathsf{PLUSEGROUP}\,\dfrac{ n_1 + n_2 = n_3 }{\GroupOf n_1 + \GroupOf n_2 \reduce \GroupOf n_3}  \\[4mm]
\mathsf{PLUSEELEM}\,\dfrac{ n_2 = n_4 \quad (n_1 + n3)\mathsf{mod}(n_2) = n_5 }{n_1[n_2] + n_3[n_4] \reduce n_5[n_2]}  \\[4mm]
\mathsf{NEGATION}\,\dfrac{(-n_1)\mathsf{mod}(n_2) = n_3}{ -n_1[n_2] \reduce n_3[n_2]}  \\[4mm]
\mathsf{MODULOGROUP}\,\dfrac{ (n_1)\mathsf{mod}(n_2) = n_3 }{\GroupOf n_1 \MOD \GroupOf n_2 \reduce n_3[n_2]}  \\[4mm]
\mathsf{MODULOELEM}\,\dfrac{ (n_1)\mathsf{mod}(n_3) = n_4 }{ n_1[n_2] \MOD \GroupOf n_3 \reduce n_4[n_3]}  \\[4mm]
\mathsf{DEFAULTNOPE}\,\dfrac{ }{\Default e_1\; (\Nope : e_2) \reduce e_1}  \\[4mm]
\mathsf{DEFAULTACTUALLY}\,\dfrac{ }{\Default e_1\; (\Actually e_2) \reduce e_2}  \\[4mm]
\mathsf{LET}\,\dfrac{ }{a}  \\[4mm]
\mathsf{IFTRUE}\,\dfrac{ }{a}  \\[4mm]
\mathsf{IFFALSE}\,\dfrac{ }{a}  \\[4mm]
\mathsf{BETA}\,\dfrac{ }{a}  \\[4mm]
\mathsf{DROP}\,\dfrac{ }{\Gamma \vdash v \reduce v}  \\[4mm]
\end{array}
$$

# $\Gamma$ Rules
### combination and elimination
$$
\begin{array}{c}
\mathsf{SHADOW}\,\dfrac{ }{\Gamma_1 \vdash \Gamma_2 \vdash e \reduce \Gamma_1\CONS\Gamma_2 \vdash e}  \\[4mm]
\mathsf{EMPTYHEAD}\,\dfrac{ }{\Gamma\CONS[] \vdash e \reduce \Gamma \vdash e}  \\[4mm]
\mathsf{EMPTYTAIL}\,\dfrac{ }{[] \vdash e \reduce e}  \\[4mm]
\end{array}
$$

### elimination to value

$$
\begin{array}{c}
\mathsf{DROPFROMTAU}\,\dfrac{ }{\Gamma \vdash \tau \reduce \tau}  \\[4mm]
\mathsf{DROPFROMELEM}\,\dfrac{ }{\Gamma \vdash g \reduce g}  \\[4mm]
\end{array}
$$

### push down recursively

$$
\begin{array}{c}
\mathsf{GR\_ACTUALLY}\,\dfrac{ }{\Gamma \vdash \Actually e \reduce \Actually (\Gamma \vdash e)}  \\[4mm]
\mathsf{GR\_NOPE}\,\dfrac{ }{\Gamma \vdash \Nope : e \reduce \Nope : (\Gamma \vdash e)}  \\[4mm]
\mathsf{GR\_GROUPOF}\,\dfrac{ }{\Gamma \vdash \GroupOf e \reduce \GroupOf (\Gamma \vdash e)}  \\[4mm]
\mathsf{GR\_PERHAPS}\,\dfrac{ }{\Gamma \vdash \Perhaps e \reduce \Perhaps (\Gamma \vdash e)}  \\[4mm]
\mathsf{GR\_FUNC}\,\dfrac{ }{\Gamma \vdash e_1 \to e_2 \reduce (\Gamma \vdash e_1) \to (\Gamma \vdash e_2)}  \\[4mm]
\mathsf{GR\_EQUALITY}\,\dfrac{ }{\Gamma \vdash e_1 \booleq e_2 \reduce (\Gamma \vdash e_1) \booleq (\Gamma \vdash e_2)}  \\[4mm]
\mathsf{GR\_PLUS}\,\dfrac{ }{\Gamma \vdash e_1 + e_2 \reduce (\Gamma \vdash e_1) + (\Gamma \vdash e_2)}  \\[4mm]
\mathsf{GR\_NEGATION}\,\dfrac{ }{\Gamma \vdash -e \reduce -(\Gamma \vdash e)}  \\[4mm]
\mathsf{GR\_MODULO}\,\dfrac{ }{\Gamma \vdash e_1 \MOD e_2 \reduce (\Gamma \vdash e_1) \MOD (\Gamma \vdash e_2)}  \\[4mm]
\mathsf{BETA}\,\dfrac{ }{a}  \\[4mm]
\end{array}
$$

### the hard cases

$$
\begin{array}{c}
\mathsf{RECURSELAMBDA}\,\dfrac{ }{\Gamma \vdash \lambda\,x:e_1\,.\;e_2 \reduce \lambda\, x:(\Gamma \vdash e_1)\,.\;(\Gamma \vdash e_2)}  \\[4mm]
\mathsf{BETA}\,\dfrac{ }{a}  \\[4mm]
\end{array}
$$

# Typing Rules

$$
\begin{array}{c}
\mathsf{T\_TYPE}\,\dfrac{ }{\TypeOf \Type \reduce \Type}  \\[4mm]
\mathsf{T\_FUNC}\,\dfrac{ }{\TypeOf (e_1 \to e_2) \reduce \Type}  \\[4mm]
\mathsf{T\_PERHAPS}\,\dfrac{ }{\TypeOf (\Perhaps e) \reduce \Type}  \\[4mm]
\mathsf{T\_GROUP}\,\dfrac{ }{\TypeOf \Group \reduce \Type}  \\[4mm]
\mathsf{T\_GROUPOFN}\,\dfrac{ }{\TypeOf (\GroupOf n) \reduce \Group}  \\[4mm]
\mathsf{T\_GROUPOFE}\,\dfrac{ }{\TypeOf (\GroupOf e) \reduce \Group}  \\[4mm]
\mathsf{T\_ELEM}\,\dfrac{ }{\TypeOf n_1[n_2] \reduce \GroupOf n_2}  \\[4mm]
\mathsf{T\_ACTUALLY}\,\dfrac{ }{\TypeOf (\Actually e) \reduce \Perhaps (\TypeOf e)}  \\[4mm]
\mathsf{T\_NOPE}\,\dfrac{ }{\TypeOf (\Nope : e) \reduce \Perhaps e}  \\[4mm]
\mathsf{T\_LAMBDA}\,\dfrac{ \Gamma;(x,\bot,e_1,\Gamma) \vdash \TypeOf e_2 \reduce e' }{\Gamma \vdash \TypeOf (\lambda\,x:e_1\,.\;e_2) \reduce e_1 \to e' }  \\[4mm]
\mathsf{T\_X}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_EQUALITY}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_PLUS}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_NEGATION}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_MODULO}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_DEFAULT}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_TYPEOF}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_LET}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_IF}\,\dfrac{ }{a}  \\[4mm]
\mathsf{T\_GAMMA}\,\dfrac{ }{\Gamma \vdash e }  \\[4mm]
\end{array}
$$

# Contextual Rules

$$
\begin{array}{c}
\mathsf{GROUPOFC}\,\dfrac{e \reduce e'}{\GroupOf e \reduce \GroupOf e'}  \\[4mm]
\mathsf{PERHAPS}\,\dfrac{e \reduce e'}{\Perhaps e \reduce \Perhaps e'}  \\[4mm]
\mathsf{FUNCTIONTYPER}\,\dfrac{e_2 \reduce e'}{e_1 \to e_2 \reduce e_1 \to e'}  \\[4mm]
\mathsf{FUNCTIONTYPEL}\,\dfrac{e \reduce e'}{e \to v \reduce e' \to v}  \\[4mm]
\mathsf{ACTUALLY}\,\dfrac{e \reduce e'}{\Actually e \reduce \Actually e'}  \\[4mm]
\mathsf{EQAULITYL}\,\dfrac{e_1 \reduce e'}{e_1 \booleq e_2 \reduce e' \booleq e_2}  \\[4mm]
\mathsf{EQAULITYR}\,\dfrac{e \reduce e'}{v \booleq e \reduce v \booleq e'}  \\[4mm]
\mathsf{PLUSL}\,\dfrac{e_1 \reduce e'}{e_1 + e_2 \reduce e' + e_2}  \\[4mm]
\mathsf{PLUSR}\,\dfrac{e \reduce e'}{v + e \reduce v + e'}  \\[4mm]
\mathsf{NEGATIONC}\,\dfrac{e \reduce e'}{-e \reduce -e'}  \\[4mm]
\mathsf{MODULOL}\,\dfrac{e_1 \reduce e'}{e_1 \MOD e_2 \reduce e' \MOD e_2}  \\[4mm]
\mathsf{MODULOR}\,\dfrac{e \reduce e'}{v \MOD e \reduce v \MOD e'}  \\[4mm]
\mathsf{DEFAULTC}\,\dfrac{e_2 \reduce e'}{\Default e_1\; e_2 \reduce \Default e_1\; e'}  \\[4mm]
\mathsf{IFC}\,\dfrac{ }{a}  \\[4mm]
\end{array}
$$


