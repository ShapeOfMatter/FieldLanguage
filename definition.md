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
\newcommand{\TypeOf}{\mathrm{TypeOf\;}}
\newcommand{\Let}{\mathrm{Let\;}}
\newcommand{\In}{\mathrm{\;In\;}}
\newcommand{\If}{\mathrm{If\;}}
\newcommand{\Then}{\mathrm{\;Then\;}}
\newcommand{\Else}{\mathrm{\;Else\;}}
\newcommand{\OP}{%
  \,\overrightarrow{\mathtt{op}}\,%
}\newcommand{\OPP}{%
  \,\overleftrightarrow{\mathtt{op}}\,%
}


# Syntax

The concrete syntax has the following context-free grammar:

$$
\begin{array}{c c l l l}
  x & \in & \mathrm{Variables} && \text{variables}  \\[2mm]
  n & \in & \mathbb{N} && \text{*} \\[2mm]
  g & \define & n[n] &&  \text{n as an element of the cyclic group of order n.**} \\[2mm]
  \tau & \define & \GroupOf n \OR \Group \OR \Perhaps \tau \OR \tau \to \tau \OR \Type && \text{types} \\[2mm]
  \Gamma & \define & [] \OR (x,e,\tau,\Gamma) \OR \Gamma\CONS\Gamma && \text{contexts*} \\[2mm]
  v & \define & \tau \OR g \OR \Actually v \OR \Nope : e \OR \lambda\,x:e\,.\;e && \text{values} \\[2mm]
  \OP & \define & \GroupOf \OR \Perhaps \OR \Actually \OR - \OR \TypeOf && \text{unary oporators*} \\[2mm]
  \OPP & \define & \to \OR \booleq \OR + \OR \MOD \OR \lor && \text{binary oporators*} \\[2mm]
  e & \define & x \OR v \OR \OP e \OR e \OPP e && \text{expressions} \\[1mm]
    &         & \OR \Let x:e=e\In e \OR \If e \Then e \Else e \OR e\; e \OR \Gamma\vdash e
\end{array}
$$

\* These are not value or expressions on their own.  
** There is no group of order 0.

# Operations on expressions

We'll leave substitution as self-explainatory for now. $$(x + x + y)[v/x] = (v + v + y)$$

To avoid confusion between function types and reduction steps, we'll use "$\to$" for the function-type constructor, and "$\reduce$" ("leads to") for a single reduction step. "$\freduce$" will indicate transitive (and reflexive) reduction.

Equality of expressions ("$=$", not to be confused with the equality-operator "$\booleq$") should always be understoond as applying to a reduced state. Making this work for divergent expressions may not be pracical though.
$$ e_1 = e_2 \iff \exists e_3 : e_1 \freduce e_3 \land e_2 \freduce e_3$$


# Computational Rules

All of these take precidence over the contextual rules
$$
\begin{array}{c l}
\mathsf{GROUPOF}\,\dfrac{ n_1 \neq 0 }{\GroupOf n_1[n_2] \reduce \GroupOf n_1}  \\[5mm]
\mathsf{GROUPOFZERO}\,\dfrac{ n_1 = 0 }{\GroupOf n_1[n_2] \reduce \GroupOf n_2}  \\[5mm]
\mathsf{NEGATION}\,\dfrac{(-n_1)\mathsf{mod}(n_2) = n_3}{ -n_1[n_2] \reduce n_3[n_2]}  \\[5mm]
\mathsf{EQUALITYT}\,\dfrac{v_1 = v_2}{v_1 \booleq v_2 \reduce 1[2]}  & \text{(maybe limit what $\booleq$ applies to?)}   \\[5mm]
\mathsf{EQUALITYF}\,\dfrac{v_1 \ne v_2}{v_1 \booleq v_2 \reduce 0[2]}  \\[5mm]
\mathsf{PLUSEGROUP}\,\dfrac{ n_1 + n_2 = n_3 }{\GroupOf n_1 + \GroupOf n_2 \reduce \GroupOf n_3}  \\[5mm]
\mathsf{PLUSEELEM}\,\dfrac{ n_2 = n_4 \quad (n_1 + n3)\mathsf{mod}(n_2) = n_5 }{n_1[n_2] + n_3[n_4] \reduce n_5[n_2]}  \\[5mm]
\mathsf{MODULOGROUP}\,\dfrac{ (n_1)\mathsf{mod}(n_2) = n_3 }{\GroupOf n_1 \MOD \GroupOf n_2 \reduce n_3[n_2]}  \\[5mm]
\mathsf{MODULOELEM}\,\dfrac{ (n_1)\mathsf{mod}(n_3) = n_4 }{ n_1[n_2] \MOD \GroupOf n_3 \reduce n_4[n_3]}  \\[5mm]
\mathsf{DEFAULTNOPE}\,\dfrac{ }{(\Nope : e_1) \lor e_2 \reduce e_2}  \\[5mm]
\mathsf{DEFAULTACTUALLY}\,\dfrac{ }{(\Actually e_1) \lor e_2 \reduce e_1}  \\[5mm]
\mathsf{LET}\,\dfrac{ }{\Let x:e_1 = e_2 \In e_3 \reduce (x,e_2,e_1,[]) \vdash e_3}  \\[5mm]
\mathsf{IFTRUE}\,\dfrac{ }{\If 1[2] \Then e_1 \Else e_2 \reduce e_1}  \\[5mm]
\mathsf{IFFALSE}\,\dfrac{ }{\If 0[2] \Then e_1 \Else e_2 \reduce e_2}  \\[5mm]
\mathsf{APPLICATION}\,\dfrac{ }{(\lambda\,x:e_1\,.\;e_2) \; e_3 \reduce (x,e_3,e_1,[]) \vdash e_2}  \\[5mm]
\end{array}
$$

\pagebreak
# $\Gamma$ Rules
### combination and elimination
$$
\begin{array}{c}
\mathsf{SHADOW}\,\dfrac{ }{\Gamma_1 \vdash \Gamma_2 \vdash e \reduce \Gamma_1\CONS\Gamma_2 \vdash e}  \\[5mm]
\mathsf{EMPTYHEAD}\,\dfrac{ }{[]\CONS\Gamma \vdash e \reduce \Gamma \vdash e}  \\[5mm]
\mathsf{EMPTYTAIL}\,\dfrac{ }{[] \vdash e \reduce e}  \\[5mm]
\end{array}
$$

### elimination to value

$$
\begin{array}{c}
\mathsf{DROPFROMTAU}\,\dfrac{ }{\Gamma \vdash \tau \reduce \tau}  \\[5mm]
\mathsf{DROPFROMELEM}\,\dfrac{ }{\Gamma \vdash g \reduce g}  \\[5mm]
\end{array}
$$

### push down recursively

$$
\begin{array}{c}
\mathsf{GR\_OP1}\,\dfrac{ }{\Gamma \vdash \OP e \reduce \OP (\Gamma \vdash e)}  \\[5mm]
\mathsf{GR\_OP2}\,\dfrac{ }{\Gamma \vdash (e_1 \OPP e_2) \reduce (\Gamma \vdash e_1) \OPP (\Gamma \vdash e_2)}  \\[5mm]
\mathsf{GR\_NOPE}\,\dfrac{ }{\Gamma \vdash \Nope : e \reduce \Nope : (\Gamma \vdash e)}  \\[5mm]
\mathsf{GR\_LAMBDA}\,\dfrac{ }{\Gamma \vdash \lambda\,x:e_1\,.\;e_2 \reduce \lambda\,x:(\Gamma\vdash e_1)\,.\;(\Gamma\vdash e_2)}  \\[5mm]
\mathsf{GR\_LET}\,\dfrac{ }{\Gamma \vdash \Let x : e_1 = e_2 \In e_3 \reduce \Let x : (\Gamma \vdash e_1) = (\Gamma \vdash e_2) \In (\Gamma \vdash e_3)}  \\[5mm]
\mathsf{GR\_IF}\,\dfrac{ }{\Gamma \vdash \If e_1 \Then e_2 \Else e_3 \reduce \If (\Gamma \vdash e_1) \Then (\Gamma \vdash e_2) \Else (\Gamma \vdash e_3)}  \\[5mm]
\mathsf{GR\_APP}\,\dfrac{ }{\Gamma \vdash e_1\;e_2 \reduce (\Gamma \vdash e_1)\;(\Gamma \vdash e_2)}  \\[5mm]
\end{array}
$$

### variables

$$
\begin{array}{c l}
\mathsf{ISOLATEVARIABLE}\,\dfrac{ x' = x }{(x',e',\tau',\Gamma')\CONS\Gamma \vdash x \reduce (x',e',\tau',\Gamma') \vdash x} & \text{may be used here or in TypeOf}  \\[5mm]
\mathsf{VARIABLE}\,\dfrac{ x' = x }{(x',e',\tau',\Gamma') \vdash x \reduce (x',e',\tau',\Gamma')\CONS\Gamma' \vdash e'}  \\[5mm]
\mathsf{VARIABLESEARCH}\,\dfrac{ x' \neq x \quad \Gamma\vdash x \reduce e }{(x',e',\tau',\Gamma')\CONS\Gamma \vdash x \reduce e}  \\[5mm]
\end{array}
$$

# Typing Rules

$$
\begin{array}{c p{3cm}}
\mathsf{T\_TYPE}\,\dfrac{ }{\TypeOf \Type \reduce \Type}  \\[5mm]
\mathsf{T\_FUNC}\,\dfrac{ }{\TypeOf (e_1 \to e_2) \reduce \Type}  \\[5mm]
\mathsf{T\_PERHAPS}\,\dfrac{ }{\TypeOf (\Perhaps e) \reduce \Type}  \\[5mm]
\mathsf{T\_GROUP}\,\dfrac{ }{\TypeOf \Group \reduce \Type}  \\[5mm]
\mathsf{T\_GROUPOFN}\,\dfrac{ }{\TypeOf (\GroupOf n) \reduce \Group}  \\[5mm]
\mathsf{T\_GROUPOFE}\,\dfrac{ }{\TypeOf (\GroupOf e) \reduce \Group}  \\[5mm]
\mathsf{T\_ELEM}\,\dfrac{ }{\TypeOf n_1[n_2] \reduce \GroupOf n_2}  \\[5mm]
\mathsf{T\_ACTUALLY}\,\dfrac{ }{\TypeOf (\Actually e) \reduce \Perhaps (\TypeOf e)}  \\[5mm]
\mathsf{T\_NOPE}\,\dfrac{ }{\TypeOf (\Nope : e) \reduce \Perhaps e}  \\[5mm]
\mathsf{T\_LAMBDA}\,\dfrac{ }{ \TypeOf (\lambda\,x:e_1\,.\;e_2) \reduce e_1 \to \TypeOf ((x,\bot,e_1, [])\vdash e_2) } & \text{This feels wrong.}\newline\text{Are we handling recursive-x right?}\newline\text{shadowing?}  \\[5mm]
\mathsf{T\_VARIABLE}\,\dfrac{ x' = x }{\TypeOf ((x',e',\tau',\Gamma') \vdash x) \reduce (x',e',\tau',\Gamma')\CONS\Gamma' \vdash e'}  \\[5mm]
\mathsf{T\_EQUALITY}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_PLUS}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_NEGATION}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_MODULO}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_DEFAULT}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_TYPEOF}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_LET}\,\dfrac{ }{a}  \\[5mm]
\mathsf{T\_IF}\,\dfrac{ }{a}  \\[5mm]
\end{array}
$$

# Contextual Rules

$$
\begin{array}{c l}
\mathsf{OP1}\,\dfrac{e \reduce e'}{\OP e \reduce \OP e'}  \\[5mm]
\mathsf{OP2L}\,\dfrac{e_1 \reduce e'}{e_1 \OPP e_2 \reduce e' \OPP e_2}  \\[5mm]
\mathsf{OP2R}\,\dfrac{e \reduce e'}{v \OPP e \reduce v \OPP e'}  & \text{(should we make this not apply to $\lor$?)}  \\[5mm]
\mathsf{IFC}\,\dfrac{ }{a}  \\[5mm]
\mathsf{APPC}\,\dfrac{ }{a}  \\[5mm]
\end{array}
$$


