---
title: Decomposing (co)limits
date: 2025-05-21
---

\newcommand{\Map}{\mathrm{Map}}
\newcommand{\Fun}{\mathrm{Fun}}
\newcommand{\colim}{\mathrm{colim}}
\newcommand{\cS}{\mathcal{S}}
\newcommand{\cC}{\mathcal{C}}

Most intuition for (co)limits in $\infty$-categories comes from an explicit understanding of pushouts and pullbacks. While in a $1$-category, a pushout $A\amalg_C B$ corepresents pairs of morphisms $A\to X$ and $B\to X$ which *become equal* on $C$, in an $\infty$-category it instead corepresents triples of $A\to X$, $B\to X$ and a homotopy between the two resulting maps $C\to X$. More precisely, one really has a homotopy equivalence between $\Map(A\amalg_C B,X)$ and a space of triples of points in $\Map(A,X)$, $\Map(B,X)$ and a path in $\Map(C,X)$.

For example, in $\cS$, the $\infty$-category of spaces, a pushout can be described by the classical homotopy pushout construction, obtained by gluing the two ends of a cylinder $C\times [0,1]$ to $A$ and $B$. Similarly, in the derived category of $R$-modules, pushouts can be computed by a mapping cone of $C\to A\oplus B$, which similarly adds a copy of $C$ with a degree shift to $A\oplus B$, instead of directly identifying things.

These explicit descriptions dualize well to pullbacks, and translate into other contexts as well. They also can be connected to some computational understanding: We're used to conceptualize spaces, chain complexes and spectra through homology or homotopy groups, and these interact with pushouts and pullbacks in terms of various long exact sequences.

So most people are happy dealing with pushouts and pullbacks in $\infty$-categories in an intuitive way. When it comes to (co)limits over bigger diagrams, this intuition quickly fails us. The best ways to formally deal with them (e.g. cofinality arguments, Kan extensions,...) seem often orthogonal to the more intuitive tools we discussed above for pushouts and pullbacks. 

There is a very useful tool to decompose more general limits and colimits into pullbacks and pushouts:

**Proposition.** [4.4.2.2. in [HTT](https://www.math.ias.edu/~lurie/papers/HTT.pdf)] Given a simplicial set $K$ which is a pushout $K' \amalg_{L'} L$ where $L'\to L$ is a monomorphism. Let $p: K\to \cC$ be a diagram such that $p|_{K'}, p|_{L'}$ and $p|_{L}$ admit colimits. Then $p$ has a colimit, and
$$
\colim_K p = (\colim_{K'} p)\amalg_{\colim_{L'} p} \colim_L p.
$$

*Proof sketch.* Cones under $p$ with tip $Y$ correspond to pairs of cones under $p|_{K'}$ and $p|_L$ whose restricted cones under $p|_{L'}$ agree. Now the space of cones under $p|_{K'}$ with tip $Y$ is equivalent to the space of maps $\Map(\colim_{K'} p, Y)$, and thus one gets an equivalence
$$
\Map(\colim_K p,Y) = \Map(\colim_{K'} p, Y) \times_{\Map(\colim_{L'}p,Y)} \Map(\colim_L p, Y),
$$
which proves the desired equivalence.

Analogously, one has the same statement for limits (but still $K=K'\amalg_{L'} L$). As a special case, if $K$ is the union of $A,B\subseteq K$, one can decompose (co)limits over $K$ as pushouts (pullbacks) of colimits (limits) over $A$, $B$ and $A\cap B$.

This statement is well-known in the context of proving various generation properties for colimits, for example that a category has all finite colimits if it has an initial object and pushouts. But it is also incredibly useful in practice for getting a handle on (co)limits of various shapes. I want to demonstrate this with some examples:

## Coequalizers

Coequalizers are colimits of diagrams of the shape
```tikzpicture
\node at (0,0) {$\bullet$};
\node at (1,0) {$\bullet$};
\draw[->] (0.1,0.1) -- (0.9,0.1);
\draw[->] (0.1,-0.1) -- (0.9,-0.1);
```
which means more precisely: Diagrams indexed by the nerve of the $1$-category that looks like this. This nerve is given by two non-degenerate $1$-simplices glued at the endpoints. We may decompose this simplicial set $K$ as union of these two simplices $A$ and $B$, and have $A\cap B=\{0,1\}$ discrete. It follows that the coequalizer of a diagram $X_0\rightrightarrows X_1$ admits a description as pushout of the span

```tikzcd
X_0 \amalg X_1 \rar\dar & X_1\\
X_1,
```
where the two copies of $X_1$ come about as colimits over $A$ and $B$, but since $A=B=\Delta^1$ they have a terminal object.

## Restricted coequalizers

As a variant of the above, we can look at diagrams $X_0\to X_1 \rightrightarrows X_2$ where the two composites $X_0\to X_2$ agree, i.e. a pair of morphisms $X_1\to X_2$ which agree on $X_0$. In that case, the nerve consists of two $2$-simplices which share the $[0,1]$ and $[0,2]$ edges. We take those to be $A$ and $B$ again. $A$ and $B$ both have a terminal object, but their intersection is a span. We get a description as iterated pushout
```tikzcd
X_1\amalg_{X_0} X_2 \rar\dar & X_2\\
X_2,
```

## Zig-zag diagrams

For a diagram of shape

```tikzcd
 & X_1\ar[ld]\ar[rd] & & X_3\ar[ld]\ar[rd] & \\
X_0 & & X_2 & & X_4,
```
we can take $A$ and $B$ to be the left and right span, intersecting in a single object $X_2$. The colimit comes out as pushout of pushouts,
$$
(X_0\amalg_{X_1} X_2) \amalg_{X_2} (X_2 \amalg_{X_3} X_4).
$$
We can give a slightly different decomposition as well, where we let $A$ consist of the part of the diagram up to and including $X_3$, and $B$ just the arrow $X_3\to X_4$. In that case, the colimit over $A$ still is the pushout $X_0\amalg_{X_1} X_2$ (either by a direct cofinality argument or by splitting $A$ again at $X_2$ and using that one of the resulting parts has a terminal object), and the colimit over $B$ is just $X_4$ since we have a terminal object. We get a description of the same colimit as
$$
(X_0\amalg_{X_1} X_2) \amalg_{X_3} X_4.
$$
The same approach allows us to decompose a longer zig-zag as an iterated pushout.

## Day convolution over $\bN$

$\bN$ with its total ordering gives rise to a category such that one can think of functors $\bN\to \cC$ as "ascending filtrations" in $\cC$. Addition gives a symmetric-monoidal structure, and the resulting Day convolution on $\Fun(\bN,\cC)$ is the correct monoidal structure on filtered objects. To understand what this does, the pointwise formula for Kan extensions gives
$$
(F\otimes G)(n) = \colim_{i+j \leq n} F(i)\otimes G(j).
$$
Here the colimit is taken over a subposet of $\bN\times \bN$ consisting of all $(i,j)$ with $i+j\leq n$. How can we get a handle on that colimit? We can observe that it is actually the union of a sequence of subsets $S_{k,n-k} \subseteq \bZ\times \bZ$, where $S_{k,n-k}$ denotes the subset of all $(i,j)$ with $i\leq k$, $j\leq n-k$. Each of the $S_{k,n-k}$ clearly has a terminal object, and the intersection of two of them is again of this form! In fact, if we let $A_k$ denote the subset which is the union of $S_{0,n},S_{1,n-1},\ldots,S_{k,n-k}$, then $A_{k+1}=A_k \cup S_{k+1,n-k-1}$, and $A_k\cap S_{k+1} = S_{k,n-k-1}$. Since the entire $(F\otimes G)(n)$ is a colimit over $A_n$, this allows us to decompose $(F\otimes G)(n)$ inductively as a sequence of pushouts, with
$$
(F\otimes G)(n) = \colim_{A_n} F(i)\otimes G(j),
$$
and
$$
\colim_{A_{k+1}} F(i)\otimes G(j) = (\colim_{A_{k}} F(i)\otimes G(j)) \amalg_{F(k)\otimes G(n-k-1)} F(k) \otimes G(n-k).
$$
Note that this leads to the same description as the "iterated pushout" description of the colimit of the zig-zag
```tikzcd
[column sep=0.5cm]
& \ar[ld]\ar[rd]F(n-1)\otimes G(0) & & \ar[ld]\ar[rd] F(0)\otimes G(n-1)&\\
F(n)\otimes G(0)  & & \cdots & & F(0)\otimes G(n)
```
(which one can also directly relate with the full colimit using a cofinality argument.)

## Cubes

An $n+1$-dimensional cube can be written as nerve of the poset of subsets of $\{0,\ldots,n\}$. This does of course have terminal object, but we get an interesting simplicial subset of the full cube by looking at the nerve of the poset of *proper* subsets of $\{0,\ldots,n\}$. Let us denote this by $C_n$. For example, $C_1$ is the nerve of proper posets of $\{0,1\}$, which form a span, contained in the full cube as unit of all faces that do not touch the terminal vertex. More generally, $C_n$ is the unit of all faces of the $n+1$-dimensional cube that do not touch the terminal vertex. Colimits of shape $C_n$ appear in various places, for example in Goodwillie calculus.

We may decompose $C_n$ as follows: The $n+1$ many codimension $1$ faces of the cube which do not contain the terminal vertex are given as nerves of subposets, specifically the $i$-th such face is the nerve of the subposet consisting of subsets of $\{0,\ldots,n\}$ which do not contain $i$. Take $A$ to be the union of the first $n$ of those faces, and $B$ the last of those faces. Then $A\cap B$ sits in $B$ as the nerve of the subposet of proper subsets of $\{0,\ldots,n-1\}$, $B$ has a terminal object, and the colimit over $A$ agrees with the colimit over the subposet of proper subsets of $\{0,\ldots,n\}$ which contain $n$, by a cofinality argument.

We thus have decomposed
$$
\colim_{I\subsetneq \{0,\ldots,n\}} X_I = (\colim_{I\subsetneq \{0,\ldots,n-1\}} X_{I\cup \{n\}}) \amalg_{\colim_{I\subsetneq \{0,\ldots,n-1\}} X_I} X_{\{0,\ldots,n-1\}}
$$

For example, in the case of a $3$-dimensional cube,
$$
\colim_{C_2} X_I = (X_{02} \amalg_{X_2} X_{12}) \amalg_{X_0\amalg_{X_\emptyset} X_1} X_{01}.
$$

```tikzpicture
\begin{scope}[x=2cm,y=2cm]
\node[orange] (A) at (0,0) {$X_\emptyset$};
\node[orange] (A0) at (-0.866,.5) {$X_0$};
\node[orange] (A1) at (0.866,.5) {$X_1$};
\node[green!80!black] (A2) at (0,-1) {$X_2$};
\node[blue] (A01) at (0,1) {$X_{01}$};
\node[green!80!black] (A02) at (-0.866,-.5) {$X_{02}$};
\node[green!80!black] (A12) at (0.866,-.5) {$X_{12}$};
\draw[orange,->] (A) -- (A0);
\draw[orange,->] (A) -- (A1);
\draw[blue,->] (A0) -- (A01);
\draw[blue,->] (A1) -- (A01);
\draw[green!80!black,->] (A) -- (A2);
\draw[green!80!black,->] (A0) -- (A02);
\draw[green!80!black,->] (A1) -- (A12);
\draw[green!80!black,->] (A2) -- (A12);
\draw[green!80!black,->] (A2) -- (A02);
\end{scope}
```
