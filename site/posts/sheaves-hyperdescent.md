---
title: Sheaves and hyperdescent
date: 2025-05-30
---

\DeclareMathOperator{\Open}{Open}
\DeclareMathOperator{\Et}{Étale}
\DeclareMathOperator{\Set}{Set}
\DeclareMathOperator{\Hom}{Hom}
\DeclareMathOperator{\Map}{Map}
\DeclareMathOperator{\Fun}{Fun}
\DeclareMathOperator{\Shv}{Shv}
\DeclareMathOperator{\PShv}{PShv}
\DeclareMathOperator{\colim}{colim}
\DeclareMathOperator{\cosk}{cosk}
\newcommand{\op}{\mathrm{op}}
\newcommand{\pt}{\mathrm{pt}}
\newcommand{\cU}{\mathcal{U}}
\newcommand{\cF}{\mathcal{F}}
\newcommand{\cS}{\mathcal{S}}
\newcommand{\cC}{\mathcal{C}}
\newcommand{\cD}{\mathcal{D}}

These are notes I wrote in preparation of a talk I gave in our topology seminar at UiO. They are supposed to be an introduction for how to work with sheaves with values in $\infty$-categories.

# Sheaves classically

Given a topological space $X$, we have a poset $\Open(X)$ of open subsets of $X$. A presheaf (with values in $\Set$) is a functor $\Open(X)^{\op}\to \Set$. A presheaf is a sheaf if sections (= elements of $\cF(U)$) can be recovered from open covers, that is: For $U = \bigcup U_i$, we have an equalizer diagram 
$$
\cF(U) \to \prod_i \cF(U_i) \rightrightarrows \prod_{i,j} \cF(U_i\cap U_j),
$$
which expresses that sections over $U$ can be given as a family of sections on each $U_i$, such that their images in $U_i\cap U_j$ agree.

There are several helpful special cases. For example:

1. $\cF(\emptyset)\cong\pt$
2. $\cF(U\cup V)\cong \cF(U)\times_{\cF(U\cap V)}\cF(V)$
3. If $U_n$ is an ascending sequence of opens with union $U$, $\cF(U) \cong \lim_n \cF(U_n)$. More generally, this holds for a filtered system of opens.

And in fact, one can prove by an inductive argument combined with a filtered colimit argument that the sheaf condition for arbitrary covers follows from the above three special cases, and some people take these three properties as definition of sheaves instead. The same definitions also work for sheaves with values in some other category which admits those limits.

If one attempts to naively make the same definition of sheaves with values in an $\infty$-category, one runs into problems very quickly. For example, in the above equalizer of sets, it didn't matter whether we included $\cF(U_i\cap U_j)$ for $i=j$ in the second product or not, but if we take this equalizer in an $\infty$-category, it does make a difference. It is also not hard to see that neither version of equalizers will be equivalent to the characterisation in terms of the above properties 1, 2, 3.

# Sieves

It turns out that the right approach to fix this goes back all the way to Grothendieck. In order to see how this goes, we first observe that we can express the sheaf condition for set-valued presheaves internally in the presheaf category $\PShv(X,\Set) = \Fun(\Open(X)^{\op},\Set)$, without direct reference to "evaluating $\cF$ on an open $U$". Indeed, $\cF(U) = \Hom(\underline{U},\cF)$, where we write $\underline{U}$ for the functor $\Hom_{\Open(X)}(-,U)$. (This is just the Yoneda lemma). We can very explicitly think of $\underline{U}$ as some kind of "indicator functor" which takes $V$ to $\pt$ if $V\subseteq U$, and to $\emptyset$ otherwise. For clarity, let us try to use this to restate the special case of the sheaf condition for $\cF(U\cup V)$ from above. This compares
$$
\cF(U\cup V) \cong \cF(U)\times_{\cF(U\cap V)}\cF(V),
$$
which we may write as
$$
\Hom(\underline{U\cup V},\cF) \cong \Hom(\underline{U}\amalg_{\underline{U\cap V}}\underline{V}, \cF),
$$
by turning the pullback outside into a pushout inside. What does this pushout look like? It assigns $\pt$ to any open which is contained in $U$ *or* $V$, and $\emptyset$ otherwise. So it is itself some kind of "indicator functor" for "small enough" subsets of $U\cup V$, and the map $\underline{U}\amalg_{\underline{U\cap V}}\underline{V}\to \underline{U\cup V}$ is not an isomorphism, but the sheaf condition expresses exactly that applying $\Hom(-,\cF)$ to it turns it into one.

**Definition.** [Grothendieck] A *sieve on $U$* is a subobject of $\underline{U}$. For a cover $\cU=\{U_i\}$ of $U$, the associated *covering sieve* $S_{\cU}$ is the subfunctor of $\underline{U}$ which assigns $\pt$ to every open set $V$ contained in at least one of the $U_i$, and $\emptyset$ to all other open subsets of $X$.

**Theorem.** A presheaf $\cF$ of sets is a sheaf if and only if for every covering sieve $S_\cU$ on $U$, the map
$$
\Hom(\underline{U},\cF) \to \Hom(S_\cU,\cF)
$$
is an isomorphism.

It is worth noting that this is in a sense more canonical than the sheaf condition in terms of equalizers: One recovers the equalizer description by writing $S_\cU$ explicitly as colimit of representable presheaves, but there can be multiple ways to do this (for example, we can write $S_{\{U_0,U_1\}}$ as pushout, leading to the pullback formula, or as coequalizer, leading to the equalizer formula.) The data of a covering sieve is also a bit more canonical than the data of an actual cover, for example a cover also involves a choice of indexing set, we might have redundant open sets etc., whereas in the sieve we really only specify a suitable class of "small open subsets".

If you're used to everything being a little harder in $\infty$-category theory, it may now be a little surprising to hear that *this perspective on sheaves generalizes in the completely naive way:*

1. Let $\cS$ be the $\infty$-category of spaces. The $\infty$-category of presheaves on $\Open(X)$ is $\Fun(\Open(X)^\op, \cS)$.
2. For any $\Set$-valued presheaf $\cF$ we may view $\cF$ as object of $\Fun(\Open(X)^\op,\cS)$ via the "discrete" functor $\Set\to \cS$. In particular, we have presheaves $\underline{U}$ and $S_\cU$. $\underline{U}$ is also still represented by $U$ in the $\infty$-categorical sense (since $\Open(X)^\op$ is a $1$-category), and Yoneda still gives $\Map(\underline{U},\cF) = \cF(U)$.
3. We can define sheaves as those objects $\cF\in \Fun(\Open(X)^\op,\cS)$ for which
$$
\Map(\underline{U},\cF) \to \Map(S_\cU,\cF)
$$
is an equivalence for all covering sieves $S_\cU$ on $U$.

One can analogously define $\cC$-valued presheaves as $\Fun(\Open(X)^\op,\cC)$, and then call $\cF$ a sheaf if $\Map_{\cC}(c,\cF(-))$ is a sheaf for every $c\in \cC$. In fact, if $\cC$ has enough limits, one can define $\cF(S_\cU)$ for a $\cC$-valued presheaf as the object of $\cC$ which corepresents $\Map(S_\cU, \Map_{\cC}(c,\cF(-)))$ (as functor in $c$), and then the condition that $\cF$ is a sheaf amounts to $\cF(\underline{U})\to\cF(S_\cU)$ is an equivalence in $\cC$.

**Example.** In an algebraic topology 1 lecture, one proves the excision axiom for singular homology by reducing it to the following statement: For any space $X$ and any open cover $\cU = \{U_i\}$, $S^\cU_*(X) \to S_*(X)$ induces an equivalence on homology, where $S_*(X)$ denotes singular chains, and $S_*^\cU(X)$ denotes the subcomplex of "small chains": Those chains where every simplex is contained in one of the $U_i$. Passing to cochains, we have a restriction map $S^*(X) \to S^*_{\cU}(X)$. Viewing $S^*(-)$ as a presheaf $\cF$ with values in the derived category $\cD(\bZ)$, this becomes exactly of the form $\cF(\underline{X})\to \cF(S_\cU)$, so the "small chains" version of excision exactly expresses that cochains form a sheaf with values in $\cD(\bZ)$.

# Recovering explicit limits

From the definition of sheaves in terms of covering sieves, we can recover descriptions of the sheaf condition in terms of limits.

1. For $U=\emptyset$, we have that $\underline{U}$ is the functor which maps $\emptyset \mapsto \pt$, and all other opens to $\emptyset$. for $\cU$ the empty cover, $S_\cU$ is the subfunctor which takes *everything* to $\emptyset$, i.e. the initial object of the presheaf category. The sheaf condition hence says that $\cF(\emptyset)\simeq \cF(S_\cU) = \pt$.
2. For $U$, $V$ two opens, we already saw that for $\cU$ the cover of $U\cup V$ by $U$ and $V$, $S_\cU = \underline{U} \amalg_{\underline{U\cap V}} \underline{V}$.
3. If $U = \bigcup U_i$ is an ascending union (indexed by the natural numbers, or more generally a filtered poset), then if $\cU$ denotes the cover of $U$ by the $U_i$, $S_\cU$ agrees with the filtered colimit $\colim \underline{U_i}$: Both take every open that's contained in at least one of the $U_i$ to $\pt$, and the rest to $\emptyset$. 

We can also prove the fact mentioned earlier that these three special cases imply the general sheaf condition:

**Proposition.** If $\cF(\underline{U}) \to \cF(S_\cU)$ is an equivalence for $\cU$ the empty cover of $\emptyset$, binary covers, and ascending covers indexed by a filtered poset, $\cF$ is a sheaf.

*Proof.* We first prove by induction that $\cF(\underline{U}) \to \cF(S_\cU)$ is an equivalence for finite covers. By assumption, this holds for covers with $0$ or $2$ opens, and for a cover by $1$ open we just have $S_\cU = \underline{U}$. Now if $U= \bigcup_{i=1}^n U_i$, we have a pushout square
```tikzcd
S_{\{U_1,\ldots,U_{n-1}\}} \rar\dar         & S_{\cU}\dar\\
\underline{\bigcup_{i=1}^{n-1} U_i} \rar & S_{\{\bigcup_{i=1}^{n-1} U_i, U_n\}}
```
Applying $\Map(-,\cF)$ takes the left map to an equivalence by induction, so also the right one. But also, $S_{\{\bigcup_{i=1}^{n-1} U_i,U_n\}} \to \underline{U}$ is taken to an equivalence by the binary case, to the composite $S_{\cU}\to \underline{U}$ is taken to an equivalence. This finishes the case of finite covers.
Now if $U=\bigcup_{i\in I} U_i$ is an arbitrary union, and we write $\cU$ for the corresponding cover, for any finite subset $J\subseteq I$ let $U_J$ be the union of $U_j$ for $j\in J$, and $\cU_J$ the corresponding finite cover. All the maps $S_{\cU_J} \to \underline{U_J}$ get taken by $\Map(-,\cF)$ to equivalences by the finite case. Taking a filtered colimit over all finite $J\subseteq I$, the left hand side becomes $S_{\cU}$, and the right hand side becomes $S_{\cU'}$ where $\cU'$ denotes the cover of $U$ by the $U_J$. This is an ascending union indexed over a filtered poset, so the final assumption ensures that $S_{\cU'}\to \underline{U}$ gives an equivalence under $\Map(-,\cF)$.

We can also see now the problem with the equalizer formula. For a cover $U_i$ of $V$, consider the coequalizer in $\Fun(\Open(X)^\op, \cS)$ of
$$
\coprod_{i,j} \underline{U_i\cap U_j}\rightrightarrows \coprod_i \underline{U_i}.
$$
Evaluated at $V$, the right hand evaluates to a discrete set with one element for each $i\in I$ where $V\subseteq U_i$. Analogously, the left hand evaluates to a subset of $I\times I$ measuring which intersections contain $V$. The coequalizer may be represented by a cell complex with one $0$-cell for each $U_i$ containing $V$, and one $1$-cell for each $U_i\cap U_j$ containing $V$. This is definitely connected (if $U_i$ and $U_j$ contain $V$, then also $U_i\cap U_j$, so we get a path connecting these two $0$-cells), but it might not be simply-connected! Indeed, if we also allow $i=j$, then we have $1$-cells which form loops, but even if we modify the above to $i\neq j$, we can still end up with a non-simply connected graph. For example if $V$ is contained in three open sets $U_1,U_2,U_3$, the resulting complex contains a triangle.
So this colimit is definitely not equivalent to $S_{\cU}$ (which should be $\pt$ or $\emptyset$ everywhere).

There is a way to fix this, by replacing the above coequalizer by a simplicial diagram which has $n+1$-fold intersections in level $n$. Earlier the colimit was represented by a graph with vertices given by all $U_i$ containing $V$ and edges between all those. For the simplicial variant, the geometric realisation will actually have one $n$-simplex for every $U_0,\ldots,U_n$ that all contain $V$. So it is contractible if there is at least one such $U_i$, and empty otherwise, exactly like $S_{\cU}$! This proves:

**Proposition.** $\cF$ is a sheaf if and only if for every cover $U=\bigcup U_i$, the augmented cosimplicial diagram
$$
\cF(U) \to \prod \cF(U_i) \rightrightarrows \prod \cF(U_i\cap U_j) \ldots
$$
is a limit diagram.

# Effective epimorphisms and sheaves as a localisation

Let's take a step back. In our presheaf category, we had objects $\underline{U}$, and morphisms $\coprod \underline{U_i} \to \underline{U}$ arising from covers. Out of such a $Y\to X$, we formed a simplicial diagram with terms $Y^{\times_X s+1}$, its Čech nerve. The $S_{\cU}$ was the colimit. For an $Y\to X$ let us more generally write $S_Y$ (or $S_{Y\to X}$ for the colimit of the Čech nerve. We call $Y\to X$ an *effective epimorphism* if $S_Y\to X$ is an equivalence.

**Example.** If $Y \to X$ is a morphism in $\cS$, $S_Y \to X$ is an equivalence onto the disjoint union of the components in the image. In particular, effective epimorphisms in $\cS$ are exactly those morphisms which are surjective on $\pi_0$.
*Proof.* Using that $\cS$ is locally cartesian closed (meaning that pullback functors between slice categories preserve colimits, so "colimits are computed fibrewise"), this reduces to the case $X=\pt$. In that case it says that for any nonempty $Y$, the realisation of $Y^{\times \bullet+1}$ is contractible. This colimit has an obvious nullhomotopy (by forming cones towards an arbitrary point).

Now in presheaves, the maps $\coprod \underline{U_i}\to \underline{U}$ are not effective epimorphisms. That would mean that $j_\cU\to \underline{U}$ is an equivalence. But *for the purposes of sheaves, these maps are equivalences*. Let's try to say that better: We have defined the full subcategory $\Shv(X,\cS) \subseteq \PShv(X,\cS)$ as those objects $\cF$ for which $\Map(-,\cF)$ turns all $j_\cU\to \underline{U}$ into equivalences. So $\Shv(X,\cS)$ is a left Bousfield localisation, where the left adjoint $\PShv(X;\cS)\to \Shv(X;\cS)$ is characterized by turning the morphisms $j_\cU\to \underline{U}$ into equivalences. So we uncover the following point of view: $\Shv(X,\cS)$ is a left Bousfield localisation of $\PShv(X,\cS)$ which universally makes all $\coprod \underline{U_i}\to \underline{U}$ into effective epimorphisms.

This viewpoint generalizes cleanly to much larger generality. For example, it allows us to replace the poset $\Open(X)$ by an arbitrary category $\cC$, and the covering families $U_i\to U$ by an arbitrary family of maps. (There are axioms that ensure that the resulting Bousfield localisation has the properties one would expect. One then calls the resulting family of "covering sieves" a Grothendieck topology on $\cC$, and $\cC$ a site.)

One example which necessitated this historically is the étale topology on a scheme $X$, which cannot be expressed purely in terms of open subsets in an ordinary topology. However, this also has a very nice topological analogue. For a space $X$, we can replace $\Open(X)$ by a bigger category $\Et(X)$ consisting of all $Y$ with a local homeomorphism $Y\to X$. This still contains open subsets, but also coverings of $X$ (among other things). We can again take covering families to be jointly surjective families of maps. It is then not very hard to see that the resulting sheaf categories are equivalent (this uses that any such surjective étale map $Y\to X$ splits locally on $X$). It allows us to view sheaves on $\Open(X)$ as sheaves on $\Et(X)$. But now if $Y\to X$ is a $G$-principal covering map, the morphism
$$
\underline{Y}_{hG} \to \underline{X}
$$
becomes an equivalence in $\Shv(\Et(X),\cS)$, since this can be checked locally, and locally $X$ looks like the quotient of $Y$ by a free $G$-action. Applying $\Map(-,\cF)$ for any sheaf, we learn that $\cF(X) = \cF(Y)^{hG}$. For example, for the previous "cochains" sheaf, we learn useful things like $C^*(BG)\simeq \bZ^{hG}$.

# Hypercovers

A common issue in algebraic geometry is that for a cover of a scheme $X$ by affine $U_i$, the intersections $U_i\times_X U_j$ are not necessarily affine themselves. Many arguments then proceed in two steps, using an affine cover on $X$ and then another affine cover $V_{ijk}$ of each $U_i\times_X U_j$. In the setting of sheaves in $1$-categories, one can combine these two steps into one by considering the diagram
$$
\coprod V_{ijk} \rightrightarrows \coprod U_i \to X
$$
as a "hypercover", the condition here is that $\coprod U_i\to X$ is a covering and $\coprod V_{ijk} \to (\coprod U_i)\times_X (\coprod U_i)$ is also a covering map.

We now want a version of this notion that also works in $\infty$-categories, so we want to pass to an augmented simplicial diagram. What should the higher conditions look like? So we are dealing with an augmented simplicial diagram
$$
\ldots Y_1 \rightrightarrows Y_0 \to X,
$$
and will have a condition on $Y_0 \to X$, a condition on $Y_1\to Y_0\times_X Y_0$, and higher conditions. To get an idea, let us first work in the category of sets and the case $X=\pt$. In that case, $Y_\bullet$ is just a simplicial set, $Y_0\times_X Y_0 = Y_0\times Y_0$ is the set $\Hom(\partial \Delta^1, Y_\bullet)$, and $Y_1=\Hom(\Delta^1, Y_\bullet)$. So the second condition says something about the restriction map $\Hom(\Delta^1,Y_\bullet)\to \Hom(\partial\Delta^1, Y_\bullet)$. So the general condition should say something about a morphism $Y_n \to \Hom(\partial\Delta^n, Y_\bullet)$. The target of this can be written as limit of $Y_i$'s (since $\partial\Delta^n$ can be written as colimit of simplices), and this is what works more generally. In general, there is a functor $\cosk_{n-1}$ from augmented simplicial objects to augmented simplicial objects, which restricts from $\Delta_{+}$ to $\Delta_{\leq n-1, +}$ and right Kan extends back, on simplicial sets this has the effect of making $(\cosk_{n-1}Y)_n = \Hom(\partial\Delta^n, Y)$. Let us also call a simplicial object $n-1$-coskeletal if it is in the image of $\cosk_{n-1}$.

**Definition.** We call an augmented simplicial sheaf a hypercover if for each $n$, the map $Y_n \to (\cosk_{n-1}Y)_n$ is an effective epimorphism (in sheaves).

For example, the Čech nerve of a morphism $Y\to X$ is actually $0$-coskeletal, so it is a hypercover if and only if $Y\to X$ is an effective epimorphism. More generally, a "two-step hypercover" consisting of an effective epimorphism $Y_0\to X$ and an effective epimorphism $Y_1\to Y_0\times_X Y_0$ Kan extends to a kind of "2-Čech nerve" which is also a hypercover.

When can we recover $\cF(X)$ from $\cF(Y_\bullet)$ as cosimplicial limit? If we want this to work for *all* $\cF$, we need that $\colim Y_\bullet \simeq X$ in sheaves. 

**Definition.** We call a hypercover effective if $\colim Y_\bullet \simeq X$.

**Lemma.** [HTT 6.5.3.9] If a hypercover is $n$-coskeletal, it is automatically effective.

So if we inductively think of a hypercover as obtained by "further covering the $n$-fold intersections of the opens", and we only do this finitely often, then we get a sheaf condition for the hypercover for free from the usual sheaf condition, just as in algebraic geometry. This works more generally for sheaves with values in an $n$-category (i.e. an $\infty$-category where all mapping spaces are $n$-truncated):

**Proposition.** For a sheaf $\cF$ with values in an $n$-category, we have
$$
\cF(X) \simeq \lim \cF(Y_\bullet)
$$
for *any* hypercover.

*Proof.* In an $n$-category, the cosimplicial limit agrees with the limit over its restriction to $\Delta_{\leq n}$. In particular, the limit is the same for $Y_\bullet$ or $\cosk_{n-1}Y_\bullet$.

But if we are dealing with $\infty$-categories, it turns out that this condition is strictly stronger than the usual sheaf condition. Since we say that $\cF$ "satisfies descent" with respect to a family of coverings if it is a sheaf, we say that $\cF$ "satisfies hyperdescent" if we have $\cF(X) \simeq \lim \cF(Y_\bullet)$ for all hypercovers. From the above result for $n$-categories, this holds for example for sheaves which take values in $n$-truncated spaces. This also leads to a very useful alternative characterisation of hyperdescent:

**Proposition.** A $\cS$-valued sheaf $\cF$ satisfies hyperdescent if its Postnikov tower converges, i.e. if $\cF \simeq \lim_n \tau_{\leq n} \cF$, where $\tau_{\leq n} \cF$ denotes the sheafification of the pointwise Postnikov truncation.

We call a site (or more generally a category of sheaves, i.e. an $\infty$-topos) hypercomplete if *any* sheaf satisfies hyperdescent. From the above characterisation, this is related to Postnikov towers of sheaves converging, and often closely related to some kind of finite cohomological dimension.
