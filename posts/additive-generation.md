---
title: Additively generating modular units
date: 04.05.2025
---

This post answers affirmatively a question asked by user Irwin at [Mathoverflow](https://mathoverflow.net/questions/491704).

**Definition.** For any positive integer $m$, let $S_m\subseteq (\bZ/m)^\times$ be the smallest subset which contains $1$ and is closed under addition in the following sense: If $x,y\in S_m$ and $x+y\in (\bZ/m)^\times$, then also $x+y\in S_m$.

As was already pointed out in the MO thread, clearly $S_m=\{1\}$ if $m$ is even. Moreover, one explicitly checks $S_{15}=\{1,2,4,8\}$, so $S_{15}$ is a proper subset of $(\bZ/15)^\times$. The MO question amounts to the following:

**Conjecture.** For odd $m$, $S_m = (\bZ/m)^\times$ if and only if $15$ does not divide $m$.

The "only if" part is straightforward: If $d\mid m$, the map $(\bZ/m)^\times \to (\bZ/d)^\times$ takes $S_m$ into $S_d$ (for example because the preimage of $S_d$ is also closed under "coprime addition"). So if $S_d \neq (\bZ/d)^\times$, then also $S_m\neq (\bZ/m)^\times$.
The evidence discussed for this statement in the MO thread is extremely thin, the OP mentions having checked it for $m<100$. I hacked together a small python script implementing the most naive algorithm to check for a slightly larger counterexample; this was practical up to $m<1200$ and further confirmed the guess. Now usually numerical evidence is not worth a lot in number theory, even if carried out into the billions, and my expectation was that there would probably some large counterexample made up of many prime factors conspiring in exactly the right way, but that it would be impossible to find. While trying to deduce as many constraints on such a counterexample, I was very surprised to find the following:

**Theorem.** The conjecture is true!

We will prove this below. Our strategy will be to first deduce a couple of basic results about small numbers in $S_m$, and then produce some powerful multiplicative properties of $S_m$. Among those will be one that allows us to decompose any hypothetical counterexample into smaller ones, leading to a contradiction.

**Lemma 1.** If $p_1 < p_2 < \ldots$ denotes the prime factors of $m$ (odd and not divisible by $15$) ordered by size, then $S_m$ contains all numbers coprime to $m$ between $1$ and $p_3$ (or all numbers coprime to $m$ if $m$ has fewer than $3$ prime factors).

*Proof.* $S_m$ clearly contains $1,2,\ldots,p_1-1$. We now prove by induction over $k$ that $S_m$ contains all numbers coprime to $m$ which are smaller than $\min(kp_1,p_3)$, starting with $k=1$. Assume this statement for $k=1$. Among the numbers $kp_1+1,\ldots,kp_1+p_1-1$, at most one is divisible by $p_2$, and all others are coprime to $m$. There are three cases:

- $kp_1+1$ and $kp_1-1$ are not divisible by $p_2$. Then we may reach $kp_1+1 = (kp_1-1)+2$ and from there all of the numbers from $kp_1+1$ to $kp_1+p_1-1$ using steps of $1$ or $2$.
- $kp_1+1$ is not divisible by $p_2$, but $kp_1-1$ is. In that case, if $p_1>3$, we may reach $kp_1+1 = (kp_1-2)+3$ using a step of $3$ and all numbers to $kp_1+p_1-1$ using steps of size $1$. If $p_1=3$, $p_2>5$, and we can use $kp_1+1 = (kp_1-4)+5$ instead.
- $kp_1+1$ is divisible by $p_2$. In that case, we may reach $kp_1+2$ using $(kp_1-2)+4$, and all numbers until $kp_1+p_1-1$ using steps of $1$.

This shows in particular that a potential counterexample must have at least $3$ prime factors. Similar "additive" arguments may be used to reach further, depending on bounds from below on the smallest prime factors. They do not alone seem sufficient to prove the theorem though, since in principle many prime factors can collude to produce long stretches of numbers not coprime to $m$, and we will require "multiplicative" arguments in addition. These are based on the following observation:

**Lemma 2.** $S_m$ is a multiplicative subgroup of $(\bZ/m)^\times$.

*Proof.* Take $r\in S_m$. The set of all $x\in S_m$ with $rx\in S_m$ contains $1$, and whenever $x,y$ lie in that set and $x+y\in (\bZ/m)^\times$, also $x+y$ lies in that set. So it contains $S_m$ and $rS_m\subseteq S_m$. So $S_m$ is closed under multiplication, and since every element of $(\bZ/m)^\times$ has finite order, it is a subgroup.

This allows us not only to multiply, but also to divide in $S_m$. We use this to deduce a decomposition property for $S_m$. Recall that if $m=ab$ with coprime $a,b$, then $(\bZ/m)^\times = (\bZ/a)^\times \times (\bZ/b)^\times$. Since the projections take $S_m$ to $S_a$ and $S_b$, $S_m$ is a subgroup of $S_a\times S_b$. We have the following:

**Lemma 3.** For coprime odd $a,b$, if $S_{ab}\subseteq (\bZ/a)^\times \times (\bZ/b)^\times$ contains $(1,2)$ (or $(2,1)$), then $S_{ab}=S_a\times S_b$.

*Proof.* Note first that if $S_{ab}$ contains $(2,1)$, it also contains $(1,2)$, since it contains $(2,2)=2=1+1$, and we may divide since it is a subgroup. Consider the set $S$ of all $x\in S_a$ such that $(x,1)\in S_{ab}$. If we have $x,y\in S$ such that $x+y\in (\bZ/a)^\times$, $(x+y,2)\in S_{ab}$. Dividing by $(1,2)$, we learn that $(x+y,1)\in S_{ab}$, so $x+y\in S$. Since $1\in S$ also, we have that $S_a\subseteq S$, hence $S_a\times \{1\}\subseteq S_{ab}$. Analogously, $\{1\}\times S_b\subseteq S_{ab}$, so $S_{ab}=S_a\times S_b$.

If we had a counterexample $m$ that admits such a decomposition $m=ab$ and $(1,2)\in S_{ab}$, then $S_m = S_a\times S_b$ would mean that one of $a,b$ also is a counterexample, giving a smaller one. If we find such a decomposition for any hypothetical *minimal* counterexample, this gives a contradiction. There is a helpful perspective here coming from algebraic geometry: For $m= \prod p_i^{e_i}$, the decomposition $(\bZ/m)^\times = \prod (\bZ/p_i^{e_i})^\times$ allows us to view elements of $(\bZ/m)^\times$ as "functions" on a "space" $\mathrm{Spec}(\bZ/m)$ with one point for every prime divisor $p_i$ of $m$, where the value at $p_i$ lands in $(\bZ/p_i^{e_i})^\times$. Our strategy can thus be summarized as trying to find elements of $(\bZ/m)^\times$ which attain values $1$ and $2$ and are non-constant.

Elements of the form $(1,2)$ are hard to produce. Fortunately, we can construct them from $\pm 1$-valued elements. This is easiest in the case where $3$ does not divide $m$:

**Lemma 4.** If $m=ab$ with coprime odd $a,b$ not divisible by $3$, and $(1,-1)\in S_{ab}$, $S_{ab}=S_a\times S_b$.

*Proof.* $S_{ab}$ contains $(3,3)$, hence $(4,2)$. Dividing by $2$, it contains $(2,1)$ and can be decomposed using Lemma 3.

If $3$ divides $m$, we have a similar, but slightly more complicated statement.

**Lemma 5.** If $m=ab$ with coprime odd $a,b$, with $a$ divisible by $3$, and both $a,b$ not divisible by $5$, and $(1,-1)\in S_{ab}$, $S_{ab} = S_a\times S_b$.

*Proof.* $S_{ab}$ contains $(5,5)$ and $(5,3) = (4,4)+(1,-1)$. It also contains $(5,-3)=(5,3)\cdot (1,-1)$, and thus $(10,2) = (5,5)+(5,-3)$. Furthermore, it contains $(10,6)=(5,3)+(5,3)$, and the quotient $(1,3) = (10,6)/(10,2)$. Now finally it contains $(2,4)=(1,3)+(1,1)$, and dividing by $2$, it contains $(1,2)$ and can be decomposed using Lemma 3.

Now we only need to construct such values which are $\pm 1$ everywhere:

**Lemma 6.** If $p_1$ and $p_2$ are the two smallest primes dividing $m$, $S_m$ contains an element which is $\pm 1$ everywhere, not constant, and $+1$ at $p$.

*Proof.* $(p_2+p_1)/2$ and $(p_2-p_1)/2$ are both coprime to $m$ and smaller than $p_2$, hence contained in $S_m$ by Lemma 1. Their quotient $q$ is $+1$ mod $p_1$ and $-1$ mod $p_2$. For a prime $p_i$ dividing $m$ with multiplicity $e_i$, let us refer as "the order of $q$ at $p_i$" to the order of $q$ in $(\bZ/p_i^{e_i})^\times$. We observe that the order of $q$ at $p_1$ is a power of $p_1$ (hence odd), and the order of $q$ at $p_2$ is $2$ times a power of $p_2$. We may raise $q$ to the power of the product of the odd part of all those orders to obtain a $q'$ whose order is a power of $2$ everywhere, and $1$ at $p_1$ and $2$ at $p_2$. Now either the order of $q'$ at all other $p_i$ is at most $2$, in which case $q'$ is $\pm 1$ everywhere, $-1$ at $p_2$ and $+1$ at $p_1$, or we may raise $q'$ to a $2^k$-th power in order to obtain $q''$ whose order is at most $2$ everywhere and nonconstant, so that $q''$ is $\pm 1$ everywhere, $+1$ at $p_1$, and $-1$ somewhere.

With this, we are now ready to prove the theorem.

*Proof of the Theorem.* Assume $m$ is a minimal counterexample. By Lemma 1, $m$ has at least three prime factors. Let $q\in S_m$ be an element (guaranteed by Lemma 6) which is $\pm 1$ at all primes $p_i$ dividing $m$, $+1$ at the smallest prime $p_1$, and not constant. Let $a$ be the product of the prime powers modulo which $q$ is $1$, and $b$ the product of the prime powers modulo which $q$ is $-1$, so that $q=(1,-1)$ in $(\bZ/m)^\times = (\bZ/a)^\times \times (\bZ/b)^\times$. Now if $p_1>3$, Lemma 4 applies to show $S_m = S_a\times S_b$. If $p_1=3$, Lemma 5 applies to show $S_m=S_a\times S_b$, since $a$ contains the smallest prime factor and is thus divisible by $p_1=3$. In either case, one of $a$ and $b$ must also be a counterexample, contradicting minimality.
