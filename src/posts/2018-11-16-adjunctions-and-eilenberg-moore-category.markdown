---
layout: post
title: "Adjunctions and Eilenberg-Moore category"
date: 2018-11-16 16:20:35 +800
comments: true
tags:
  - explanation
categories:
  - category theory
lang: en
mathjax.xypic: true
---

This post is the sequel to the yesterday's post [Algebras compatible with list
monad gives rise to a monoid](/blog/algebras-in-list-monad). To illustrate how
Eilenberg-Moore category can be constructed with algebras on monads.

A question rises as we learned that an adjunction gives rise to a monad and a
comonad, does the converse hold as well? Is there a natural way to get an
adjunction from a monad? The answer to this is yes. The intuition is that with
monad $T$, we have plenty of choices for $C$, the point is to find out these two
functors $L:T\to C$ and $R:C\to T$ that satisfies the triangular identity.

I'll review from horizontal composition of natural transformations; then I'll
review the definition of adjunctions and how adjunctions give rise to a monad;
finally, I'll talk about how a Eilenberg-Moore category can be constructed and
why it is left adjoint to a monad category.

## Horizontal composition between natural transformations

Natural transformations, of course, can be composed. Natural transformations can
compose in two different ways: horizontally and vertically. Vertical composition
is just plain morphism composition with little surprise. Horizontal composition
is what gets more interesting. Here I'll denote the identity natural
transformation over $F$ by simply $F$ when there is no ambiguity.

Let's see what happens if we compose $\alpha\circ F$ in the following diagram
(called a "whiskering", because of its shape):

$$
\xymatrix {
E & & D \ar@/_2pc/[ll]^{G'}="a" \ar@/^2pc/[ll]_{G}="b" & C \ar[l]^{F}
\ar @2{->}_{\alpha} "b";"a"
}
$$

Now we take $a:C$, and see what we get on $(\alpha \circ F)_a$.

$$
\xymatrix {
G'(F a)  \\
& F a \ar[ul]^{G'} \ar[dl]_{G} & a\ar[l]^F \\
G(F a) \ar[uu]_{\alpha_{F a}}
}
$$

From the diagram we can see $(\alpha \circ F)_a = \alpha_{F a}$. Now let's see
another version of whiskering:

$$
\xymatrix {
E & D \ar[l]^{G} & & C \ar@/_2pc/[ll]^{F'}="a" \ar@/^2pc/[ll]_{F}="b"
\ar @2{->}_{\alpha} "b";"a"
}
$$

Similarly, take $a: C$, let's see what we get by composing $(G \circ \alpha)_a$.

$$
\xymatrix {
G (F' a) & F' a \ar[dd]_{\alpha_a}\ar[l]^G & \\
& & a \ar[ul]^{F'}\ar[dl]^{F} \\
G (F a)\ar[uu]_{G \alpha_a} & F a \ar[l]^G & \\
}
$$

Thus $(G\circ \alpha)_a = G \alpha_a$, not exactly symmetric to the left
whiskering version.

## Adjunction

Given two categories $C$ and $D$, two morphisms that goes between them $L: D\to
C$, $R: C\to D$. We say $L$ is left adjoint to $R$ when the following condition
holds for all $a:C$, $b:D$.

$$C(L b, a) \simeq D(b, R a)$$

By replacing $a$ with $L b$, we get

$$C(L b, L b) \simeq D(b, R(L b))$$

On the left we have $id_{L b}$, by the isomorphism of hom-set, this morphism
will select a morphism on the right: $b \to R (L b)$. This is an natural
transformation called a unit, denoted as $\eta: 1_D \to R\circ L$.

Similarly by replacing $b$ with $R a$, we can get the dual of unit -- the counit
-- on the left of the hom-set isomorphism: $\epsilon: L \circ R \to 1_C$.

An adjunction defined by unit and counit has to satisfy the triangle identities,
namely:

$$
\xymatrix {
L \ar[r]^{L\circ \eta} \ar@2{-}[rd]^{} & LRL \ar[d]^{\epsilon \circ L} \\
& L \\
}
$$

and:

$$
\xymatrix {
R \ar[r]^{\eta \circ R} \ar@2{-}[rd]^{} & RLR \ar[d]^{R\circ\epsilon} \\
& R \\
}
$$

Adjunction defines a very loose equivalence relation between functors $L$ and
$R$. Where one can think of $L$ as to introduce some structures, and $R$ to
remove the structures. A typical example for adjunction is the [free-forgetful
adjunction](https://ncatlab.org/nlab/show/free-forgetful+adjunction).

## Monad from Adjunction

In last post I mentioned an adjunction can give rise to a monad, here's how.

Given an adjunction $L \dashv R$, where $L: D\to C$ and $R: C\to D$, we define a
functor $T = R \circ L$ be the underlying functor of the monad. Let $\mu = R
\circ \epsilon \circ L$ be the monad multiplication and the monad unit -
$\eta$ - be the same as the unit for the adjunction.

We first check the types. $\mu: RLRL \to RL = T^2 \to T$, check. $\eta: 1 \to RL
= 1 \to T$, also check. Then we need to check if $\mu$ and $\eta$ works
subjecting to the monad laws.

First the unit laws:

$$
\xymatrix {
T \ar[d]_{T\circ\eta} \ar[r]^{\eta\circ T} & T^2 \ar[d]^{\mu} \\
T^2 \ar[r]^{\mu} & T
}
$$

This law can be derived from triangle identities by left composing the first
identity with an $R$ and right composing the second identity with an $L$. The
following diagram will automatically communites thanks to the triangle
identities.

$$
\xymatrix {
RL \ar[d]_{RL \circ \eta} \ar[r]^{\eta \circ RL} \ar@2{-}[rd]^{} & RLRL \ar[d]^{R\circ\epsilon\circ L} \\
RLRL \ar[r]_{R\circ\epsilon\circ L} & RL \\
}
$$

Now the multiplication law (or associativity law):

$$
\xymatrix {
T^3 \ar[r]^{\mu \circ T} \ar[d]_{T\circ\mu} & T^2\ar[d]^\mu \\
T^2 \ar[r]^\mu & T
}
$$

Or,

$$
\xymatrix {
RLRLRL \ar[rr]^{R\circ\epsilon\circ L \circ RL} \ar[dd]_{RL\circ R\circ\epsilon\circ L} & & RLRL\ar[dd]^{R\circ\epsilon\circ L} \\\
& \\
RLRL \ar[rr]^{R\circ\epsilon\circ L} & & RL
}
$$

This is the naturality square of $\epsilon$ left composed with $R$! Let's remove
the $R$ on the left and see what we get:

$$
\xymatrix {
LRLRL \ar[rr]^{\epsilon_{LRL}} \ar[dd]_{LR \circ f} & & LRL\ar[dd]^{f} \\\
& \\
LRL \ar[rr]^{\epsilon_L} & & RL
}
$$

This is exactly the naturality square of $\epsilon: LR\to1$. The monad
associativity law holds naturally.

In fact, an adjunction also gives rise to a comonad on $RL$, in a very similar
fashion, by defining the counit (`extract`) to be $\epsilon$ and $\delta$
(`duplication`) to be $L\circ\eta\circ R$.

## Adjunction from Monad and Algebra

Now you might be curious, if we have a Monad.


## References

- [Category Theory III 4.2, Monad algebras part 3](https://www.youtube.com/watch?v=9f8PumwS2gU)
- [Bartosz Milewski: Adjunctions](https://bartoszmilewski.com/2016/04/18/adjunctions/)
- [nLab: triangle identities](https://ncatlab.org/nlab/show/triangle+identities)
- [Catsters guide 2](https://byorgey.wordpress.com/catsters-guide-2/#adjunctions-part-1)
