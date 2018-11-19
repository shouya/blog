---
layout: post
title: "Eilenberg-Moore category"
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

Given two categories $C$ and $D$, two morphisms that goes between them $L: C\to
D$, $R: D\to C$. We say $L$ is left adjoint to $R$ when the following condition
holds for all $a:C$, $b:D$.

$$C(a,R b) \simeq D(L a, b)$$

$C(R b, R b) \simeq D(LR b, b)$


## References

- [Category Theory III 4.2, Monad algebras part 3](https://www.youtube.com/watch?v=9f8PumwS2gU)
