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

A question arises as we learned that an adjunction gives rise to a monad and a
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

In the rest of the articles, I'll using the result from this sections a lot and
won't get into the details.

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

## Monad from adjunction

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

This is exactly the naturality square of $\epsilon: LR\to1$. Thus we proved that
the monad associativity law holds naturally just because $\epsilon$ is a natural
transform.

In fact, an adjunction also gives rise to a comonad on $RL$, in a very similar
fashion, by defining the counit (`extract`) to be $\epsilon$ and $\delta$
(`duplication`) to be $L\circ\eta\circ R$.

## Monad algebra category

Now let's get back to monad algebra which we discussed extensively in the last
post. Last time we proved that an algebra compatible with a list monad is a
monoid, it's a rather surprising finding.

Let's now revisit the definition of monad compatible algebra. Given a monad $(T:
C \to C, \eta: 1 \to T, \mu: T^2 \to T)$ and an algebra $(a: C, \sigma: T a \to
a)$, we define the coherence conditions as follows:

- $\sigma \circ \eta_a = 1_a$
- $\sigma \circ \mu_a = \sigma \circ T \sigma$

Algebras that satisfy these conditions are compatible with the given monad, and
we call them monad algebras.

Not very surprisingly, monad algebras for a monad $(T, \tau, \mu)$ do form a
category. The objects are $(a:C, \sigma:T a\to a)$, and the morphisms are the
same morphisms in $C$. This category is called monad algebra category, or
Eilenberg-Moore category, denoted as $\textbf{mAlg}_T$, or $C^T$.

Identity morphism on $(a,\sigma)$ in $C^T$ is just the identity morphism on $a$
in $C$. Morphism compositions also work the same way as they're in $C$.

I just missed one thing. Before we can claim $(T a, \mu_a)$ is a monad algebra,
we need to check if this algebra meets the coherence conditions.

First, $\mu \circ \eta_{T a} = 1_{T a}$. This one is just the right identity law
for monad, so it automatically holds. Then we check $\mu\circ \mu_{T a}=\mu\circ
T \mu$. It's just the monad associativity (multiplication) law! See how these
all fits so perfectly. Just because of monad laws, $\mu_a$ will always be a
compatible algebra on $T a$.

## Free monad algebra

The cannonical functor from $C^T$ to $C$ is the forgetful functor that "forget"
about the algebra part, i.e. $U: (a,\sigma) \mapsto a$. In addition, $U: f
\mapsto f$ for morphisms.

It's much more tricker to define the free functor $F: C\to C^T$. We need to find
a valid algebra for every $a$. Fortunately, it turns out we already have a very
good candidate -- $\mu_a: T (T a) \to T a$ from the monad. $\mu$ is a natural
transformation so it works on any $a$, for every $a$ we have an algebra from $T
(T a)$ to $T a$.

Now we can define the free functor $F: C \to C^T$ as $a \mapsto (T a, \mu_a)$,
which is kind of neat. Of course, $F$ should also maps $f: a \to b$ to $F f: T a
\to T b$, which is just $T f$.

## Monad algebra adjunctions

Now we have got a pair of free and forgetful functors, it's time to prove they
are really adjoint. We say $F$ is left adjoint to $U$, or $F \dashv U$.

To play with adjunction, we need to first define our pair of natural
transformations $\eta: 1_C \to U\circ F$ and $\epsilon: F\circ U \to 1_{C^T}$.

Let's write down $\eta$ in its components form: $\eta_a: a \to U (F a)$, and we
know $U (F a) = U (T a, \sigma) = T a$. We can just use the unit $\eta$ from the
monad!

What about $\epsilon$? $\epsilon_{(a,\sigma)}: F (U (a,\sigma)) \to (a,\sigma)$.
Where $F (U (a, \sigma)) = F a = (T a, \mu_a)$. We need to find a map from $(T
a, \mu_a)$ to $(a, \sigma)$. We can just use the forgotten evaluator $\sigma: T
a\to a$.

In order for the unit and counit to form an adjunction, we need to check the
triangle laws.

$$
\xymatrix {
(T a, \mu_a) \ar[r]^{F\circ \eta_a} \ar@2{-}[rd]^{} & (T(T a), \mu_{T a}) \ar[d]^{\mu_{a}} \\
& (T a, \mu_a) \\
}
$$

We can check the algebras' carrier types.

$$
\xymatrix {
  T a \ar[r]^\eta & T (T a) \ar[r]^\mu & T a
}
$$

This is essentially just $\mu \circ \eta$, which is equal to the identity by the
right unit law on monad. The evaluator follows automatically, because they're
just regular morphisms in $C$. Then we check another triangle identity:

$$
\xymatrix {
a \ar[r]^{\eta_{U (a, \sigma_a)}} \ar@2{-}[rd]^{} & T a \ar[d]^{U\circ\sigma_a} \\
& a \\
}
$$

Omitting the unimportant part, we get:

$$\xymatrix { a \ar[r]^{\eta_a} & T a \ar[r]^{\sigma_a} & a }$$

Looking familiar? Yes, $\sigma \circ \eta = 1$ must hold by one of the coherence conditions for $\sigma$ to be compatible with our monad.

Therefore we have shown the Eilenberg-Moore category $C^T$ is left adjoint to a
monad category $C$. I'm not sure if it's valid to say a category is left adjoint
to another category. But anyway, we have discovered another free-forgetful
functor pair that are adjoint to each other, and what makes it so fascinating is
that it's an adjunction we can get from ANY monad.

## References

- [Category Theory III 4.2, Monad algebras part 3](https://www.youtube.com/watch?v=9f8PumwS2gU)
- [Bartosz Milewski: Adjunctions](https://bartoszmilewski.com/2016/04/18/adjunctions/)
- [nLab: triangle identities](https://ncatlab.org/nlab/show/triangle+identities)
- [Catsters guide 2](https://byorgey.wordpress.com/catsters-guide-2/#adjunctions-part-1)
