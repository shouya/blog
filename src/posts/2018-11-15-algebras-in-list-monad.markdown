---
layout: post
title: "Algebras compatible with list monad gives rise to a monoid"
date: 2018-11-15 15:14:35 +800
comments: true
tags:
  - explanation
categories:
  - category theory
lang: en
---

I'm currently watching Dr. Bartosz Milewski's video lecture series on category
theory on YouTube. In this lecture [Category Theory III 4.2, Monad algebras part
3](https://www.youtube.com/watch?v=9f8PumwS2gU), he stated an interesting fact
that in an algebra that is compatible with the list monad is a monoid.

In his video lecture he explained it very briefly and drawn the conclusion
quickly before I can follow to convince myself on the fact. After that, I saw in
comment below someone else had the similar questions on the notation Dr.
Milewski uses. I derived it from the beginning and made clear my understanding.
I think the outcome is kind of interesting that worths a post about it.

I want to write this post in a very beginner-friendly manner, to explain the
concepts to people whom knew only some basic category theory. Hopefully it's
gonna also help me to clear things out.

## Monoid

Monoid captures the generalized idea of multiplication. A monoid on a set $S$ is
made of an element $\eta\in S$ called unit and binary operation $\mu: S \times S
\to S$ called multiplication that satisfies these laws:

- left identity law: $\mu(\eta, a) = a$, for $a \in S$
- right identity law: $\mu(a, \eta) = a$, for $a \in S$
- associativity law: $\mu(a,\mu(b,c)) = \mu(\mu(a,b),c)$, for $a,b,c\in S$

Here are some common examples of monoids:

- list monoid on list, where $\eta$ is empty list and $\mu$ is the append operator (`++` in Haskell)
- additive monoid on integer, where $\eta$ is $0$ and $\mu$ is $+$
- multiplicative monoid on integer, where $\eta$ is $1$ and $\mu$ is $\times$

## Monad

A monad is defined as a functor $T$ along with two natural transformations
$\eta: a \to T a$ called unit and $\mu: T^2 a\to T a$ called multiplication that
satisified these laws:

- identity law:

$$
\require{AMScd}\begin{CD}
a @>\eta>> T a \\
@VVTV  @VTVV \\
T a @>T \eta>> T^2 a
\end{CD}
$$

- mutiplication law:

$$
\begin{CD}
T^3 a @>T \mu>> T^2 a \\
@VV\mu V @VV\mu V \\
T^2 a @>\mu >> T a
\end{CD}
$$

These laws are actually monoid laws (left/right identity law and associativity
law) in the category of endofunctors.


## List monad

The list functor is a monad with $\eta$ and $\mu$ defined as following:

```haskell
η x = [x]
μ xs = concat xs
```

where $\eta$ sends a value to a singleton list containing that value, and $\mu$
is concatenation function of a list of lists.

It's easy show the monad laws for this list monad hold, since it's not today's
focus, I'll skip it.

## Algebra

An algebra on functor $F: C\to D$ is given by a tuple $(a, \sigma)$, where $a$
is an object in $C$ and $\sigma$ is an endofunction $F a \to a$. It's worth
noting that an algebra is not a natural transformation as it seems.

A natural transformation has no knowledge on its component, therefore must be a
polymorphic function. This restriction is, however, not required for an algebra.
In an algebra, $\sigma$ is bound to a specific object $a: C$, thus it can do
transformations on $a$ or generate an $a$ from nowhere.

An algebra can be viewed as a map to evaluate a functor on values (e.g.
algebraic expression) into an single value. Here are some examples of algebras:

- `sum` on list of additive numbers
- `length` on polymorphic list
- `foo (x:_) = x; foo [] = 1` on list of integers
- `eval: ExprF a -> a` on an expression of type `a`

In an algebra the functor plays the role to form an expression, and the $\sigma$
function evaluates it.

## Category of algebras

Algebras on a functor $F:C\to D$ can form a category, where the objects are the
algebras $(a, \sigma)$, and the morphisms from $(a,\sigma)$ to $(b,\tau)$ can be
defined as morphisms in $C(a,b)$. We now show that the morphisms are composible:

$$
\begin{CD}
F a @>F f>> F b \\
@VV\sigma V @VV\tau V \\
a @>f>> b
\end{CD}
$$

Since $F$ is a functor, this diagram automatically commutes.

## Monad algebra

Given an endofunctor $T$, A monad algebra on $T$ is a monad on $T$ along with an
compatible algebra on $T$. A monad algebra contains these operations:

- $\eta: a \to T a$
- $\mu: T^2 a \to T a$
- $\sigma: T a \to a$

Be noted that a specific algebra can have a specific $a$.

To make the algebra compatible with the monad, we need to impose these two conditions:

- with unit, $(\sigma \circ \eta) a = a$
- with multiplication, the diagram below should commute
$$
\begin{CD}
T^2 a @>\mu>> T a \\
@VV T\sigma V @VV\sigma V \\
T a @>\sigma>> a
\end{CD}
$$

These two conditions are strong. Only a few algebras on $T$ is compatible with a
given monad on $T$. For example, in the list monad of integers, the condition
requires `η [x] = x`; this eliminates all algebras that don't satisfy this
property, like the `length` algebra.

## Algebra on list monad

Now we finally get to the interesting one. There are many monad-compatible
algebras on list, for example: `sum`, `product`, `concat` etc. These algebras
does various of things but there's one thing in common: they seems all related
to some monoids. In fact they indeed do. We will now prove it.

First we see what properties do algebras on list monad hold. By the
compatibility we discussed eariler, we always have:

- `η [x] = x` and,
- `(σ∘Tσ) x = (σ∘μ) x`, where $\mu$ is the `concat` operator for list

Let `σ [] = e` and `σ [x,y] = x <> y`, we now show `e` is an unit and `x <> y`
is the multiplication operator in a monoid.

We now prove the left identity law for the monoid. We prove this by evaluting
`(σ∘Tσ) [[], [x]]` in two ways. On the left we got `(σ∘Tσ) [[], [x]] = σ [e, x]
= e <> x`, on the right we got `(σ∘Tσ) [[], [x]] = (σ∘μ) [[], [x]] = σ [x] = x`.
This shows `e <> x = x`. The right identity law can be proved in the same
fashion.

Now the associativity law. First we get `(σ∘Tσ) [[x,y],z] = [x <> y, z] = (x <>
y) <> z`, and `(σ∘Tσ) [x,[y,z]] = [x, y<>z] = x <> (y <> z)`. We also no that
they both equal to `(σ∘μ) [[x,y],z] = (σ∘μ) [x,[y,z]] = σ [x,y,z]`. For
consistency, this means `σ [x,y,z]` must be defined as `(x <> y) <> z` or `x <>
(y <> z)` and they are both equal. Now we have proved that the algebra must
gives rise to a monoid, and $\sigma$ is the mconcat function.

## References

- [Category Theory III 4.2, Monad algebras part 3](https://www.youtube.com/watch?v=9f8PumwS2gU)
- [Understanding F-Algebras](https://bartoszmilewski.com/2013/06/10/understanding-f-algebras/)
- [Monad (category theory)](https://en.wikipedia.org/wiki/Monad_(category_theory))
