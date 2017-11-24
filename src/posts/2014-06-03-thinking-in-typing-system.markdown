---
layout: post
title: "thinking in typing system"
date: 2014-06-03 03:28:27 +0800
comments: true
categories: note
published: false
tags:
  - haskell
  - category theory
  - lambda calculus
  - typing
---

I am reading the paper 'Practical type inference for arbitrary-rank
types' [[pdf], [official webpage]]. Here are some ideas raised while I
was reading the paper. Those ideas might be not complete or not
conscientious.


## Concept of "subsumption"

**DISCLAIMER**: In this section I have a misunderstanding about the
subsumption relation. Please check on
[my question](http://stackoverflow.com/questions/24084808/what-do-we-mean-when-we-say-t1-is-more-polymorphic-than-t2)
on stackoverflow.

In the article it poses that we call types A and B in the relation
'subsumption' if A is more polymorphic than B.

The concept *polymorphism* is studied elsewhere and has different
meanings accordingly. Referring to
[wikipedia](http://en.wikipedia.org/wiki/Polymorphism_(computer_science)#Parametric_polymorphism),
there are three types of polymorphism. One is some like overloading
with different types *overloading*, another one *sub-typing* in object
oriented programming. We're discussing the polymorphism in haskell, in
which a polymorphic function/data type is regarded as a kind of
function/data type written generically. This kind of polymorphism is
called *parametric polymorphism*.

So what exactly is the relation 'more polymorphic than' understood? I
conceived the definition myself, not sure if it's accurate.

> To say a type `T1` is more polymorphic than `T2`, is to say that,
> all instances that satisfy the type T2, satisfy T1.

Below I will also use term 'subsumption' to represent this relation.
Obviously, the equivalence relation is also regarded as subsumption.
In this defintion, or exactly we can define the equivalent relation
as:

> T1 is equivalent to T2 if and only if
> T1 subsumes T2 AND T2 subsumes T1.

Let me give some examples to illustrate the relation subsumption'.

    k1: a -> b -> b
	k2: a -> a -> a
	k3: Int -> String
	k4: Int -> Int

In this case, `k1` is more polymorphic than `k2` because all objects
that satisfy type `k2` satisfy `k1`. While reversely the relation
doesn't apply. While looking at `k3` and `k4`, we know that they are
only rank-0 types. Therefore there are no saying of subsumption on
these types. If type matches, `k3` and `k4` are least polymorphic
types. (e.g. `k1` subsumes `k3`, `k2` subsumes `k4`) If the types do
not match, we cannot use *subsumption* describe their
relations. (e.g. `k3` and `k4`) Okay, there are the basic description,
let's go on the usage of subsumption predication in argument type
checking.

As mentioned in section 3.3,

> an argument is acceptable to a function if its type is more
> polymorphic than the function's argument type.

Given,

    foo: (a -> a -> a) -> Int
	bar: (a -> b -> b) -> Int

Obviously, `foo k4` is valid, as we only need to substitute `a` with
`Int`. Similarly obviously, `foo k2` is valid as the `foo`'s argument
type completely matches the `k2`'s type. Then how about `foo k1`?
Let's reason in this way. `foo` requires an argument of a function
that takes two arguments that are in identical type.

## What is a polymorphic kind type
(to be updated)

## What is a type class
(to be updated)

On `ghci`, we can check the *kind* of a type with `:k[ind]` command




[pdf]: http://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/putting.pdf
[official webpage]: https://research.microsoft.com/en-us/um/people/simonpj/papers/higher-rank/
