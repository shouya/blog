---
layout: post
title: "banana algebra"
date: 2015-06-08 22:31:25 -0400
comments: true
categories:
    - "category theory"
    - haskell
tags:
    - math
---

Abstract: I'll be talking about the F-Algebra Category and related
applications of F-Algebra in function programming in this post.

## F-Algebra

So first of all, an algebra over a type $a$ is set of functions that
converts an algebra structure $f a$ to $a$ [CoAlg]. An algebra consists of:

* An algebra structure: $\rm{F}$
* A carrier type: $\rm{a}$
* A total function: $\rm{F(a)} \to \rm{a}$

An example of an algebra looks like:

* Algebra struct: `data F1 a = Zero | One | Plus a a`
* A carrier type: it could be any instance of `a`: `Int`, `String`, etc.
* A total function:

```haskell
f1 :: F1 Int -> Int
f1 Zero       = 0
f1 One        = 1
f1 (Plus a b) = a + b
```

Or we can have:

```haskell
f1' :: F1 String -> String
f1' Zero       = ""
f1' One        = "1"
f1' (Plus a b) = a ++ b
```

## F-Algebra Arrows

All algebras for an algebra structure $\rm{F}$ forms a category
$\cal{C}$. The objects are, of course, the algebras, while the arrows
are defined as morphisms between each two pair of algebras that
transforming the carrier type:
$\hom_{\cal{C}}(\rm{Alg}(\rm{F},\rm{a}), \rm{Alg}(\rm{F},\rm{b}))$.


```text
       Alg(F,a)
F a --------------> a
          |
          |
          | <- hom(Alg(F,a), Alg(F,b))
          |
          v
F b --------------> b
      Alg(F,b)
```

For an arrow in F-algebra category, we need a transformation from `F a`
to `a`.



## References

* [CoAlg]: http://web.cecs.pdx.edu/~sheard/course/AdvancedFP/notes/CoAlgebras/Code.html "Advanced Functional Programming, Tim Sheard's course notes."
