---
layout: post
title: "Undeepening Continuations"
date: 2015-03-13 15:40:18 -0500
comments: true
published: false
categories:
    - haskell
tags:
    - tutorial
---

Continuation was always a mystery to me since I started to learn
monad.

My friend Javran once sent me a link to 'the mother of all monads',
from where I start to rethink the significance of the continuation
monad. That article, to me as a novice, wasn't enough explanative for
me to comprehend. Therefore, to demystify the significance of the
continuation monad, I spend several hours to read through articles
about it last night. In the final, I eventually carried my
understanding to continuation out into code and
[achieved enlightenment](http://learnyouanagda.liamoc.net/). (not
really)

So let's get started. First of all, we need to know, at least have a
primitive concept of, what continuation is used for. So here is my
understanding,

> A continuation is an intermediate state that allows computation to
> run at that point.

You don't need to try hardly on comprehending the statement above. By
going through this tutorial you will make your own understanding out.

<!-- more -->

## Computation with Holes

As we said above a continuation is a *intermediate* state that we can
delay the computation to the future. Let's see how these functions
look like:

```haskell
transformOne = ??? 1

doSomethingOnOne = do
  putStrLn "before execution"
  ??? 1
  putStrLn "after execution"
```

So we're defining two functions with this feature. The first one is
rather easy to see, on the blank (`???`) there should be a function,
which takes an integer and yields whatever things. In the
`doSomethingOnOne` there should also be a function like that in the
first, expect being constrained to return an `IO a`.

Here's the modified finished functions and their corresponding types:

```haskell
transformOne :: (Int -> r) -> r
transformOne f = f 1

doSomethingOnOne :: (Int -> IO a) -> IO ()
doSomethingOnOne f = do
  putStrLn "before execution"
  val <- f 1
  putStrLn ("we get " ++ show val)
```

If you have other experiences about `Control.Monad.Cont`, you might be
able to recognize the pattern in the types. Strictly, the latter one
isn't what continuation monad looks like because `IO a` is not the
same as `IO ()`. Well, this is normal in our real life programming,
but we can't call them continuation because they're not
composable. And one of the main purposes we want to generalize
continuation is to make them composable so they could be an instance
of `Monad`. <del>Why? Well, a monad is just a MONOID in the category
endofunctors, monoid implies composablity, what's the problem?  Why?
Well, a monoid is a just a SEMIGROUP in combine with an identity
object, what's the problem? But why? Well, that's how semigroup is
defined, what's the problem?</del>

Anyway, we'll see if we can make a continuation stronger, that is, in
type of `(a -> r) -> r`, we make our life easier. For now, just
remember it, that `(a -> r) -> r` is the type of continuation. Which
means we can't have the second piece of code above unless we add a
`return val` at the end of the `do` notation. It also means you can't
have continuation like `foo f = show (f 1)`, which modifies the
result.

These functions are what we called `continuations`. It's intuitive to
think them as functions with a hole to be filled. To fill the hole in
a continuation we need to feed it with a function that takes the
passed in value and return something we needed from outside of the
hole. I call these functions fed to a continuation *hole-filler*
functions. For the arguments these hole-fillers take, I call them
*seed* and the returning thing was barely named *yield*. I made these
names just for referring the components more intuitively.


## How to Fill in the Holes

We now see how `(a -> r) -> r` tastes like. You might think, well,
continuation is weak that we are even restricted from modifying with
the hole-filler's yield. Well, yes, we cannot modify the yield. But
continuation is not as weak as you might think, because modifying the
yield is not the *correct* way to use a continuation.

If you just want to modify the result of a passed in function and
carry on the computation, you want a Monad instead of a continuation.
Yet without being capable to modify the yield, you're allowed to modify
the seed freely.

The significance of continuation is, in my understanding, just
**hole-filling**. It's like, if we're playing with continuation, our
aim will be giving out a seed to a incoming hole-filler, for which we
don't know what it is or what it will do. In the other words, we pass
our result of computation IN instead of return it OUT.

If we've composed a bunch of continuation together, that is, we get a
very deeply nested continuations, how can we take the result out?
Since the computation always throw their results inwards, by the
seeds. Can we acquire the seed out of the continuation? Just think of
the simplest example: `transformOne f = f 1`. The answer is, `id`
(`id x = x`). If we feed the continuation with `id` function as
hole-filler, it will yield the seed without any modification and then
we can acquire it.

As we take out the result, we can do whatever we want with it. Also we
can take some actions directly onto the result. Think of feed a
continuation with a `print` hole-filler, then we will see the seed
printed out as expect.

Now let's think, how do we generate a continuation from a single
value such that we can take out result out by feeding the continuation
with `id`? The solution isn't very hard:

```haskell
genCont :: a -> (a -> r) -> r
genCont val f = f val
```

or more point-<del>lessly</del>freely:

```haskell
genCont = flip ($)
```

So far, we have gain a basic concept how continuations are created and
composed together.


## Functor Property of Continuations

We now look on how to transform the seed with a function.  If we have
a function `f :: a -> b`, we hope we can use it to transform a
continuation with type `(a -> r) -> r` into `(b -> r) -> r`. What does
it mean? If I have a continuation with seed of type `Int`, how can use
feed it with a hole-filler that eats `String`s? The answer is to have
a transforming function of type `Int -> String`.

Here's how we might these functions look like:

```haskell
toStr :: Int -> String
toStr = show

foo :: (Int -> r) -> r
foo f = f 1

bar :: (String -> r) -> r
bar f = f "1"
```

If now we have `toStr` and `foo`, how should we combine them together
to form `bar`?

Some might recognize what we're trying to implement is
just `fmap`, if we make continuation an instance of
`Functor`. We can do that, let's try. First we wrap the function in
this pattern it into a `newtype`, call it 'Cont'.

```haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
```

The reason I write `Cont r a` instead of `Cont a r` is, the varying
type for a continuation is its type of seed, rather than that of
yield. So now we try:

```haskell
instance Functor (Cont r) where
  fmap f (Cont cnt) = ???
```

The returning value of fmap should be a continuation with seed type
`b`. So first of all, we should have a continuation that takes an
argument with a hole-filler with type `b -> r`.

```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \(hf :: b -> r) -> (??? :: r)
```

We need a `r` as result, which should be generated by the passed in
`hf`. And `hf` takes a value with type `b`. Obviously, this `b` should
be transformed by `f` from `a`, i.e. `f (??? ::: a)`. So now we would
have:

```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \hf -> hf (f (??? :: a))
```

Now the problem has been converted to, how can we extract `a` from the
passed in continuation `cnt`? The word `extract` implies that we have
to get INTO it for what we want. Just as what we did above with
`id`. `(cnt id)` will give us `a`, pretty cool. For some purpose, here
I will expand the definition of `id`, which is `\x -> x`, you'll see
why I do it in that way soon:

```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \hf -> hf (f (cnt (\a -> a)))
```

Looks good, seems in this way we can extract `a` out of `cnt`. But
wait, it doesn't typecheck? Let's see what happens. Without the type
constrain above, the code compiles. When we query the type of `fmap`,
it was in type `(a -> b) -> (Cont a a) -> (Cont r b)`.

This is not what we want. The returning type of `cnt` shouldn't be
restricted to `a` as what it is, rather, it should share the same `r`
with the one in `Cont r b` as `fmap` finally returns. So we start to
see how we can achieve that.

```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \hf -> hf (f (cnt (\a -> (??? :: r))))
```

However, `f` expects to have an argument with type `a` isn't it? How
can we have the `a` in the context of `f` while keeping the returning
type of `cnt` to be `r`?

Okay, we look for where `r` is needed in the context and we easily
found it:

```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \hf -> ((hf (f a)) :: r)
```

That means, if we have the `a` to feed into `f`, then we get a `b` to
feed into the hole-filler `hf`, and the hole-filler will yield an `r`,
which is what we wanted. But we don't have an `a` for `f` at this
point. The solution is to wrap the extraction of `a` around `hf (f
a)`:


```haskell
fmap :: (a -> b) -> (Cont r a) -> (Cont r b)
fmap f (Cont cnt) = Cont $ \hf -> cnt (\a -> (hf (f a)))
```

It typechecks, so it should be correct. Try it out:

```haskell
(fmap (+1) $ genCont 3) id     -- => 4
```

Good.


## Monadic Continuations

We now make continuation an instance of a monad:

```haskell
instance Monad (Cont r) where
  return           = ???
  (Cont cnt) >>= f = ???
```

The type of `return` is `a -> Cont r a`. Unwrap the `Cont` we have
`a -> (a -> r) -> r`. Looks familiar? Yup it is just `genCont` we have
above. The aim of `return` is to create a continuation from a value,
the same as what `genCont` does. So this part is easy.

```haskell
return x = Cont $ \hf -> hf x

-- or pointlessly
return = Cont . flip ($)
```

Actually, after we deduced `fmap`, we'll feel much easier to catch
`>>=`. The significance is to take the value out from the continuation
supplied as the first argument of `>>=`. The way to do this is similar
to `fmap`.

```haskell
-- cnt     :: (a -> r) -> r
-- f       :: a -> Cont r b
-- (>>=)   :: (Cont r a) -> (a -> Cont r b) -> Cont r b
-- hf      :: b -> r
-- runCont :: (Cont r b) -> ((b -> r) -> r)

(Cont cnt) >>= f = Cont $ \hf -> cnt (\x -> runCont (f x) hf)
```

This solution typechecks. Let's look into it to see what it does in
`>>=`. First of all, a holefiller


## Why monad?

Disclaimer: DO NOT READ THIS SECTION. This section was purely my OWN
understanding to continuation monad. I try to write correct things but
my thought was specific and could be quite misleading to beginners. If
you think you haven't understood continuation yet, don't read this
section because it will muddle you up once again. Otherwise, you've
understood continuation well, you don't need to read on my premitive
and partial and inaccurate opinion. Anyway, don't read it.


Continuation, is just a kind of computations. A continuation could
generate a dependent output that relies on a value, either a plain
value or the output of another continuation, we call it composability
property. (Although I used some general terms here, the way
continuations take input is still very different from that of
functions) On the other hand, we know we can produce a continuation
that will generate a specified plain output. Therefore a continuation
is a monad, whose 'bind' operation is the 'compose' operation of
continuations.




composed. 'Composing' two computations means to collapse them into
one. On the other hand, computations are definitely the arrows in the
category of inputs and outputs. On the third hand, we know we can
always create a computation that gives no matter what input,
because it is a  at the






We're now entering the domain of monad, so we now need to make use of
our knowledge of how to implement `(>>=)`.


## References

* [All about monads: Continuation](https://wiki.haskell.org/All_about_monads#The_Continuation_monad)
* [Understanding continuations](https://www.fpcomplete.com/user/jwiegley/understanding-continuations)
* [How continuation monad works](http://www.haskellforall.com/2014/04/how-continuation-monad-works.html)
* [The mother of all monads](http://blog.sigfpe.com/2008/12/mother-of-all-monads.html)
