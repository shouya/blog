---
layout: post
title: "Function Calling Syntax"
date: 2014-08-03 17:59:14 +0800
comments: true
categories: think
tags:
  - "programming language"
  - paradigm
---

We use programming languages to indicate a process of
actions/computations. Therefore programming languages tends to be more
procedual. In other words, they are used to indicate how a thing is
done.

Just as the role of verbs in a nature language sentence, an action is
the most essential part of 'doing a task' in programming language. Of
course the syntax of a programming language could be complex. I'd be
here to discuss only the simplest cases of function calling among
different programming languages.

<!-- more -->

Here's a table of the terms used in different paradigms.

| Paradigm | Term for the Acting Object | Term for the Action |
|----------|----------------|----------------|
| Imperative | Function/Procedure | Call/Invoke
| OO | Method | Invoke/Send Message
| Functional | Function/Closure | Apply
| Stack      | Operation | Act
| Lambda Calculus | Lambda/Combinator | Apply


## Object Oriented Languages (Ruby, C++)

OOP is an analog of object manipulation in the real world. Actually,
semantically `foo.bar(baz)` does not mean `foo` does an action `bar`
on `baz`. Rather, according to the OOP mechanism, it should be
regarded as a message `bar` send to the actor `foo` with argument
`baz`. Look at the code snippets below in Ruby and C++.


```ruby
$stdout.puts "hello"
```

```c++
vec.push_back("hello")
```

How would we translate them in English? The result is probably similar
to these:

> (The computer) puts "hello" on $stdout.
>
> (The computer) pushes "hello" into vec.

In such invocations, obviously the subject is uslally omitted because
we're always ordering the computer to do the actions.
The receivers, or target objects, are put at the front. Then
follows the actions we want to apply on them. And finally the
arguments, or carried objects.

Notice that the in Ruby the object is often omitted because it is
implied in the current context (`self`). And in C++ `this` is
sometimes omitted if we're operating in a method within the same class.
It is similar to the case omitting object in a nature language when
we're in a specific environment. For example, if we are working on a
task and then we say 'finished', the object can be understood as the
task we were working on.


## Stack-based/Concatenative Languages (dc, Factor, Assembly)

The core of stack-based languages is the operation on a stack involves
pushing and popping.

`dc` is a tiny calculator. The language of it is succinct and handy. Here's an example:

```bash
[hello]p
```

The square brackets quotes a string of characters and push them into a
stack. Then the operation `p` pops the string out and then prints it.

Stack-based language can be as simple as dc, while also can be as
complex as Factor. Yet either of them have the same syntax
structure. The following Factor code reverses an array.

```haskell
{1 2 3} reverse
```

We cannot directly translate them into one sentence of nature
languages because the invocation of a function should not be regarded
as a single process. As how we can see the process more clearly in
Assembly language:

```c
push 0x0001   (0x0001 points to "world")
push 0x0010   (0x0010 points to "hello, %s.\n")
call 0x0100   (0x0100 points to the `printf` function in libc)
```

The invocation of such functions can be seen as a kind of
argument-free operation. Their arguments are pushed to stack before
the function invocations and in the function the arguments would be
popped out to be manipulated.

If we still want to see the process of invocation as a single, we
would derive a pattern that the actions, or the verbs, are always put
at the last.


## Functional languages (Scheme)

I consider the syntax of function invocation an opposition to the
stack-based languages. Unlike the invocation syntax in stack-based
languages, which put the action at the last, functional programming
languages tend to pose the action/function as precedent to the
arguments. I guess this phenomenon originates from the application
syntax of FP's ancestor lambda calculus.

In Scheme, a typical hello world program looks like:

```scheme
(display "hello world")
```

It just looks like the stack-based language. In fact we can convert
functional operations into stack operations through
[continuation-passing style transformation](https://en.wikipedia.org/wiki/Continuation-passing_style),
so easily convertible into Assembly. In fact this technique is often
used in Scheme compilers.


## References
* [Factor Programming Language](http://factorcode.org/)
* [WP: Whitespace (PL)](https://en.wikipedia.org/wiki/Whitespace_(programming_language))
* [dc (1) - man](http://www.linuxmanpages.com/man1/dc.1.php)
* [Concatenative Languages](http://concatenative.org/wiki/view/Concatenative%20language)
* [WP: Stack-based](https://en.wikipedia.org/wiki/Stack-based)
* [WP: Stack-oriented Programming Languages](https://en.wikipedia.org/wiki/Stack-oriented_programming_language)
* [WP: Word Order](https://en.wikipedia.org/wiki/Word_order)
