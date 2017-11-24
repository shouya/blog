---
layout: post
title: "A brief journal about learning Coq"
date: 2014-07-05 01:08:34 +0800
comments: true
categories:
  - think
tags:
  - coq
  - programming
---

_This article is completely an informal journal about my learning process on
Coq. It is just created as my whim prompted. Please don't academically refer
any part of it as studying material to learn Coq._


## Pre

I first heard of Coq in my admiring mathematician,
[&commat;txyyss](https://twitter.com/txyyss)'s tweets. At the time I saw him talked
that he's glad and immersed in re-proving all the theorems he had learned.
He also said, as deepening in Coq, he found a boost on other related
academic aspects. Driven by curiosity, I went to know about what a heck Coq
is. Then I saw it describe itself as a 'Proof Assistant'. I felt something
uneven about it. Looking through all the programming languages I had
learned, even those as logically abstract as Prolog, were not capable to do a
proof.

<!-- more -->

At the time I was still in my high school. I just downloaded a Coq environment
on my
machine in the computer lab. Then I tried to follow the
[official tutorial guide](http://coq.inria.fr/V8.1/tutorial.html) to explore how
to use it to do a proof. The experience was terrible. The official tutorial
was
completely not-novice-friendly, at least to me. I followed it for a section,
doing what the guide instructed me to do, and they all works. I learned `0`
has a
type of `nat`, and `nat` has a type of `Type`, and that's all, no more things
substantial. The following sections are tough enough so I don't even know what I
was doing. Thus I gived it up.

My curiousity was evoked the second time by the <span
title="狂讚士">"berserk-faver"</span> [&commat;javranw](https://twitter.com/javranw). He
express similar opinion and excitement as @txyyss towards Coq. At the time it
was the onset of the long summer vacation after high school graduation. I got
plenty of free time to learn the extracurricular interesting stuff. So I
decided to pick Coq up again.

## On

This time I found a Virginia University course about Coq, published as
[website](https://www.kevinjsullivan.com/00-syllabus):
I decide to start here because I
saw the syllabus are well arranged and besides, I thought the course material
might be easier for a layman to get started.
([link to the tutorial](http://kevinjsullivan.org/Courses/Reasoning/L02_Intro.html))

The study curve of this course seems still steep. In the other words, the
course goes complicated soon after it teaches some common-sense-like
knowledge that I can hardly catch up with it. Besides, I don't think the
guide is written in obvious and clear language.

I had also checked the well-known
[Software Foundations](http://www.cis.upenn.edu/~bcpierce/sf/current/). And
I discovered it's actually more clear and friendly to novice like me. So I head
on to study following this guide.

The progress was slow. While a few of the exercises, mostly rated 3+
stars, are really challenging. I tried to work on all exercises myself. And I
maintained a [Github repository](https://github.com/shouya/sf-sol) to publish
my solutions.

I usually study it after midnight. So I could have a quiet and distract-free
environment for me to think. I was concentrated while doing the proofs. It
feels good. Especially after proving a difficult theorem, I acquired great
pleasance from the accomplishment. I can't describe the the feeling as I
first proved the commutation law of multiplication, which took me about 3 hours.

In fact, in each new chapter I learned something new about Coq. Yet, as I
turned to the chapter of
'[Logic](http://www.cis.upenn.edu/~bcpierce/sf/current/Logic.html)', I learned
not only about Coq skills but also a mode of critical thinking towards the
base, the logic, and the logic of logic. I learned the logic system also need
an axiom, which subverted my view to logic before. A logical proposition can
either be true or false, and besides, it cannot guarantee to be in one of these
two states only. This axiom that establishes the whole logic system can be
expressed in many forms. An optional exercise in the guide is to prove they are
equivalent. Rated full 5 stars, this question is the most difficult exercise
I've met. Anyway I still finished 4/5 of it.


### Sequela

I once tweeted (translated into English)
[link](https://twitter.com/54c3/status/478478760371429376):

> I just dreammed in the nap. In the dream there was an arena-like place. The
> fighters used with logic symbols as weapons. They battled, and exerted
> various tactics. As a round over, they proved some theorem. #dream #coq
> #broken_brain

Really. My mind is full of logics in the days I study Coq. I can feel it
vividly that my mind start to ask about the logic of everything I see. Logic
would spring from my mind on every affairs. That is very interesting experience.
Especially after immersed in Coq for a whole night, I see everything and think:
can I prove it?

Somewhat I like that feeling :)

<!--

Of course I still put my girlfriend Cres in the first position. I tried to
spare some times accompanying with her. But to do this I have to convince
myself in a illogical way (because there is no logic): my emotion will
nevertheless take all control of my flesh body than my ration in coping with
matters related to my girlfriend. So I stop thinking about Coq while chatting
with her.

I will have various affections while talking to her. However,
after she sleep and I get back to the world of Coq, every emotional sense just
go vain.

Oh... I can't let Cres know about this... She'll think that I'm belittling her.
Actually I'm regarding her in the most important position to me. Anyway. I'll
just comment it out. Nobody will read this paragraph :)

-->

### Intelligence

In an ideal condition, to prove a theorem, I will first understand the theorem
itself, and conceive a way to prove it. Then I'll translate the idea into
tactics in Coq. However, as I got more and more practiced in Coq, I do not
always follow this way to prove a theorem. For some simple theorems, I can prove
them without understanding them. I know what tactic should be used under
specific condition. I would only concern about the hypotheses and the goals. My
empirical intuition will guide me to the most likely way to prove them.

I first realized the situation while I was proving a theorem related to
`override` function in the next chapter after `override` is defined. I then
disinclined to review the definition of `override` again, because it's a little
bit convoluting. Naturally I tried to prove it without reviewing the definition
of `override`. Also naturally I did it successfully. I realized the problem
just as I finished the proof. The first impression I raised is that I felt
panicky on myself. While as I then carefully checked the definition of
`override` function and the proof process, I found that's already the optimal
way. I will do exactly the same after I understood the definition.

At that time, I had a sense that I was no longer a human, but a proving
machine. I knew the tactics, and I knew when to use them. Prooving theorems
seemed to be a machinery work. I was somewhat depressed on this kind of
loss-of-intelligence.

However, sometimes I thought in the other way. If I could grasp my empirical
intuition, turn them into substantial rules, can I make a program that can
prove whatever I can prove as a human? The program will never understand a
theorem, but they can prove them in a Coq'ly logical way.


## Comments on the design

I thought the design is the point I want dissing about Coq mostly. Although
powerful, Coq's design on its syntax is as terrible as its "mother language"
OCaml, and even worse.

It might be improper for me, as a totally novice, to comment on Coq's design. I
know I'm yet far from qualified to do that. Nonetheless I still want to talk a
little bit about it.

Of course, I know neither syntax nor some mechanism are the most crucial
things to a programming language. However, my steps followed the road of Ruby
&rarr; Scheme &rarr; Haskell. Studying those PLs was pleasing, at least they
give me a sense of elegance. In contrast, I see Coq as a work from scratch
than being well designed.

### Type naming

Actually, types in Coq are not as obvious as in other PLs. Unusual types
are ubiquitous. I use 'unusual type' just because I don't know how to descibe
them. The nature of each type are not just transparent, but also must be
defined by me manually. (At least I'm instructed to do so following the guide.)

The case of type names, in the beginning, confused me. Why are the type names
`nat`, `bool` lower-cased while `Set`, `Type`, `Prop` capitalized? I knew I
won't know the why of every unknowns just as I touched it. So I memorized it
in a silly mode: names of the types we need to define by ourself are
lower-cased, and the others are capitalized.

This gave me the first intuition about differnet kinds of types.
While then I walked through the guide, and gradually realized the difference
between these two sort of types.

Here's correct<sup>(?)</sup> understanding about it. `nat` and `bool` are
lower-cased because they are *inductive* types. In the other words, they are
defined in a specific number of cases. Contrarily, we don't see this feature in
`Set`, `Type`, nor `Prop`, which are what we called *non-inductive types*.


### Tactic naming

Well, I shall just pose here the most frequently used tactic names.

`simpl`, `reflexivity`, `destruct`, `induction`, `left`, `rewrite`.

Did you see them? The names range among abbreviations, nouns,
verbs, and adjectives. All of them are used as tactic names. While they're not
funtioning in different categories. They're really alike. The naming gives me
an impression that they are picked just from the whims of Coq inventors, so
they're not strict at all. There is no choice that I have to memorize them.

### Tactic functioning

It is understandable that tactics function in very different ways. The
behavior of them can sometimes be categorized with different fashions. For
example, some of them will mark a goal as proved so we can continue to next
goal or mark a `Qed` happily if all goals are proved. I call this feature
'commit'. This is an informal term invented by me because I don't know what's
the accurate terminology for that.

It can be thought to categorize the tactics by whether they commit. But it
doesn't apply to all the tactics. Now I have to pose three types:

* commits: `reflexivity`, `assumption`, `exact`, ...
* does not commit: `simpl`, `rewrite`, `unfold`, `symmetry` ...
* <span title="看心情">depends on mood</span>: `apply`, `inversion`, ...

Actually, there is an other case, because some tactics will split the goal into
more subgoals. I will put they in an different category.

* yields subgoals: `assert`, `replace`, `destruct`, `induction`

In fact, these do not always yield a subgoal. The number of subgoals `destruct`
and `induction` yield depends on the number of inductive constructors of
the type of the expression they are operating on.

The behavior of inversion is ridiculous. One can conclude its usage as to
simplify the *injective* case and *disjoint* case. Even the Software
Foundations guide admits:

> The inversion tactic is probably easier to understand by seeing it in action
> than from general descriptions like the above.

Surely, it's not difficult to use. I will list the places it usually applys. Of
course these are just empirical conclusions.

1. false (disjoint equality) hypothesis (committing)
2. `foo a = foo b` &#8658; `a = b`, some like applying `f_equal`
theorem to a goal. (simplifing the hypothesis)
3. `a :: b = x :: y` &#8658; `a = x` and `b = y` (spliting injective cases)

In use case 1, it commits. In case 2, it does not commit but simplies
a hypothesis. And in case 3, it split the cases. (As I write to here, I found
it not so ridiculous as I thought it be before.)

### Tactic option syntax

The disparity of tactic options are really confusing me. They're even more
difficult to remember than those tactic names. Voilà:

`induction n, m` vs `intros n m`. Comma separated or space separated?

`induction n as [| [x y] l]`. This syntax is used to name the induction
variables in different case. We can think of `n` here in type of `list (X * Y)`.
Notice that the cases ` ` and `[x y] l` are sperated with a `|` symbol, the
list head (`[x y]`) and tail (`l`) are seperated with a space, and one
individual element, of a pair, are embeded in another nested pair of square
bracket. Additionally, I don't even know till now how Coq recognize
the user defined pair indicated in a specialized syntax `[x y]` correctly.

`rewrite xxx` vs `rewrite <- xxx`. In fact, the left is an abbreviation of
`rewrite -> xxx`. I was confused on the special using of `<-`. I thought it
is an abuse of the symbol. A better way I can conceive is to split the function
of `rewrite` into some like `rewrite[l,r]`.

`destruct eqn:<eqn>`. This is yet another ridiculous non-uniform syntax. You
can imagine some use of destruct are like
`destruct <expr> as <cases> eqn:<eqn>`. It really happens. And I have to
memorized which goes first, the `as <cases>` or the `eqn:<eqn>`.

### Notation syntax

I couldn't comprhend the usage of `Notation` so far. `Notation` is used to
define notations, or say, syntatic sugars. The part that embarrassed me is
its way to describe a syntax, which is a string, yes, a plain string. On the
anesthetic and semantic aspect, syntatic description in a PL should have the
the same syntatic level as the PL itself, rather than a degraded string.

On the other hand, the notation in Coq is so beautiful yet powerful. It even
capable to rival with standard macro definition in Scheme. I'll pose some
examples.

    Notation "x + y" := (plus x y)
      (at level 50, left associativity).
    Notation "( x , y )" := (pair x y).
    Notation "x :: l" := (cons x l)
      (at level 60, right associativity).
    Notation "[ ]" := nil.
    Notation "[ x ; .. ; y ]" :=
      (cons x .. (cons y nil) ..).

I don't yet know how the definition the in the last case comes in
nature. I'm curious about the implementation of the runtime syntatic definition
to such a complex case. Contrarily, it's at least conceivable how haskell infix
operators work. Nor talk about the macros in the highly uniform syntax in
Lisp.

## Last

At last, I want to say:

> 在在一個定理上辛苦工作了兩個小時後敲 `Qed.` 的那種爽快的成就感簡直無以言表有沒有！
