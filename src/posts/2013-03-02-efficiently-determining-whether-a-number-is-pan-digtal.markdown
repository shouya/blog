---
layout: post
title: "Efficiently determining whether a number is pan-digital"
date: 2013-03-02 23:12
comments: true
categories:
  - solution
published: false
tags:
  - coding
  - ruby
  - projecteuler
  - algorithm
hidden: true
---

Today I spent nearly a whole afternoon on solving
[projecteuler](http://projecteuler.net) problems, from where I started to
consider some kinds of 'efficiencies'.

At the end while I was solving the
[problem 38](https://projecteuler.net/problem=38), I planned to write
a method that returns if a number's pandigital.

Firstly my basic idea is converting a number into a string, then split
it into chars, sort & unique it, and finally see if its length's the
same as before:

```ruby
def pandigital?(n)
  n.to_s.chars.sort.uniq.length == n.to_s.length
end
```
<!-- more -->
This is a good method & intuitive to everyone. Whilst I started to
consider the efficiency problem. I seldom deal with these as I thought
efficiency's not a big deal on modern computers mostly in
practice. However, the former examples of solving problems gave me a
concept that even a smaller improvement on a fundamental level method can
save a huge amount of time. Then I started to consider to optimize
this. (by nothing, mostly it's just for fun.)

Soon i conceived this, to transfer a double used `n.to_s` to a local variable:
```ruby
s = n.to_s
s.chars.sort.uniq.length == s.length
```

It had not very obvious improvement. I think it is some kind of that
it had to deal with one more local variable that drains some
efficiencies from it.

That's cool, then I had an idea to suppress the last calling,
`s.length`, as I still thought function calling is a key-point of
efficiency factor.

```ruby
n.to_s.chars.sort.uniq == (1..9).to_a
```
Or,
```ruby
n.to_s.chars.sort.uniq == [1, 2, 3, 4, 5, 6, 7, 8, 9]
```
to prevent extra calling.

In this way it does work better, but it's still not enough good.

Then I thought about that a pandigital number's limitation, so I added
a checking process in the beginning:
```ruby
return false if n.to_s.length != 9
n.to_s.chars.sort.uniq == [1, 2, 3, 4, 5, 6, 7, 8, 9]
```

As I tested my code with a randomly generated number with this:
```ruby
Benchmark.bm do |b|
  b.report do
    n = (rand * 10 ** 9).floor
    # do the test
  end
end
```

Therefore the terms with improper length accounts about 10% of all, so this
code has reduced the time by nearly 10%, really. Which was really cool.

Not longer I brainstormed another limitation of pandigits, that
there shouldn't be any zero in digits allowed.

So the updated code is:
```ruby
return false if n.to_s.length != 9
return false if n.to_s.include? '0'
# blah blah blah
```

A '0' appears on any bits of a nine digital integer is `1/10`, so in
this way, a probability of `1-((1-1/10)^9)` which be the total chance
of when a '0' will appear in an 9-digit number. It's about 61%, so
after adding that statement, the efficiency was improved by more than
one half.

I know the `sort` method will sort a duplicated array as process;
the same is `uniq`. So I suppressed this two into their 'bang' mode,
since `uniq!` can return a result whether it has compressed the array,
I am possible to inverse the result to a boolean directly.
This is what I did:
```ruby
not n.to_s.chars.to_a.sort!.uniq!
```

Cool it was, then I started to consider to optimize the sort & uniq
line, since I thought it was not elegant and very feasible. A good
method is compact the process of recognizing pandigits with a set, of
course basically I have brought hashes, which perform the same as
sets.
```ruby
return false if n.to_s.length != 9
return false if n.to_s.include? '0'
ns = {}
n.to_s.each_char {|c| return false if ns.key? c; ns[c] = true }
true
```

This method doesn't only compacted the process. Consider the former
program, `sort` method in ruby is implemented by quick sort algorithm,
which has the time complexity of O(Nlog(N)), and beside the `uniq`
took an O(N) as well. These are two significant and obvious efficiency
losts and the new algorithm doesn't have this problem. A hash table
accounts O(1) to store and O(1) to check, and the most time it will just
be fled by the iteration of the characters, which took O(N) to run.

There I found another point to optimize it -- the hash table. A hash
table, in this case, was created and destroyed frequently, and a hash
table's usually a big object so that will cause a large memory
exchange that might take time. On the other hand, the hash table
needs a hash function for each characters. I don't know how ruby might
implement it, but the hash algorithm will still account some
time, as I know.

Then I reduced it into array:
```ruby
return false if n.to_s.length != 9
return false if n.to_s.include? '0'
ns = []
n.to_s.each_char {|c| return false if ns[c.to_i]; ns[c.to_i] = true }
true
```

Cool but there's still a place to improve: the `c.to_i` part. `atoi`
function in C implementation is linear proportional to the string's
length. But in this case I don't need to deal with that much cases. So
I considered to change them to `ns[c.ord - '0'.ord]`.

An static array could prevent it from dynamically allocating memories:
```ruby
ns = Array.new(10)
```

And in the final I did some small tweaks, such as, to hard code some lexical
constants in, optimize the multiple used calls. The result's as below:
```
class Integer
  def pandigital?
    s = to_s
    return if s.length != 9
    return if s.include? '0'
    ns = Array.new(10)
    s.each_char do |c|
      return false if ns.at(c.ord-48)
      ns[c.ord-48] = true
    end
    true
  end
end
```

Compare this result to the most former one; the new program has improved
it's efficiency to 2/3 of the origin, here's the result in a bench
mark test:
```plain
            user       system     total       real
       old  0.920000   0.000000   0.920000 (  0.915701)
       new  0.300000   0.000000   0.300000 (  0.304713)
```

Seems cool and I was in some extent like such challenges. I don't
still consider a lot of efficiency, of course, in practice use. But
I enjoy the pleasure to dig deeply, to drain the extreme. That's where
I find fun in.

Good luck.

## UPDATE
I am currently reading the book
"[Programming Pearls](http://netlib.bell-labs.com/cm/cs/pearls/)", in
which I've had a lot of amazes about algorithms and programming thinking.
So when I picked the previous code, I found that method should be not
the final solution.

I just wrote an more optimized version, which improved the speed by
25% extra.

Here goes the code:
```ruby
def pandigital?(n)
  s = n.to_s
  return false if s.length != 9
  return false if s.include? '0'
  return false unless s.include? '1'
  return false unless s.include? '2'
  return false unless s.include? '3'
  return false unless s.include? '4'
  return false unless s.include? '5'
  return false unless s.include? '6'
  return false unless s.include? '7'
  return false unless s.include? '8'
  return false unless s.include? '9'
  return true
end
```

This method wasn't so cool, and I know it looks a little tedious. In
fact I don't like such repeating code. However, it works so good. This
method is learnt from Programming Pearls, page 94, an expanded loop
for a binary search algorithm with that the maximum number of elements is
1000. It optimized an _[O(log N)](https://en.wikipedia.org/wiki/Big_O_notation)_ algorithm into _O(1)_, which is
really awesome.
