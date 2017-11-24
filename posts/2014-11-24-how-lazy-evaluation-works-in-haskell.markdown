---
layout: post
title: "翻譯：Haskell 怎麼實現惰性求值"
date: 2014-11-24 19:14:57 +0800
comments: true
tags:
  - translation
categories:
  - haskell
---

> 原文由 [Heinrich Apfelmus](http://apfelmus.nfshost.com/blog.html) 發表於 [Hackhands](https://hackhands.com/lazy-evaluation-works-haskell/)，標題：How Lazy Evaluatoin Works in Haskell。


*Lambda 醬想遲些再去打掃房間～*

惰性求值是 Haskell 用得最廣泛的代碼執行方法，通過之我們的程序可以寫得更簡單，更模塊化，不過惰性求值帶來的一個問題是不那麼直觀內存佔用，對新手來講這往往是個坑。譬如說，下面這個看起來很正常的表達式跑起來將會佔用上 G 的內存空間：

```haskell
foldl (+) 0 [1..10^8]
```

在這個教程裡，我想解釋一下惰性求值的實現原理，並講清楚 Haskell 的惰性求值在時間和空間上的佔用情況。我會先講一些關於圖規約（Graph Reduction）基礎，然後討論一下關於嚴格（Strict）的左褶疊（Left Fold），用於幫助理解內存空間洩漏問題並解決之。

惰性求值相關的主題在很多教科書裡都有涉及，譬如 Simon Thompson 的《[Haskell -- The Craft of Functional Programming](http://www.haskellcraft.com/)》一書，但是線上版本似乎不太容易找。但願這篇教程能夠起到些幫助作用吧。

惰性求值是一個需要權衡的語言特性。一方面，它能使代碼更模塊化。（很遺憾，這次我沒有時間演示這個作用。）另一方面，它使得我們無法完全理解在任一程序中求值的過程 -- 它的確比你想像要難一些。在本文末尾，我會提供一些對付這種情況的方法。我們開始吧！

<!-- more -->

## 基礎：圖規約

### 表達式，圖，和 Redex

Haskell 程式的執行就是求值表達式。這是函數式應用（Function Application）的主要思想。對於下面這個函數定義：

```haskell
square x = x*x
```

我們可以對下面的表達式求值：

```haskell
square (1+2)
```

方式是通過替換左手邊的`square`為其定義，然後將變量`x`換成實際參數：

```haskell
square (1+2)
=> (1+2)*(1+2)
```

再對`+`和`*`這兩個函數求值：

```haskell
(1+2)*(1+2)
=> 3*(1+2)
=> 3*3
=> 9
```

注意，在這個例子裡，`(1+2)` 被求值了兩次。但事實上我們知道，兩個`(1+2)`其實是一樣的。因為他們都對應同一個函數參數`x`。

為了避免這種重複的求值，我們採用一個叫作**圖規約**（Graph Reduction）的方法。用這種方法，每個表達式將會被表示為一個圖。我們的例子這樣表示：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-square-0.png)

每個方塊對應一個函數式應用，函數名字寫在白色的區域，灰色區域指向函數參數。事實上，這種圖的標記法類似於編譯器在內存中通過指針來表示的表達式。

每個程序員定義的函數都對應一個**規約規則**（Reduction Rule）。對`square`函數而言，規則如下：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-square-rule.png)

標記著`x`的圓圈是一個子圖的佔位符。注意`*`函數的兩個參數都指向同一個子圖。這種共享子圖的策略是避免重複求值的關鍵所在。

有規約規則的子圖被稱為**可規約表達式**（Reducible Expression），或者簡單稱之 **redex**。只要我們有一個 redex，我們就能**規約**（Reduce）之，只要根據規約規則去改變高亮的方塊就行了。在我們的例子裡，我們有兩個 redex：我們能夠規約`square`函數和`+`函數。

我們先規約`square`函數的 redex，然後進一步規約`+`函數的 redex，得到這樣一個過程：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-square-eval.png)

每一個步驟，我們都給正在要規約的 redex 加上顏色。在倒數第二個步驟中，產生了一個新的對應著`*`函數的 redex。對之求值，我們會得到最終的結果`9`。

### 模範式和弱首模範式

當一個表達式（圖）不包含任何 redex 時，我們就不能再進一步規約下去了，所以規約就完成了。這時，我們就稱這個表達式為**規範式**（Normal form），這就是求值的最終結果。在上面的例子裡，規範式是一個數字，表示為下面這樣的一個圖：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-9.png)

但是像`Just`，`Nothing`這樣的構建子，又如`:`和`[]`這種列表的構建子都會規約出模範式，他們看起來像是函數，但是他們是通過`data`聲明的，而且不存在像函數一樣的定義，所以他們沒有進一步規約規則。譬如說，圖：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-nf-list.png)

就是`1:2:3:[]`的模範式。

事實上，一個圖要被稱為模範式還必須滿足另外兩個條件：它必須是*有窮*的（Finite），而且不能包含*循迴*結構（No Cyles）。有時遞歸就會照成這種情況。舉例來說，下面的表達式定義：

```haskell
ones = 1 : ones
```

就對應這樣的循迴圖（Cyclic Graph）：

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-ones.png)

這個圖就不包含 redex，但它卻*不*是模範式，因為它包含循迴結構：列表的尾（Tail）指向列表自身，以至於這個列表是無窮的。正如這樣，很多表達式並沒有模範式，因為他們對應無窮循環。

在 Haskell，我們並不會求值所有表達式至其模範式。相反，我們常常會在圖達到**弱首模範式**（Weak Head Normal Form）時就停下來，為了簡略，我們稱弱首模範式為 WHNF。只要一個圖的最上級節點是構建子，我們就稱之為 WHNF。譬如說，表達式`(7+12):[]`，或者圖

![](https://hackhands.com/wp-content/uploads/2014/11/blocks-whnf-list.png)

就屬於 WHNF，因為它最上級的節點是列表構件子`(:)`。它並非模範式，因為它的第一個參數包含一個 redex。

另一方面，任何*不*屬於 WHNF 的圖都可以被稱作**待求值表達式**（Unevaluated Expression）或者**次程式**（Thunk）。以構建子開頭的表達式都是 WHNF，但這個構建子的參數可以是待求值表達式。

上面描述的表達式 `ones` 是一個有趣的 WHNF 圖。畢竟它的最上級節點是一個構建子。在 Haskell 中，我們能輕鬆表達無窮列表並操縱之！因而我們可以使代碼變得更模塊化。


### 求值順序，惰性求值

一個表達式常常包括多個 redex，我們以不同的順序規約他們會有區別嗎？

一種規約順序，我們稱之為**貪婪求值**（Eager Evaluation）。依這種順序，我們會先對函數式應用的每個參數都規約到其規範式，然後再規約函數式應用本身。這種策略是大多數程式語言所採用的。

然而 Haskell 編譯器採用另一種規約順序，我們稱之**惰性求值**（Lazy Evaluation）。惰性求值會先規約最上級的函數式應用，因而，最終一些參數會被求值，只有在必要的時候他們才會被求值。函數是通過構建子模式匹配（Pattern Matching）來定義的，所以其參數只有在其最上級節點為構造子時才會被求值。也就是說，至少在參數被規約為 WHNF 之前，這些參數會由左至右被求值。

希望這個概念能通過下面的例子闡述清楚。讓我們想像一下`(&&)`函數，這個函數的作用是實現邏輯「與」的操作。它的定義如下：

```haskell
(&&) :: Bool -> Bool -> Bool
True  && x = x
False && x = False
```

根據第一個參數是`True`還是`False`，這個函數會有兩種求值規則：

![](https://hackhands.com/wp-content/uploads/2014/11/and-rule-true.png)
![](https://hackhands.com/wp-content/uploads/2014/11/and-rule-false.png)

現在，再看此表達式：

```haskell
('H' == 'i') && ('a' == 'm')
```

圖的形式表示如下：

![](https://hackhands.com/wp-content/uploads/2014/11/and-expr-0.png)

它的兩個參數都是 redex，惰性求值將會從左到右求值參數，所以我們從左邊開始規約：

![](https://hackhands.com/wp-content/uploads/2014/11/and-expr-1.png)

現在，因為最左邊的函數變成了一個 redex，因為它的第一個參數現在成了一個構造式。惰性求值總是會先規約最上級節點，所以我們就這麼做。根據`(&&)`的規約規則，我們會得到：

![](https://hackhands.com/wp-content/uploads/2014/11/and-expr-2.png)

這個表達式屬於模範式，所以我們的求值就完成了！

注意，當我們儘可能先求值`(&&)`的函數式應用時，我們就不再需要求值第二個參數了，以此節省我們計算所需的時間。有些命令式程式語言也用了類似的技巧，叫作「[短路求值](http://en.wikipedia.org/wiki/Short-circuit*evaluation)」（Short-circuit Evaluation）。不過這種短路求值一般被編譯器內部實現，而且只對邏輯操作有效。但在 Haskell 裡，所有函數都能從懶惰求值裡實現到這樣的效果。

一般而言，惰性求值一個表達式得到的最終的模範式和對其貪婪求值得到的結果沒有任何區別。因此我們可以說，不同求值順序並不會關係。然而，惰性求值會因而有更少的求值步驟，而且不像貪婪求值，惰性求值還能處理帶循迴（無窮）的圖。


### 文字表示法

但願把表達式可視化地表示圖能幫你理解惰性求值的基礎，更特別的是因為圖的形式能夠明確表示 redex 的概念和求值順序的重要性。然而，在實際的計算中，畫圖表示是有點太肥了。要追蹤規約，我們一般用 Haskell 語法的**文字表示法**（Textual Representation）來表達。

圖令得我們可以清晰看到共享子圖。在文字表示法裡，我們會給他們用`let`關鍵字來*命名*，譬如說，在我們第一個例子裡的`square (1+2)`的規約可以寫為：

```haskell
square (1+2)
=> let x = (1+2) in x*x
=> let x = 3 in x*x
=> 9
```

`let ... in` 語法使我們可以共享子表達式（Subexpression）`x = (1+2)`。再次注意`square`是被先規約的，然後才是其參數`x`。

在我們第二個例子裡，邏輯「與」，變成了：

```haskell
('H' == 'i') && ('a' == 'm')
=> False && ('a' == 'm')
=> False
```

在這個例子裡，我們沒有共享子表達式，所以沒甚麼必要用`let`關鍵字。

從現在開始，我們都會用文字表示法。


## 時間和空間

我們現在來看惰性求值對 Haskell 程式的時間空間佔用情況。如果你只用過貪婪求值，那麼這些可能會讓你震驚，特別是空間佔用上。


### 時間

求值一個表達式需要多少步？對貪婪求值而言，答案很簡單，對每次函數式應用，我們都把求值函數參數和求值函數體的時間加起來就可以了。而惰性求值呢？非常幸運的，惰性求值會佔的時間總有一個上限：

> **定理：**惰性求值不會執行比貪婪求值更多的求值步驟。

這意味著當我們分析一個算法的運行時間時，我們總能把它當成是貪婪求值來評估。譬如說，我們可以把一個排序算法用 Haskell 改寫，並保證其算法複雜度和貪婪求值下一樣（在少數情況下甚至更佳）。

然而呢，惰性求值器實現起來會帶來一些額外的代價。對於圖形處理和數值模擬這樣的要求高效能的應用程式，可能放棄惰性求值而直接接觸底層架構實現會更實際一些。即便除此，以和簡潔和模塊化著稱的惰性求值依然在這些領域頑強存在。一種叫作「[流融合](http://stackoverflow.com/questions/578063/what-is-haskells-stream-fusion)」（Stream Fusion）的編譯器優化策略就能帶給高效率的數組操作一個模塊化，用起來像列表一樣的接口。這個技術就在 [vector](http://hackage.haskell.org/package/vector) 庫裡實現了。

### 空間

不幸的是，空間佔用的情況就要複雜多了。問題的關鍵待求值表達式的內存佔用和它規約下來的規範式可以差別很大。因為一個表達式所佔用的空間等價於表示它的圖的所佔用的空間。譬如下面的表達式：

```haskell
((((0 + 1) + 2) + 3) + 4)
```

就比其模範式`10`所佔的空間多得多了。但再看下面表達式：

```haskell
enumFromTo 1 1000
```

或者表示為更常見的`[1..1000]`。這個函數式應用表達式只包含三個節點，當然空間佔用也會比其模範式，列表 `1:2:3:...:1000:[]` 佔用的空間少多了，因為後者包含上千個節點。

當第一種情況越發嚴重導致無法控制時，我們稱之為**空間洩漏**（Space Leak）。解決方法就是手動控制求值過程，確保表達式僅可能早被求值。Haskell 為這種需求提供了這樣一個組合子：

```haskell
seq :: a -> b -> b
```

正如其類型表示的那樣，這個表達式會像`const`函數一樣返回其第二個參數<sub>1</sub>。然而，對`seq x y`求值確總會先把`x`求值到 WHNF 的形式，然後才會繼續求值`y`。相對的，`const`函數就沒有必要先求值其參數到 WHNF。

每個 Haskell 程序員都應該知道怎麼用`seq`組合子，我們先來看一個具有代表性的例子：**嚴格的左褶疊**（Strict Left Fold）。看下面的求和 1 到 100 的代碼。我們用左褶疊，用累加參數（Accumulating Paramter）的方式求和：

```haskell
foldl (+) 0 [1..100]
```

作為參考，在 [Haskell Prelude](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html) 裡，`foldl`函數定義如下：

```haskell
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs
```

那麼上面例子的求值過程如下：

```haskell
foldl (+) 0 [1..100]
=> foldl (+) 0 (1:[2..100])
=> foldl (+) (0 + 1) [2..100]
=> foldl (+) (0 + 1) (2:[3..100])
=> foldl (+) ((0 + 1) + 2) [3..100]
=> foldl (+) ((0 + 1) + 2) (3:[4..100])
=> foldl (+) (((0 + 1) + 2) + 3) [4..100]
=> ...
```

如上所示，累加參數增長起來愈來愈多 -- 空間洩漏。解決方法就是將累加參數保持在 WHNF，下面的修改過的`foldl`函數就能做到這一點：

```haskell
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f a []     = a
foldl' f a (x:xs) = let a' = f a x in seq a' (foldl' f a' xs)
```

這個函數的定義可以在 [Data.List](http://hackage.haskell.org/package/base-4.7.0.1/docs/Data-List.html#v:foldl-39-) 模塊裡找到。現在求值過程變成了這樣：

```haskell
foldl' (+) 0 [1..100]
=> foldl' (+) 0 (1:[2..100])
=> let a' = 0 + 1 in seq a' (foldl' (+) a' [2..100])
=> let a' = 1 in seq a' (foldl' (+) a' [2..100])
=> foldl' (+) 1 [2..100]
=> foldl' (+) 1 (2:[3..100])
=> let a' = 1 + 2 in seq a' (foldl' (+) a' [3..100])
=> let a' = 3 in seq a' (foldl' (+) a' [3..100])
=> foldl' (+) 3 [3..100]
=> ...
```

在求值的時候，可以看到表達式佔用的空間不再不斷增長下去了。用`seq`能確保累加參數總是先求值到 WHNF 然後才考慮剩下的元素。

憑經驗來看，`foldl`會導致空間洩漏，所以你應該用`foldl'`或者`foldr`。

順便一提，對於貪婪求值語言，你根本用不著寫上面這種代碼來求和`1`到`100`之間的數。因為貪婪求值會先把列表`[1..100]`規約到模範式，這樣子的空間效率佔用和我們上面低效的`foldl`版本一樣。要是想要做到高效率，那你必須把這個表達式寫成遞歸循環（Recursive Loop）才行。但得益於惰性求值，在 Haskell 裡，我們可以用通用的列表組合子<sub>2</sub>來「按需」對`[1..100]`計算。也就說明了惰性求值怎樣帶來更高的模塊化效果。

這個例子裡我們還要注意到另一個重要的概念。我上面演示的求值過程並非完全準確，如果我們這樣定義`[n..m]`：

```haskell
enumFromTo n m = if n < m then n : enumFromTo (n+1) m else []
```

那麼規約到 WHNF 其實是這樣的：

```haskell
[1..100]
=> 1 : [(1+1)..100]
```

其中第一個參數是待求值表達式`(1+1)`而非`2`。在這裡這倒沒有多大關係，關鍵是你要非常小心才能精確追蹤惰性求值過程 -- 它也許並不一定按你理想當然地來。真正的`enumFromTo`的[源碼實現](https://www.haskell.org/onlinereport/haskell2010/haskellch9.html#verbatim-226)並不是這樣的。特別地，請留意`[1..]`，它會構建一列*不*屬於 WHNF 的數。

事實上，我只能說，除非是對像上面這樣簡單的例子，要仔細追蹤惰性求值過程幾乎不可能。所以很難去分析 Haskell 的空間佔用情況。我的建議是只有在你的程序出現嚴重的空間洩漏時才去分析它，用[性能分析工具](http://stackoverflow.com/a/3276557/403805)來找到問題產生的源頭所在。一旦確認了問題源頭，就可以用 [Space invariants](http://apfelmus.nfshost.com/blog/2013/08/21-space-invariants.html) 和`seq`來確保相關表達式被規約城 WHNF，而無須管惰性求值具體是怎樣工作的。

這就是我今天要講的關於惰性求值和它空間佔用相關的內容了。其實還有另外一個有代表性的空間洩漏的例子，如下：

```haskell
let small' = fst (small, large) in ... small' ...
```

即使`fst`函數會把`large`丟棄，表達式`small'`還是會保存一個到`large`的引用。你可能會希望在某個時候把`small'`規約到 WHNF，這樣`large`所佔的空間就可以被釋放掉了。



----

**譯注：**

1. 其實不太一樣，`const`的類型是`a -> b -> a`。這裡應該說`seq`的類型和`flip const`類似。
2. 這裡指`foldl`，`foldl'`和`foldr`這些函數。


（聲明：此文章的翻譯及發佈已經經過原文作者的許可。此譯文版權歸譯者所有，並在 [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/) 下發佈）
