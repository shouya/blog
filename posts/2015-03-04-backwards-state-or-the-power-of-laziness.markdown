---
layout: post
title: "翻譯：逆向狀態，又：惰性之力"
date: 2015-03-04 21:14:35 -0600
comments: true
tags:
  - translation
categories:
  - haskell
---

> 原文 「[Backwards State, or: The Power of Laziness](http://panicsonic.blogspot.jp/2007/12/backwards-state-or-power-of-laziness.html)」由 Antoine Latter 發佈於其個人[Blogger](http://panicsonic.blogspot.jp/)上。特別地，對 Philip Wadler 及其著作的 [The Essence of Functional Programming](http://citeseer.ist.psu.edu/wadler92essence.html)表示至高感謝。


近期我參加了一個關於 Haskell 中自動微分（Automatic Differentiaion）的討論，因為之我拜讀了 Jerzy Karczmarczuk 的論文「[Lazy Time Reversal, and Automatic Differentiation](http://users.info.unicaen.fr/~karczma/arpap/revpearl.pdf)」。這篇論文進一步引用了 Philip Wadler 的 [The Essence of Functional Programming](http://citeseer.ist.psu.edu/wadler92essence.html) 來介紹*逆向*（Backward） State Monad，我覺得非常有趣，在此向大家講一下這種技術。

在此我期待讀者各位對 Haskell 的 [State Monad](http://haskell.org/ghc/docs/latest/html/libraries/mtl-1.1.0.0/Control-Monad-State-Lazy.html) 已有所了解，其實簡單來說 State Monad 就是一個函數，從上一個狀態映射到結果以及下一個狀態。

逆向 State Monad 和 State Monad 的區別就在於它和 State Monad 執行的順序恰好相反，也就是說，逆向 Monad 是從一個最終狀態執行到其最初狀態並產生一系列值的。

<!-- more -->

此文是一篇 [文學 Haskell](http://zh.wikipedia.org/wiki/%E6%96%87%E5%AD%A6%E7%BC%96%E7%A8%8B) 文章，所以你可以把整個文件拷貝到一個 `.lhs` 文件中並用 Haskell 解釋器來跑之，譬如說用 GHCi。（譯者注：保留此段只為了完整性，本文並非用文學 Haskell 編寫，請讀者自行忽略此段。）

首先，我們要引入一些需要用到的樣板代碼：

```haskell
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}

import Data.List
import Control.Monad.State
```

## 一個例子

先來做一個練習：假設給你一棵樹，你要做的是把樹的每個節點上的元素映射到整數上，這些整數從 0 開始並逐漸增加。如果有些元素出現了多次，那麼它們應該被映設到同樣的整數上。

用到 *Control.Monad.State.Lazy* 的解決方案就是，遍歷這棵數並用 State Monad 保存至今為止見過的所有元素作為狀態。也就是說，每個節點映射到其元素在此列表上的下標。這樣子第一個出現的元素會被映射到 `0`，第二個映射到 `1`，如此不斷進行下去。

但現在問題變了，如果我想把最後一個遇到的節點映射到 `0`，倒數第二個映射到 `1`，如此直到第一個節點，我應該怎麼做呢？對上面用 *Control.Monad.State.Lazy* 的解決方案我得改變多少才能滿足新的需求？

答案是，只要改一點點！我只要換成*逆向* State Monad 就可以了，因為對之而言狀態流是反轉過來的。

修改過的解決方案看起來大概是這樣子的：

```haskell
data Tree a = Nil | Node a (Tree a) (Tree a) deriving (Show, Eq)
type Table a = [a]


numberTree :: Eq a => Tree a -> StateB (Table a) (Tree Int)
numberTree Nil = return Nil
numberTree (Node x t1 t2)
       =  do num <- atomically $ numberNode x
             nt1 <- numberTree t1
             nt2 <- numberTree t2
             return (Node num nt1 nt2)
   where
    numberNode :: Eq a => a -> State (Table a) Int
    numberNode x
       = do table <- get
            (newTable, newPos) <- return (nNode x table)
            put newTable
            return newPos

    nNode::  (Eq a) => a -> Table a -> (Table a, Int)
    nNode x table
       = case elemIndex x table of
         Nothing -> (table ++ [x], length table)
         Just i  -> (table, i)
```

相應的狀態求值調用如下：

```haskell
numTree :: (Eq a) => Tree a -> Tree Int
numTree t = evalStateB (numberTree t) []
```

測試一下結果：

```haskell
testTree = Node "Zero" (Node "One" (Node "Two" Nil Nil) (Node "One" (Node "Three" Nil Nil) Nil)) Nil
```

跑一下 `numTree testTree` 會生成這樣的樹：

```haskell
Node 3 (Node 1 (Node 2 Nil Nil) (Node 1 (Node 0 Nil Nil) Nil)) Nil
```

正中吾需！


代碼幾乎和用 *Control.Monad.State.Lazy* 的原問題解決方法一模一樣，區別在於我們用了 `evalStateB` 取代我們熟悉的 `evalState`，用了一個神奇的函數 `atomically`，以及 `StateB` Monad。我下面會詳細講他們是何方神聖乃至於究竟是怎麼實現逆轉狀態的。


## API

我們現在要有一個新的 Monad：`StateB s`，其中 `s` 為其存儲的狀態的類型。`StateB s` 是 `MonadState s` 的一個實例，所以裡所應當應該實現 `get` 和 `put` 函數。

當然還有這些：

```haskell
runStateB :: StateB s a -> s -> (a, s)
evalStateB :: StateB s a -> s -> a
execStateB :: StateB s a -> s -> s
```

應該很熟悉吧，對應的就是 State Monad 裡的那些操作。技巧在於我們傳給它的狀態 `s` 是最終狀態而它返回的是初始狀態。回憶在上面的例子中，在我們遍歷樹的時候最後看到的元素被賦予第一個標籤（`0`），而第一個見到的元素被賦予最後的標籤。

在 Control.Monad.State.Class 中默認的 `modify` 函數實現如下：

```haskell
modify :: MonadState s m => (s -> s) -> m ()
modify f = do
    s <- get
    put (f s)
```

而在 `StateB` Monad 中，這段代碼直接就得碰壁了，因為兩個 Monadic 的行為會相互循迴依賴，`(>>=)` 會把現在的結果向前傳遞，而在 `StateB` 中，運算結果的方向是調轉過來的傳遞的。也就是說，上面那段代碼會產生一個循迴引用：第一行得到更新過的狀態，而這個狀態卻是來自第二行放進去的。

要讓這樣的函數工作，我們要定義這個函數的 `StateB` 版本。

```haskell
modifyB :: (s -> s) -> StateB s ()
```

但如果你還想返回結果，你還會需要下面這位小朋友：

```
atomically :: State s a -> StateB s a
```

`atomically` 會把正常 `State` 的動作轉換為 `StateB` 的動作，這樣你可以直接用現成的代碼。（或者你也可以用 `mdo` 語法）


## 實現

**這裡的實現基於 Wadler 的論文。**

StateB Monad 和 State Monad 幾乎一樣，每個產生 `a` 的動作都是一個類型為 `\s -> (a, s)` 的函數。區別在於 `(>>=)` 的實現。

讓我們開始定義！

```haskell
newtype StateB s a = StateB { runStateB :: s -> (a,s) }

instance Monad (StateB s) where
    return = StateB . unitS
    (StateB m) >>= f = StateB $ m `bindS` (runStateB . f)
```

因為封裝解封這個 newtype 的話太麻煩，所以他們只用在被導出的函數（如 `return` 和 `(>>=)`）上用。剩下處理細節用的函數我都用 'S' 做為其後綴了。

```haskell
m `bindS` k  = \s2 -> let (a, s0) = m s1
                          (b, s1) = k a s2
                      in  (b, s0)

unitS a = \s2 -> (a, s2)
```

（*譯者：我第一次看到上面這段代碼時興奮了一個晚上！短短三行就平直地描述並實現了狀態逆流的效果，非常簡潔而優美。*）

正如君所見，傳進來的狀態（`s2`）被應用於 `bindS` 的右邊的參數（`k`）上，產生的狀態被 `bindS` 左邊的參數（`s1`）消耗，並產生出最後的狀態 `s0`。就這樣就可以了嗎？嗯就這麼點！其他 API 實現如下：

```haskell
execStateB m = snd . runStateB m

evalStateB m = fst . runStateB m

modifyB = StateB . modify'
   where modify' f = \s -> ((), f s)

atomically = StateB . runState
```

還可以把這些也寫了來玩：

```haskell
instance Functor (StateB s) where
    fmap f m = StateB $ mapS f (runStateB m)

mapS f m = \s -> let (a, s') = m s in (f a, s')

instance MonadState s (StateB s) where
    get = StateB get'
     where get' = \s -> (s,s)

    put = StateB . put'
     where put' s = const ((),s)

instance MonadFix (StateB s) where
    mfix = StateB . mfixS . (runStateB .)

mfixS f = \s2 -> let (a,s0) = (f b) s1
                     (b,s1) = (f a) s2
                 in (b,s0)
```


## 變形金剛（譯者：沒錯我故意的）

下面這些你要稍微注意一下，因為我沒測試過，不過看起來應該是工作的，這些風格基本和 `Control.Monad.State.Lazy` 的差不多。


```haskell
newtype StateBT s m a = StateBT {runStateBT :: s -> m (a,s)}

unitST a = \s -> return (a,s)

m `bindST` k = \s2 -> mdo ~(a,s0) <- m s1
                          ~(b,s1) <- k a s2
                          return (b,s0)

execStateBT :: Monad m => StateBT s m a -> s -> m s
execStateBT m s = do ~(_,s') <- runStateBT m s
                     return s'

evalStateBT :: Monad m => StateBT s m a -> s -> m a
evalStateBT m s = do ~(a,_)  <- runStateBT m s
                     return a

modifyBT :: Monad m => (s -> s) -> StateBT s m ()
modifyBT = StateBT . modify'
 where modify' f = \s -> return ((),f s)

atomicallyT :: Monad m => State s a -> StateBT s m a
atomicallyT m = StateBT $ \s-> return $ runState m s

atomicallyTM :: Monad m => StateT s m a -> StateBT s m a
atomicallyTM = StateBT . runStateT

mapST f m = \s -> do ~(a,s') <- m s
                     return (f a,s')

liftST m = \s -> do a <- m
                    return (a,s)

mfixST f = \s2 -> mdo ~(a,s0) <- (f b) s1
                      ~(b,s1) <- (f a) s2
                      return (b,s0)

instance Monad m => Functor (StateBT s m) where
    fmap f m = StateBT $ mapST f (runStateBT m)

instance MonadFix m => Monad (StateBT s m) where
    return = StateBT . unitST
    (StateBT m) >>= f = StateBT $ m `bindST` (runStateBT . f)
    fail = StateBT . const . fail

instance MonadTrans (StateBT s) where
    lift = StateBT . liftST

instance MonadFix m => MonadState s (StateBT s m) where
    get = StateBT get'
      where get' = \s -> return (s,s)
    put = StateBT . put'
     where put' s = const $ return ((),s)

instance MonadFix m => MonadFix (StateBT s m) where
    mfix = StateBT . mfixST . (runStateBT .)
```


## 譯後記

這篇文章第一次閱讀就給了我極大的震驚。我已知 Haskell 的惰性求值策略，而且也知道一些與之相關的優雅應用（譬如著名的 `fib = 1 : 1 : zipWith (+) fib (tail fib)`），不過讀到這篇文章時我還是大呼「神奇！」。此文雖然沒有在內容中著筆墨於惰性求值之中，卻在標題中直接強調了「The Power of Laziness」。從其他語言來的讀者可能會對上面 `bindS` 感到不可思議，覺得「怎麼可以直接這樣？」，是的，一般情況下當然不行，但是 Haskell 已裝備了強大的惰性求值，所以這樣寫也不是問題。

最後再次感謝 Wadler 提出的理論和 Latter 的這篇科普向（？）文章帶我們展現了依賴惰性求值實現的這個逆向 State Monad。



（聲明：此文章的翻譯及發佈已經經過原文作者的許可。此譯文版權歸譯者所有，
並在 [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/) 下發佈）
