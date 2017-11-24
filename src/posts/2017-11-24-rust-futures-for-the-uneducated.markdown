---
layout: post
title: "一篇面向初學者的簡短而又不失趣味(但願)的 Rust futures 入門指南"
date: 2017-11-24 15:14:35 +800
comments: true
tags:
  - translation
categories:
  - rust
lang: zh-Hant
---

> 原文《[Rust futures: an uneducated, short and hopefully not boring tutorial](https://dev.to/mindflavor/rust-futures-an-uneducated-short-and-hopefully-not-boring-tutorial---part-1-3k3)》由 [Francesco Cogno](https://dev.to/mindflavor) 發佈 [於其博客](https://dev.to/mindflavor/rust-futures-an-uneducated-short-and-hopefully-not-boring-tutorial---part-1-3k3)。本文是一篇系列教程，原文已更新至[第四部分](https://dev.to/mindflavor/rust-futures-an-uneducated-short-and-hopefully-not-boring-tutorial---part-4---a-real-future-from-scratch-734)，目前譯文只完成了前三部分。



## 第一部分

### 引言

如果你喜歡 Rust，你可能會注意到整個 Rust 社區正在流行使用 _future_。很多知名的 crate 開始徹底擁抱 _future_（譬如說 [Hyper](https://github.com/hyperium/hyper)），我們也得學會用才行。如果你覺得自己是小白程序員，可能會覺得很難理解 _future_ 的原理。當然原作者 [Crichton 的教程](https://tokio.rs/docs/getting-started/futures/) 是很好的教材，雖然講得很通透，但我覺得這個教材有點難理解，不適合上手。

我想我肯定不是唯一這麼想的人，所以我就把自己的發現和理解分享給大家，幫大家熟悉 future 的用法。



### Future 簡述

Future 可以被理解為一種不會**立即**執行的古怪函數，相反，它們在**未來**才會執行（所以才叫做 future）。使用 future 而非普通函數的原因很多，譬如說為了性能，為了優雅，為了可組合性，等等。Future 的缺點在於寫起來有點難，好吧，是**很**難。如果你都不知道一個函數何時會執行，你怎麼知道它的前因後果是什麼？

因此，不同的編程語言會用不同的語言特徵來拯救我們這些可憐又無助的程序員。



### Rust 的 Future

Rust 社區的發展是迅速的，Rust 中 future 的實現也是。所以，一如繼往要聲明一下，你從這裡學到的知識可能會在一段時間後顯得過時，所以要注意一下。

Rust 的 futures 其實就是 [Results](https://doc.rust-lang.org/std/result/)：也就是說你需要指定預期返回類型和錯誤類型。

讓我們把普通函數轉換成 future 吧。在本例中的函數返回 `u32` 或者 Box 的 Error trait 對象：

```rust
fn my_fn() -> Result<u32, Box<Error>> {
    Ok(100)
}
```

很簡單對吧，現在看看它對應的 future 實現：

```rust
fn my_fut() -> impl Future<Item = u32, Error = Box<Error>> {
    ok(100)
}
```

我們來看看一下這兩種實現的區別。首先，返回值不再是 `Result` 類型，而是 `impl `[Future](http://alexcrichton.com/futures-rs/futures/future/trait.Future.html). 這個語法（目前還只能在 Rust nightly 版本上用）意味著我們會返回實現 Future trait 的類型。其中的 `<Item = u32, Error = Box<Error>>` 其實指定了返回類型和錯誤類型，只是語法比 Result 囉嗦一點。

> 注意這種語法需要啟用 `conservative_impl_trait` nightly feature。當然你也可以返回 `Box<Future<...>>`，但我個人覺得後者比較笨拙。

還應該注意一點的是作為返回值的 `Ok(100)`。Result 風格的函數我們會用大寫的 `Ok`，因為它是一個 enum；而在 future 中我們使用的是小寫的 `ok` 方法。

> 首條規則：在 future 中記得使用小寫的函數返回方法。



還不錯，對吧？現在的問題是怎麼執行這種函數？Result 版本的函數可以直接調用，哦注意我們返回的是 `Result` 所以我們得 `unwrap()` 掉之才能訪問裡面的值。

```rust
let retval = my_fn().unwrap();
println!("{:?}", retval);
```

Future 則會在實際執行前就返回了（或者更準確地講，我們先返回了準備以後執行的*代碼*），所以我們得有個方法執行它。對此我們可以用 `Reactor`，創建個 `Reactor` 並調用其 `run` 方法就能執行 future 了。對於我們上面的例子，可以這樣：

```rust
let mut reactor = Core::new().unwrap();

let retval = reactor.run(my_fut()).unwrap();
println!("{:?}", retval);
```

注意這裡我們 `unwrap` 的是 `run` 的返回值，不是 `my_fut()` 自身的。

真是小菜一碟。



### 鏈式方法

我們能把多個 future 鏈起來按順序執行，這正是 future 強大的原因之一。設想一下邀父母一起吃晚餐的場景。首先給他們發送短信，等待他們回應；收到回應後就著手準備晚餐（不想自己動手的話，可以裝病逃過幹活）。鏈式方法就有點像這樣子。讓我們看看下面的例子吧。

首先我們再寫個函數，叫做 `squared`，這裡我們同時寫出標準版本和 future 版本：

```rust
fn my_fn_squared(i: u32) -> Result<u32, Box<Error>> {
    Ok(i * i)
}


fn my_fut_squared(i: u32) -> impl Future<Item = u32, Error = Box<Error>> {
    ok(i * i)
}
```

我們可以這樣調用標準版的函數：

```rust
let retval = my_fn().unwrap();
println!("{:?}", retval);

let retval2 = my_fn_squared(retval).unwrap();
println!("{:?}", retval2);
```

我們也可以用同樣的流程多次調用 Reactor 來執行 future 版的函數：

```rust
let mut reactor = Core::new().unwrap();

let retval = reactor.run(my_fut()).unwrap();
println!("{:?}", retval);

let retval2 = reactor.run(my_fut_squared(retval)).unwrap();
println!("{:?}", retval2);
```

但其實有種更好的方法。Future 作為一個 trait，其實有很多方法可以調用（我們只會講到一小部分），其中 `and_then` 做的就是我們上面做的事情，但用它我們可以省去調兩次 `Reactor.run(...)`。來看看：

```rust
let chained_future = my_fut().and_then(|retval| my_fn_squared(retval));
let retval2 = reactor.run(chained_future).unwrap();
println!("{:?}", retval2);
```

看第一行，我們創造了一個新~~未來~~ future，叫做 `chained_future`，由 `my_fut` 和 `my_fut_squared` 組合而成。

把一個 future 的返回值傳給下一個 future 的部分有點棘手，在 Rust 中，我們使用閉包參數來捕獲之（`|...|` 中間的部分）。這個過程是這樣的：

1. 預定執行 `my_fut()`。
2. 當 `my_fut()` 執行完畢並返回一個叫做 `retval` 的變量，把它存入 `my_fut()` 的執行結果中。
3. 在那之後預訂執行 `my_fn_squared(i: u32)`，將 `retval` 作為參數傳進去。
4. 把這個 _指令流程_ 打包成一個 future 函數，叫做 `chained_future`。

接下來就一如既往了，我們調用 `Reactor` 的 `run` 方法執行 `chained_future` 所打包的整個 _指令流程_ 來得到執行結果。

當然，我們其實可以無限這樣把方法鏈起來，不要擔心這樣做會有性能損失：future 鏈是**零開銷**的（除非你選擇使用 `Box<Future>` 之類的手段）。

> 在寫 future 鏈的時候，Rust 的 borrow 檢查器可能跳出來報錯。這時你可以試試用 `move` 關鍵字把這些被捕獲的變量移開。



### Future 和普通函數的混合寫法

我們還可以把 future 和普通函數鏈到一起，這樣就不用要求每個函數都是 future。有時我們可能會用到外部提供的，無法修改的函數，那麼就有必要這樣做了。

如果這個普通函數不返回 `Result`，我們可以簡單地在一個閉包中直接調用。譬如說這個普通函數：

```rust
fn fn_plain(i: u32) -> u32 {
    i - 50
}
```

我們的鏈式 future 可以這樣寫：

```rust
let chained_future = my_fut().and_then(|retval| {
    let retval2 = fn_plain(retval);
    my_fut_squared(retval2)
});
let retval3 = reactor.run(chained_future).unwrap();
println!("{:?}", retval3);
```

如果這個函數返回 `Result`，那麼就有更好的做法了。讓我們試試把 `my_fn_squared(i: u32) -> Result<u32, Box<Error>>` 鏈起來吧。

雖然你無法對著 future 的 `and_then` 方法調用返回 `Result` 的函數，但是 future 的設計者已經考慮過這種情況了！Future 中的 `done` 方法的作用正是把 `Result` 轉換為 `impl Future`。

這是什麼意思呢？也就是說，我們可以把普通函數包在 `done` 方法中，進而當成一個 future 來用！

我們來試試：

```rust
let chained_future = my_fut().and_then(|retval| {
    done(my_fn_squared(retval)).and_then(|retval2| my_fut_squared(retval2))
});
let retval3 = reactor.run(chained_future).unwrap();
println!("{:?}", retval3);
```

注意第二行中的 `done(my_fn_squared(retval))` ，這樣我們就把普通函數當成 future 來鏈起來用了。再看看如果沒有 `done` 會怎樣：

```rust
let chained_future = my_fut().and_then(|retval| {
    my_fn_squared(retval).and_then(|retval2| my_fut_squared(retval2))
});
let retval3 = reactor.run(chained_future).unwrap();
println!("{:?}", retval3);
```

編譯器會報錯：

```
   Compiling tst_fut2 v0.1.0 (file:///home/MINDFLAVOR/mindflavor/src/rust/tst_future_2)
error[E0308]: mismatched types
   --> src/main.rs:136:50
    |
136 |         my_fn_squared(retval).and_then(|retval2| my_fut_squared(retval2))
    |                                                  ^^^^^^^^^^^^^^^^^^^^^^^ expected enum `std::result::Result`, found anonymized type
    |
    = note: expected type `std::result::Result<_, std::boxed::Box<std::error::Error>>`
               found type `impl futures::Future`

error: aborting due to previous error

error: Could not compile `tst_fut2`.
```

其中 `expected type std::result::Result<_, std::boxed::Box<std::error::Error>> found type impl futures::Future` 的錯誤信息有點誤導。其實這個錯誤是因為我們在該傳入 `impl Future` 的地方傳入了 `Result`（錯誤信息中反過來了）。編譯器的這個誤導會讓很多初學者困惑（見 <https://github.com/alexcrichton/futures-rs/issues/402>），我們後面再回顧這個問題。



### 泛型

我們再看看 Rust 中的泛型怎麼玩。看這個例子：

```rust
fn fut_generic_own<A>(a1: A, a2: A) -> impl Future<Item = A, Error = Box<Error>>
where
    A: std::cmp::PartialOrd,
{
    if a1 < a2 {
        ok(a1)
    } else {
        ok(a2)
    }
}
```

這個函數版回兩個參數中的較小值。注意，無論我們的函數會不會返回錯誤，我們都得給這個 future 指定一個錯誤類型，還有就是這裡返回值中的 `ok` 是全小寫的函數，而非那個 enum。

然後我們可以這樣執行這個 future：

```rust
let future = fut_generic_own("Sampdoria", "Juventus");
let retval = reactor.run(future).unwrap();
println!("fut_generic_own == {}", retval);
```

相信你已經領悟到要旨了 :)。現在這種代碼應該看起來很合理多了。你可能注意到在這裡我避免使用引用，只用了 owned 的值，這是因為 `impl Future` 對 lifetime 的處理方法不太一樣，在後面的章節會講到怎麼搞定 lifetime 的問題。



### 註釋

如果想要運行上面的這些代碼片段，要在 `Cargo.toml` 文件中加上這幾行：

```toml
[dependencies]
futures="*"
tokio-core="*"
futures-await = { git = 'https://github.com/alexcrichton/futures-await' }
```

然後在 `src/main.rs` 最上面加上這幾行：

```rust
#![feature(conservative_impl_trait, proc_macro, generators)]

extern crate futures_await as futures;
extern crate tokio_core;

use futures::done;
use futures::prelude::*;
use futures::future::{err, ok};
use tokio_core::reactor::Core;
use std::error::Error;
```

上面的內容並不需要引入 `futures-await` crate，但後面我們講到的內容會用到它。



(第一部分完)



##第二部分

###引言

第一部分的內容中我們學習了如何使用 Rust 中的 futures。如果你掌握到訣竅，會覺得非常非常簡單。在第二部分我們會著重於學習如何避開常見的陷阱。



### 麻煩的 Error

把 future 鏈起來並不難，我們已經學過 `and_then()` 方法的用法了。但是之前我們刻意避開了一個坑的地方，使用了 `Box<Error>` 來表示異常返回值。為何我們不用更具體的錯誤類型呢？因為把 future 鏈起來的時候需要 _把它們的錯誤類型都匹配上_ 才行。

> 鏈式 futures 的錯誤類型必須統一。

來看演示吧。假如說我們有兩個類型 `ErrorA` 和 `ErrorB`，分別實現為 [error::Error trait](https://doc.rust-lang.org/std/error/trait.Error.html) 的實例，雖然說並不必要，但我覺得這是個好習慣。類型需要實現 [std::fmt::Display](https://doc.rust-lang.org/std/fmt/trait.Display.html) 才能被實現為 `Error` trait，所以我們也得同時實現這個 `Display`。為了突出重點，我會儘可能舉簡單例子。

```rust
#[derive(Debug, Default)]
pub struct ErrorA {}

impl fmt::Display for ErrorA {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ErrorA!")
    }
}

impl error::Error for ErrorA {
    fn description(&self) -> &str {
        "Description for ErrorA"
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
```

`ErrorB` 也一樣：

```rust
#[derive(Debug, Default)]
pub struct ErrorB {}

impl fmt::Display for ErrorB {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "ErrorB!")
    }
}

impl error::Error for ErrorB {
    fn description(&self) -> &str {
        "Description for ErrorB"
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
```

接下來就可以開始用這些錯誤類型了。我們來寫幾個返回錯誤的 future，再提醒一下，future 只是一種返回 `Result<A, B>` 的函數，只是語法有點不一樣而已。

```rust
fn fut_error_a() -> impl Future<Item = (), Error = ErrorA> {
    err(ErrorA {})
}

fn fut_error_b() -> impl Future<Item = (), Error = ErrorB> {
    err(ErrorB {})
}
```

現在我們在 `main` 函數中用一下這兩個 future：

```rust
let retval = reactor.run(fut_error_a()).unwrap_err();
println!("fut_error_a == {:?}", retval);

let retval = reactor.run(fut_error_b()).unwrap_err();
println!("fut_error_b == {:?}", retval);
```

結果一點都不意外：

```rust
fut_error_a == ErrorA
fut_error_b == ErrorB
```

還不錯，現在我們來把這些 future 鏈起來：

```rust
let future = fut_error_a().and_then(|_| fut_error_b());
```

我們這裡調用了 `fut_error_a` 函數，然後進一步調用 `fut_error_b`，我們暫時不關心 `fut_error_a` 的返回值，所以用 `_` 表示把這個值丟掉了。

更具體的描述是，我們在試圖把一個 `impl Future<Item=(), Error=ErrorA>` 和一個 `impl Future<Item=(), Error=ErrorB>` 鏈起來。

我們來試試編譯一下：

```rust
Compiling tst_fut2 v0.1.0 (file:///home/MINDFLAVOR/mindflavor/src/rust/tst_future_2)
error[E0271]: type mismatch resolving `<impl futures::Future as futures::IntoFuture>::Error == errors::ErrorA`
   --> src/main.rs:166:32
    |
166 |     let future = fut_error_a().and_then(|_| fut_error_b());
    |                                ^^^^^^^^ expected struct `errors::ErrorB`, found struct `errors::ErrorA`
    |
    = note: expected type `errors::ErrorB`
               found type `errors::ErrorA`
```

錯誤信息很清楚，我們在本該有 `ErrorB` 的地方得到一個 `ErrorA`。通俗來講：

> 在把兩個 future 鏈起來的時候，第一個函數的錯誤類型應該和第二個一樣。

這正是 rustc 告訴我們的：`fut_error_b()` 返回了 `ErrorB`，那麼 `fut_error_a()` 也必須返回 `ErrorB`，但實際上它返回了 `ErrorA`，編譯就失敗了。

那怎麼解決這個問題呢？所幸我們可以用一個叫作 `map_err` 的方法。在此例中我們想要把 `ErrorA` 轉成 `ErrorB`，所以我們要在其中插入一個 `map_err` 的調用：

```rust
let future = fut_error_a()
    .map_err(|e| {
        println!("mapping {:?} into ErrorB", e);
        ErrorB::default()
    })
    .and_then(|_| fut_error_b());

let retval = reactor.run(future).unwrap_err();
println!("error chain == {:?}", retval);
```

現在這個例子能編過也能跑起來了，輸出正如所料：

```
mapping ErrorA into ErrorB
error chain == ErrorB
```

我們來進一步探索這個例子。假如說我們想要把 `ErrorA` 和 `ErrorB` 鏈起來，然後再鏈上 `ErrorA`，像這樣：

```rust
let future = fut_error_a()
    .and_then(|_| fut_error_b())
    .and_then(|_| fut_error_a());
```

和上面的規則是一樣的，這樣組合 `future` 是不合法的。所以我們要把 `fut_error_a` 和 `fut_error_b` 中間的錯誤類型轉成 `ErrorB`。然後再在 `fut_error_b` 和 `fut_error_a` 之間把錯誤類型轉回 `ErrorA`。下面的代碼有點醜但是跑得起來：

```rust
let future = fut_error_a()
    .map_err(|_| ErrorB::default())
    .and_then(|_| fut_error_b())
    .map_err(|_| ErrorA::default())
    .and_then(|_| fut_error_a());
```



### `From` 來相救

簡化上面代碼的一種方法是利用 [std::convert::From](https://doc.rust-lang.org/std/convert/trait.From.html) trait。簡單來說，使用 From trait 我們可以告訴 Rust 該怎麼全自動從源類型轉換到目標類型。雖然這個 trait 會消耗掉源錯誤類型的所有權，但對我們的例子而言這沒什麼關係。另外一點是，這個 trait 會假設轉換從不失敗。最後，我們只能對我們寫的類型實現這個 trait，所以這種方法並不是一直能用的。

我們來實現一下 `From<ErrorA> for ErrorB` 和 `FromInto<ErrorB> for ErrorA` 吧：

```rust
impl From<ErrorB> for ErrorA {
    fn from(e: ErrorB) -> ErrorA {
        ErrorA::default()
    }
}

impl From<ErrorA> for ErrorB {
    fn from(e: ErrorA) -> ErrorB {
        ErrorB::default()
    }
}
```

這樣一來，上面例子中的代碼就可以被簡化了：只要用 `from_err()` 取代 `map_err()` 函數就可以。Rust 很聰明，能自己找到對應的轉換函數：

```rust
let future = fut_error_a()
   .from_err()
   .and_then(|_| fut_error_b())
   .from_err()
   .and_then(|_| fut_error_a());
```

這裡依然混合著各種錯誤類型轉換，但是好在錯誤類型轉換的部分不用在這裡具體寫出來了，這樣代碼看起來好讀多了。

Future crate 很聰明，`from_err` 的代碼只會在出錯的情況下才會被調用到，所以這一切也是沒有任何運行時開銷的。



### Lifetimes

Rust 還有一個特性，叫作引用的顯式 lifetime 標註。Rust 支持 _lifetime 省略_，所以大部份時候我們可以省去顯式標註 lifetime。舉個例子，我們想要寫接受字符串引用作為參數的函數，如果沒出錯的話，會返回同字符串引用：

```rust
fn my_fn_ref<'a>(s: &'a str) -> Result<&'a str, Box<Error>> {
    Ok(s)
}
```

留意一下 `<'a>` 這部分，這是聲明 lifetime 的語法，接下來的 `s: &'a str` 是表示參數 `s` 是字符串引用類型，它將在 `'a` 有效的期間保持有效。返回類型 `Result<&' str, Box<Error>>` 意味著返回值會包含字符串引用，而它也必須在 `'a` 有效期間保證有效。換句話說，輸入的字符串和輸出的對象必須要有相同的 lifetime。

這種語法過於冗長，所以 Rust 允許我們在這種常見的場景省略掉顯式指定的 lifetime。因而這個函數可以這樣寫：

```rust
fn my_fn_ref(s: &str) -> Result<&str, Box<Error>> {
    Ok(s)
}
```

可以看到我們沒有再提到任何 lifetime 了，雖然他們還在。這個函數聲明要簡短多了，也更容易一眼就看懂了。

不過... 至少時至如今，我們還不能省略 future 中的 lifetime 標註。如果照著前面的代碼寫這樣一個 `impl Future` 的話：

```rust
fn my_fut_ref_implicit(s: &str) -> impl Future<Item = &str, Error = Box<Error>> {
    ok(s)
}
```

這樣就會出錯。上面的代碼在我的電腦上（`rustc 1.23.0-nightly (2be4cc040 2017-11-01)`），引起了編譯錯誤：

```
   Compiling tst_fut2 v0.1.0 (file:///home/MINDFLAVOR/mindflavor/src/rust/tst_future_2)
error: internal compiler error: /checkout/src/librustc_typeck/check/mod.rs:633: escaping regions in predicate Obligation(predicate=Binder(ProjectionPredicate(ProjectionTy { substs: Slice([_]), item_def_id: DefId { krate: CrateNum(15), index: DefIndex(0:330) => futures[59aa]::future[0]::Future[0]::Item[0] } }, &str)),depth=0)
  --> src/main.rs:39:36
   |
39 | fn my_fut_ref_implicit(s: &str) -> impl Future<Item = &str, Error = Box<Error>> {
   |                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

note: the compiler unexpectedly panicked. this is a bug.

note: we would appreciate a bug report: https://github.com/rust-lang/rust/blob/master/CONTRIBUTING.md#bug-reports

note: rustc 1.23.0-nightly (2be4cc040 2017-11-01) running on x86_64-unknown-linux-gnu

thread 'rustc' panicked at 'Box<Any>', /checkout/src/librustc_errors/lib.rs:450:8
note: Run with `RUST_BACKTRACE=1` for a backtrace.
```

別灰心，記得 `impl Future` 還只是試驗特性。要解決這個問題，我們要把所有的 lifetime 顯式標註出來：

```rust
fn my_fut_ref<'a>(s: &'a str) -> impl Future<Item = &'a str, Error = Box<Error>> {
    ok(s)
}
```

這樣就一切正常了。



### 帶 lifetime 的 `impl Future`

我們不僅僅要在參數上顯示標註 lifetime，就算只是想要返回隱含 lifetime 的 `impl Future`，那麼也得把它標註上。舉例來說，假如我們這次想寫個接受 `&str` 參數並返回 String 的 future，那麼我們必須把參數的 lifetime 也顯式標注上，所以可能會寫成這樣：

```rust
fn my_fut_ref_chained<'a>(s: &'a str) -> impl Future<Item = String, Error = Box<Error>> {
    my_fut_ref(s).and_then(|s| ok(format!("received == {}", s)))
}
```

但實際上這樣也是不行的，因為返回的類型並沒有隱含 `'a`，錯誤信息如下：

```
error[E0564]: only named lifetimes are allowed in `impl Trait`, but `` was found in the type `futures::AndThen<impl futures::Future, futures::FutureResult<std::string::String, std::boxed::Box<std::error::Error + 'static>>, [closure@src/main.rs:44:28: 44:64]>`
  --> src/main.rs:43:42
   |
43 | fn my_fut_ref_chained<'a>(s: &'a str) -> impl Future<Item = String, Error = Box<Error>> {
   |                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

要解決這個問題，我們只需要把 `'a` 加到 `impl Future` 聲明後面就可以了，像這樣：

```rust
fn my_fut_ref_chained<'a>(s: &'a str) -> impl Future<Item = String, Error = Box<Error>> + 'a {
    my_fut_ref(s).and_then(|s| ok(format!("received == {}", s)))
}
```

這裡要感謝 [HadrienG](https://users.rust-lang.org/u/hadrieng/summary) 的解決方案，參見 [Trouble with named lifetimes in chained futures](https://users.rust-lang.org/t/solved-trouble-with-named-lifetimes-in-chained-futures/11943)。

這樣你就可以一如既往使用 reactor 執行這個函數了：

```rust
let retval = reactor
    .run(my_fut_ref_chained("str with lifetime"))
    .unwrap();
println!("my_fut_ref_chained == {}", retval);
```

結果也如預期一樣：

```
my_fut_ref_chained == received == str with lifetime
```



### 小結

後面我們會具體聊聊 reactor，我們也會從頭開始自己寫一個實現 future 的類型。



(第二部分完)



## 第三部分

### 引言

本文中我們會試著解釋 reactor 的原理。前幾篇我們已經在大量使用 reactor 了，但我們只是將之當作一個黑盒來用的。現在是時候打開這個盒子，看看裡面是怎樣的了！



### Reactor？循環？

Reactor 簡單來講就是一個循環。要解釋之，我想到一個類比：假如說你發了郵件邀請了一位女生/男生約會（好吧我知道這樣有點老套），你 _想要_ 收到答覆，所以你不斷，不斷，不斷去檢查有沒有新郵件，直到你終於得到了答覆。

Rust 的 reactor 就有點像這樣子。給它一個 future，它會不斷檢查這個 future 的狀態，直到這個 future 完成或者出錯為止。它通過一個叫 `poll` （輪詢）的函數來實現這樣的功能，一點也不奇怪。Future 類型的實現者自己必須實現 `poll` 函數才行，他們要做的就是返回一個類行為 `Poll<T, E>` 的值（詳參 [Poll 文檔](http://alexcrichton.com/futures-rs/futures/type.Poll.html)）。好吧事實上 reactor 並不會不斷去調用你寫的輪詢函數，但暫時我們先別深究細節。我們來從這個例子開始看吧。



### 從頭開始實現一個 Future

為了測試我們學得怎樣，我們現在來從頭實現個 `future`。換句話說，我們要來手動實現一個滿足 `Future` trait 的類型。我們來實現最小可用的 future，這個 future 在過一段時間後才會返回預先設定的值。

我們將我們的這個 struct 類型命名為 `WaitForIt`：

```rust
#[derive(Debug)]
struct WaitForIt {
    message: String,
    until: DateTime<Utc>,
    polls: u64,
}
```

我們的這個類型會在裡面保存一條自定義的消息，等待超時的的時間，以及它被輪詢過的次數。為了讓我們的代碼看起來乾淨一點，我們來實現一下這個類型的 `new` 函數：

```rust
impl WaitForIt {
    pub fn new(message: String, delay: Duration) -> WaitForIt {
        WaitForIt {
            polls: 0,
            message: message,
            until: Utc::now() + delay,
        }
    }
}
```

我們這裡寫的  `new`  函數會創建一個 `WaitForIt` 實例，然後初始化之。

現在我們來實現 `Future` trait，我們唯一要做的就是實現好 `poll`  方法：

```rust
impl Future for WaitForIt {
    type Item = String;
    type Error = Box<Error>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let now = Utc::now();
        if self.until < now {
            Ok(Async::Ready(
                format!("{} after {} polls!", self.message, self.polls),
            ))
        } else {
            self.polls += 1;

            println!("not ready yet --> {:?}", self);
            Ok(Async::NotReady)
        }
    }
}
```

讓我們一步一步來看看。下面的這兩行有點難搞：

```rust
    type Item = String;
    type Error = Box<Error>;
```

它們叫做[關聯類型](https://doc.rust-lang.org/book/first-edition/associated-types.html)（associated types）。在這裡它們用於指示這個 future 返回什麼類型，還有出錯時會返回什麼錯誤類型。所以我們可以說：這個 future 會在將來返回要麼一個 `String`，要麼一個 `Box<Error>`。

下面這一行定義了 `poll` 函數：

```rust
    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
```

其中 `Self::Item` 和 `Self::Error` 部分引用的是上面我們定義的關聯類型。在本例中，這個方法等價於這樣寫：`fn poll(&mut self) -> Poll<String, Box<Error>>`。

然後就是邏輯部分了：

```rust
let now = Utc::now();
if self.until < now {
  // Tell reactor we are ready!
} else {
  // Tell reactor we are not ready! Come back later!
}
```

我們怎麼通知 reactor 這個 future 還沒完成呢？返回 `Ok<Async::NotReady>` 就可以了。如果這個 future 已經完成了，我們就返回 `Ok<Async::Ready(T)>`。所以這個函數就這樣實現：

```rust
impl Future for WaitForIt {
    type Item = String;
    type Error = Box<Error>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let now = Utc::now();
        if self.until < now {
            Ok(Async::Ready(
                format!("{} after {} polls!", self.message, self.polls),
            ))
        } else {
            self.polls += 1;

            println!("not ready yet --> {:?}", self);
            Ok(Async::NotReady)
        }
    }
}
```

要執行這個 future，我們要在 `main` 函數裡創建一個 `reactor`，然後用它來跑我們剛實現的 future 類型。

```rust
fn main() {
    let mut reactor = Core::new().unwrap();

    let wfi_1 = WaitForIt::new("I'm done:".to_owned(), Duration::seconds(1));
    println!("wfi_1 == {:?}", wfi_1);

    let ret = reactor.run(wfi_1).unwrap();
    println!("ret == {:?}", ret);
}
```

如果運行一下上面的程序，你應該以為這個 future 會等一秒鐘，然後正常返回。我們運行一下看看：

```rust
Running `target/debug/tst_fut_create`
wfi_1 == WaitForIt { message: "I\'m done:", until: 2017-11-07T16:07:06.382232234Z, polls: 0 }
not ready yet --> WaitForIt { message: "I\'m done:", until: 2017-11-07T16:07:06.382232234Z, polls: 1 }
```

然....而並不是這樣，在這些輸出之後，代碼就卡住了。另外值得注意的是該進程一點 CPU 都沒有佔用：

![img](https://i.imgur.com/EWwfdMq.png)

為什麼會這樣呢？這就是 reactor 神奇的地方了：reactor 並不會輪詢 _駐留函數_（parked function），除非被明確指示去那樣做。在本例中，reactor 一開始就立刻調用了我們的函數，我們返回了 `Async::NotReady`，所以 reactor 就把我們的函數 _駐留_ 在一邊了。除非有東西取消這個函數的 _駐留_ 狀態，否則 reactor 就不會再調用它了。在等待的時候 reactor 處於空閑狀態，自然也不會佔用 CPU 了。這樣的機制意味著 reactor 能有更高的 CPU 利用效率，不會浪費 CPU 去不斷輪詢 future 的結果完成與否。在前面查郵件的例子中，這相當於我們不用不斷手動刷新郵件頁面，而只需靜候郵件通知就好了，有空的時候我們還可以玩玩 守望屁股。

另一個更有意義的例子是接收從網絡來的數據。我們可以阻塞線程，等待網絡封包到來，也可以在等待的時候做些別的事情。你可能會好奇為什麼這樣做會比開個線程更好，這裡我們不涉及具體細節，只要記住一般來說這樣比線程效率高就好了。



### 取消駐留

那該怎麼修正上面的代碼呢？我們要找個方法把 `future` 的駐留狀態取消掉。理想情況下，外部事件會取消駐留 future 的狀態（譬如說收到網絡封包、按下按鍵），但是我們的例子中我們就用下面這行手動取消駐留：

```rust
futures::task::current().notify();
```

所以現在我們的 `future` 實現變成了：

```rust
impl Future for WaitForIt {
    type Item = String;
    type Error = Box<Error>;

    fn poll(&mut self) -> Poll<Self::Item, Self::Error> {
        let now = Utc::now();
        if self.until < now {
            Ok(Async::Ready(
                format!("{} after {} polls!", self.message, self.polls),
            ))
        } else {
            self.polls += 1;

            println!("not ready yet --> {:?}", self);
            futures::task::current().notify();
            Ok(Async::NotReady)
        }
    }
}
```

現在運行一下代碼吧：

![img](https://i.imgur.com/GpnyUie.png)

現在代碼成功執行完了，注意在這個例子中，該函數在一秒內被調用了 50k 次。真是浪費資源，這也就說明了為什麼只該在發生事件時取消駐留。關於這點，我們可以看看進程的 CPU 佔用率：

![img](https://i.imgur.com/Q16WP9s.png)

留意一下這裡的輪詢只用了一個線程，這裡本來就是這樣設計的，以實現更高的效率。當然，你也可以使用多線程就是了。



### Joining

Reactor 的一個實用特性是可以併發執行多個 future。我們在單線程中達到高效併發的方法是這樣的，一個 `future` 駐留時，其他 `future` 就可以趁機執行了。

我們繼續用 `WaitForIt` 類型來講這個例子，這次我們同時調用它兩次。首先我們要創建兩個 `future`：

```rust
let wfi_1 = WaitForIt::new("I'm done:".to_owned(), Duration::seconds(1));
println!("wfi_1 == {:?}", wfi_1);
let wfi_2 = WaitForIt::new("I'm done too:".to_owned(), Duration::seconds(1));
println!("wfi_2 == {:?}", wfi_2);
```

接下來我們調用 [futures::future::join_all](http://alexcrichton.com/futures-rs/futures/future/fn.join_all.html) 函數，這個函數的參數是一個包含多個 future 的迭代器，我們就用一個簡單的 vector 吧：

```rust
let v = vec![wfi_1, wfi_2];
```

簡單來說 `join_all` 函數會返回一個新 future，輪詢這個 future 會不斷交替地返回輸入的 future。

現在的完整代碼如下：

```rust
fn main() {
    let mut reactor = Core::new().unwrap();

    let wfi_1 = WaitForIt::new("I'm done:".to_owned(), Duration::seconds(1));
    println!("wfi_1 == {:?}", wfi_1);
    let wfi_2 = WaitForIt::new("I'm done too:".to_owned(), Duration::seconds(1));
    println!("wfi_2 == {:?}", wfi_2);

    let v = vec![wfi_1, wfi_2];

    let sel = join_all(v);

    let ret = reactor.run(sel).unwrap();
    println!("ret == {:?}", ret);
}
```

我們來跑一下，輸出應該是類似下面的效果：

![img](https://i.imgur.com/VlQSMI4.png)

注意這裡我們可以看到 reactor 在交替請求我們的 future，第一個被調用，然後第二個，然後又是第一個，如此反覆，直到兩個 future 都完成為止。從圖中你可以看到第一個 future 比第二個先完成了，第二個 future 在完成前被單獨調用了兩次。



### Select

`future` 中還有很多其他函數，另一個有趣的函數叫 `select`。Select 函數會同時執行兩個 future，並且會返回最先執行完成的那個。這個函數很適合用來實現超時，這裡是我們的例子：

```rust
fn main() {
    let mut reactor = Core::new().unwrap();

    let wfi_1 = WaitForIt::new("I'm done:".to_owned(), Duration::seconds(1));
    println!("wfi_1 == {:?}", wfi_1);
    let wfi_2 = WaitForIt::new("I'm done too:".to_owned(), Duration::seconds(2));
    println!("wfi_2 == {:?}", wfi_2);

    let v = vec![wfi_1, wfi_2];

    let sel = select_all(v);

    let ret = reactor.run(sel).unwrap();
    println!("ret == {:?}", ret);
}
```

### 結語

下一篇我們會探索令人興奮的 `await!` 宏，祝玩得開心！



## 第四部分

待翻譯⋯⋯



> 聲明：此文章的翻譯及發佈已經經過原文作者的許可。此譯文版權歸譯者所有，並在 [CC BY-NC-SA 4.0](http://creativecommons.org/licenses/by-nc-sa/4.0/) 下發佈。
