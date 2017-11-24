---
layout: post
title: "Linux 下 GFW 的 DNS 投毒解決方案"
date: 2014-07-26 02:27:35 +0800
comments: true
categories: solution
tags:
  - networking
  - gfw
  - unix
---

這篇日誌紀錄我在 Gentoo Linux 下配置 DNS Crypt + DNSmasq 的過程。

## 方案

DNSCrypt 是 OpenDNS 推出的一個用來加密 DNS 請求的代理，我用這個來繞開 GFW 的檢查。dnsmasq 是一款的 DNS 服務器，我主要用其提供 DNS 緩存的功能。

我將用 dnsmasq 設置本地的 DNS 服務器，然後配置 dnsmasq 從 dnscrypt 獲得結果並返回給請求者。而 dnscrypt 會把來自 dnsmasq 的請求加密丟到 opendns 的服務器，然後把解析結果返回回來。

那麼 dnsmasq 將偵聽 `53` 端口，而 dnscrypt 的端口隨意，只要和 dnsmasq 裡的配置匹配就可以了。我隨便用了 `1053` 作為 dnscrypt 的端口。

最後，把系統的 DNS 服務器設置為本機就可以了。

<!-- more -->

## 背景

長期以來我都在使用 [Opener DNS](https://code.google.com/p/openerdns/) 提供的免費 DNS 服務器
`42.120.21.30`，這個服務器工作正常，響應速度也很快，但是我在使用中遇到一個不知怎麼解決的問題而頭疼。那就是，很多 CLI 工具，包括
wget 和 curl，在設置了 Opener DNS 為 DNS 服務器後，都無法解析出任何地址，然而另一些工具，包括 nslookup 和
dig 等，卻沒有任何問題。而當切換為其他 DNS 服務器之後，wget 和 curl
之類的卻又可以正常解析了。這是很奇怪的現象，導致我後來連 emerge 都要開著 proxychains，甚是不方便。


今天下午心血來潮想隨手把這個問題解決掉。首先考慮的是用 iptables 把 GFW 發回來的假的 DNS
請求結果丟掉，只留正常的結果。網路上這種資源不少，我四處抄來抄去縫縫補補拼了一份 iptables 的配置，並丟在
[Gist](https://gist.github.com/shouya/c798a3aa7fb9a2a9b7fa) 上了。但問題是，好像 GFW
不光返回假的 DNS 結果，有時還回丟棄正確的結果，因為常常 DNS 請求只會返回假的結果，被 iptables
丟棄後導致請求超時，反而沒有效果。這種方法不能保證工作，而且速度很慢。

經測試 DNS over TCP 效果不錯，速度快而且相對穩定。所以我試了一下用 pdnsd 代理所有 DNS 請求並用 TCP 查詢上游 DNS
服務器。但是當我安裝了 pdnsd 之後，不知哪裡出錯了，總是在其偵聽本地端口時提示兩個

    [Errno 97] Address family not supported by protocol

錯誤，怎麼配置都不行，無論是 example 裡的還是 minimal 都無法解決。看起來可能是我系統配置的問題，或者是 pdnsd 的
bug。但我不是很確定到底怎麼回事，而且網路上好像沒有人有類似的情況，所以也不好上報。

## 安裝配置 dnscrypt

首先在 gentoo 的官方 portage 源裡並沒有 dnscrypt-proxy 這個程式， 所以需要手動加入 gentoo-zh 這個
overlay。 關於怎麼添加使用 overlay 這裡就不詳述了。

所以是常規地：

    $ sudo emerge -av dnscrypt-proxy

`dnscrypt-proxy` 會依賴於一個叫 `libsodium` 的庫。

在我的機器上 `libsodium` 和 `dnscrypt-proxy` 編譯時（準確說應該是`configure`的時候）會出一個錯誤：

    checking if gcc -std=gnu99 static flag -static works... *** Error in `/usr/bin/ld': corrupted double-linked list: 0x099a4178 ***

在 Github 上看到有人同樣遇到了 libsodium 的[這個問題](https://github.com/jedisct1/libsodium/issues/120)，原因是 gcc 的 `-pie` 和 `-static` 不兼容，
而解決方法是在 `configure` 時禁用 `pie` 就好了。所以手動改這兩個包的 ebuild 文件，在 `econf` 最後加上 `--disable-pie`。
於是兩個包都可以被編過了。改過後再 ebuild 一次 manifest，就可以正常安裝這兩個包了。

dnscrypt 的配置位於 `/etc/conf.d/dnscrypt-proxy`，編輯使之偵聽於 `1053` 端口。我的配置如下：

    DNSCRYPT_LOCALIP=127.0.0.1
    DNSCRYPT_LOCALPORT=1053
    DNSCRYPT_USER=dnscrypt
    DNSCRYPT_PROVIDER_NAME=2.dnscrypt-cert.opendns.com
    DNSCRYPT_PROVIDER_KEY=B735:1140:206F:225D:3E2B:D822:D7FD:691E:A1C3:3CC8:D666:8D0C:BE04:BFAB:CA43:FB79
    DNSCRYPT_RESOLVERIP=208.67.220.220
    DNSCRYPT_RESOLVERPORT=443

基本就是默認配置。

把 dnscrypt 跑起來：

    $ /etc/init.d/dnscrypt-proxy start

然後用 dig 測試之是否工作：

    $ dig @127.0.0.1 -p 1053 twitter.com

如果得到正確結果，則說明 dnscrypt 配置好了。


## 安裝 dnsmasq

安裝：

    $ sudo emerge -av dnsmasq

因為我幾乎不怎麼用國內服務，所以也用不著為國內服務設置國內的 DNS 來提高效率。所以我用 dnsmasq 純屬為了其緩存。

dnsmasq 的配置位於 `/etc/dnsmasq.conf`，這是我的配置：

    no-resolv
    no-poll
    server=127.0.0.1#1053

`no-resolv` 讓 dnsmasq 不去理睬 `/etc/resolv` 裡的配置。`no-poll` 讓其不檢測更新。`server` 裡對應的是 dnscrypt 的本地端口。

這樣就可以了，跑起來：

    $ /etc/init.d/dnsmasq start

試試：

    $ dig @127.0.0.1 -p 53 twitter.com

沒問題的話就說明 dnsmasq 已經工作了。

## 完成

設置系統 DNS！

    $ echo nameserver 127.0.0.1 | sudo tee /etc/resolv.conf


所以下一步，開機啟動來一發！

    $ sudo rc-update add dnscrypt-proxy default
    $ sudo rc-update add dnsmasq default

搞定，去玩吧。

## 參考資料

* [Issue #120: arch linux i686 build errors: corrupted double linked lists - jedisct1/libsodium](https://github.com/jedisct1/libsodium/issues/120)
* [详细安装配置 dnscrypt 和 dnsmasq](http://blog.sina.com.cn/s/blog_656126b20101ia39.html)
* [dnscrypt + dnsmasq](http://lilydjwg.is-programmer.com/2012/11/10/dnscrypt-dnsmasq.36288.html)


## 一些可能有用的鏈接

* [Opener DNS - Google Code](https://code.google.com/p/openerdns/)
* [DNSCrypt - OpenDNS](http://www.opendns.com/about/innovations/dnscrypt/)
* [DNS spoofing - Wikipedia](http://en.wikipedia.org/wiki/DNS_spoofing)
* [域名伺服器快取污染 - 維基百科](http://zh.wikipedia.org/wiki/%E5%9F%9F%E5%90%8D%E6%9C%8D%E5%8A%A1%E5%99%A8%E7%BC%93%E5%AD%98%E6%B1%A1%E6%9F%93#.E4.B8.AD.E5.9B.BD.E9.98.B2.E7.81.AB.E9.95.B7.E5.9F.8E)
* [如何本地避免GFW的DNS污染](http://igfw.net/archives/10890)
* [Gist: wen-long/DNS.md](https://gist.github.com/wen-long/9580811)
* [openwrt 上通过 pdnsd 和 dnsmasq 解决 dns 污染](https://wido.me/sunteya/use-openwrt-resolve-gfw-dns-spoofing)
* [dnsmasq如何强制用tcp与上游dns服务器通讯？](http://www.v2ex.com/t/75568)
* [比pdnsd更更更简单！用dnsmasq给你正确的DNS结果！](http://mariotaku.wordpress.com/2011/09/18/use-dnsmasq-easier-than-pdnsd/)
