---
layout: post
title: "服務器被入侵手記"
date: 2016-10-04 22:43:45 +0800
comments: true
categories: unix
tags:
  - security
  - docker
---


## 背景與起因

我想趁放假幾天玩一下 Docker 集群，順便跑個 shipyard 通過網頁管理自己 VPS 上運行的服務。

因為是完全的新手，在這之前甚至連 Swarm 是什麼都不知道，所以完全按照 shipyard 官方文檔說的配置來都沒成功。

最後發現是很愚蠢的錯誤，只是因為其中一步要使用一個服務把 `/var/run/docker.sock` 代理到 TCP/2375 端口上，而 swarm-agent 不知為何不能使用 `127.0.0.1:2375` 連接到本機，一定要用公網兜一圈才能找到本機的控制，而我忘記在防火牆上打開此特例。開了此特例，故問題解決。

因為以前一直是在本機跑 Docker，也未暴露任何端口，故沒有啟用任何加密措施。事實上 [shipyard 的手冊](http://shipyard-project.com/docs/deploy/automated/) 已經提醒過開放此端口相當於把 Docker 控制權給出去，應該設置好 Docker 通訊用 Socket 的 TLS 加密再玩，但我忽視了這個警告。

shipyard 終於成功跑起來了，我玩了一會，得意之至，忘記了上面的警告和自己親手暴露的端口。接著我就折騰 Jenkins 之類的去了，玩得不亦樂乎。

<!-- more -->

## 發現入侵


次日我繼續調戲 Docker，剛跑起 `kevana/ui-for-docker`，突然發現多了兩個正在運行的 Containers：

![ui-for-docker 截圖](http://i.imgur.com/1a2exdg.png)

（當時這兩個 Containers 還在運行）

看到「exploit.sh」我就知道完了，第一時間 kill 掉了 `romantic_snyder` 和 `thirsty_kare` 這兩個 Containers。

當務之急是立刻把 shipyard/swarm/etcd/docker-proxy 的服務暫時全停掉，還有把前一天給 2375 端口開的防火牆特例砍掉。



## 研究入侵

根據 Image 名字搜索了一下，[chrisfosterelli/rootplease](https://hub.docker.com/r/chrisfosterelli/rootplease/) 是一個很 naïve 的 exploit 程序，原理就是通過 `docker run -v /:/hostOS` 來跑這個鏡像加上 `chroot` 來獲得 root 權限。入侵者應該在裡面執行了些什麼命令。

而另一個 Container 跑了個很單純的 CentOS 加上 Bash，想必也是跑了些命令。

害怕之餘，必須先弄清楚黑客做了什麼，首先是通過 `docker inspect thirsty_kare` (這個是 CentOS 對應的 Container) 和 `docker inspect romantic_snyder` (這個是 `chrisfosterelli/rootplease` 對應的 Container) 看看這兩個 Containers 的信息。

具體信息太長，所以不貼上來，值得關注的一點：

- `chrisfosterelli/rootplease` 啟動時間是 `2016-10-02T14:04:15.86229242Z`，發生在 46 小時前
- CentOS 啟動時間是 `2016-10-03T14:44:35.271194859Z`，發生在 21 小時前

這意味著這很可能是兩次入侵，而我昨日忙於玩 Jenkins，竟然這麼久了才發現。

進一步看這兩個 Containers 的 host bind 信息：

- `chrisfosterelli/rootplease` 是用的 `-v /:/hostOS`，也就意味著他拿到了 root shell
- `CentOS` 只掛載了 `-v /root/.ssh:/mnt`，意味著他的目的是修改 `/root/.ssh/authorized_keys` 來拿到 root 帳戶的 ssh 登入的權限。但就算目的不是，破壞範圍也有限，畢竟 Docker 是個沙盒

然後我進入了兩個 Containers 文件系統鏡像（[不啟動 Container 查看文件系統鏡像](https://stackoverflow.com/questions/32750748/how-to-edit-files-in-stopped-not-starting-docker-container)）：

- `chrisfosterelli/rootplease` 的 `/root/.bash_history` 其實是 Host 機器的 `/root/.bash_history`，內容都是我自己的命令歷史。謹慎起見，額外查看到 Container 的文件系統鏡像中的 `/root/.bash_history` 不存在
- CentOS 那個 Container 的 `/root/.bash_history` 也不存在

兩種猜測：1) 入侵者抹掉了歷史紀錄；2) 黑客離開的時候這兩個 bash 還在運行，沒有退出 bash 之前 bash 不會把歷史寫入文件，而我將 Container kill 掉之後，就意味著當時還在的 bash 進程的內存也被丟掉了<s>，這段歷史也將永遠被埋沒。</s>

兩種情況我都不可能知道他們做了什麼，只能通過各種痕跡推測。


    # cat /root/.ssh/authorized_keys
    root@ubuntu:~# cat .ssh/authorized_keys
    ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDRrvCaHsH8nZ6YrTsZaTFeKW3aPzUvlK+h+KT8rT4w6EGJgl8LVANHUsl5BF3RVGjFKFnBkX6jd6tWt+435h9vrEhxynoI69ljiiP9lD8GWgp0axmupqrcU3+OBiAmQ1OrOsMeNBdlw3GjAGPLI+ACd2WPPfKlWyQqDYrtzUPm5cz7HmI5Xo10KDcAS8gRJolH1AzBLfb8gPv8X9c9pKlpkUeST7j6MWLg3QQTShqbDB5j3IvL92KPhFmsOtJFd+efRyTiFhKsiQDY1h2er4gWcAn95GgLG6ci4D3d/kCoYRwIjRRrk5/4pRRq3wpp7/anI8qqJ6pPbdV9HvA/AEOp root@localhost.localdomain

看到黑客留下的自己的 SSH Key。隨手 `: >/root/.ssh/authorized_keys` 把內容清空。



## 被入侵後的安全檢測


下一步就是看正在登入的 Sessions，看來是正常的：

    $ w
     13:06:55 up 108 days,  4:26,  6 users,  load average: 0.01, 0.03, 0.05
    USER     TTY      FROM             LOGIN@   IDLE   JCPU   PCPU WHAT
    shou     ttyS0                     18Jun16 108days  3.73s  2.98s [打碼]
    shou     pts/1    tmux(5178).%0    28Sep16  6days  1.92s  1.42s [打碼]
    shou     pts/2    tmux(5178).%1    28Sep16  5days  1:27m  1:27m [打碼]
    shou     pts/3    tmux(5178).%2    28Sep16  5days  4.68s  4.68s -zsh
    shou     pts/11   tmux(5178).%3    28Sep16  6days  0.67s  0.67s -zsh
    shou     pts/14   [連入 IP 打碼]    11:16    1.00s  3.84s  0.00s sshd: shou [priv]

然後是登入紀錄，也沒有異常：

    $ last
    shou     pts/14       [連入 IP 打碼]     Tue Oct  4 11:16    gone - no logout
    shou     pts/4        [連入 IP 打碼]     Mon Oct  3 16:26 - 19:23  (02:57)
    shou     pts/0        [連入 IP 打碼]     Mon Oct  3 10:45 - 13:07  (02:22)
    shou     pts/0        [連入 IP 打碼]     Mon Oct  3 02:29 - 07:13  (04:44)
    shou     pts/0        [連入 IP 打碼]     Sun Oct  2 05:32 - 20:00  (14:27)
    [...]

登入日誌無異常，大概可以說明前面試圖設置 SSH root 後門的那位入侵者碰壁了。他是不是登入時認證出錯了呢？想起我在 SSH 裡用了增強安全性的配置，譬如改了默認端口，關了 PermitRootLogin，允許連接的用戶也是以白名單形式寫的。那麼有沒有認證錯誤信息呢？

    $ sudo less /var/log/auth.log（或者 sudo journalctl -u ssh）
    [...]

內容很正常，沒有看到有人用 root 登入認證出錯的信息。

那位使用 CentOS + Bash 的入侵者看來是碰壁了，畢竟他沒有辦法訪問到 Host 機器的 `/etc/ssh/sshd_config` 不可能知道我做了哪些安全措施。然後我有防火牆，連去 TCP/22 的連接會被直接棄掉，所以沒有 SSH 登入紀錄當然不意外。


於是來研究第一位入侵者可能做了的事情。首先檢查 systemd 啟動項：

    $ systemctl list-unit-files | grep enabled
    accounts-daemon.service                    enabled
    cron.service                               enabled
    deploy_daemon.service                      enabled
    docker.service                             enabled
    [...]

然後挨個查看這些啟動項的詳情，如果在最近啟動過的就要注意了。

    $ systemctl status accounts-daemon.service
    $ systemctl status cron.service
    $ [...]

所幸都沒問題。不通過 systemd 服務，要設置自動啟動程序，那就只能注入 `/etc/profile`，`/etc/{rc*.d/*,rc.local}` 之類的啟動腳本了，簡單看了一下這些文件的修改日期：

    $ stat /etc/profile
    File: '/etc/profile'
    Size: 575       	Blocks: 8          IO Block: 4096   regular file
    Device: 800h/2048d	Inode: 326         Links: 1
    Access: (0644/-rw-r--r--)  Uid: (    0/    root)   Gid: (    0/    root)
    Access: 2016-04-21 19:25:20.949283213 +0000
    Modify: 2015-10-22 17:15:21.000000000 +0000
    Change: 2016-04-22 00:09:57.362266110 +0000
     Birth: -

得出這些文件也均未在近期被修改過。

這下看來入侵者不指望在啟動時運行什麼了，那麼會不會是設置成定期運行了呢？

再檢查 `/etc/crontab`，以及 `/etc/cron.d/*` 和 `/etc/cron.{hourly,daily,...}/*` 裡的所有文件。看來這些文件修改時間都未變過。

但說不定現在就有個木馬在跑，於是檢查所有正在偵聽的端口：

    $ sudo netstat -lptn | grep 0.0.0.0
    [...]
    tcp      0      0 0.0.0.0:[打碼]         0.0.0.0:*           LISTEN      29878/[某已知服務]
    tcp      0      0 0.0.0.0:[打碼]         0.0.0.0:*           LISTEN      23577/sshd
    [...]

除了 TCP，UDP 也不例外：

    $ sudo netstat -lpun | grep 0.0.0.0
    [...]

除了 IPv4，還要看看 IPv6 的，於是也要 `grep` 一下 `':::'`。

看來都很正常，這些服務也都是我自己知道的。之所以 `netstat` 要加上 `-p` 選項，是因為這樣可以獲取到正在偵聽該端口的程序的 PID，有了 PID 我們就可以通過 `/proc/[PID]/*` 下的文件來得到進程的所有信息了。



## 結論

那位試圖通過修改 `/root/.ssh/authorized_keys` 來開後門的看來九成是失敗了。

比較擔心另一位使用 `chrisfosterelli/rootplease` 的入侵者，畢竟他得到的是 root shell，也能不確定他做了什麼。

根據排查，基本上也排除了被安裝木馬的可能性，而且直觀看來似乎也沒有做任何破壞性操作，猜測他也可能在某處碰壁而不繼續了。

此外，我不太認為入侵者是深思熟慮並且小心翼翼的，畢竟他們很粗心地留下這幾個正在運行的 Docker Containers 這麼明顯的線索。所以這次入侵看來並沒有什麼問題。


但也可能並非這麼簡單。

既然兩天前就有人入侵並且輕鬆得到了 root shell，沒有任何理由假設我只被這兩人入侵過。可能其他入侵者侵入後小心地刪掉了入侵所用的 Container，這樣我就沒有線索來知道他們從何入侵，又做了什麼了。

也有可能入侵者使用非常小心的手段把惡意程序注入了系統，譬如說修改了文件的修改時間或者在已知的服務中加了後門，而後門也不是簡單偵聽在 TCP/UDP 的，而還在運行的 Container 則是故意留下的。這樣的話排除入侵可能性的難度就大大提升了，畢竟我沒有精力檢查一個個文件內容是否正常。

當然既然已知服務器被入侵過，就不能依然像以前一樣放心使用了，這幾天我會把數據遷移一下，然後重裝系統。


## 教訓

- 配置好防火牆很重要
- 配置好 ssh 安全性很重要
- 給防火牆白名單加規則前思考一下可能的後果，特別以 root 權限運行的進程
- 開放端口時一定要改默認端口


