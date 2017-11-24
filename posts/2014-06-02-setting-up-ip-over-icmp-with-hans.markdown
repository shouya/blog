---
layout: post
title: "Setting up IP over ICMP with hans"
date: 2014-06-02 23:19:29 +0800
comments: true
categories: unix
tags:
  - networking
---

## Background
The ISP, China Telecom, interrupted the internet access from my home
yesterday due to the expiration of annually subscription of the
internet service. While I found that ICMP packets are not blocked.
Even though I can't even do a DNS query, I can ping any server as usual.

The inaccessibility of internet has greatly evoked my anxiety on
seeking changes. Well, it's theoretically possible to carry data in
ICMP packets. Therefore, according to the hacker community's
principle, there should already been hackers who made use of this and
opened the sources of their programs.

Indeed, searching 'ICMP tunneling', I found
[hans](http://code.gerade.org/hans/) and
[icmptx](http://thomer.com/icmptx/). The latter one is more
primitive and complicated to use. After several trials and failures, I
decide to give __icmptx__ up, and try __hans__ instead.

This article is a tutorial/note about how to establish normal internet
connection in the condition that only ICMP packets are allowed to pass
through the firewall with __hans__.

<!-- more -->

## Step 1: Establish the tunnel
From the server side, first start the server.

      # ./hans -s 10.2.0.1 -m 10000

The option `-s` specifies the inet address for the tunnel interface,
and the `-m` specifies the size of mtu.

Then try to connect from the client.

     # ./hans -c <server_ip> -m 10000

Please beware the mtu should be matched between the server and the
client,
otherwise it might cause problems.

In addition, you should switch off the normal response to ICMP echo
request.

     # sysctl net.ipv4.icmp_echo_ignore_all=1

Because the ICMP packets should now be handled all by hans.
But if you still want to have it working, add `-s` option whiling
launching the server side of hans.

### Choosing the size of MTU

Without specifying the mtu manually in the command's argument,
hans will automatically choose the system default one.
It will normally be around 1500. But this would be a little bit
small to communicate over ICMP. In practice, with a default mtu
value of 1500, the download speed can hardly reach 7 KiB/second.
While as it changes 10 000, the speed went around 10 KiB/second.

Nonetheless, you can try various values of mtu and find your
best fit.

### Testing the tunnel
Now the tunnel should be established. From the client side,
you should see that the inet address of `tun0` is chosen automatically
according to the server's configuration. Otherwise, if you
haven't seen any valid inet address for the client `tun0`
interface, the connection is not established successfully. At this
time,
check if the operations above take effect and ensure you have a
working
internet connection (at least ICMP packet is not blocked).

From the server, you can ping the the ip of `tun0` in the client-side
and it should work good.
While pinging from the client-side would possibly be not working
except
you have specified the `-s` option to `hans` command.


## Step 2: Configure forwarding routing on the server
On the server:

     # iptables -F
     # iptables -F -t nat
     # iptables -A FORWARD -i tun0 -j ACCEPT
     # iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE

Don't forget to enable IP forwarding:

     # sysctl net.ipv4.ip_forwarding=1

## Step 3: Configure the routing table

We should now redirect all packets through the tunnel
to the server.

     # ip route del default
     # ip route add default via 10.2.0.1

It's necessary to specify how the server side can be reached.

     # ip route add <server_ip> via <original_router>

So there won't be a circular packet transmission.

Done.


## Resources that might help
* [github: friedrich/hans](https://github.com/friedrich/hans)
* [official guide of hans](http://code.gerade.org/hans/)
* [icmptx: how to](http://thomer.com/icmptx/)
* [github: jakkarth/icmptx](http://github.com/jakkarth/icmptx)
* [iptables theory: traversing of
tables](http://www.faqs.org/docs/iptables/traversingoftables.html)
* [a beginner's guide to
iptables](http://www.howtogeek.com/177621/the-beginners-guide-to-iptables-the-linux-firewall/)
* [home router - gentoo wiki](http://wiki.gentoo.org/wiki/Home_Router)
* [another person log about the usage of
icmptx](http://itsecworks.com/2013/02/15/fire-in-the-hole-of-the-firewall/)
