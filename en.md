---
layout: page
title:  Summary
date:   пт, 27-лис-2015 19:40:51 +0200
categories: en
---
   OL (Owl Lisp) is a purely *(mostly)* functional dialect of Lisp.
It implements an extended subset of [R5RS](http://www.schemers.org/Documents/Standards/R5RS/){:target="_blank"} Scheme but has been renamed as a Lisp by its author mainly to avoid possible confusion.

   You can immediately try Ol in the provided terminal on this page. For example, type:
<pre><code id="sample1" data-language="scheme">(+ 3/7 2+4i)</code><button class="doit" onclick="doit(sample1.textContent)">send to the terminal</button></pre>
      
   Or you can try more interesting example
<pre><code id="sample2" data-language="scheme">(fold * 1 (iota 1 1 100))</code><button class="doit" onclick="doit(sample2.textContent)">send to the terminal</button></pre>
which is another way of
<pre><code id="sample3" data-language="scheme">(let factorial ((n 99))
   (if (= n 1)
      1
      (* n (factorial (- n 1)))))</code><button class="doit" onclick="doit(sample3.textContent)">send to the terminal</button></pre>
      
   This must output 99! for you.

### Learn

   I'm writing docs right now. It will be available very soon. You can check <b><a href="?ru/examples">SAMPLES</a></b> for now.

<small style="float: right">Copyright (c) 2015 Yuriy Chumak</small>

