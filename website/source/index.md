{include resources/header.md}

<div class="contents">
<div class="system-links">

  * [Mailing Lists][3]
  * [Getting it][4]
  * [Documentation][5]
  * [News][6]
  * [Test results][tr]
  * [Changelog][7]

   [3]: #mailing-lists
   [4]: #downloads
   [5]: documentation/ "documentation link"
   [6]: #news
   [7]: changelog.html
   [tr]: test-report.html
   

</div>
<div class="system-description">

### What it is

Until a few days ago, trivial-http was simple-http and
simple-http was a fork of Brian Mastenbrook's trivial-http.
Brian has kindly offered to let me continue the trivial
tradition and so trivial-http is reborn ready to stumble
forth again. It that adds several features and bug fixes and
works more the way I'd like it to. Where its functionality
overlaps with Trivial-HTTP, it performs exactly the same.

trivial-http has functions for

* HTTP Get
* HTTP Post
* HTTP Head
* HTTP Resolve - resolve 301 and 302 redirects
* HTTP Download

{anchor mailing-lists}

### Mailing Lists

* [trivial-http-devel][devel-list]: A list for questions,
  patches, bug reports, and so on.

{anchor downloads}

### Where it is {id downloads}

A [darcs][] repository is available. The darcs command to
grab the source code is:

    darcs get http://common-lisp.net/project/trivial-http

trivial-http is also [ASDF-installable][asdf-install]. Its
CLiki home is right [where][cliki] you'd expect. Finally,
there is a also a handy [gzipped][gz] tar file.


### What is happening {id news}

2008-July-27 - Thanks to Brian Mastenbrook for letting me
take the name. The other big news is that trivial-http is now
based on [usocket][] rather than trivial-socket.


</div>
</div>

{include resources/footer.md}
