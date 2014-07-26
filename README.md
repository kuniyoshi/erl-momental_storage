NAME
====

momental storage

SYNOPSIS
========

``` bash
  $ curl -O https://example.com/momental_storage/r/somefile.txt

  $ curl -T somefile.txt https://example.com/momental_storage/s/
```

DESCRIPTION
===========

Blah blah blah.

URLs
====

/r/
---

*R*eceives `GET` requiest, and tell the client to a URL
that may `POST` from a *S*ender.

1. receive `GET` request
1. redirect new secret URL
1. wait a sender `POST` a file

The name after `/r/` will be ignored, but is used to notify client
program to the filename.

/s/
---

Waits `POST` request from *S*ender.
