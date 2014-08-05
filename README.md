NAME
====

momental storage

SYNOPSIS
========

``` bash
  $ curl -v -k -L -O https://localhost:8080/r/somefile.txt
  ...
  < HTTP/1.1 302 Found
  ...
  < location: http://localhost:8080/d/BB88A3F45FE1DE8A62759AA4C148EB68859A9EEADEED6BD8EA1388D4732B756B3D45242C6C6ECB35D86EA826D3DBEAEC9AF67ECF221F3193DD353A8741268
  ...
  $ curl -k -T README.md https://localhost:8080/s/BB88A3F45FE1DE8A62759AA4C148EB68859A9EEADEED6BD8EA1388D4732B756B3D45242C6C6ECB35D86EA826D3DBEAEC9AF67ECF221F3193DD353A8741268
```

DESCRIPTION
===========

This module provides a link between receiver and sender.  The link is used to copy a file.

We have many way to this work, e.g., `rsync`, `scp`, and more, but these require localy link.

If there is no link that connects receiver and sender with directly, then this module makes
a bridge to these.

HOW IT WORKS
============

1. server waits request
1. receiver requests `GET` to the server
1. server notifies session ID to the receiver by Location HTTP header
1. receiver requests to the location, and waits response from the server
1. receiver tells session ID to the sender (ordinary, this may done by copy and paste)
1. sender `PUT` a file to the server with session ID
1. server receives a file from sender, and reply to the receiver
1. sender finishes send, and server too, then receriver completes `GET`

URLs
====

/r/:filename
------------

URL for the *R*eceiver.

Receives `GET` request, and tell the receiver to a URL that may `PUT` from a sender.

`filename` is used to specify a filename to save.

/d/:id
------

Is *D*ata path.

This internal URL shows session which bridges between receiver and sender.

`id` shows session ID which connects both clients.

/s/:id
------

URL for the *S*ender.

Sender send `PUT` request to the URL which includes session ID.

`id` shows session ID which connects both clients.

INSTALL
=======

Place SSL certificate file to `priv/ssl`, or change code to use http instad of https.

The code can change editing `src/momental_storage_app.erl`, and `src/momental_storage_url.erl`.

- `src/momental_storage_app.erl`
  - change from `https` to `http`
  - delete ssl option of `cacertfile`, `certfile`, `keyfile`
- `src/momental_storage_url.erl`
  - change from `https` to `http`

``` bash
  $ make
  $ _rel/momental_storage/bin/momental_storage-0.1 console
```
