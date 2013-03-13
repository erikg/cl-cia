# cl-cia

## Installing
cl-cia is written for [SBCL](http://www.sbcl.org) and uses ASDF to install dependancies, all of which are available in [quicklisp](http://www.quicklisp.org).
Simply check out to ~/quicklisp/local-projects/cl-cia and do "(ql:quickload :cl-cia)".

## XML-RPC
XML-RPC requests are handle using #'xmlrpc with the passed argument being the string holding the XML.
The format of the package is the same as cia.vc's.
The modified UCW at http://github.com/erikg/ucw-core has a patch to handle xml requests, https://github.com/erikg/ucw-core/commit/581589fc4c22b8a4f8ae29424cd0495c679f1868#src/backend/common.lisp .

## Mail processing
The maildir.lisp file has some fairly crude mail parsing capabilities, including custom Google Code-In handling, xml email, and sourceforge commit email.
Mail parsing frequently breaks due to unicode packing and formatting twitchiness. Try to avoid it.

## IRC bot
This is all contained in irc.lisp and is very basic.
A simple command handler is added in to report statistics to channel.

## ToDo
- [ ] add xmlrpc scripts
- [ ] add help for integrating with github
- [ ] send email on new notice? should it be a selectable flag?
- [ ] fix unicode parsing in multipart emails (have a test file, file->strings fails)
- [ ] remove the unhandled privmsg event stuff
- [ ] kill the (contents, props changed) coming through in file listings
- [ ] make bot reconnect on disconnect
- [ ] investigate sms messaging?
- [ ] investigate ios push type thing?
- [ ] add public/private markings
- [X] fix delete last notification in message queue issue
- [X] allow change of email address, maybe some profile info?
- [X] add create and completed times to todo items?
- [X] fix utf8 parse issue in email
- [X] add support for xmlrpc email
- [X] add mail send capability
- [X] fix mail spool for fbsd
- [X] 12:13 <@brlcad> ``Erik: how about truncating on a word boundary? and something to indicate it was truncated...
- [X] fix irc add todo message logic
