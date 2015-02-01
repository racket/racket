#lang racket/base
#|
net/cookies/server     : RFC6265-compliant server cookie handling
 - read the Cookie header
 - make a cookie struct
 - write the Set-Cookie header

net/cookies/user-agent : RFC6265-compliant user agent cookie handling
 - read the Set-Cookie header
 - make a cookie struct
 - write the Cookie header

net/cookies/common : Any code that winds up being common to UA & server
|#
