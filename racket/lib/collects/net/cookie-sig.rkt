#lang racket/signature

cookie-name?
cookie-value?
cookie?
valid-domain?
set-cookie
cookie:add-comment
cookie:add-domain
cookie:add-max-age
cookie:add-path
cookie:secure
cookie:version
;; To actually return a cookie (string formated as a cookie):
print-cookie
;; To parse the Cookies header:
get-cookie
get-cookie/single
;; exceptions
(struct cookie-error ())
