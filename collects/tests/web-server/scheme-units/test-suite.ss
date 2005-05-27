#!/bin/sh
#|
exec mzscheme -r "$0" "$@"
|#
;; Mike Burns, July 28th, 2004, netgeek@speakeasy.net
;; Run from collects/tests/web-server/scheme-units
(require (lib "text-ui.ss" "schemeunit")
         "test-authentication.ss"
         "test-serve-static-html.ss"
         "test-serve-static-jpeg.ss"
         "test-web-server.ss"
         "test-servlets.ss"
         "test-errors.ss"
         "test-send.ss")

(test/text-ui test-web-server)
(test/text-ui test-serve-static-html)
(test/text-ui test-serve-static-jpeg)
(test/text-ui test-authentication)
(test/text-ui test-errors)
(test/text-ui test-servlets)
(test/text-ui test-send)
