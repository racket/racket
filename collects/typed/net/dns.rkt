#lang typed/racket/base

(require typed/private/utils)

(require/typed/provide net/dns
  [dns-get-address (String String -> String)]
  [dns-get-name (String String -> String)]
  [dns-get-mail-exchanger (String String -> String )]
  [dns-find-nameserver (-> (Option String))])
