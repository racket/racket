#! /usr/bin/env racket

#lang racket/base

(require "crypt.rkt")

(define passwd "foo")
(define salt "xz")
(printf ">>> crypt(~s,~s) = ~s\n" passwd salt (crypt passwd salt))

;; md5-based version
(set! salt "$1$somesalt$")
(printf ">>> crypt(~s,~s) = ~s\n" passwd salt (crypt passwd salt))

(newline)
(define foo "foo bar")
(define key (string->bytes/utf-8 "my key"))
(printf ">>> ~s --encrypt--> ~s --decrypt--> ~s\n"
        foo (encrypt foo key) (decrypt (encrypt foo key) key))
