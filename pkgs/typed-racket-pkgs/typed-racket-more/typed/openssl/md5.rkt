#lang typed/racket/base

(require/typed/provide openssl/md5
  [md5 (-> Input-Port String)]
  [md5-bytes (-> Input-Port Bytes)]
  )
