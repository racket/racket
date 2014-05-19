#lang typed/racket/base

(require/typed/provide openssl/sha1
  [sha1 (-> Input-Port String)]
  [sha1-bytes (-> Input-Port Bytes)]
  [bytes->hex-string (-> Bytes String)]
  [hex-string->bytes (-> String Bytes)]
  )
