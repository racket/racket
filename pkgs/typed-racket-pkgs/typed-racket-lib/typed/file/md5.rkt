#lang typed/racket/base
(require/typed file/md5
               [md5 ((U Bytes Input-Port) -> Bytes)])
(provide md5)
