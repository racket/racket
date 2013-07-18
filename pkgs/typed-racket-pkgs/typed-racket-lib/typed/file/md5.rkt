#lang typed/racket/base
(require/typed file/md5
               [md5 (case->
                      ((U Bytes String Input-Port Boolean) -> Bytes)
                      ((U Bytes String Input-Port Boolean) Boolean -> Bytes))])
(provide md5)
