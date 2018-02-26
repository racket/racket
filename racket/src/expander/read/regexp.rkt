#lang racket/base
(require "config.rkt"
         "special.rkt"
         "wrap.rkt"
         "readtable.rkt"
         "consume.rkt"
         "accum-string.rkt"
         "error.rkt"
         "string.rkt")

(provide read-regexp)

(define (read-regexp mode-c accum-str in config)
  (define c3 (read-char/special in config))
  (define no-wrap-config (disable-wrapping config))
  
  (define rx
    (case c3
      [(#\")
       (accum-string-abandon! accum-str config)
       (define str (read-string in no-wrap-config))
       (catch-and-reraise-as-reader
        in config
        ((if (char=? mode-c #\r) regexp pregexp) str))]
      [(#\#)
       (accum-string-add! accum-str c3)
       (define c4 (read-char/special in config))
       (case c4
         [(#\")
          (accum-string-abandon! accum-str config)
          (define bstr
            (read-string in no-wrap-config #:mode '|byte string|))
          (catch-and-reraise-as-reader
           in config
           ((if (char=? mode-c #\r) byte-regexp byte-pregexp) bstr))]
         [else
          (reader-error in config #:due-to c4
                        "expected `\"` after `~a`"
                        (accum-string-get! accum-str config))])]
      [else
       (reader-error in config #:due-to c3
                     "expected `\"` or `#` after `~a`"
                     (accum-string-get! accum-str config))]))
  
  (wrap rx
        in
        config
        #f))
