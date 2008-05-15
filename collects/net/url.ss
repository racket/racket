#lang scheme/base
(require scheme/unit
         scheme/contract
         (only-in mzlib/contract opt->)
         "url-structs.ss"
         "url-sig.ss"
         "url-unit.ss"
         "tcp-sig.ss"
         "tcp-unit.ss")

(define-compound-unit/infer url+tcp@
  (import) (export url^)
  (link tcp@ url@))

(define-values/invoke-unit/infer url+tcp@)

(provide (struct-out url) (struct-out path/param))

(provide/contract
 (string->url ((or/c bytes? string?) . -> . url?))
 (path->url ((or/c path-string? path-for-some-system?) . -> . url?))
 (url->string (url? . -> . string?))
 (url->path ((url?) ((one-of/c 'unix 'windows)) . opt-> . path-for-some-system?))

 (get-pure-port (opt-> (url?) ((listof string?)) input-port?))
 (get-impure-port (opt-> (url?) ((listof string?)) input-port?))
 (post-pure-port (opt-> (url? (or/c false/c bytes?)) ((listof string?)) input-port?))
 (post-impure-port (opt-> (url? bytes?) ((listof string?)) input-port?))
 (head-pure-port (opt-> (url?) ((listof string?)) input-port?))
 (head-impure-port (opt-> (url?) ((listof string?)) input-port?))
 (delete-pure-port (opt-> (url?) ((listof string?)) input-port?))
 (delete-impure-port (opt-> (url?) ((listof string?)) input-port?))
 (put-pure-port (opt-> (url? (or/c false/c bytes?)) ((listof string?)) input-port?))
 (put-impure-port (opt-> (url? bytes?) ((listof string?)) input-port?))
 (display-pure-port (input-port? . -> . void?))
 (purify-port (input-port? . -> . string?))
 (netscape/string->url (string? . -> . url?))
 (call/input-url  (case->
                   (-> url?
                       (-> url? input-port?)
                       (-> input-port? any)
                       any)
                   (-> url?
                       (-> url? (listof string?) input-port?)
                       (-> input-port? any)
                       (listof string?)
                       any)))
 (combine-url/relative (url? string? . -> . url?))
 (url-exception? (any/c . -> . boolean?))
 (current-proxy-servers
  (parameter/c (or/c false/c (listof (list/c string? string? number?)))))
 (file-url-path-convention-type
  (parameter/c (one-of/c 'unix 'windows))))
