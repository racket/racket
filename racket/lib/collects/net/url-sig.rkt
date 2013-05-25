#lang racket/base
(require racket/unit)

(provide url^ url+scheme^)

(define-signature url^
  (get-pure-port
   get-impure-port
   post-pure-port
   post-impure-port
   head-pure-port
   head-impure-port
   delete-pure-port
   delete-impure-port
   put-pure-port
   put-impure-port
   display-pure-port
   purify-port
   netscape/string->url
   string->url
   url->string
   path->url
   url->path
   call/input-url
   combine-url/relative
   url-exception?
   current-proxy-servers
   file-url-path-convention-type))

(define-signature url+scheme^ extends url^
  (current-connect-scheme))



