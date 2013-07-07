#lang racket/base

(require tests/eli-tester net/mime)

;; This test is based on an example from Jordan Schatz

(define ip
  (open-input-string
   (regexp-replace* #rx"(\r\n|\n)"
#<<EOS
Server: MochiWeb/1.1 WebMachine/1.9.0 (blah blah)
Expires: Fri, 06 Jan 2012 02:01:12 GMT
Date: Fri, 06 Jan 2012 01:51:12 GMT
Content-Type: multipart/mixed; boundary=9nbsYRvJBLRyuL4VOuuejw9LcAy
Content-Length: 817


--9nbsYRvJBLRyuL4VOuuejw9LcAy
Content-Type: multipart/mixed; boundary=NdzDrpIQMsJKtfv9VrXmp4YwCPh

--NdzDrpIQMsJKtfv9VrXmp4YwCPh
X-Riak-Vclock: a85hYGBgzGDKBVIcypz/fvp9087NYEpkzGNlaGCpPMGXBQA=
Location: /buckets/invoices/keys/RAQpCw8SssXlXVhiGAGYXsVmwvk
Content-Type: application/json
Link: </buckets/invoices>; rel="up"
Etag: 1qS8Wrr2vkTBxkITOjo33K
Last-Modified: Wed, 04 Jan 2012 17:12:32 GMT

{"date": "11/02/2011"}
--NdzDrpIQMsJKtfv9VrXmp4YwCPh--

--9nbsYRvJBLRyuL4VOuuejw9LcAy--
EOS
"\r\n")))

(provide tests)
(module+ main (tests))
(define (tests)
  (define analyzed      (mime-analyze ip))
  (define our-entity    (message-entity analyzed))
  (define parts         (entity-parts our-entity))
  (define inner-message (car parts))
  (define inner-entity  (message-entity inner-message))
  (define body-proc     (entity-body inner-entity))
  (define tmp           (open-output-string))
  (define sub           (message-entity (car (entity-parts inner-entity))))
  (test (message-fields analyzed)
        => '("Server: MochiWeb/1.1 WebMachine/1.9.0 (blah blah)"
             "Expires: Fri, 06 Jan 2012 02:01:12 GMT"
             "Date: Fri, 06 Jan 2012 01:51:12 GMT")
        (length parts) => 1
        body-proc => '()
        (length (entity-parts inner-entity)) => 1
        (entity-type sub) => 'application
        (entity-subtype sub) => 'json
        ((entity-body sub) tmp)
        (get-output-string tmp) => "{\"date\": \"11/02/2011\"}"
        (mime-analyze
         (open-input-string "Content-Type: multipart/mixed\r\n\r\n"))
        =error> missing-multipart-boundary-parameter?
        ))

(module+ test (require (submod ".." main))) ; for raco test & drdr
