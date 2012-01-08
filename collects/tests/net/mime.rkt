#lang racket/base
(require net/mime)

(define-syntax-rule (test expect expr)
  (let ([val expr])
    (unless (equal? expect val)
      (error 'test "failed at ~s: ~e" 'expr val))))

;; This test is based on an example from Jordan Schatz

(define ip
  (open-input-string
   (regexp-replace* #rx"(\r\n|\n)"
#<<EOS
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
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

(let* ([analyzed (mime-analyze ip)]
       [our-entity (message-entity analyzed)]
       [parts (entity-parts our-entity)]
       [inner-message (car parts)]
       [inner-entity (message-entity inner-message)]
       [body-proc (entity-body inner-entity)]
       [tmp (open-output-string)]) 
  (test '("Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)"
          "Expires: Fri, 06 Jan 2012 02:01:12 GMT"
          "Date: Fri, 06 Jan 2012 01:51:12 GMT")
        (message-fields analyzed))
  (test 1 (length parts))
  (test '() body-proc)
  (test 1 (length (entity-parts inner-entity)))
  (define sub  (message-entity (car (entity-parts inner-entity))))
  (test 'application (entity-type sub))
  (test 'json (entity-subtype sub))
  ((entity-body sub) tmp)
  (test "{\"date\": \"11/02/2011\"}" (get-output-string tmp)))

(test 'not-there (with-handlers ([exn:fail?
                                  (lambda (exn)
                                    (and (missing-multipart-boundary-parameter? exn)
                                         'not-there))])
                   (mime-analyze
                    (open-input-string "Content-Type: multipart/mixed\r\n\r\n"))))
