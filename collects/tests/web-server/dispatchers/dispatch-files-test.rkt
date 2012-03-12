#lang racket
(require rackunit
         (only-in mzlib/file
                  file-name-from-path
                  make-temporary-file)
         net/url
         mzlib/list
         xml/xml
         web-server/http
         web-server/private/util
         web-server/dispatchers/dispatch
         (prefix-in files: web-server/dispatchers/dispatch-files)
         "../util.rkt")
(require/expose web-server/dispatchers/dispatch-files
                (looks-like-directory?))
(provide dispatch-files-tests)

(define tmp-file (make-temporary-file))
(with-output-to-file tmp-file 
  (lambda ()
    (display
     (xexpr->string 
      `(html (head (title "A title"))
             (body "Here's some content!")))))
  #:exists 'truncate/replace)

(define a-dir (directory-part tmp-file))
(define not-there (build-path "I/probably/do/not/exist"))

(define (dispatch i? . paths)
  (define b (box 0))
  (files:make #:url->path
              (lambda (url)
                (begin0 (values (list-ref paths (min (unbox b) (sub1 (length paths)))) empty)
                        (set-box! b (add1 (unbox b)))))
              #:indices (list (if i? (file-name-from-path tmp-file) not-there))))

(define file-url (string->url "http://test.com/foo"))
(define dir-url (string->url "http://test.com/foo/"))
(define (req d? meth heads)
  (make-request meth (if d? dir-url file-url) heads (delay empty) #"" "host" 80 "client"))
  
(define-syntax-rule (test-equal?* n lhs rhs)
  (test-equal? n (bytes-sort lhs) (bytes-sort rhs)))

(define dispatch-files-tests
  (test-suite
   "Files"
   
   (local [(define (yes s) (test-not-false s (looks-like-directory? s)))
           (define (no s) (test-false s (looks-like-directory? s)))]
   (test-suite
    "Looks like directory"

    (no "") (no "foo") (no "/foo") (no "/foo/bar")
    (yes "/") (yes "/foo/") (yes "foo/" )(yes "/bar/zog/trog/")))
   
   (test-case
    "read-range-header: missing and badly formed headers"
    (check-false (files:read-range-header (list (make-header #"Ranges" #"bytes=1-10"))) "check 1")
    (check-false (parameterize ([current-error-port (open-output-nowhere)])
                   (files:read-range-header (list (make-header #"Range" #"completely wrong")))) "check 2")
    (check-false (parameterize ([current-error-port (open-output-nowhere)])
                   (files:read-range-header (list (make-header #"Range" #"byte=1-10")))) "check 3")
    (check-false (parameterize ([current-error-port (open-output-nowhere)])
                   (files:read-range-header (list (make-header #"Range" #"bytes=a-10")))) "check 4")
    (check-false (parameterize ([current-error-port (open-output-nowhere)])
                   (files:read-range-header (list (make-header #"Range" #"bytes=1-1.0")))) "check 5"))
   
   (test-case
    "read-range-header: single range"
    (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=1-10"))) (list (cons 1 10)) "check 1")
    (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=1-"))) (list (cons 1 #f)) "check 2")
    (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=-10"))) (list (cons #f 10)) "check 3"))
   
   (test-equal?
    "read-range-header: multiple ranges"
    (files:read-range-header (list (make-header #"Range" #"bytes=1-10,20-,-30")))
    (list (cons 1 10) (cons 20 #f) (cons #f 30)))
   
   (test-equal?* "file, exists, whole, no Range, get"
                (collect (dispatch #t tmp-file) (req #f #"GET" empty))
                #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
   (test-equal?* "file, exists, whole, no Range, head"
                (collect (dispatch #t tmp-file) (req #f #"HEAD" empty))
                #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
   (test-equal?* "file, exists, whole, Range, get"
                (collect (dispatch #t tmp-file) (req #f #"GET" (list (make-header #"Range" #"bytes=0-80"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
   (test-equal?* "file, exists, whole, Range, head"
                (collect (dispatch #t tmp-file) (req #f #"HEAD" (list (make-header #"Range" #"bytes=0-80"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")
   (test-equal?* "file, exists, part, get"
                (collect (dispatch #t tmp-file) (req #f #"GET" (list (make-header #"Range" #"bytes=5-9"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 5-9/81\r\n\r\n><hea")
   (test-equal?* "file, exists, part, head"
                (collect (dispatch #t tmp-file) (req #f #"HEAD" (list (make-header #"Range" #"bytes=5-9"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 5-9/81\r\n\r\n")
   
   (test-exn "path, non"
             exn:dispatcher?
             (lambda () (collect (dispatch #t not-there) (req #f #"GET" empty))))
   
   (test-equal?* "dir, exists, no Range, get"
                (collect (dispatch #t a-dir) (req #t #"GET" empty))
                #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
   (test-equal?* "dir, exists, no Range, head"
                (collect (dispatch #t a-dir) (req #t #"HEAD" empty))
                #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
   (test-equal?* "dir, exists, Range, get"
                (collect (dispatch #t a-dir) (req #t #"GET" (list (make-header #"Range" #"bytes=0-80"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
   (test-equal?* "dir, exists, Range, head"
                (collect (dispatch #t a-dir) (req #t #"HEAD" (list (make-header #"Range" #"bytes=0-80"))))
                #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")
   (test-equal?* "dir, not dir-url, get"
                (collect (dispatch #t a-dir) (req #f #"GET" empty))
                #"HTTP/1.1 302 Moved Temporarily\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html\r\nLocation: /foo/\r\n\r\n")
   (test-exn "dir, not exists, get"
             exn:dispatcher?
             (lambda () (collect (dispatch #f a-dir) (req #t #"GET" empty))))
   (test-exn "dir, not exists, head"
             exn:dispatcher?
             (lambda () (collect (dispatch #f a-dir) (req #t #"HEAD" empty))))))
