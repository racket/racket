(module dispatch-files-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "file.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "xml.ss" "xml")
           (lib "request-structs.ss" "web-server" "private")
           (lib "util.ss" "web-server" "private")
           (lib "dispatch.ss" "web-server" "dispatchers")
           (prefix files: (lib "dispatch-files.ss" "web-server" "dispatchers"))
           "../util.ss")
  (provide dispatch-files-tests)
  
  (define tmp-file (make-temporary-file))
  (with-output-to-file tmp-file 
    (lambda ()
      (display
       (xexpr->string 
        `(html (head (title "A title"))
               (body "Here's some content!")))))
    'truncate/replace)
  
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
    (make-request meth (if d? dir-url file-url) heads empty #"" "host" 80 "client"))
  
  (define dispatch-files-tests
    (test-suite
     "Files"
     
     (test-case
      "read-range-header: missing and badly formed headers"
      (check-false (files:read-range-header (list (make-header #"Ranges" #"bytes=1-10"))) "check 1")
      (check-false (files:read-range-header (list (make-header #"Range" #"completely wrong"))) "check 2")
      (check-false (files:read-range-header (list (make-header #"Range" #"byte=1-10"))) "check 3")
      (check-false (files:read-range-header (list (make-header #"Range" #"bytes=a-10"))) "check 4")
      (check-false (files:read-range-header (list (make-header #"Range" #"bytes=1-1.0"))) "check 5"))
     
     (test-case
      "read-range-header: single range"
      (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=1-10"))) (list (cons 1 10)) "check 1")
      (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=1-"))) (list (cons 1 #f)) "check 2")
      (check-equal? (files:read-range-header (list (make-header #"Range" #"bytes=-10"))) (list (cons #f 10)) "check 3"))
     
     (test-equal?
      "read-range-header: multiple ranges"
      (files:read-range-header (list (make-header #"Range" #"bytes=1-10,20-,-30")))
      (list (cons 1 10) (cons 20 #f) (cons #f 30)))
     
     (test-equal? "file, exists, whole, no Range, get"
                  (collect (dispatch #t tmp-file) (req #f 'get empty))
                  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
     (test-equal? "file, exists, whole, no Range, head"
                  (collect (dispatch #t tmp-file) (req #f 'head empty))
                  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
     (test-equal? "file, exists, whole, Range, get"
                  (collect (dispatch #t tmp-file) (req #f 'get (list (make-header #"Range" #"bytes=0-80"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
     (test-equal? "file, exists, whole, Range, head"
                  (collect (dispatch #t tmp-file) (req #f 'head (list (make-header #"Range" #"bytes=0-80"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")
     (test-equal? "file, exists, part, get"
                  (collect (dispatch #t tmp-file) (req #f 'get (list (make-header #"Range" #"bytes=5-9"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 5-9/81\r\n\r\n><hea")
     (test-equal? "file, exists, part, head"
                  (collect (dispatch #t tmp-file) (req #f 'head (list (make-header #"Range" #"bytes=5-9"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 5\r\nContent-Range: bytes 5-9/81\r\n\r\n")
     
     (test-exn "path, non"
               exn:dispatcher?
               (lambda () (collect (dispatch #t not-there) (req #f 'get empty))))
     
     (test-equal? "dir, exists, no Range, get"
                  (collect (dispatch #t a-dir) (req #t 'get empty))
                  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
     (test-equal? "dir, exists, no Range, head"
                  (collect (dispatch #t a-dir) (req #t 'head empty))
                  #"HTTP/1.1 200 OK\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\n\r\n")
     (test-equal? "dir, exists, Range, get"
                  (collect (dispatch #t a-dir) (req #t 'get (list (make-header #"Range" #"bytes=0-80"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n<html><head><title>A title</title></head><body>Here's some content!</body></html>")
     (test-equal? "dir, exists, Range, head"
                  (collect (dispatch #t a-dir) (req #t 'head (list (make-header #"Range" #"bytes=0-80"))))
                  #"HTTP/1.1 206 Partial content\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html; charset=utf-8\r\nAccept-Ranges: bytes\r\nContent-Length: 81\r\nContent-Range: bytes 0-80/81\r\n\r\n")
     (test-equal? "dir, not dir-url, get"
                  (collect (dispatch #t a-dir) (req #f 'get empty))
                  #"HTTP/1.1 302 Moved Temporarily\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: PLT Scheme\r\nContent-Type: text/html\r\nContent-Length: 0\r\nLocation: /foo/\r\n\r\n")
     (test-exn "dir, not exists, get"
               exn:dispatcher?
               (lambda () (collect (dispatch #f a-dir) (req #t 'get empty))))
     (test-exn "dir, not exists, head"
               exn:dispatcher?
               (lambda () (collect (dispatch #f a-dir) (req #t 'head empty)))))))