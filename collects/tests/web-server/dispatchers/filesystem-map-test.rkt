#lang scheme/base
(require schemeunit
         net/url
         web-server/private/util
         web-server/dispatchers/filesystem-map)
(provide filesystem-map-tests)

(define base-dir (collection-path "web-server"))
(define test-map (make-url->path base-dir))
(define test-valid-map (make-url->valid-path test-map))
(define test-filter-map (filter-url->path #rx"\\.(ss|scm)$" test-map))
(define test-filter-valid-map (filter-url->path #rx"\\.(ss|scm)$" test-valid-map))

(define (test-url->path 
         url->path file
         #:url-string 
         [url-string
          (format "http://test.com/~a" (path->string file))]
         #:expected
         [expected file])
  (define vs
    (call-with-values
     (lambda () (url->path (string->url url-string)))
     (lambda vs vs)))
  (check-equal? vs (list (build-path base-dir expected) (explode-path* expected))))

(define filesystem-map-tests
  (test-suite
   "Filesystem Map"
   
   (test-suite
    "url->path"
    (test-case "Simple case"
               (test-url->path test-map (build-path "dispatchers/filesystem-map.ss")))
    (test-case "Strips parameters"
               (test-url->path test-map (build-path "dispatchers/filesystem-map.ss")
                               #:url-string "http://test.com/dispatchers/filesystem-map.ss;foo"))
    (test-case "Strips outs bad '..'s"
               (test-url->path test-map (build-path "dispatchers/filesystem-map.ss")
                               #:url-string "http://test.com/../../dispatchers/filesystem-map.ss"))
    (test-case "Leaves in good '..'s"
               (test-url->path test-map (build-path "dispatchers/../dispatchers/filesystem-map.ss"))))
   
   (test-suite
    "url->valid-path"
    (test-suite
     "Preserves url->path"
     (test-case "Simple case"
                (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.ss")))
     (test-case "Strips parameters"
                (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.ss")
                                #:url-string "http://test.com/dispatchers/filesystem-map.ss;foo"))
     (test-case "Strips outs bad '..'s"
                (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.ss")
                                #:url-string "http://test.com/../../dispatchers/filesystem-map.ss"))
     (test-case "Leaves in good '..'s"
                (test-url->path test-valid-map (build-path "dispatchers/../dispatchers/filesystem-map.ss"))))
    (test-case "Finds valid path underneath"
               (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.ss/not-a-file")
                               #:expected (build-path "dispatchers/filesystem-map.ss"))))
   
   
   (test-suite
    "filter-url->path"
    (test-case "Allows right suffix"
               (test-url->path test-filter-map (build-path "dispatchers/filesystem-map.ss")))
    (test-case "Allows right suffix"
               (test-url->path test-filter-map (build-path "dispatchers/filesystem-map.scm")))
    (test-case "Disallows wrong suffix"
               (check-exn
                exn:fail:filesystem:exists?
                (lambda ()
                  (test-url->path test-filter-map (build-path "dispatchers/filesystem-map.gif")))))
    (test-case "Disallows wrong suffix"
               (check-exn
                exn:fail:filesystem:exists?
                (lambda ()
                  (test-url->path test-filter-map (build-path "dispatchers/filesystem-map.html")))))
    (test-case "Allows content after w/ valid"
               (test-url->path test-filter-valid-map (build-path "dispatchers/filesystem-map.ss/extra/info")
                               #:expected (build-path "dispatchers/filesystem-map.ss"))))))
