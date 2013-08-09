#lang racket/base
(require rackunit
         net/url
         racket/list
         racket/path
         web-server/private/util
         racket/runtime-path
         web-server/dispatchers/filesystem-map)
(provide filesystem-map-tests)

(module+ test
  (require rackunit/text-ui)
  (run-tests filesystem-map-tests))

(define base-dir
  (path-only (collection-file-path "main.rkt" "web-server")))

(define test-map (make-url->path base-dir))
(define test-valid-map (make-url->valid-path test-map))
(define test-filter-map (filter-url->path #rx"\\.(ss|scm|rkt)$" test-map))
(define test-filter-valid-map (filter-url->path #rx"\\.(ss|scm|rkt)$" test-valid-map))

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
               (test-url->path test-map (build-path "dispatchers/filesystem-map.rkt")))
    (test-case "Strips parameters"
               (test-url->path test-map (build-path "dispatchers/filesystem-map.rkt")
                               #:url-string "http://test.com/dispatchers/filesystem-map.rkt;foo"))
    (test-case "Strips out bad '..'s"
               (check-exn exn:fail? (λ () (test-map (string->url "http://test.com/../../dispatchers/filesystem-map.rkt")))))
    (test-case "Handles // okay"
               (check-equal? (second (call-with-values (λ () (test-map (string->url "http://test.com//dispatchers/filesystem-map.rkt"))) list))
                             (list 'same (build-path "dispatchers") (build-path "filesystem-map.rkt"))))
    (test-case "Leaves in good '..'s"
               (test-url->path test-map (build-path "dispatchers/../dispatchers/filesystem-map.rkt"))))

   (test-suite
    "url->valid-path"
    (test-suite
     "Preserves url->path"
     (test-case "Simple case"
                (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.rkt")))
     (test-case "Strips parameters"
                (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.rkt")
                                #:url-string "http://test.com/dispatchers/filesystem-map.rkt;foo"))
     (test-case "Strips out bad '..'s"
                (check-exn exn:fail? (λ () (test-valid-map (string->url "http://test.com/../../dispatchers/filesystem-map.rkt")))))
     (test-case "Leaves in good '..'s"
                (test-url->path test-valid-map (build-path "dispatchers/../dispatchers/filesystem-map.rkt"))))
    (test-case "Finds valid path underneath"
               (test-url->path test-valid-map (build-path "dispatchers/filesystem-map.rkt/not-a-file")
                               #:expected (build-path "dispatchers/filesystem-map.rkt"))))


   (test-suite
    "filter-url->path"
    (test-case "Allows right suffix"
               (test-url->path test-filter-map (build-path "dispatchers/filesystem-map.rkt")))
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
               (test-url->path test-filter-valid-map (build-path "dispatchers/filesystem-map.rkt/extra/info")
                               #:expected (build-path "dispatchers/filesystem-map.rkt"))))))
