(module filesystem-map-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (lib "kw.ss")
           (lib "url.ss" "net")
           (lib "util.ss" "web-server" "private")
           (lib "filesystem-map.ss" "web-server" "dispatchers"))
  (provide filesystem-map-tests)
  
  (define base-dir (collection-path "web-server"))
  (define test-map (make-url->path base-dir))
  (define test-valid-map (make-url->valid-path test-map))
  
  (define/kw (test-url->path url->path file
                             #:key
                             [url-string
                              (format "http://test.com/~a" (path->string file))]
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
                                  #:expected (build-path "dispatchers/filesystem-map.ss")))))))