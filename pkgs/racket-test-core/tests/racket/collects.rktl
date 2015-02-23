
(load-relative "loadtest.rktl")

(Section 'collects)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/file)

(let ()
  (define tmp-dir (make-temporary-file "collects~a" 'directory))

  (make-directory* (build-path tmp-dir "zjhyq-1"))
  (with-output-to-file (build-path tmp-dir "zjhyq-1" "m.rkt")
    (lambda ()
      (displayln "#lang racket/base")
      (displayln "(provide v)")
      (displayln "(define v 1)")))
  
  (test #f collection-path "zjhyq-1" #:fail (lambda (s) #f))
  (define (test-found)
    (test (build-path (build-path tmp-dir "zjhyq-1"))
          collection-path "zjhyq-1")
    (test (build-path (build-path tmp-dir "zjhyq-1" "m.rkt"))
          collection-file-path "m.rkt" "zjhyq-1"))

  ;; Add to paths
  (parameterize ([current-library-collection-paths
                  (append (current-library-collection-paths)
                          (list tmp-dir))])
    (test-found))

  ;; Add to link as hash table from #f:
  (parameterize ([current-library-collection-links
                  (append (current-library-collection-links)
                          (list (hash #f (list tmp-dir))))])
    (test-found))

  ;; Add to link as hash table from 'zjhyq-1:
  (parameterize ([current-library-collection-links
                  (append (current-library-collection-links)
                          (list (hash 'zjhyq-1 (list (build-path tmp-dir "zjhyq-1")))))])
    (test-found))

  (delete-directory/files tmp-dir))

;; ----------------------------------------

(err/rt-test (current-library-collection-paths 5))
(err/rt-test (current-library-collection-paths (list 5)))
(err/rt-test (current-library-collection-paths (list "relative")))
;; strings coreced to paths:
(test #t andmap path? (parameterize ([current-library-collection-paths (list (path->string
                                                                              (current-directory)))])
                        (current-library-collection-paths)))

(err/rt-test (current-library-collection-links 5))
(err/rt-test (current-library-collection-links (list 5)))
(err/rt-test (current-library-collection-links (list #t)))
(err/rt-test (current-library-collection-links (list "relative")))
(err/rt-test (current-library-collection-links (list (hash 'bad! null))))
(err/rt-test (current-library-collection-links (list (hash 'ok 5))))
(err/rt-test (current-library-collection-links (list (hash 'ok (list 5)))))
(err/rt-test (current-library-collection-links (list (hash 'ok (list "relative")))))
;; strings coreced to paths:
(test #t andmap path? (parameterize ([current-library-collection-links (list (path->string
                                                                              (build-path (current-directory)
                                                                                          "links.rktd")))])
                        (current-library-collection-links)))

;; ----------------------------------------

(report-errs)
