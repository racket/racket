#lang racket
(require rackunit
         rackunit/text-ui
         xml
         racket/runtime-path)

(module test racket/base)

(define (validate-xml? xml)
  (error 'validate-xml? "Not implemented"))
(define (well-formed-xml? xml)
  (error 'well-formed-xml? "Not implemented"))

(define (read-xml/file f)
  (with-input-from-file f
    (lambda () (read-xml))))

(define (dir->test-suite d name path->test-case)
  (test-suite
   name
   (for ([p (directory-list d)]
         #:when (let ([ext (filename-extension p)])
                  (and ext (bytes=? #"xml" ext))))
     (path->test-case (build-path d p)))))

(define (not-wf-dir->test-suite d)
  (define (path->test-case f)
    (test-not-false
     (path->string f)
     (with-handlers ([exn:xml? (lambda _ #t)])
       (not (well-formed-xml? (read-xml/file f))))))
  (test-suite
   "Not Well-Formed"
   (dir->test-suite 
    (build-path d "sa") "Stand-alone"
    path->test-case)
   (dir->test-suite 
    (build-path d "ext-sa") "External Stand-alone"
    path->test-case)
   (dir->test-suite 
    (build-path d "not-sa") "Not Stand-alone"
    path->test-case)))
(define (invalid-dir->test-suite d)
  (dir->test-suite 
   d "Invalid"
   (lambda (f)
     (test-false (path->string f)
                 (validate-xml? (read-xml/file f))))))
; XXX also check canonical xml
(define (valid-dir->test-suite d)
  (define (path->test-case f)
    (test-not-false (path->string f)
                    (validate-xml? (read-xml/file f))))
  (test-suite
   "Valid"
   (dir->test-suite 
    (build-path d "sa") "Stand-alone"
    path->test-case)
   (dir->test-suite 
    (build-path d "ext-sa") "External Stand-alone"
    path->test-case)
   (dir->test-suite 
    (build-path d "not-sa") "Not Stand-alone"
    path->test-case)))

(define (directory->test-suite d)
  (test-suite
   "James Clark's XML Test Cases"
   
   (not-wf-dir->test-suite (build-path d "not-wf"))
   (invalid-dir->test-suite (build-path d "invalid"))
   (valid-dir->test-suite (build-path d "valid"))))

(define-runtime-path
  clark-tests-zip
  "xmltest.zip")
(define-runtime-path
  clark-tests-target
  ".")
(define-runtime-path
  clark-tests-dir
  (list 'lib "xml/xmltest" "tests"))

(require racket/system)
(system* (find-executable-path "unzip") clark-tests-zip "-d" clark-tests-target)

(define clark-tests
  (directory->test-suite 
   clark-tests-dir))

(run-tests clark-tests)
