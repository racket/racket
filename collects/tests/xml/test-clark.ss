#lang scheme
(require schemeunit
         schemeunit/private/check
         schemeunit/private/test-case
         schemeunit/private/test-suite
         schemeunit/text-ui
         xml
         scheme/runtime-path)
(require/expose schemeunit/private/test-suite
                (current-seed))

(define (validate-xml? xml)
  (error 'validate-xml? "Not implemented"))
(define (well-formed-xml? xml)
  (error 'well-formed-xml? "Not implemented"))

(define (read-xml/file f)
  (with-input-from-file f
    (lambda () (read-xml))))
(define (dir->test-suite d name path->test-case)
  (make-schemeunit-test-suite
   name
   (lambda (fdown fup fhere seed)
     (parameterize
         ([current-seed seed]
          [current-test-case-around (test-suite-test-case-around fhere)]
          [current-check-around (test-suite-check-around fhere)])
       (for-each (lambda (p)
                   (define t (path->test-case (build-path d p)))
                   (if (schemeunit-test-suite? t)
                       (current-seed (apply-test-suite t fdown fup fhere (current-seed)))
                       t))
                 (filter (lambda (p)
                           (define ext (filename-extension p))
                           (and ext (bytes=? #"xml" ext)))
                         (directory-list d)))) 
     (current-seed))
   void
   void))

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
  clark-tests-dir
  (list 'lib "xml/clark-tests" "tests"))

(define clark-tests
  (directory->test-suite 
   clark-tests-dir))

(run-tests clark-tests)
