
(load-relative "loadtest.rktl")

(Section 'path)

(require racket/path)

(define (rtest f args result)
  (test result f args))

;; ----------------------------------------

(test #t path-element? (build-path "filename"))
(test #t path-element? (bytes->path #"filename" 'unix))
(test #t path-element? (bytes->path #"filename" 'windows))
(test #f path-element? (build-path "file" "next"))
(test #f path-element? (build-path (bytes->path #"file" 'unix)
                                   (bytes->path #"next" 'unix)))
(test #f path-element? (build-path (bytes->path #"file" 'windows)
                                   (bytes->path #"next" 'windows)))
(test #f path-element? (build-path 'up))
(test #f path-element? (build-path 'same))
(test #f path-element? (build-path/convention-type 'unix 'up))
(test #f path-element? (build-path/convention-type 'unix 'same))
(test #f path-element? (build-path/convention-type 'windows 'up))
(test #f path-element? (build-path/convention-type 'windows 'same))
(test #f ormap path-element? (filesystem-root-list))

;; ----------------------------------------

(rtest explode-path "a/b" (list (string->path "a")
                                (string->path "b")))
(rtest explode-path "a/../b" (list (string->path "a")
                                   'up
                                   (string->path "b")))
(rtest explode-path "./a/b" (list 'same
                                  (string->path "a")
                                  (string->path "b")))
(rtest explode-path (bytes->path #"./a/b" 'unix) (list 'same
                                                       (bytes->path #"a" 'unix)
                                                       (bytes->path #"b" 'unix)))
(rtest explode-path (bytes->path #"./a\\b" 'windows) (list 'same
                                                           (bytes->path #"a" 'windows)
                                                           (bytes->path #"b" 'windows)))

;; ----------------------------------------

(rtest file-name-from-path "a/" #f)
(rtest file-name-from-path "a/b" (string->path "b"))
(rtest file-name-from-path (bytes->path #"a/b" 'unix) (bytes->path #"b" 'unix))
(rtest file-name-from-path (bytes->path #"a\\b" 'windows) (bytes->path #"b" 'windows))

;; ----------------------------------------

(rtest filename-extension "a" #f)
(rtest filename-extension "a.sls" #"sls")
(rtest filename-extension (bytes->path #"b/a.sls" 'unix) #"sls")
(rtest filename-extension (bytes->path #"b\\a.sls" 'windows) #"sls")

;; ----------------------------------------

(test (string->path "a") find-relative-path (path->complete-path "b") (path->complete-path "b/a"))
(test (build-path 'up 'up "b" "a") find-relative-path (path->complete-path "c/b") (path->complete-path "b/a"))
(test (bytes->path #"a" 'unix) find-relative-path (bytes->path #"/r/b" 'unix) (bytes->path #"/r/b/a" 'unix))
(test (bytes->path #"a" 'windows) find-relative-path (bytes->path #"c:/r/b" 'windows) (bytes->path #"c:/r/b/a" 'windows))
(test (bytes->path #"d:/r/b/a" 'windows) find-relative-path (bytes->path #"c:/r/b" 'windows) (bytes->path #"d:/r/b/a" 'windows))
(test (bytes->path #"../b/a" 'unix) find-relative-path (bytes->path #"/r/c" 'unix) (bytes->path #"/r/b/a" 'unix))
(test (bytes->path #"../../q/b/a" 'unix) find-relative-path (bytes->path #"/r/c" 'unix) (bytes->path #"/q/b/a" 'unix))
(test (bytes->path #"/q/b/a" 'unix) 'find-relative-path (find-relative-path (bytes->path #"/r/c" 'unix) (bytes->path #"/q/b/a" 'unix) 
                                                                            #:more-than-root? #t))
(test (bytes->path #"q/b/a" 'unix) 'find-relative-path (find-relative-path (bytes->path #"/" 'unix) (bytes->path #"/q/b/a" 'unix) 
                                                                           #:more-than-root? #t))
(test (bytes->path #"../.." 'unix) 'find-relative-path (find-relative-path (bytes->path #"/r/c" 'unix) (bytes->path #"/" 'unix) 
                                                                           #:more-than-root? #t))

;; ----------------------------------------

;; normalize-path needs tests

;; ----------------------------------------

(rtest path-only "a" #f)
(rtest path-only "a/b" (string->path "a/"))
(rtest path-only "a/b/" (string->path "a/b/"))
(rtest path-only "a/.." (string->path "a/.."))
(rtest path-only (bytes->path #"a/z" 'unix) (bytes->path #"a/" 'unix))
(rtest path-only (bytes->path #"a/z/" 'unix) (bytes->path #"a/z/" 'unix))
(rtest path-only (bytes->path #"a/z" 'windows) (bytes->path #"a/" 'windows))
(rtest path-only (bytes->path #"a/z/" 'windows) (bytes->path #"a/z/" 'windows))

;; ----------------------------------------

;; simple-form-path needs tests

;; ----------------------------------------

(test "a" some-system-path->string (string->path "a"))
(test "a" some-system-path->string (bytes->path #"a" 'unix))
(test "a" some-system-path->string (bytes->path #"a" 'windows))
(test #t path-for-some-system? (string->some-system-path "a" 'unix))
(test #t path-for-some-system? (string->some-system-path "a" 'windows))
(test "a" some-system-path->string (string->some-system-path "a" 'unix))
(test "a" some-system-path->string (string->some-system-path "a" 'windows))

;; ----------------------------------------

(test #f shrink-path-wrt (build-path "x.rkt") '())
(test #f shrink-path-wrt (build-path "x.rkt") (list (build-path "x.rkt")))
(test (build-path "x.rkt") shrink-path-wrt (build-path "x.rkt") (list (build-path "x.rkt")
                                                                      (build-path "y.rkt")))
(test (build-path "a" "x.rkt") shrink-path-wrt
      (build-path "a" "x.rkt")
      (list (build-path "a" "x.rkt")
            (build-path "b" "x.rkt")))

(test (build-path "d" "a" "x.rkt") shrink-path-wrt
      (build-path "d" "a" "x.rkt")
      (list (build-path "b" "x.rkt")
            (build-path "c" "a" "x.rkt")
            (build-path "d" "a" "x.rkt")))

(test (build-path "d" "a" "x.rkt") shrink-path-wrt
      (build-path "d" "a" "x.rkt")
      (list (build-path "b" "x.rkt")
            (build-path "p" "c" "a" "x.rkt")
            (build-path "p" "d" "a" "x.rkt")))

(test #f shrink-path-wrt
      (build-path "d" "a" "x.rkt")
      (list (build-path "d" "a" "x.rkt")
            (build-path "d" "a" "x.rkt")))


(report-errs)
