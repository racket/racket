
(load-relative "loadtest.rktl")

(Section 'path)

(require scheme/path)

(define (rtest f args result)
  (test result f args))

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

(report-errs)
