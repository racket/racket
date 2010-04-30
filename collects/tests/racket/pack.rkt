
;; WARNING: this test writes a "packed" collection to the main and user
;; collection directories.

(load-relative "loadtest.rkt")

(Section 'pack)

(require setup/pack
	 setup/unpack
	 mzlib/process
	 setup/dirs
	 mzlib/file)

;; Test via mzc interface

(define mzc (build-path (find-console-bin-dir) "mzc"))

(define (make-x-plt-str mod)
  (path->string (build-path (find-system-path 'temp-dir) (format "x~a.plt" mod))))

(define x-plt-str (make-x-plt-str ""))
(define x-replace-plt-str (make-x-plt-str "r"))
(define x-user-collect-plt-str (make-x-plt-str "u"))
(define x-collect-plt-str (make-x-plt-str "c"))
(define collection-plt-str (make-x-plt-str "cc"))
(define user-collection-plt-str (make-x-plt-str "uc"))

(define src-dir
  (build-path (find-system-path 'temp-dir) "packed"))
(when (directory-exists? src-dir)
  (delete-directory/files src-dir))

(make-directory src-dir)
(with-output-to-file (build-path src-dir "apple")
  (lambda () (printf "APPLE\n")))
(with-output-to-file (build-path src-dir "banana")
  (lambda () (printf "BANANA\n")))

(parameterize ([current-directory (find-system-path 'temp-dir)])
  (system* mzc "--plt" x-plt-str "packed")
  (system* mzc "--plt" x-replace-plt-str "--replace" "packed")
  (make-directory "collects")
  (rename-file-or-directory "packed" "collects/packed")
  (system* mzc "--plt" x-user-collect-plt-str "--at-plt" "collects")
  (system* mzc "--plt" x-collect-plt-str "--at-plt" "--all-users" "collects")
  (rename-file-or-directory "collects/packed" "packed")
  (delete-directory "collects"))

(let ([dest (build-path (find-system-path 'temp-dir) "unpacked")])
  (when (directory-exists? dest)
    (delete-directory/files dest))
  (make-directory dest)
  (parameterize ([current-directory dest])
    (unpack x-plt-str))
  
  (parameterize ([current-directory (find-system-path 'temp-dir)])
    (test "APPLE\n" with-input-from-file "unpacked/packed/apple" (lambda () (read-string 800)))
    (test "BANANA\n" with-input-from-file "unpacked/packed/banana" (lambda () (read-string 800)))

    (with-output-to-file "unpacked/packed/banana"
      (lambda () (printf "COCONUT\n"))
      #:exists 'truncate))

  (parameterize ([current-directory dest])
    (unpack x-plt-str))

  (parameterize ([current-directory (find-system-path 'temp-dir)])
    (test "COCONUT\n" with-input-from-file "unpacked/packed/banana" (lambda () (read-string 800))))

  (parameterize ([current-directory dest])
    (unpack x-replace-plt-str))

  (parameterize ([current-directory (find-system-path 'temp-dir)])
    (test "BANANA\n" with-input-from-file "unpacked/packed/banana" (lambda () (read-string 800))))

  (delete-directory/files dest))

(define (try-collect-plt dir x-plt-str pack-plt flag)
  (when (directory-exists? (build-path dir "packed"))
    (raise-syntax-error "packed collection exists; aborting"))
  
  (parameterize ([current-directory (find-system-path 'temp-dir)])
    (unpack x-plt-str))
  
  (test #t directory-exists? (build-path dir "packed"))
  (test 'APPLE with-input-from-file (build-path (collection-path "packed") "apple") read)
  (test 'BANANA with-input-from-file (build-path (collection-path "packed") "banana") read)
  
  (when pack-plt
    (system* mzc "--collection-plt" pack-plt flag "packed"))
  
  (delete-directory/files (build-path dir "packed")))

(try-collect-plt (find-collects-dir) x-collect-plt-str user-collection-plt-str "--at-plt")
(try-collect-plt (find-user-collects-dir) x-user-collect-plt-str collection-plt-str "--force-all-users")

(try-collect-plt (find-collects-dir) collection-plt-str #f #f)
(try-collect-plt (find-user-collects-dir) user-collection-plt-str #f #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
