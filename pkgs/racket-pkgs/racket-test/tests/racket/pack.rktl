
;; WARNING: this test writes a "packed" collection to the main and user
;; collection directories.

(load-relative "loadtest.rktl")

(Section 'pack)

(require setup/pack
	 setup/unpack
	 racket/system
	 setup/dirs
	 racket/file)

;; Test via mzc interface

(define raco (build-path (find-console-bin-dir) "raco"))

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
  (system* raco "pack" x-plt-str "packed")
  (system* raco "pack" "--replace" x-replace-plt-str "packed")
  (make-directory "collects")
  (rename-file-or-directory "packed" "collects/packed")
  (system* raco "pack" "--at-plt" x-user-collect-plt-str "collects")
  (system* raco "pack" "--at-plt" "--all-users" x-collect-plt-str "collects")
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
    (system* raco "pack" flag "--collect" pack-plt "packed"))
  
  (delete-directory/files (build-path dir "packed")))

(try-collect-plt (find-collects-dir) x-collect-plt-str user-collection-plt-str "--at-plt")
(try-collect-plt (find-user-collects-dir) x-user-collect-plt-str collection-plt-str "--force-all-users")

(try-collect-plt (find-collects-dir) collection-plt-str #f #f)
(try-collect-plt (find-user-collects-dir) user-collection-plt-str #f #f)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ()
  (define tmp-src-dir
    (make-temporary-file "example~a" 'directory))

  (write-to-file 0 (build-path tmp-src-dir "foo"))
  (write-to-file 1 (build-path tmp-src-dir "bar"))
  (make-directory (build-path tmp-src-dir "zog"))
  (write-to-file 2 (build-path tmp-src-dir "zog" "zip"))

  ;; The structure is...
  ;; root
  ;;  + foo => 0
  ;;  + bar => 1
  ;;  - zog
  ;;    + zip => 2

  ;; Turn it into a .plt archive
  (define tmp-plt
    (make-temporary-file "example~a.plt"))

  (pack-plt tmp-plt "example" (list tmp-src-dir)
            #:as-paths (list "."))

  ;; Now unpack it
  (define tmp-dest-dir
    (make-temporary-file "example~a" 'directory))

  ;; This errors because #f is given to build-path, because target-dir
  ;; is bound to #f at some point, because of the (values #f 'same) is
  ;; the result when v is just '(same) to shuffle-path
  (parameterize ([current-directory tmp-dest-dir])
    (unpack tmp-plt (current-directory) void))

  (fold-plt-archive
   tmp-plt
   void
   void
   (λ (dir _)
      ;; Each of these fails because the values are actually `(same
      ;; ,path), despite the contract claiming to give path-string?
      ;; values (same with below)
      ;;
      ;; The code calls expr->path-descriptor which returns this and
      ;; another list'd value
      ;;
      ;; The only use of fold-plt-archive in the tree, planet/util,
      ;; works around this by just pulling out the path component or
      ;; erroring.
      (test #t path-string? dir))
   (λ (file content-p _1 [_2 #f])
      (test #t path-string? file))
   #f)

  (delete-directory/files tmp-plt)
  (delete-directory/files tmp-dest-dir))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(report-errs)
