#lang racket

#|

This runs a bunch of integration tests for the planet command-line tool,
using 'system' to call out to the tool and then reading its results, etc.

|#

(require racket/system
         planet/config
         net/url)

(define debug? #f)

(define raco-bin-path
  (simplify-path (build-path (collection-path "racket") 'up 'up  
                             (if (eq? (system-type) 'windows)
                                 "raco.exe"
                                 (build-path "bin" "raco")))))

(define test-connection-spec '("planet" "test-connection.plt" "1" "0"))
(define test-connection.plt-cache
  (apply build-path
         (UNINSTALLED-PACKAGE-CACHE) 
         (append test-connection-spec
                 (list (list-ref test-connection-spec 1)))))

(define (call-planet . args)
  (when debug? (printf "~s\n" (cons 'call-planet args)))
  (let ([sp (open-output-string)])
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port sp]
                   [current-error-port sp])
      (apply system* raco-bin-path "planet" args))
    (when debug? (display (get-output-string sp)))
    (get-output-string sp)))

(let ([result (call-planet "show")])
  (unless (regexp-match #rx"No packages" result)
    (error 'cmdline-tool.rkt 
           (string-append 
            "please clear out all planet packages before running this test.\n"
            "============================================================\n~a" 
            result))))
  
(when (file-exists? test-connection.plt-cache)
  (delete-file test-connection.plt-cache))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   planet install, w/out cached file
;;

(define (do-install w-w/out)
  (printf "Installing test-connection.plt (~a cached .plt) ... " w-w/out) (flush-output)
  (void (apply call-planet "install" test-connection-spec))
  (printf "done\n")
  (cond
    [(regexp-match #rx"test-connection.plt" (call-planet "show"))
     (printf "Installed successfully\n")]
    [else
     (error 'cmdline-tool.rkt "Installation failed\n")])
  
  (unless (file-exists? test-connection.plt-cache)
    (error 'cmdline-tool.rkt "Installation did not populate ~s" test-connection.plt-cache)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet remove
;;

(define (do-remove)
  (printf "Removing test-connection.plt ... ") (flush-output)
  (void (apply call-planet "remove" test-connection-spec))
  (printf "done\n")
  (cond
    [(regexp-match #rx"test-connection.plt" (call-planet "show"))
     (error 'cmdline-tool.rkt "Removal failed\n")]
    [else
     (printf "Removed successfully\n")])
  
  (unless (file-exists? test-connection.plt-cache)
    (error 'cmdline-tool.rkt "Removal removed ~s" test-connection.plt-cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet fetch vs planet url
;;  

;; NB: this test leaves behind test-connection.plt, which other tests rely on

(define (fetch-vs-url)
  (define direct-file (format "direct-~a" (list-ref test-connection-spec 1)))
  

  (ensure-not-there direct-file)
  (ensure-not-there (list-ref test-connection-spec 1))
  (printf "Downloading test-connection.plt (2 ways) ... ")
  (flush-output)
  
  (define direct-download-thread
    (thread
     (λ ()
       (call-with-output-file direct-file
         (λ (f-port)
           (call/input-url (string->url (apply call-planet "url" test-connection-spec))
                           get-pure-port
                           (λ (u-port)
                             (copy-port u-port f-port))))
         #:exists 'truncate))))
  
  (void (apply call-planet "fetch" test-connection-spec))
  (thread-wait direct-download-thread)
  (printf "done\n")
  
  (unless (same-file? direct-file (list-ref test-connection-spec 1))
    (error 'cmdline-tool.rkt
           (string-append
            "expected planet fetch and planet url to point at the same file,"
            " but found different ones")))
  
  (delete-file direct-file)
  (printf "Download succesful\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet fileinject
;;

(define (do-fileinject)
  (printf "Running fileinject ... ")
  (flush-output)
  (apply call-planet "fileinject" test-connection-spec)
  (printf "done\n")
  (unless (regexp-match #rx"test-connection.plt" (call-planet "show"))
    (error 'cmdline-tool.rkt "expected a fileinject to show up in planet info"))
  (apply call-planet "remove" test-connection-spec)
  (when (regexp-match #rx"test-connection.plt" (call-planet "show"))
    (error 'cmdline-tool.rkt "expected remove (after fileinject) to remove test-connection.plt"))
  (printf "Fileinject successful\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet sructure & open
;;

(define (do-structure)
  (printf "Running open vs structure & print test ... ")
  (flush-output)
  (define structure-files 
    (filter
     (λ (x) (not (equal? "" x)))
     (sort 
      (regexp-split #rx"\n" (call-planet "structure" (list-ref test-connection-spec 1)))
      string<=?)))
  (define tmp-dir "test-connection-contents")
  (when (directory-exists? tmp-dir)
      (error 'cmdline-tool.rkt "expected the directory ~a to not exist" tmp-dir))
  (call-planet "open" (list-ref test-connection-spec 1) tmp-dir)
  (define open-files
    (sort (let f/d-loop ([f/d #f]
                         [acc '()])
            (let ([this-one (if f/d
                                (build-path tmp-dir f/d)
                                tmp-dir)])
              (cond
                [(file-exists? this-one) 
                 (cons (path->string f/d) acc)]
                [(directory-exists? this-one)
                 (let loop ([contents (directory-list this-one)]
                            [acc acc])
                   (cond
                     [(null? contents) acc]
                     [else
                      (loop (cdr contents)
                            (f/d-loop (if f/d 
                                          (build-path f/d (car contents))
                                          (car contents))
                                      acc))]))]
                [else acc])))
          string<=?))
  
  (define doc.txt-print (call-planet "print" (list-ref test-connection-spec 1) "doc.txt"))
  (define doc.txt-fetch (let ([sp (open-output-string)])
                          (call-with-input-file (build-path tmp-dir "doc.txt")
                            (λ (port)
                              (copy-port port sp)))
                          (get-output-string sp)))
  
  (delete-directory/files tmp-dir)
  (printf "done\n")
  (unless (equal? open-files structure-files)
    (error 'cmdline-tool.rkt 
           "expected planet structure to produce the same files as planet open, got ~s and ~s"
           structure-files 
           open-files))
 
  (unless (equal? doc.txt-fetch doc.txt-print)
    (error 'cmdline-tool.rkt
           "expected planet print to produce the same content as the actual file, got\n~s\nand\n~s"
           doc.txt-print
           doc.txt-fetch)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet create
;;

(define files
  '(("info.rkt"
     #<<--
#lang info

(define name "the-name")
(define blurb
  (list "the blurb"))
(define primary-file "main.rkt")
(define scribblings '(("doc.scrbl")))
(define release-notes '("release notes"))
(define categories '(misc))
(define repositories '("4.x"))
--
     )
    ("doc.scrbl" 
     #<<--
#lang scribble/doc
@(require scribble/base)
@title{the docs}
--
     )
    ("main.rkt" "#lang racket\n(provide the-export)\n(define the-export 1)\n")))
     
(define (do-create)
  (printf "Running create test ... ")
  (flush-output)
  (define tmp-root-dir (make-temporary-file "planet-cmdline-tool-test-create-~a" 'directory))
  (define tmp-dir (build-path tmp-root-dir "the-source"))
  (define plt-file (build-path tmp-root-dir "the-source.plt"))
  (make-directory tmp-dir)
  (for ([f (in-list files)])
    (define file (list-ref f 0))
    (define contents (list-ref f 1))
    (call-with-output-file (build-path tmp-dir file)
      (λ (port) (display contents port))))
  (define output (parameterize ([current-directory tmp-root-dir])
                   (call-planet "create" (path->string tmp-dir))))
  (cond
    [(or (regexp-match #rx"[Ee]rror" output)
         (regexp-match #rx"Refusing" output)
         (regexp-match #rx"=== context ===" output))
     (eprintf "error during planet create test:\n~a" output)]
    [(regexp-match #rx"WARNING" output)
     (eprintf "warning during planet create test:\n~a" output)]
    [else
     (define contents 
       (filter
        (λ (x) (not (equal? x "")))
        (sort (regexp-split #rx"\n" (call-planet "structure" (path->string plt-file)))
              string<=?)))
     (unless (equal? contents
                     (list "    index.html"
                           "    scribble-common.js"
                           "    scribble-style.css"
                           "    scribble.css"
                           "  doc:"
                           "doc.scrbl"
                           "info.rkt"
                           "main.rkt"
                           "planet-docs:"))
       (eprintf "planet create test doesn't contain expected stuff; got:\n  ~s\n"
                contents))])
  
  (delete-directory/files tmp-root-dir)
  (printf "done\n"))
                           
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  util
;;
  
(define (ensure-not-there fn)
  (when (file-exists? fn)
    (error 'cmdline-tool.rkt
           (string-append
            "test script expects no file named ~a to exist in the current directory"
            " (may have been left behind by test script, tho ... (which would be a bug))")
           fn)))

;; same-file? : string-or-port string-or-port -> boolean
;; compares the contents of f1 and f2
(define (same-file? f1 f2)
  (call-with-input-file f1
    (λ (direct-port)
      (call-with-input-file f2
        (λ (via-planet-port)
          (let loop ()
            (let ([b1 (read-byte direct-port)]
                  [b2 (read-byte via-planet-port)])
              (cond
                [(equal? b1 b2)
                 (if (eof-object? b1)
                     #t
                     (loop))]
                [else #f]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; main
;;

(fetch-vs-url)
(do-install "without")
(do-remove)
(do-install "with")
(do-remove)
(do-fileinject)
(do-structure)
(do-create)

(delete-file (list-ref test-connection-spec 1))
(printf "Finished tests\n")
