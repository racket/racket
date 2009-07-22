#lang scheme

#|

This runs a bunch of integration tests for the planet command-line tool,
using 'system' to call out to the tool and then reading its results, etc.

|#

(require scheme/system
         planet/config
         net/url)

(define planet-bin-path
  (simplify-path (build-path (collection-path "scheme") 'up 'up "bin" "planet")))

(define test-connection-spec '("planet" "test-connection.plt" "1" "0"))
(define test-connection.plt-cache
  (apply build-path
         (UNINSTALLED-PACKAGE-CACHE) 
         (append test-connection-spec
                 (list (list-ref test-connection-spec 1)))))

(define debug? #f)

(define (call-planet . args)
  (when debug? (printf "~s\n" (cons 'call-planet args)))
  (let ([sp (open-output-string)])
    (parameterize ([current-input-port (open-input-string "")]
                   [current-output-port sp]
                   [current-error-port sp])
      (apply system* planet-bin-path args))
    (when debug? (display (get-output-string sp)))
    (get-output-string sp)))

(unless (regexp-match #rx"No packages" (call-planet "show"))
  (error 'cmdline-tool.ss "please clear out all planet packages before running this test"))

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
     (error 'cmdline-tool.ss "Installation failed\n")])
  
  (unless (file-exists? test-connection.plt-cache)
    (error 'cmdline-tool.ss "Installation did not populate ~s" test-connection.plt-cache)))

  
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
     (error 'cmdline-tool.ss "Removal failed\n")]
    [else
     (printf "Removed successfully\n")])
  
  (unless (file-exists? test-connection.plt-cache)
    (error 'cmdline-tool.ss "Removal removed ~s" test-connection.plt-cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet fetch vs planet url
;;  

;; NB: this test leaves behind test-connection.plt, which other test rely on

(define (fetch-vs-url)
  (define direct-file (format "direct-~a" (list-ref test-connection-spec 1)))
  
  (define stupid-internal-definition-syntax1 
    (begin (ensure-not-there direct-file)
           (ensure-not-there (list-ref test-connection-spec 1))
           (printf "Downloading test-connection.plt (2 ways) ... ")
           (flush-output)))
  
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
    (error 'cmdline-tool.ss "expected planet fetch and planet url to point at the same file, but found different ones"))
  
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
    (error 'cmdline-tool.ss "expected a fileinject to show up in planet info"))
  (apply call-planet "remove" test-connection-spec)
  (when (regexp-match #rx"test-connection.plt" (call-planet "show"))
    (error 'cmdline-tool.ss "expected remove (after fileinject) to remove test-connection.plt"))
  (printf "Fileinject successful\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  planet sructure & open
;;

(define (do-structure)
  (define stupid-internal-definition-syntax0
    (begin (printf "Running open vs structure & print test ... ")
           (flush-output)))
  (define structure-files 
    (filter
     (λ (x) (not (equal? "" x)))
     (sort 
      (regexp-split #rx"\n" (call-planet "structure" (list-ref test-connection-spec 1)))
      string<=?)))
  (define tmp-dir "test-connection-contents")
  (define stupid-internal-definition-syntax1
    (when (directory-exists? tmp-dir)
      (error 'cmdline-tool.ss "expected the directory ~a to not exist" tmp-dir)))
  (define stupid-internal-definition-syntax2
    (call-planet "open" (list-ref test-connection-spec 1) tmp-dir))
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
  
  (system (format "rm -rf ~a" tmp-dir))
  (printf "done\n")
  (unless (equal? open-files structure-files)
    (error 'cmdline-tool.ss "expected planet structure to produce the same files as planet open, got ~s and ~s"
           structure-files 
           open-files))
 
  (unless (equal? doc.txt-fetch doc.txt-print)
    (error 'cmdline-tool.ss "expected planet print to produce the same content as the actual file, got\n~s\nand\n~s"
           doc.txt-print
           doc.txt-fetch)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  util
;;
  
(define (ensure-not-there fn)
  (when (file-exists? fn)
    (error 'cmdline-tool.ss
           "test script expects no file named ~a to exist in the current directory (may have been left behind by test script, tho ... (which would be a bug))"
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

(delete-file (list-ref test-connection-spec 1))
(printf "Finished tests\n")
