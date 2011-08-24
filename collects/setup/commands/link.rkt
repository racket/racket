#lang scheme/base
(require racket/cmdline
         raco/command-name
         "../link.rkt")

(define link-file (make-parameter #f))
(define link-name (make-parameter #f))
(define link-version (make-parameter #f))
(define remove-mode (make-parameter #f))
(define repair-mode (make-parameter #f))
(define show-mode (make-parameter #f))
(define user-mode (make-parameter #t))

(define link-symbol (string->symbol (short-program+command-name)))

(define dirs
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-l" "--list") "Show the link table (after changes)"
    (show-mode #t)]
   [("-n" "--name") name "Set the collection name (for a single directory)"
    (link-name name)]
   [("-x" "--version-regexp") regexp "Set the version pregexp"
    (with-handlers ([exn:fail:contract? (lambda (exn)
                                          (raise-user-error link-symbol
                                                            "bad version regexp: ~a"
                                                            regexp))])
      (link-version (pregexp regexp)))]
   [("-r" "--remove") "Remove links for the specified directories"
    (remove-mode #t)]
   #:once-any
   [("-i" "--installation") "Adjust user-independent links in the installation"
    (user-mode #f)]
   [("-f" "--file") file "Select an alternate link file"
    (link-file (path->complete-path file))]
   #:once-each
   [("--repair") "Enable repair mode to fix existing links"
    (repair-mode #t)]
   #:args 
   dir dir))

(when (and (link-name)
           (not (= 1 (length dirs))))
  (raise-user-error link-symbol
                    "expected a single directory for `--name' mode"))

(define show-both?
  (and (null? dirs)
       (show-mode)
       (user-mode)
       (not (remove-mode))
       (not (link-file))))

(when show-both?
  (printf "User links:\n"))

(void
 (apply links
        dirs
        #:user? (user-mode)
        #:file (link-file)
        #:name (link-name)
        #:version-regexp (link-version)
        #:error (lambda (who . args)
                  (apply raise-user-error link-symbol args))
        #:remove? (remove-mode)
        #:show? (show-mode)
        #:repair? (repair-mode)))

(when show-both?
  (printf "Installation links:\n")
  (void (links #:user? #f #:show? #t)))

