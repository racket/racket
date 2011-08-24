#lang scheme/base
(require racket/cmdline
         raco/command-name
         "../link.rkt")

(define link-file (make-parameter #f))
(define link-name (make-parameter #f))
(define root-mode (make-parameter #f))
(define link-version (make-parameter #f))
(define remove-mode (make-parameter #f))
(define repair-mode (make-parameter #f))
(define show-mode (make-parameter #f))
(define install-only (make-parameter #f))
(define user-only (make-parameter #f))

(define link-symbol (string->symbol (short-program+command-name)))

(define dirs
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-l" "--list") "Show the link table (after changes)"
    (show-mode #t)]
   #:once-any
   [("-n" "--name") name "Collection name to add (single <dir>) or remove"
    (link-name name)]
   [("-d" "--root") "Treat <dir> as a collection root"
    (root-mode #t)]
   #:once-each
   [("-x" "--version-regexp") regexp "Set the version pregexp"
    (with-handlers ([exn:fail:contract? (lambda (exn)
                                          (raise-user-error link-symbol
                                                            "bad version regexp: ~a"
                                                            regexp))])
      (link-version (pregexp regexp)))]
   [("-r" "--remove") "Remove links for the specified directories"
    (remove-mode #t)]
   #:once-any
   [("-u" "--user") "Adjust/list user-specific links"
    (user-only #t)]
   [("-i" "--installation") "Adjust/list installation-wide links"
    (install-only #t)]
   [("-f" "--file") file "Select an alternate link file"
    (link-file (path->complete-path file))]
   #:once-each
   [("--repair") "Enable repair mode to fix existing links"
    (repair-mode #t)]
   #:args 
   dir dir))

(when (and (link-name)
           (not (remove-mode))
           (not (= 1 (length dirs))))
  (raise-user-error link-symbol
                    "expected a single directory for `--name' mode"))

(define show-both?
  (and (null? dirs)
       (show-mode)
       (not (user-only))
       (not (install-only))
       (not (link-file))))

(when show-both?
  (printf "User links:\n"))

(define (go user?)
  (apply links
         dirs
         #:root? (root-mode)
         #:user? user?
         #:file (link-file)
         #:name (link-name)
         #:version-regexp (link-version)
         #:error (lambda (who . args)
                   (apply raise-user-error link-symbol args))
         #:remove? (remove-mode)
         #:show? (show-mode)
         #:repair? (repair-mode)))

(define l1
  (go (not (install-only))))
(define l2
  (if (and (not (or (user-only)
                    (install-only)))
           (remove-mode))
      (go #f)
      null))

(when show-both?
  (printf "Installation links:\n")
  (void (links #:user? #f #:show? #t)))

(when (and (remove-mode)
           (null? l1)
           (null? l2))
  (printf "[no links removed]\n"))

