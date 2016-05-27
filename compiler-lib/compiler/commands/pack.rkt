#lang scheme/base
(require scheme/cmdline
         raco/command-name
         setup/pack
         setup/getinfo
         compiler/distribute)

(define verbose (make-parameter #f))

(define collection? (make-parameter #f))

(define default-plt-name "archive")

(define plt-name (make-parameter default-plt-name))
(define plt-files-replace (make-parameter #f))
(define plt-files-plt-relative? (make-parameter #f))
(define plt-files-plt-home-relative? (make-parameter #f))
(define plt-force-install-dir? (make-parameter #f))
(define plt-setup-collections (make-parameter null))
(define plt-include-compiled (make-parameter #f))

(define mzc-symbol (string->symbol (short-program+command-name)))

(define-values (plt-output source-files)
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("--collect") "<path>s specify collections instead of files/dirs"
    (collection? #t)]
   [("--plt-name") name "Set the printed <name> describing the archive"
    (plt-name name)]
   [("--replace") "Files in archive replace existing files when unpacked"
    (plt-files-replace #t)]
   [("--at-plt") "Files/dirs in archive are relative to user's add-ons directory"
    (plt-files-plt-relative? #t)]
   #:once-any
   [("--all-users") "Files/dirs in archive go to PLT installation if writable"
    (plt-files-plt-home-relative? #t)]
   [("--force-all-users") "Files/dirs forced to PLT installation"
    (plt-files-plt-home-relative? #t) (plt-force-install-dir? #t)]
   #:once-each
   [("--include-compiled") "Include \"compiled\" subdirectories in the archive"
    (plt-include-compiled #t)]
   #:multi
   [("++setup") collect "Setup <collect> after the archive is unpacked"
    (plt-setup-collections (append (plt-setup-collections) (list collect)))]
   #:once-each
   [("-v") "Verbose mode"
    (verbose #t)]
   #:args (dest-file . path)
   (values dest-file path)))

(if (not (collection?))
    ;; Files and directories
    (begin
      (for ([fd source-files])
        (unless (relative-path? fd)
          (error mzc-symbol
                 "file/directory is not relative to the current directory: \"~a\""
                 fd)))
      (pack-plt plt-output
                (plt-name)
                source-files
                #:collections (map list (plt-setup-collections))
                #:file-mode (if (plt-files-replace) 'file-replace 'file)
                #:plt-relative? (or (plt-files-plt-relative?)
                                    (plt-files-plt-home-relative?))
                #:at-plt-home? (plt-files-plt-home-relative?)
                #:test-plt-dirs (if (or (plt-force-install-dir?)
                                        (not (plt-files-plt-home-relative?)))
                                    #f
                                    '("collects" "doc" "include" "lib"))
                #:requires
                null)
      (when (verbose)
        (printf " [output to \"~a\"]\n" plt-output)))
    ;; Collection
    (begin
      (pack-collections-plt
       plt-output
       (if (eq? default-plt-name (plt-name)) #f (plt-name))
       (map (lambda (sf)
              (let loop ([sf sf])
                (let ([m (regexp-match "^([^/]*)/(.*)$" sf)])
                  (if m (cons (cadr m) (loop (caddr m))) (list sf)))))
            source-files)
       #:replace? (plt-files-replace)
       #:extra-setup-collections (map list (plt-setup-collections))
       #:file-filter (if (plt-include-compiled)
                         (lambda (path)
                           (or (regexp-match #rx#"compiled$" (path->bytes path))
                               (std-filter path)))
                         std-filter)
       #:at-plt-home? (plt-files-plt-home-relative?)
       #:test-plt-collects? (not (plt-force-install-dir?)))
      (when (verbose)
        (printf " [output to \"~a\"]\n" plt-output))))

(module test racket/base)
