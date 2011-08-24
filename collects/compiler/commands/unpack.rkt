#lang scheme/base
(require scheme/cmdline
         raco/command-name
         setup/unpack
         racket/file
         racket/port
         racket/match
         racket/string
         racket/pretty)

(define verbose (make-parameter #f))

(define just-show? (make-parameter #f))
(define replace? (make-parameter #f))
(define show-config? (make-parameter #f))

(define mzc-symbol (string->symbol (short-program+command-name)))

(define files
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("-l" "--list") "just list archive content"
    (just-show? #t)]
   [("-c" "--config") "show archive configuration"
    (show-config? #t)]
   [("-f" "--force") "replace existing files when unpacking"
    (replace? #t)]
   #:args archive
   archive))

(define (desc->path dir)
  (if (path? dir)
      dir
      (apply build-path
             (symbol->string (car dir))
             (cdr dir))))

(for ([filename (in-list files)])
  (fold-plt-archive filename 
                    (lambda (config a)
                      (when (show-config?)
                        (match config
                          [`(lambda (request failure) 
                              (case request 
                                ((name) ,name) 
                                ((unpacker) (quote mzscheme)) 
                                ((requires) (quote ,reqs)) 
                                ((conflicts) (quote ,conflicts)) 
                                ((plt-relative?) ,plt-rel?) 
                                ((plt-home-relative?) ,plt-home-rel?) 
                                ((test-plt-dirs) ,test-plt-dirs) 
                                (else (failure))))
                           (printf "config:\n")
                           (printf " name: ~s\n" name)
                           (printf " requires:\n")
                           (for ([c (in-list reqs)])
                             (printf "  ~s ~s\n" (string-join (car c) "/") (cadr c)))
                           (printf " conflicts:\n")
                           (for ([c (in-list conflicts)])
                             (printf "  ~s\n" (string-join c "/")))
                           (cond
                            [plt-home-rel? (printf " unpack to main installation\n")]
                            [plt-rel? (printf " unpack to user add-ons\n")]
                            [else (printf " unpack locally\n")])]
                          [else
                           (printf "config function:\n")
                           (pretty-write config)]))
                      a)
                    (lambda (setup i a)
                      (when (show-config?)
                        (match setup
                          [`(unit (import main-collects-parent-dir mzuntar) (export) (mzuntar void) (quote ,c))
                           (printf "setup collections:\n")
                           (for ([c (in-list c)])
                             (printf " ~s\n" (string-join c "/")))]
                          [else
                           (printf "setup unit:\n")
                           (pretty-write setup)]))
                      a)
                    (lambda (dir a)
                      (unless (eq? dir 'same)
                        (if (just-show?)
                            (printf "~a\n" (path->directory-path (desc->path dir)))
                            (make-directory* (desc->path dir))))
                      a)
                    (lambda (file i kind a)
                      (if (just-show?)
                          (printf "~a~a\n" (desc->path file)
                                  (if (eq? kind 'file-replace)
                                      " [replace]"
                                      ""))
                          (call-with-output-file*
                           (desc->path file)
                           #:exists (if (or (eq? kind 'file-replace)
                                            (replace?))
                                        'truncate/replace
                                        'error)
                           (lambda (o)
                             (copy-port i o))))
                      a)
                    (void)))
