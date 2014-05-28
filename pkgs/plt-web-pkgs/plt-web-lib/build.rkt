#lang racket/base

(require racket/cmdline racket/runtime-path racket/file scribble/html
         pkg/path net/url
         "private/roots.rkt")

(define build-mode 'web)
(define output-dir (current-directory))
(define warn? #t)
(define extra-files '())

(command-line
 #:once-any
 [("-w" "--web")
  "web mode: create content that is viewable via HTTP"
  (set! build-mode 'web)]
 [("-l" "--local")
  "local mode: create content that is viewable in the build directory"
  (set! build-mode 'local)]
 [("-r" "--relative")
  "local mode, but all links are relative"
  (set! build-mode 'relative)]
 #:once-each
 [("-o" "--output") dir
  "output directory"
  "  (defaults to the current directory)"
  (unless (directory-exists? dir)
    (printf "Creating \"~a\"\n" dir) (make-directory dir))
  (set! output-dir dir)]
 [("-f" "--force")
  "avoid warning about directory cleanup"
  (set! warn? #f)]
 #:multi
 [("+e" "++extra") extra
  "extra file to render more content"
  (set! extra-files (cons extra extra-files))])

(let ([cache (make-hash)])
  (define (check-dest p)
    (when (path->pkg p #:cache cache)
      (raise-user-error
       'plt-web
       "destination overlaps with package directories, refusing to build (use `-o')")))
  (check-dest output-dir)
  (for ([p (in-directory output-dir)])
    (when (directory-exists? p)
      (check-dest p))))

(parameterize ([current-directory output-dir])
  (define paths (sort (map path->string (directory-list)) string<?))
  (when (pair? paths)
    (if (or (not warn?)
            (begin (printf "Directory not empty, these will be deleted: ~a.\n"
                           (string-join paths ", "))
                   (printf "Continue? ") (flush-output)
                   (regexp-match? #rx" *[yY]" (read-line))))
      (for-each delete-directory/files paths)
      (raise-user-error 'build "Aborting."))))

(printf "Building ~a content...\n" build-mode)
(parameterize ([url-roots (case build-mode
                            [(web) (registered-url-roots)]
                            [(local) (map (lambda (s)
                                            (list* (car s)
                                                   (url->string (path->url (build-path output-dir (car s))))
                                                   ;; Drop 'abs, if any, from (cddr s)
                                                   '(index)))
                                          (registered-url-roots))]
                            [else (url-roots)])])
  (for ([extra (in-list extra-files)])
    (if (file-exists? extra)
      (dynamic-require `(file ,extra) #f)
      (printf "  ignoring missing extra file: ~a\n" extra)))
  (parameterize ([current-directory output-dir])
    (render-all)))

(case build-mode
  [(web) (call-with-output-file (build-path output-dir "sites.rktd")
           #:exists 'truncate
           (lambda (o)
             (write (for/hash ([i (in-list (registered-url-roots))])
                      (values (car i) (cadr i)))
                    o)
             (newline o)))]
  [else (void)])

(printf "Done.\n")

(module test racket/base)
