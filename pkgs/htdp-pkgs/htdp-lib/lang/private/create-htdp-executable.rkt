#lang racket/base
(require compiler/embed
         wxme/wxme)
(provide create-htdp-lang-executable
         cannot-compile?)

(define oprintf
  (let ([o (current-output-port)])
    (λ args
      (apply fprintf o args))))

(define (create-htdp-lang-executable program-filename exe-name reader-module)
  (create-embedding-executable 
   exe-name
   #:modules `((#f ,reader-module)
               (#f (lib "wxme/read.ss"))
               (#f (lib "mred/mred.ss"))
               (#f racket/gui/init)
               (#f ,program-filename))
   #:configure-via-first-module? #t
   #:cmdline `("-l"
               "racket/base"
               "-e"
               ,(format "~s" `(#%require ',(filename->require-symbol program-filename))))
   #:src-filter
   (λ (path) (cannot-compile? path))
   #:get-extra-imports
   (λ (path cm)
     (call-with-input-file path
       (λ (port)
         (cond
           [(is-wxme-stream? port)
            ;; Extract snip-related modules:
            (define-values (snip-class-names data-class-names)
              (extract-used-classes port))
            (define used-mods (append snip-class-names data-class-names))
              (append
               (filter values (map (λ (x) (string->lib-path x #t)) used-mods))
               (filter values (map (λ (x) (string->lib-path x #f)) used-mods))
               ;; Extract reader-related modules:
               (begin
                 (file-position port 0)
                 (let ([mods null])
                   (parameterize ([current-reader-guard
                                   (let ([g (current-reader-guard)])
                                     (lambda (p)
                                       (set! mods (cons p mods))
                                       (g p)))])
                     (read-language (wxme-port->port port) (lambda () #f)))
                   mods)))]
           [else
            '()]))))
   #:mred? #t))

(define (filename->require-symbol fn)
  (let-values ([(base name dir) (split-path fn)])
    (string->symbol
     (path->string
      (path-replace-suffix name #"")))))

;; cannot-compile? : path -> boolean
;; returns #t if the file cannot be compiled, #f otherwise
(define (cannot-compile? path)
  (call-with-input-file path
    (λ (port) 
      (let ([ok-to-compile-names 
             (map (λ (x) (format "~s" x))
                  '(wxtext
                    (lib "comment-snip.ss" "framework")
                    (lib "xml-snipclass.ss" "xml")
                    (lib "scheme-snipclass.ss" "xml")))])
        (and (is-wxme-stream? port)
             (let-values ([(snip-class-names data-class-names)
                           (extract-used-classes port)])
               (not (and (andmap
                          (λ (used-name) (member used-name ok-to-compile-names))
                          snip-class-names)
                         (andmap
                          (λ (used-name) (member used-name ok-to-compile-names))
                          data-class-names)))))))))




