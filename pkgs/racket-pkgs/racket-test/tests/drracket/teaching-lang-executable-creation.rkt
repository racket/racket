#lang racket/base
(require lang/private/create-htdp-executable
         racket/file
         racket/system)
(define tmpfile (make-temporary-file "executable-creation-test~a"))

(create-htdp-lang-executable (collection-file-path
                              "image-and-comment-box.rkt"
                              "tests"
                              "drracket")
                             tmpfile
                             '(lib "htdp-beginner-reader.ss" "lang"))

(define sp (open-output-string))
(define res
  (let/ec k
    (parameterize ([error-escape-handler (λ () (k #f))])
      (parameterize ([current-output-port sp]
                     [current-error-port sp]
                     [current-input-port (open-input-string "")])
        (case (system-type)
          [(macosx) 
           (define-values (base name dir) (split-path tmpfile))
           (system
            (path->string
             (build-path tmpfile "Contents" "MacOS" name)))]
          [(unix)
           (system (path->string tmpfile))]
          [(windows)
           (error 'teaching-lang-executable-creation.rkt "test not supported")])))))
(delete-directory/files tmpfile)

(define expected "1\n")
(unless (and res (equal? (get-output-string sp) expected))
  (eprintf "TEST FAILED; res ~s\n~a\n" res (get-output-string sp)))
