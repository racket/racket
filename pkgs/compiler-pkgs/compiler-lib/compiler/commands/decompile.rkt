#lang racket/base
(require racket/cmdline
         raco/command-name
         compiler/zo-parse
         compiler/decompile
         compiler/compilation-path
         racket/pretty
         racket/format)

(define (get-name)
  (string->symbol (short-program+command-name)))

(define force? #f)

(define source-files
  (command-line
   #:program (short-program+command-name)
   #:once-each
   [("--force") "Ignore timestamp mimatch on associated \".zo\""
    (set! force? #t)]
   [("--columns" "-n") n "Format for <n> columns"
    (let ([num (string->number n)])
      (unless (exact-positive-integer? num)
        (raise-user-error (get-name)
                          "not a valid column count: ~a" n))
      (pretty-print-columns num))]
   #:args source-or-bytecode-file
   source-or-bytecode-file))

(define (check-files orig-file alt-file)
  (cond
   [(not (file-exists? alt-file))
    (cond
     [(file-exists? orig-file)
      (unless (is-bytecode-file? orig-file)
        (raise-user-error (get-name)
                          (~a "not a bytecode file, and no associated \".zo\" file\n"
                              "  path: ~a\n"
                              "  tried associated path: ~a")
                          orig-file
                          alt-file))]
     [else
      (raise-user-error (get-name)
                        (~a "no such file, and no associated \".zo\" file\n"
                            "  path: ~a\n"
                            "  tried associated path: ~a")
                        orig-file
                        alt-file)])]
   [(not (is-bytecode-file? alt-file))
    (raise-user-error (get-name)
                      (~a "associated \".zo\" file is not a bytecode file\n"
                          "  original path: ~a\n"
                          "  associated path: ~a")
                      orig-file
                      alt-file)]
   [(and (not force?)
         ((file-or-directory-modify-seconds orig-file
                                            #f
                                            (lambda () -inf.0))
          . > .
          (file-or-directory-modify-seconds alt-file)))
    ;; return a warning:
    (raise-user-error (get-name)
                      (~a "associated \".zo\" file's date is older than given file's date;\n"
                          " consider using `raco make` to rebuild the source file, or use `--force`\n"
                          " to skip the date check\n"
                          "  original path: ~a\n"
                          "  associated path: ~a")
                      orig-file
                      alt-file)]))

(define (is-bytecode-file? orig-file)
  (call-with-input-file*
   orig-file
   (lambda (i)
     (equal? #"#~" (read-bytes 2 i)))))

(for ([zo-file source-files])
  (let ([zo-file (path->complete-path zo-file)])
    (let-values ([(base name dir?) (split-path zo-file)])
      (let ([alt-file (get-compilation-bytecode-file zo-file)])
        (check-files zo-file alt-file)
        (parameterize ([current-load-relative-directory base]
                       [print-graph #t])
          (pretty-write
           (decompile
            (call-with-input-file*
             (if (file-exists? alt-file) alt-file zo-file)
             (lambda (in)
               (zo-parse in))))))))))
