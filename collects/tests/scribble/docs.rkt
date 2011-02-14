#lang racket/base

;; Use text renderer to check some Scribble functionality

;; ----------------------------------------

(require scribble/base-render
         racket/file
         racket/class
         (prefix-in text:  scribble/text-render))

(define (build-text-doc src-file dest-file)
  (define dir (find-system-path 'temp-dir))
  (let ([renderer (new (text:render-mixin render%)
                    [dest-dir dir])])
    (let* ([docs (list (dynamic-require `(file ,src-file) 'doc))]
           [fns (list (build-path dir dest-file))]
           [fp (send renderer traverse docs fns)]
           [info (send renderer collect docs fns fp)])
      (let ([r-info (send renderer resolve docs fns info)])
        (send renderer render docs fns r-info)))))

(define (check-text-build name)
  (define src-file (string-append "docs/" name ".scrbl"))
  (define expect-file (string-append "docs/" name ".txt"))
  (build-text-doc src-file "gen.txt")
  (unless (string=? (file->string expect-file)
                    (file->string (build-path (find-system-path 'temp-dir)
                                              "gen.txt")))
    (error 'check-text-build "mismatch from: ~e expected: ~e"
           src-file expect-file)))

;; ----------------------------------------

(check-text-build "print-lines")
