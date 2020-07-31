#lang racket/base
(require racket/cmdline
         setup/cross-system
         racket/file)

(module test racket/base)

;; Remove debugging and CGC files

(define keep-cgc? #f)
(define keep-3m? #f)
(define keep-cs? #f)

(define dir
  (command-line
   #:once-each
   [("--keep-cgc") "Keep CGC executables and libraries"
    (set! keep-cgc? #t)]
   [("--keep-3m") "Keep 3m/BC executables and libraries"
    (set! keep-3m? #t)]
   [("--keep-cs") "Keep CS executables and libraries"
    (set! keep-cs? #t)]
   #:args
   (dir)
   dir))

(define (delete-file* p)
  (printf "Deleting ~a\n" p)
  (delete-file p))

(when (eq? 'windows (cross-system-type))
  (define type
    (call-with-input-file*
     (build-path dir "Racket.exe")
     (lambda (i)
       (define m (regexp-match "bINARy tYPe:..(.)" i))
       (case (and m (cadr m))
         [(#"c") 'cgc]
         [(#"s") 'cs]
         [else '3m]))))
  
  (for ([a (directory-list dir)])
    (define f (build-path dir a))
    (define b (path-element->bytes a))
    (when (and (file-exists? f)
               (or (regexp-match? #rx#"[.](?i:pdb|ilk)$" b)
                   (and (not keep-cgc?)
                        (regexp-match? #rx#"(?i:CGC[.]exe)$" b))
                   (and (not keep-3m?)
                        (regexp-match? #rx#"(?i:(?:3m|BC)[.]exe)$" b))
                   (and (not keep-cs?)
                        (regexp-match? #rx#"(?i:CS[.]exe)$" b))))
      (delete-file* f)))

  (for ([f (in-directory (build-path dir "lib"))])
    (when (and (file-exists? f)
               (let ([b (path-element->bytes
                         (let-values ([(base name dir?) (split-path f)])
                           name))])
                 (or (regexp-match? #rx#"[.](?i:pdb|ilk|manifest|ipdb|iobj)$" b)
                     (and (not keep-cgc?)
                          (regexp-match? #rx#"(?i:CGC[.](?:dll|exe))$" b))
                     (and (not keep-3m?)
                          (regexp-match? #rx#"(?i:(?:3m|BC)[.]exe)$" b))
                     (and (not keep-cs?)
                          (regexp-match? #rx#"(?i:CS[.]exe)$" b))
                     (and (regexp-match? #rx#"(?i:[.](?:dll|exp|obj|lib|def))$" b)
                          (regexp-match? #rx#"(?i:racket|mzgc)" b)
                          (let ([dll-type
                                 (cond
                                   [(regexp-match? #rx#"(?i:racketcs)" b) 'cs]
                                   [(regexp-match? #rx#"(?i:racket3m)" b) '3m]
                                   [else 'cgc])])
                            (cond
                              [(eq? dll-type type) #f]
                              [else (case dll-type
                                      [(cgc) (not keep-cgc?)]
                                      [(cs) (not keep-cs?)]
                                      [else (not keep-3m?)])]))))))
      (delete-file* f)))
  
  ;; Delete any subdirectory that contains ".lib" files.
  ;; Those are compiler-specific directories, and so we
  ;; don't want to include them in a distribution.
  (for ([f (in-directory (build-path dir "lib"))])
    (when (and (directory-exists? f)
               (for/or ([f (in-directory f)])
                 (regexp-match? #rx"[.]lib$" f)))
      (delete-directory/files f))))
