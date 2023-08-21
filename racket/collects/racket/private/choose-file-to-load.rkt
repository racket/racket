#lang racket/base
(provide choose-file-to-load
         linklet-bundle-hash-code
         linklet-directory-start)


#|

This code is shared from the expander (specifically
default value of current-load/use-compiled) and
get-module-code / get-module-path. The main functionality
is to decide depend which file to load, among the
.rkt/.ss/.zo/.so/etc possibilities. That also depends
on being able to identify .zo files via the first
few bytes, so the functions with the name `linklet`
also got dragged along too.

|#

#;
;; the first two arguments are the same as the current-load/use-compiled
;; handler's ahe next two are the values of the parameters
;; current-compiled-file-roots and use-compiled-file-paths
;; the results tell us which file the default load/use-compiled handler will
;; load and what it expects to find in the file (if any)
(-> path?
    (or/c #f symbol?
          (cons/c (or/c #f symbol?)
                  (non-empty-listof symbol?)))
    (listof (or/c path? 'same))
    (listof (and/c path? relative-path?))

            ;; current-module-declare-source
    (values (or/c #f (and/c path? complete-path?))

            ;; indicates what kind of file this is
            (or/c 'so 'zo 'src)

            ;; the file (or #f if none was found)
            (or/c #f (and/c path? complete-path?))))

(define choose-file-to-load
  (let* ([resolve (lambda (s)
                    (if (complete-path? s)
                        s
                        (let ([d (current-load-relative-directory)])
                          (if d (path->complete-path s d) s))))]
         [date-of-1 (lambda (a)
                      (let ([v (file-or-directory-modify-seconds a #f (lambda () #f))])
                        (and v (cons a v))))]
         [date-of (lambda (a modes roots)
                    (ormap (lambda (root-dir)
                             (ormap
                              (lambda (compiled-dir)
                                (let ([a (a root-dir compiled-dir)])
                                  (date-of-1 a)))
                              modes))
                           roots))]
         [date>=?
          (lambda (modes roots a bm)
            (and a
                 (let ([am (date-of a modes roots)])
                   (or (and (not bm) am)
                       (and am bm (>= (cdr am) (cdr bm)) am)))))])
    (λ (path expect-module so-okay? rkt-try-ss? choose compiled-file-roots compiled-file-paths)
      (let*-values ([(orig-path) (resolve path)]
                    [(base orig-file dir?) (split-path path)]
                    [(file alt-file) (if rkt-try-ss?
                                         (let* ([b (path->bytes orig-file)]
                                                [len (bytes-length b)])
                                           (cond
                                             [(and (len . >= . 4)
                                                   (bytes=? #".rkt" (subbytes b (- len 4))))
                                              ;; .rkt => try .rkt then .ss
                                              (values orig-file
                                                      (bytes->path (bytes-append (subbytes b 0 (- len 4)) #".ss")))]
                                             [else
                                              ;; No search path
                                              (values orig-file #f)]))
                                         (values orig-file #f))]
                    [(path) (if (eq? file orig-file)
                                orig-path
                                (build-path base file))]
                    [(alt-path) (and alt-file
                                     (if (eq? alt-file orig-file)
                                         orig-path
                                         (build-path base alt-file)))]
                    [(base) (if (eq? base 'relative) 'same base)]
                    [(modes) compiled-file-paths]
                    [(roots) compiled-file-roots]
                    [(reroot) (lambda (p d)
                                (cond
                                  [(eq? d 'same) p]
                                  [(relative-path? d) (build-path p d)]
                                  [else (reroot-path p d)]))])
        (let* ([main-path-d (date-of-1 path)]
               [alt-path-d (and alt-path
                                (not main-path-d)
                                (date-of-1 alt-path))]
               [path-d (or main-path-d alt-path-d)]
               [get-so (lambda (file rep-sfx?)
                         (and (eq? 'racket (system-type 'vm))
                              (lambda (root-dir compiled-dir)
                                (build-path (reroot base root-dir)
                                            compiled-dir
                                            "native"
                                            (system-library-subpath)
                                            (if rep-sfx?
                                                (path-add-extension
                                                 file
                                                 dll-suffix)
                                                file)))))]
               [zo (lambda (root-dir compiled-dir)
                     (build-path (reroot base root-dir)
                                 compiled-dir
                                 (path-add-extension file #".zo")))]
               [alt-zo (lambda (root-dir compiled-dir)
                         (build-path (reroot base root-dir)
                                     compiled-dir
                                     (path-add-extension alt-file #".zo")))]
               [so (get-so file #t)]
               [alt-so (get-so alt-file #t)]
               [try-main? (or main-path-d (not alt-path-d))]
               [try-alt? (and alt-file (or alt-path-d (not main-path-d)))])
          (define choice
            (cond
              [choose
               (define p (if try-main? path alt-path))
               (define-values (base name dir?) (split-path p))
               (define subdir (if (pair? compiled-file-paths) (car compiled-file-paths) "compiled"))
               (choose (if (string? p) (string->path p) p)
                       (build-path base subdir (replace-extension name #".zo"))
                       (build-path base subdir (replace-extension name #".so")))]
              [else #f]))
          (cond
            [(and (not (equal? choice 'src))
                  so-okay?
                  so
                  try-main?
                  (date>=? modes roots so path-d))
             => (lambda (so-d)
                  (values #f 'so (car so-d)))]
            [(and (not (equal? choice 'src))
                  so-okay?
                  alt-so
                  try-alt?
                  (date>=? modes roots alt-so alt-path-d))
             => (lambda (so-d)
                  (values alt-path 'so (car so-d)))]
            [(and (not (equal? choice 'src))
                  try-main?
                  (date>=? modes roots zo path-d))
             => (lambda (zo-d)
                  (values #f 'zo (car zo-d)))]
            [(and (not (equal? choice 'src))
                  try-alt?
                  (date>=? modes roots alt-zo path-d))
             => (lambda (zo-d)
                  (values alt-path 'zo (car zo-d)))]
            [(or (not (pair? expect-module))
                 (car expect-module)
                 (is-compiled-file? (if try-main? path alt-path)))
             (let ([p (if try-main? path alt-path)])
               (values (and expect-module
                            (not try-main?)
                            p) ;; current-module-declare-source
                       'src ;; plain file
                       (and (or (not (pair? expect-module))
                                (file-exists? p))
                            ;; the load handler fails quietly here,
                            ;; when asking for a submodule
                            p)))]
            [else
             ;; here the load-handler should do nothing
             (values #f 'src #f)]))))))

(define (replace-extension pth what)
  (bytes->path
   (bytes-append
    (regexp-replace
     #rx#"[.]([^.]*)$"
     (path->bytes pth)
     #"_\\1")
    what)))

(define-values (dll-suffix)
  (system-type 'so-suffix))

(define (is-compiled-file? p)
  (and (file-exists? p)
       (call-with-input-file* p linklet-directory-start)))

(define version-bytes (string->bytes/utf-8 (version)))
(define version-length (bytes-length version-bytes))
(define vm-bytes (string->bytes/utf-8 (symbol->string (system-type 'vm))))
(define vm-length (bytes-length vm-bytes))

(define (linklet-bundle-or-directory-start i tag)
  (and (equal? (peek-byte i) (char->integer #\#))
       (equal? (peek-byte i 1) (char->integer #\~))
       (equal? (peek-byte i 2) version-length)
       (equal? (peek-bytes version-length 3 i) version-bytes)
       (equal? (peek-byte i (+ 3 version-length)) vm-length)
       (equal? (peek-bytes vm-length (+ 4 version-length) i) vm-bytes)
       (equal? (peek-byte i (+ 4 version-length vm-length)) (char->integer tag))
       (+ version-length
          vm-length
          ;; "#~" and tag and version length byte and vm length byte:
          5)))

(define (linklet-directory-start i)
  (define pos (linklet-bundle-or-directory-start i #\D))
  (and pos (+ pos
              ;; Bundle count:
              4)))

(define (linklet-bundle-hash-code i)
  (define pos (linklet-bundle-or-directory-start i #\B))
  (define hash-code (and pos (peek-bytes 20 pos i)))
  (and (bytes? hash-code)
       (= 20 (bytes-length hash-code))
       (for/or ([c (in-bytes hash-code)])
         (not (eq? c 0)))
       hash-code))
