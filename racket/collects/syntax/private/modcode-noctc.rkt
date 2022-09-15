#lang racket/base
(require racket/list
         racket/path
         racket/private/choose-file-to-load
         "../modread.rkt")

(provide moddep-current-open-input-file
         exn:get-module-code
         exn:get-module-code?
         exn:get-module-code-path
         make-exn:get-module-code

         get-module-code
         get-module-path
         get-metadata-path

         default-compiled-sub-path)

(define moddep-current-open-input-file
  (make-parameter open-input-file))

(define (resolve s)
  (if (complete-path? s)
      s
      (let ([d (current-load-relative-directory)])
        (if d (path->complete-path s d) s))))

(define (date>=? a bm)
  (and a
       (let ([am (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
                   (file-or-directory-modify-seconds a))])
         (and am (if bm (>= am bm) #t)))))

(define (read-one orig-path path src? read-src-syntax)
  (define p ((moddep-current-open-input-file) path))
  (when src? (port-count-lines! p))
  (define (reader)
    (define-values (base name dir?) (split-path orig-path))
    (define unchecked-v
      (with-module-reading-parameterization
        (lambda () 
          ;; In case we're reading a .zo, we need to set
          ;;  the load-relative directory for unmarshaling
          ;;  path literals.
          (parameterize ([current-load-relative-directory
                          (if (path? base) base (current-directory))])
            (read-src-syntax path p)))))
    (when (eof-object? unchecked-v)
      (error 'read-one "empty file; expected a module declaration in: ~a" path))
    (define sym
      (string->symbol
        (bytes->string/utf-8 (path->bytes (path-replace-extension name #"")) #\?)))
    (define checked-v (check-module-form unchecked-v sym path))
    (unless (eof-object? (read p))
      (error 'read-one
             "file has more than one expression; expected a module declaration only in: ~a"
             path))
    (if (and (syntax? checked-v) (compiled-expression? (syntax-e checked-v)))
        (syntax-e checked-v)
        checked-v))
  (define (closer) (close-input-port p))
  (dynamic-wind void reader closer))

(define-struct (exn:get-module-code exn:fail) (path))

(define (reroot-path* base root)
  (cond
    [(eq? root 'same) base]
    [(relative-path? root) (build-path base root)]
    [else (reroot-path base root)]))

;; : (or/c path-string? 'same) -> (or/c path? 'same)
(define (path-string->path ps)
  (if (string? ps) (string->path ps) ps))

;; : (listof (or/c path-string? 'same)) -> (listof (or/c path? 'same))
(define (root-strs->roots root-strs)
  (map path-string->path root-strs))

(define (get-metadata-path
          #:roots [root-strs (current-compiled-file-roots)]
          base-str . arg-strs)
  (define base (path-string->path base-str))
  (define roots (root-strs->roots root-strs))
  (define args (root-strs->roots arg-strs))
  (cond
    [(or (equal? roots '(same)) (null? roots))
     (apply build-path base args)]
    [else
     (or (for/or ([root (in-list (if (null? (cdr roots)) null roots))])
           (define p (apply build-path (reroot-path* base root) args))
           (and (file-exists? p) p))
         (apply build-path (reroot-path* base (car roots)) args))]))

(define (get-metadata-path/multiple-sub-paths #:roots roots path0-base sub-paths . args)
  (define candidates
    (for/list ([sub-path (in-list (if (list? sub-paths) sub-paths (list sub-paths)))])
      (apply get-metadata-path #:roots roots path0-base sub-path args)))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (file-exists? candidate))
        candidate)
      (car candidates)))

(define (default-compiled-sub-path)
  (let ([l (use-compiled-file-paths)])
    (if (pair? l)
        (car l)
        "compiled")))

(define (get-module-path
         path0-str
         #:roots [root-strs (current-compiled-file-roots)]
         #:submodule? [submodule? #f]
         #:sub-path [sub-path/kw (use-compiled-file-paths)]
         [_sub-paths sub-path/kw]
         #:choose [choose #f]
         #:rkt-try-ss? [rkt-try-ss? #t])
  (define-values (the-module-declare-source file-type the-path)
    (choose-file-to-load path0-str
                         #f
                         (not submodule?)
                         rkt-try-ss?
                         choose
                         root-strs
                         (if (list? sub-path/kw)
                             sub-path/kw
                             (list sub-path/kw))))
  (unless the-path
    (raise (make-exn:get-module-code
            (format "get-module-code: no such file: ~e" path0-str)
            (current-continuation-marks)
            #f)))
  (values (if (string? the-path)
              (string->path the-path)
              the-path)
          file-type))

(define (get-module-code
          path0-str
          #:roots [root-strs (current-compiled-file-roots)]
          #:submodule-path [submodule-path '()]
          #:sub-path [sub-path/kw (use-compiled-file-paths)]
          [sub-path sub-path/kw]
          #:compile [compile/kw compile]
          [compiler compile/kw]
          #:extension-handler [ext-handler/kw #f]
          [ext-handler ext-handler/kw] 
          #:choose [choose #f]
          #:notify [notify void]
          #:source-reader [read-src-syntax read-syntax]
          #:rkt-try-ss? [rkt-try-ss? #t])
  (define path0 (path-string->path path0-str))
  (define roots (root-strs->roots root-strs))
  (define-values (path type)
    (get-module-path
     path0
     #:roots roots
     #:submodule? (pair? submodule-path)
     #:sub-path sub-path
     #:choose choose
     #:rkt-try-ss? rkt-try-ss?))
  (define (extract-submodule m [sm-path submodule-path])
    (cond
      [(null? sm-path) m]
      [else 
       (extract-submodule
         (or (for/or ([c (in-list (append (module-compiled-submodules m #t)
                                          (module-compiled-submodules m #f)))])
               (and (eq? (last (module-compiled-name c)) (car sm-path))
                    c))
             (raise
               (make-exn:get-module-code
                 (format "get-module-code: cannot find submodule: ~e" sm-path)
                 (current-continuation-marks)
                 #f)))
         (cdr sm-path))]))
  (case type
    [(zo)
     (notify path)
     (extract-submodule (read-one path0 path #f read-syntax))]
    [(so)
     (if ext-handler
         (begin
           (notify path)
           (ext-handler path #f))
         (raise (make-exn:get-module-code
                  (format "get-module-code: cannot use extension file; ~e" path)
                  (current-continuation-marks)
                  path)))]
    [(src)
     (notify path)
     (define (compile-one)
       (define-values (path0-base path0-name path0-dir?) (split-path path0))
       (parameterize ([current-load-relative-directory
                       (if (path? path0-base) path0-base (current-directory))])
         (compiler (read-one path0 path #t read-src-syntax))))
     (if (null? submodule-path)
         ;; allow any result:
         (compile-one)
         ;; expect a compiled-module result:
         (extract-submodule (compile-one)))]))
