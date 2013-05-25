#lang racket/base
(require racket/contract/base
         racket/list
         racket/path
         "modread.rkt")

(provide moddep-current-open-input-file
         exn:get-module-code
         exn:get-module-code?
         exn:get-module-code-path
         make-exn:get-module-code)

(provide/contract
 [get-module-code
  (->* (path?)
       (#:roots (listof (or/c path? 'same))
        #:submodule-path (listof symbol?)
        #:sub-path (and/c path-string? relative-path?)
        (and/c path-string? relative-path?)
        #:compile (-> any/c any)
        (-> any/c any)
        #:extension-handler (or/c false/c (path? boolean? . -> . any))
        (or/c false/c (path? boolean? . -> . any))
        #:choose (path? path? path? . -> . (or/c 'src 'zo 'so #f))
        #:notify  (any/c . -> . any)
        #:source-reader (any/c input-port? . -> . (or/c syntax? eof-object?))
        #:rkt-try-ss? boolean?)
       any)]
 [get-module-path
  (->* (path?)
       (#:roots (listof (or/c path? 'same))
        #:submodule? boolean?
        #:sub-path (and/c path-string? relative-path?)
        (and/c path-string? relative-path?)
        #:choose (path? path? path? . -> . (or/c 'src 'zo 'so #f))
        #:rkt-try-ss? boolean?)
       (values path? (or/c 'src 'zo 'so)))]
 [get-metadata-path
  (->* (path?)
       (#:roots (listof (or/c path? 'same)))
       #:rest (listof (or/c path-string? 'same))
       path?)])

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
        (bytes->string/utf-8 (path->bytes (path-replace-suffix name #"")) #\?)))
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

(define (get-metadata-path
          #:roots [roots (current-compiled-file-roots)]
          base . args)
  (cond
    [(or (equal? roots '(same)) (null? roots))
     (apply build-path base args)]
    [else
     (or (for/or ([root (in-list (if (null? (cdr roots)) null roots))])
           (define p (apply build-path (reroot-path* base root) args))
           (and (file-exists? p) p))
       (apply build-path (reroot-path* base (car roots)) args))]))

(define (get-module-path
          path0
          #:roots [roots (current-compiled-file-roots)]
          #:submodule? [submodule? #f]
          #:sub-path [sub-path/kw "compiled"]
          [sub-path sub-path/kw]
          #:choose [choose (lambda (src zo so) #f)]
          #:rkt-try-ss? [rkt-try-ss? #t])
  (define resolved-path (resolve path0))
  (define-values (path0-rel path0-file path0-dir?) (split-path path0))
  (define-values (main-src-file alt-src-file)
    (if rkt-try-ss?
        (let* ([b (path->bytes path0-file)]
               [len (bytes-length b)])
          (cond
            [(and (len . >= . 4) (bytes=? #".rkt" (subbytes b (- len 4))))
             ;; .rkt => try .rkt then .ss
             (values path0-file
                     (bytes->path (bytes-append (subbytes b 0 (- len 4))
                                                #".ss")))]
            [else
             ;; No search path
             (values path0-file #f)]))
      (values path0-file #f)))
  (define main-src-path
    (if (eq? main-src-file path0-file)
        resolved-path
        (build-path path0-rel main-src-file)))
  (define alt-src-path
    (and alt-src-file
         (if (eq? alt-src-file path0-file)
             resolved-path
             (build-path path0-rel alt-src-file))))
  (define path0-base (if (eq? path0-rel 'relative) 'same path0-rel))
  (define main-src-date
    (file-or-directory-modify-seconds main-src-path #f (lambda () #f)))
  (define alt-src-date
    (and alt-src-path
         (not main-src-date)
         (file-or-directory-modify-seconds alt-src-path #f (lambda () #f))))
  (define src-date (or main-src-date alt-src-date))
  (define src-file (if alt-src-date alt-src-file main-src-file))
  (define src-path (if alt-src-date alt-src-path main-src-path))
  (define try-alt? (and alt-src-file (not alt-src-date) (not main-src-date)))
  (define (get-so file)
    (get-metadata-path #:roots roots
                       path0-base
                       sub-path
                       "native"
                       (system-library-subpath)
                       (path-add-suffix file (system-type 'so-suffix))))
  (define zo
    (get-metadata-path #:roots roots
                       path0-base
                       sub-path
                       (path-add-suffix src-file #".zo")))
  (define alt-zo
    (and try-alt?
         (get-metadata-path #:roots roots
                            path0-base
                            sub-path
                            (path-add-suffix alt-src-file #".zo"))))
  (define so (get-so src-file))
  (define alt-so (and try-alt? (get-so alt-src-file)))
  (define prefer (choose src-path zo so))
  (cond
    ;; Use .zo, if it's new enough
    [(or (eq? prefer 'zo)
         (and (not prefer)
              (pair? roots)
              (or (date>=? zo src-date)
                  (and try-alt?
                       (date>=? alt-zo src-date)))))
     (let ([zo (if (date>=? zo src-date)
                   zo
                   (if (and try-alt? (date>=? alt-zo src-date))
                       alt-zo
                       zo))])
       (values (simple-form-path zo) 'zo))]
    ;; Maybe there's an .so? Use it only if we don't prefer source
    ;; and only if there's no submodule path.
    [(and (not submodule?)
          (or (eq? prefer 'so)
              (and (not prefer)
                   (pair? roots)
                   (or (date>=? so src-date)
                       (and try-alt?
                            (date>=? alt-so src-date))))))
     (let ([so (if (date>=? so src-date)
                   so
                   (if (and try-alt? (date>=? alt-so src-date))
                       alt-so
                       so))])
       (values (simple-form-path so) 'so))]
    ;; Use source if it exists
    [(or (eq? prefer 'src) src-date)
     (values (simple-form-path src-path) 'src)]
    ;; Report a not-there error
    [else (raise (make-exn:get-module-code
                   (format "get-module-code: no such file: ~e" resolved-path)
                   (current-continuation-marks)
                   #f))]))

(define (get-module-code
          path0
          #:roots [roots (current-compiled-file-roots)]
          #:submodule-path [submodule-path '()]
          #:sub-path [sub-path/kw "compiled"]
          [sub-path sub-path/kw]
          #:compile [compile/kw compile]
          [compiler compile/kw]
          #:extension-handler [ext-handler/kw #f]
          [ext-handler ext-handler/kw] 
          #:choose [choose (lambda (src zo so) #f)]
          #:notify [notify void]
          #:source-reader [read-src-syntax read-syntax]
          #:rkt-try-ss? [rkt-try-ss? #t])
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
