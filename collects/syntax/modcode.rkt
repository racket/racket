#lang racket/base
  (require racket/contract/base
           racket/list
           "modread.rkt")

  (provide moddep-current-open-input-file
           exn:get-module-code
           exn:get-module-code?
           exn:get-module-code-path
           make-exn:get-module-code)

  (provide/contract
   [get-module-code (->* (path?)
                         (#:roots
                          (listof (or/c path? 'same))
                          #:submodule-path
                          (listof symbol?)
                          #:sub-path 
                          (and/c path-string? relative-path?)
                          (and/c path-string? relative-path?)
                          #:compile (-> any/c any)
                          (-> any/c any)
                          #:extension-handler (or/c false/c (path? boolean? . -> . any))
                          (or/c false/c (path? boolean? . -> . any))
                          #:choose 
                          (path? path? path? . -> . (or/c (symbols 'src 'zo 'so) false/c))
                          #:notify  (any/c . -> . any)
                          #:source-reader (any/c input-port? . -> . (or/c syntax? eof-object?))
                          #:rkt-try-ss? boolean?)
                         any)])

  (define moddep-current-open-input-file
    (make-parameter open-input-file))

  (define (resolve s)
    (if (complete-path? s)
      s
      (let ([d (current-load-relative-directory)])
        (if d (path->complete-path s d) s))))

  (define (date>=? a bm)
    (and a (let ([am (with-handlers ([exn:fail:filesystem? (lambda (x) #f)])
                       (file-or-directory-modify-seconds a))])
             (or (and (not bm) am) (and am bm (>= am bm))))))

  (define (read-one orig-path path src? read-src-syntax)
    (let ([p ((moddep-current-open-input-file) path)])
      (when src? (port-count-lines! p))
      (dynamic-wind
        void
        (lambda ()
          (let ([v (with-module-reading-parameterization
                    (lambda () 
                      ;; In case we're reading a .zo, we need to set
                      ;;  the load-relative directory for unmarshaling
                      ;;  path literals.
                      (parameterize ([current-load-relative-directory
                                      (let-values ([(base name dir?) (split-path orig-path)])
                                        (if (path? base)
                                            base
                                            (current-directory)))])
                        (read-src-syntax path p))))])
            (when (eof-object? v)
              (error 'read-one
                     "empty file; expected a module declaration in: ~a" path))
            (let* ([name (let-values ([(base name dir?) (split-path orig-path)])
                           (string->symbol
                            (bytes->string/utf-8
                             (path->bytes (path-replace-suffix name #""))
                             #\?)))]
                   [v (check-module-form v name path)])
              (unless (eof-object? (read p))
                (error 'read-one
                       "file has more than one expression; expected a module declaration only in: ~a"
                       path))
              (if (and (syntax? v) (compiled-expression? (syntax-e v)))
                (syntax-e v)
                v))))
        (lambda () (close-input-port p)))))

  (define-struct (exn:get-module-code exn:fail) (path))

  (define (get-module-code path
                           #:roots [roots (current-compiled-file-roots)]
                           #:submodule-path [submodule-path '()]
                           #:sub-path [sub-path0 "compiled"]
                           #:compile [compile0 compile]
                           #:extension-handler [ext-handler0 #f]
                           [sub-path sub-path0] [compiler compile0] [extension-handler ext-handler0] 
                           #:choose [choose (lambda (src zo so) #f)]
                           #:notify [notify void]
                           #:source-reader [read-src-syntax read-syntax]
                           #:rkt-try-ss? [rkt-try-ss? #t])
    (let*-values ([(orig-path) (resolve path)]
                  [(base orig-file dir?) (split-path path)]
                  [(main-file alt-file)
                   (if rkt-try-ss?
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
                  [(main-path) (if (eq? main-file orig-file)
                                   orig-path
                                   (build-path base main-file))]
                  [(alt-path) (and alt-file
                                   (if (eq? alt-file orig-file)
                                       orig-path
                                       (build-path base alt-file)))]
                  [(base) (if (eq? base 'relative) 'same base)])
      (define (build-found-path base . args)
        (cond
         [(or (equal? roots '(same)) (null? roots))
          (apply build-path base args)]
         [else
          (let ([reroot-path* (lambda (base root)
                                (cond
                                 [(eq? root 'same) base]
                                 [(relative-path? root) (build-path base root)]
                                 [else (reroot-path base root)]))])
            (or (for/or ([root (in-list (if (null? (cdr roots)) null roots))])
                  (define p (apply build-path (reroot-path* base root) args))
                  (and (file-exists? p) p))
                (apply build-path (reroot-path* base (car roots)) args)))]))
      (let* ([main-path-d (file-or-directory-modify-seconds orig-path #f (lambda () #f))]
             [alt-path-d (and alt-path
                              (not main-path-d)
                              (file-or-directory-modify-seconds alt-path #f (lambda () #f)))]
             [path-d (or main-path-d alt-path-d)]
             [file (if alt-path-d alt-file main-file)]
             [path (if alt-path-d alt-path main-path)]
             [try-alt? (and alt-file (not alt-path-d) (not main-path-d))]
             [get-so (lambda (file)
                       (build-found-path
                        base sub-path "native"
                        (system-library-subpath)
                        (path-add-suffix file (system-type 'so-suffix))))]
             [zo (build-found-path base sub-path (path-add-suffix file #".zo"))]
             [alt-zo (and try-alt?
                          (build-found-path base sub-path (path-add-suffix alt-file #".zo")))]
             [so (get-so file)]
             [alt-so (and try-alt? (get-so alt-file))]
             [with-dir (lambda (t)
                         (parameterize ([current-load-relative-directory
                                         (if (path? base)
                                           base
                                           (current-directory))])
                           (t)))]
             [prefer (choose path zo so)])
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
        (cond
          ;; Use .zo, if it's new enough
          [(or (eq? prefer 'zo)
               (and (not prefer)
                    (pair? roots)
                    (or (date>=? zo path-d)
                        (and try-alt?
                             (date>=? alt-zo path-d)))))
           (let ([zo (if (date>=? zo path-d)
                         zo
                         (if (and try-alt?
                                  (date>=? alt-zo path-d))
                             alt-zo
                             zo))])
             (notify zo)
             (extract-submodule (read-one path zo #f read-syntax)))]
          ;; Maybe there's an .so? Use it only if we don't prefer source
          ;; and only if there's no submodule path.
          [(and (null? submodule-path)
                (or (eq? prefer 'so)
                    (and (not prefer)
                         (pair? roots)
                         (or (date>=? so path-d)
                             (and try-alt?
                                  (date>=? alt-so path-d))))))
           (let ([so (if (date>=? so path-d)
                         so
                         (if (and try-alt?
                                  (date>=? alt-so path-d))
                             alt-so
                             so))])
             (if extension-handler
                 (begin
                   (notify so)
                   (extension-handler so #f))
                 (raise (make-exn:get-module-code
                         (format "get-module-code: cannot use extension file; ~e" so)
                         (current-continuation-marks)
                         so))))]
          ;; Use source if it exists
          [(or (eq? prefer 'src)
               path-d)
           (notify path)
           (define (compile-one)
             (with-dir (lambda () (compiler (read-one orig-path path #t read-src-syntax)))))
           (if (null? submodule-path)
               ;; allow any result:
               (compile-one)
               ;; expect a compiled-module result:
               (extract-submodule (compile-one)))]
          ;; Report a not-there error
          [else (raise (make-exn:get-module-code
                        (format "get-module-code: no such file: ~e" orig-path)
                        (current-continuation-marks)
                        #f))]))))
