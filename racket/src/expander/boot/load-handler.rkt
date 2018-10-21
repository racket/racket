#lang racket/base
(require '#%paramz
         "../eval/collection.rkt"
         "../syntax/api.rkt"
         "../eval/main.rkt"
         "../eval/dynamic-require.rkt"
         "../eval/parameter.rkt"
         "../host/linklet.rkt"
         "../namespace/namespace.rkt"
         "../namespace/api.rkt"
         "../eval/module-read.rkt"
         "../eval/module-cache.rkt"
         "../eval/reflect.rkt"
         "../read/api.rkt"
         "../read/primitive-parameter.rkt")

(provide default-load-handler)

(define default-load-handler
  (lambda (path expected-mod)
    (unless (path-string? path)
      (raise-argument-error 'default-load-handler "path-string?" path))
    (unless (or (not expected-mod)
                (symbol? expected-mod)
                (and (pair? expected-mod)
                     (list? expected-mod)
                     (or (not (car expected-mod)) (symbol? (car expected-mod)))
                     (andmap symbol? (cdr expected-mod))))
      (raise-argument-error 'default-load-handler
                            "(or/c #f symbol? (cons/c (or/c #f symbol?) (non-empty-listof symbol?)))"
                            expected-mod))
    (define (maybe-count-lines! i)
      (unless (regexp-match? #rx"[.]zo$" path)
        (port-count-lines! i)))
    (cond
     [expected-mod
      ((call-with-input-module-file
        path
        (lambda (i)
          (maybe-count-lines! i)
          (with-module-reading-parameterization+delay-source
              path
              (lambda ()
                (cond
                 [(linklet-directory-start i)
                  => (lambda (pos)
                       ;; Find and load individual submodule
                       (define b-pos (search-directory i pos (encode-symbols expected-mod)))
                       (cond
                        [b-pos
                         (file-position i b-pos)
                         (or (cached-bundle i)
                             (let ([v (read i)])
                               (if (compiled-module-expression? v)
                                   (lambda () ((current-eval) v))
                                   (error 'default-load-handler
                                          (string-append "expected a compiled module\n"
                                                         "  in: ~e\n"
                                                         "  found: ~e")
                                          (object-name i)
                                          v))))]
                        [(and (pair? expected-mod))
                         ;; Cannot load submodule, so do nothing
                         void]
                        [else
                         (error 'default-load-handler
                                (string-append "could not find main module\n"
                                               "  in: ~e")
                                (object-name i))]))]
                 [(and (pair? expected-mod) (not (car expected-mod)))
                  ;; Cannot load submodule independently, so do nothing
                  void]
                 [(cached-bundle i)
                  => (lambda (thunk) thunk)]
                 [else
                  (define s (read-syntax (object-name i) i))
                  (when (eof-object? s)
                    (error 'default-load-handler
                           (string-append "expected a `module' declaration;\n"
                                          " found end-of-file\n"
                                          "  in: ~e")
                           (object-name i)))
                  (define m-s (check-module-form s path))
                  (define s2 (read-syntax (object-name i) i))
                  (unless (eof-object? s2)
                    (error 'default-load-handler
                           (string-append "expected a `module' declaration;\n"
                                          " found an extra form\n"
                                          "  in: ~e\n"
                                          "  found: ~.s")
                           (object-name i)
                           s2))
                  (lambda () ((current-eval) m-s))]))))))]
     [else
      (define (add-top-interaction s)
        (namespace-syntax-introduce
         (datum->syntax #f (cons '#%top-interaction s) s)))
      (call-with-input-file*
       path
       (lambda (i)
         (maybe-count-lines! i)
         (let loop ([vals (list (void))])
           (define s
             (parameterize ([read-accept-compiled #t]
                            [read-accept-reader #t]
                            [read-accept-lang #t])
               (if (load-on-demand-enabled)
                   (parameterize ([read-on-demand-source (path->complete-path path)])
                     (read-syntax (object-name i) i))
                   (read-syntax (object-name i) i))))
           (if (eof-object? s)
               (apply values vals)
               (loop
                (call-with-continuation-prompt
                 (lambda ()
                   (call-with-values (lambda () ((current-eval) (add-top-interaction s))) list))
                 (default-continuation-prompt-tag)
                 (lambda args
                   (apply abort-current-continuation (default-continuation-prompt-tag) args))))))))])))

(define version-bytes (string->bytes/utf-8 (version)))
(define version-length (bytes-length version-bytes))
(define vm-bytes (string->bytes/utf-8 (symbol->string (system-type 'vm))))
(define vm-length (bytes-length vm-bytes))

(define (linklet-bundle-or-directory-start i tag)
  (define version-length (string-length (version)))
  (define vm-length (string-length (symbol->string (system-type 'vm))))
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

(define (cached-bundle i)
  (cond
   [(module-cache-ref (make-module-cache-key (linklet-bundle-hash-code i)))
    => (lambda (declare-module)
         ;; The `declare-module` function has registered in the cace by
         ;; `eval-module` in "eval/module.rkt"; we can call the function
         ;; instead of loading from scratch and `eval`ing;
         ;; FIXME: go though `current-eval`
         (lambda ()
           (declare-module (current-namespace))))]
   [else #f]))

(define (read-number i)
  (define (read-byte/not-eof i)
    (define v (read-byte i))
    (if (eof-object? v) 0 v))
  (bitwise-ior (read-byte/not-eof i)
               (arithmetic-shift (read-byte/not-eof i) 8)
               (arithmetic-shift (read-byte/not-eof i) 16)
               (arithmetic-shift (read-byte/not-eof i) 24)))

(define (search-directory i pos bstr)
  (cond
   [(zero? pos) #f]
   [else
    (file-position i pos)
    (define name-len (read-number i))
    (define v (read-bytes name-len i))
    (unless (and (bytes? v) (= (bytes-length v) name-len))
      (error 'deafult-load-handler
             (string-append "failure getting submodule path\n"
                            "  in: ~e\n"
                            "  at position: ~a\n"
                            "  expected bytes: ~a\n"
                            "  read bytes: ~e")
             (object-name i)
             pos
             name-len
             v))
    (cond
     [(bytes=? bstr v) (read-number i)]
     [(bytes<? bstr v)
      (read-number i)
      (read-number i)
      (search-directory i (read-number i) bstr)]
     [else
      (read-number i)
      (read-number i)
      (read-number i)
      (search-directory i (read-number i) bstr)])]))

(define (encode-symbols expected-mod)
  (cond
   [(symbol? expected-mod) #""]
   [else
    (apply
     bytes-append
     (for/list ([s (in-list (cdr expected-mod))])
       (define bstr (string->bytes/utf-8 (symbol->string s)))
       (define len (bytes-length bstr))
       (cond
        [(len . < . 255) (bytes-append (bytes len) bstr)]
        [else (bytes-append 255 (integer->integer-bytes len 4 #f #f) bstr)])))]))

(define (with-module-reading-parameterization+delay-source path thunk)
  (if (load-on-demand-enabled)
      (parameterize ([read-on-demand-source (path->complete-path path)])
        (with-module-reading-parameterization thunk))
      (with-module-reading-parameterization thunk)))

(define (call-with-input-module-file path proc)
  (define i #f)
  (dynamic-wind
   (lambda () (set! i (open-input-file path #:for-module? #t)))
   (lambda () (proc i))
   (lambda () (close-input-port i))))
