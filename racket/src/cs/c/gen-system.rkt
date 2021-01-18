(module gen-system '#%kernel
  
  ;; Command-line argument: <dest-file> <target-machine> <cross-target-machine> <srcdir> <slsp-suffix>

  (define-values (machine) (string->symbol (vector-ref (current-command-line-arguments) 1)))
  (define-values (srcdir) (vector-ref (current-command-line-arguments) 3))
  (define-values (slsp-suffix) (vector-ref (current-command-line-arguments) 4))

  (define-values (definitions)
    (call-with-input-file
     (build-path srcdir 'up "rumble" "system.ss")
     (lambda (i)
       (letrec-values ([(loop)
                        (lambda ()
                          (let-values ([(v) (read i)])
                            (if (eof-object? v)
                                null
                                (cons v (loop)))))])
         (loop)))))

  (define-values (lookup)
    (lambda (key)
      (letrec-values ([(loop)
                       (lambda (l)
                         (if (null? l)
                             (error 'lookup "could not find ~e" key)
                             (let-values ([(a) (car l)])
                               (if (eq? 'define (car a))
                                   (if (eq? key (cadr a))
                                       (parse-cond (caddr a))
                                       (loop (cdr l)))
                                   (loop (cdr l))))))])
        (loop definitions))))

  (define-values (parse-cond)
    (lambda (e)
      (if (matches? e '(case (machine-type) . _))
          (letrec-values ([(loop)
                           (lambda (l)
                             (if (null? l)
                                 (error 'parse-cond "no match")
                                 (let-values ([(a) (car l)])
                                   (if (matches? a '[else _])
                                       (parse-expr (cadr a))
                                       (if (matches? a '[_ _])
                                           (if (memq machine (car a))
                                               (parse-expr (cadr a))
                                               (loop (cdr l)))
                                           (loop (cdr l)))))))])
            (loop (cddr e)))
          (error 'parse-cond "could not parse ~e" e))))

  (define-values (parse-expr)
    (lambda (e)
      (if (matches? e '(quote _))
          (cadr e)
          (if (matches? e '(string->utf8 _))
              (string->bytes/utf-8 (cadr e))
              (if (matches? e '(if unix-style-macos? _ _))
                  (if (eq? (system-type) 'macosx)
                      (parse-expr (cadddr e))
                      (parse-expr (caddr e)))
                  (if (matches? e '(if unix-link-shared? _ _))
                      ;; Currently assuming shared-library mode is not a cross compile:
                      (if (eq? (system-type 'link) 'shared)
                          (parse-expr (caddr e))
                          (parse-expr (cadddr e)))
                      (error 'parse-expr "could not parse ~e" e)))))))

  (define-values (matches?)
    (lambda (e pat)
      (if (eq? pat '_)
          #t
          (if (pair? pat)
              (if (pair? e)
                  (if (matches? (car e) (car pat))
                      (matches? (cdr e) (cdr pat))
                      #f)
                  #f)
              (equal? e pat)))))

  (define-values (memq)
    (lambda (a l)
      (if (null? l)
          #f
          (if (eq? a (car l))
              #t
              (memq a (cdr l))))))

  (define-values (os) (lookup 'os-symbol))
  (define-values (os*) (lookup 'os*-symbol))
  (define-values (arch) (lookup 'arch-symbol))
  (define-values (link) (lookup 'link-symbol))
  (define-values (so-suffix) (lookup 'so-suffix-bytes))
  (define-values (so-mode) (lookup 'so-mode))

  (define-values (lib-subpath)
    (string-append
     (if (eq? machine 'ta6nt)
         "win32\\x86_64"
         (if (eq? machine 'a6nt)
             "win32\\x86_64"
             (if (eq? machine 'ti3nt)
                 "win32\\i386"
                 (if (eq? machine 'ti3nt)
                     "win32\\i386"
                     (format "~a-~a" arch os*)))))
     slsp-suffix))

  (define-values (ht)
    (hash 'os os
          'os* os*
          'arch arch
          'word (if (eq? arch 'i386)
                    32
                    (if (eq? arch 'arm)
                        32
                        (if (eq? arch 'ppc)
                            32
                            64)))
          'gc 'cs
          'vm 'chez-scheme
          'link link
          'machine lib-subpath
          'library-subpath (string->bytes/utf-8 lib-subpath)
          'library-subpath-convention (if (eq? os 'windows) 'windows 'unix)
          'so-suffix so-suffix
          'so-mode so-mode
          'fs-change (if (eq? os 'windows)
                         '#(supported scalable low-latency #f)
                         ;; Warning: not necessarily right for cross compilation:
                         (system-type 'fs-change))
          'target-machine (if (equal? "any" (vector-ref (current-command-line-arguments) 2))
                              #f
                              machine)))

  (call-with-output-file
   (vector-ref (current-command-line-arguments) 0)
   (lambda (o)
     (write ht o)
     (newline o))
   'truncate/replace))
