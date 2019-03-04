(module gen-system '#%kernel
  
  ;; Command-line argument: <dest-file> <target-machine> <cross-target-machine>

  ;; This file includes various inferences for cross-compilation, so it has
  ;; to be updated for new cross-compilation targets.

  (define-values (machine) (string->symbol (vector-ref (current-command-line-arguments) 1)))

  (define-values (machine-lookup)
    (lambda (l default)
      (if (null? l)
          default
          (if (eq? (caar l) machine)
              (cdar l)
              (machine-lookup (cdr l) default)))))

  ;; Check for cross-compile to Windows:
  (define-values (windows?)  (machine-lookup '((ta6nt . #t)
                                               (a6nt . #t)
                                               (ti3nt . #t)
                                               (i3nt . #t))
                                             #f))

  (define-values (lib-subpath)
    (machine-lookup '((ta6nt . "win32\\x86_64")
                      (a6nt . "win32\\x86_64")
                      (ti3nt . "win32\\i386")
                      (i3nt . "win32\\i386"))
                    (bytes->string/utf-8 (path->bytes (system-library-subpath #f)))))

  (define-values (ht)
    (hash 'os (if windows? 'windows (system-type 'os))
          'word (machine-lookup '((ta6nt . 64)
                                  (a6nt . 64)
                                  (ti3nt . 32)
                                  (i3nt . 32))
                                (system-type 'word))
          'gc 'cs
          'vm 'chez-scheme
          'link 'static
          'machine lib-subpath
          'library-subpath (string->bytes/utf-8 lib-subpath)
          'library-subpath-convention (if windows? 'windows 'unix)
          'so-suffix (if windows? #".dll" (system-type 'so-suffix))
          'so-mode 'local
          'fs-change '#(#f #f #f #f)
          'target-machine (if (equal? "any" (vector-ref (current-command-line-arguments) 2))
                              #f
                              machine)))

  (call-with-output-file
   (vector-ref (current-command-line-arguments) 0)
   (lambda (o)
     (write ht o)
     (newline o))
   'truncate/replace))
