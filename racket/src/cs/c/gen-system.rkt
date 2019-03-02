(module gen-system '#%kernel
  
  ;; Command-line argument: <dest-file> <target-machine> <cross-target-machine>

  (define-values (machine) (string->symbol (vector-ref (current-command-line-arguments) 1)))

  ;; Check for cross-compile to Windows:
  (define-values (windows?)  (if (eq? machine 'ta6nt)
                                 #t
                                 (if (eq? machine 'ti3nt)
                                     #t
                                     (if (eq? machine 'a6nt)
                                         #t
                                         (if (eq? machine 'i3nt)
                                             #t
                                             #f)))))

  (define-values (ht)
    (hash 'os (if windows? 'windows (system-type 'os))
          'word (if (eq? machine 'ta6nt)
                    64
                    (if (eq? machine 'a6nt)
                        64
                        (if (eq? machine 'ti3nt)
                            32
                            (if (eq? machine 'i3nt)
                                32
                                (system-type 'word)))))
          'gc 'cs
          'vm 'chez-scheme
          'link 'static
          'machine (if (eq? machine 'ta6nt)
                       "win32\\x86_64"
                       (if (eq? machine 'a6nt)
                           "win32\\x86_64"
                           (if (eq? machine 'ti3nt)
                               "win32\\i386"
                               (if (eq? machine 'i3nt)
                                   "win32\\i386"
                                   (bytes->string/utf-8 (path->bytes (system-library-subpath #f)))))))
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
