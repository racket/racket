(module |random-Xs-v3| mzscheme
  
  (define seed-list (call-with-input-file "rand.txt"
                      (lambda (file)
                        (let loop ([num (read file)])
                          (if (eof-object? num) '()
                              (loop (read file))))))))