
;; Direct access to fopen, fread, fwrite, and fclose. The interface is
;;  not remotely safe, since #f is accepted (and converted to NULL)
;;  for `(pointer "FILE")' arguments. Also, blocking reads or writes
;;  will block all Racket threads.

(c-declare "#include <stdio.h>")

(define fopen
  (c-lambda (char-string char-string) (pointer "FILE") "fopen"))
(define fread
  (c-lambda (char-string long long (pointer "FILE")) long "fread"))
(define fwrite
  (c-lambda (char-string long long (pointer "FILE")) long "fwrite"))
(define fclose
  (c-lambda ((pointer "FILE")) int "fclose"))
