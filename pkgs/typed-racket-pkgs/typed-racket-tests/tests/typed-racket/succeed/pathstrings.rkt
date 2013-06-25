#lang typed/racket

(: no-exec (-> Void))
(define (no-exec)


 (call-with-output-file "file.tmp"
  (lambda: ((port : Output-Port))
   (write "hello world" port)))
 (call-with-input-file "file.tmp"
  (lambda: ((port : Input-Port))
   (read port)))

 (directory-list "/")
 (make-directory "tmp-dir")

 (path-only "file.tmp")

 (system #"echo foo")
 (system* "/bin/echo" "zzz" #"foo" (string->path "/"))
 (system/exit-code #"echo foo")
 (system*/exit-code "/bin/echo" "zzz" #"foo" (string->path "/"))
 (system #:set-pwd? #t #"echo foo")
 (system* #:set-pwd? #t "/bin/echo" "zzz" #"foo" (string->path "/"))
 (system/exit-code #:set-pwd? #t #"echo foo")
 (system*/exit-code #:set-pwd? #t "/bin/echo" "zzz" #"foo" (string->path "/"))

 (void))

