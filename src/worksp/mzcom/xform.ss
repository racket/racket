(use-compiled-file-paths null)

(require (lib "restart.ss"))

(define cpp-flags "/D _CRT_SECURE_NO_DEPRECATE /D WIN32")
(define includes "/I ../../mzscheme/include /I . /I .. /I ../../mzcom")

(define (xform src dest)
  (parameterize ([use-compiled-file-paths (list "compiled")])
    (restart-mzscheme #() (lambda (x) x)
                      (list->vector 
                       (append
                        (list "-r"
                              "../../mzscheme/gc2/xform.ss"
                              "--setup"
                              "--indirect"
                              "--depends")
                        (list
                         "--cpp"
                         (format "cl.exe /MT /E ~a ~a" 
                                 cpp-flags 
                                 includes)
                         "-o"
                         dest
                         src)))
                      void)))

(xform "../../mzcom/mzobj.cxx" "../../mzcom/mzobj3m.cxx")
