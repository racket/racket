(module winsubsys mzscheme
  (provide set-subsystem)
  
  (define DF_NewHeaderOffset #x3C)
  (define DF_SubsystemOffset #x5C)
  
  (define (set-subsystem file subsys)
    (let-values ([(in out) (open-input-output-file file 'update)])
      (file-position in DF_NewHeaderOffset)
      (let ([offset (integer-bytes->integer (read-bytes 4 in) #f #f)])
        (file-position out (+ offset DF_SubsystemOffset))
        (write-bytes (integer->integer-bytes
                      (case subsys
                        [(windows) 2]
                        [(console) 3])
                      4 #f #f)
                     out)
        (close-input-port in)
        (close-output-port out)))))
