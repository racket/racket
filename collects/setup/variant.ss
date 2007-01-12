
(module variant mzscheme
  (require (prefix config: (lib "config.ss" "config"))
           (lib "dirs.ss" "setup"))

  (provide variant-suffix)

  (define plain-mz-is-cgc?
    (delay (let ([dir (find-console-bin-dir)]
                 [exe (case (system-type)
                        [(windows) "MzScheme.exe"]
                        [else "mzscheme"])])
             (let ([f (build-path dir exe)])
               (and (file-exists? f)
                    (with-input-from-file f
                      (lambda ()
                        (let ([m (regexp-match #rx#"bINARy tYPe:..(.)" (current-input-port))])
                          (and m
                               (equal? (cadr m) #"c"))))))))))

  (define variant-suffix
    (lambda (variant cased?)
      (case variant
        [(3m script-3m) 
         ((if cased? values string-downcase)
          (or (force config:3m-suffix) 
              (if (force plain-mz-is-cgc?)
                  "3m"
                  "")))]
        [(cgc script-cgc)
         ((if cased? values string-downcase)
          (or (force config:cgc-suffix)
              (if (force plain-mz-is-cgc?)
                  ""
                  "CGC")))]))))
