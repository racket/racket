(module setup-launch mzscheme
  (require (lib "cmdline.ss")
           (lib "file.ss")
           (lib "struct.ss"))
  (require "configuration.ss"
           "configuration-table-structs.ss"
           "util.ss"
           "configuration-util.ss")
  
  (parse-command-line
   "web-server-setup"
   (current-command-line-arguments)
   `((once-each
      [("-p" "--port")
       ,(lambda (flag port)
          (let ([p (string->number port)])
            (if (valid-port? p)
                (cons 'port p)
                (error 'web-server-setup "port expects an argument of type <exact integer in [1, 65535]>; given ~s" port))))
       ("Use an alternate network port." "port")]
      [("-d" "--destination")
       ,(lambda (flag destination)
          (let ([p (normalize-path (string->path destination))])
            (cons 'destination p)))
       ("Use an destination directory other than the current directory" "directory")]))
   (lambda (flags)
     (let ([port (extract-flag 'port flags 8080)]
           [dest (extract-flag 'destination flags (current-directory))])
       ;; Create dest
       (make-directory* dest)
       ;; Copy default-web-root into dest/default-web-root
       (copy-directory/files (build-path (collection-path "web-server") "default-web-root")
                             (build-path dest "default-web-root"))
       ;; Read default configuration-table, changing the port
       ;; Write configuration-table into dest/configuration-table
       (write-configuration-table
        (copy-struct configuration-table 
                     (get-configuration default-configuration-table-path)
                     [configuration-table-port port])
        (build-path dest "configuration-table"))))
   '()))
