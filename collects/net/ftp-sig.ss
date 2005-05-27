(module ftp-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide net:ftp^)
  
  (define-signature net:ftp^
    (ftp-cd 
     ftp-establish-connection ftp-establish-connection*
     ftp-close-connection 
     ftp-directory-list
     ftp-download-file
     ftp-make-file-seconds)))

