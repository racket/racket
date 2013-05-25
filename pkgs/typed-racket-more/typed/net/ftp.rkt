#lang typed/racket/base

(require typed/private/utils)

(require/opaque-type FTP-Connection ftp-connection? net/ftp)

(require/typed/provide net/ftp
  [ftp-cd (FTP-Connection String -> Void)]
  [ftp-establish-connection (String Number String String -> FTP-Connection)]
  [ftp-close-connection (FTP-Connection -> Void)]
  [ftp-directory-list (FTP-Connection -> (Listof (List (U "-" "d" "l") String String)))]
  [ftp-download-file (FTP-Connection Path String -> Void)]
  [ftp-make-file-seconds (String -> Number)])

(provide ftp-connection? FTP-Connection)

