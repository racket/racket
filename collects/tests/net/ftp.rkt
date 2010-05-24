#lang racket
(require net/ftp tests/eli-tester)

(define server "ftp.gnu.org")
(define port 21)
(define user "anonymous")
(define passwd "nonny")

(provide tests)
(define (tests)
  (define conn #f)
  (define pth "=README-about-.diff-files")
  (define tmp-dir (make-temporary-file "ftp~a" 'directory))
  (test (ftp-connection? 1) => #f
        (set! conn (ftp-establish-connection server port user passwd))
        (ftp-connection? conn)
        (ftp-cd conn "gnu")
        (for ([f (in-list (ftp-directory-list conn))])
          (match-define (list type ftp-date name) f)
          (test
           (ftp-make-file-seconds ftp-date)))
        
        (ftp-download-file conn tmp-dir pth)
        (delete-file (build-path tmp-dir pth))
        (delete-directory/files tmp-dir)
        
        (ftp-close-connection conn)))

(tests)