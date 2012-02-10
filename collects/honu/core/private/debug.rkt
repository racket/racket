#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide debug)

(define-for-syntax (filename path)
  (if path
    (let ()
      (define-values (base name dir?)
                     (split-path (build-path path)))
      name)
    "no source"))

(define (colorize string color)
  (define colors (hash 'none "0"
                       'black "0;30"
                       'red "0;31"
                       'green "0;32"
                       'brown "0;33"
                       'blue "0;34"
                       'purple "0;35"
                       'cyan "0;36"
                       'light-gray "0;37"
                       'dark-gray "1:30"
                       'light-red "1;31"
                       'light-green "1;32"
                       'yellow "1;33"
                       'light-blue "1;34"
                       'light-purple "1;35"
                       'light-cyan "1;36"
                       'white "1;37"))
  (define (get-color color)
    (hash-ref colors color (lambda () "0")))
  (format "\033[~am~a\033[0m" (get-color color) string))

(define-for-syntax verbose? (getenv "HONU_DEBUG"))
(define-syntax (debug stx)
  (if verbose?
    (syntax-parse stx
      [(_ str:str x ...)
       (with-syntax ([file (filename (syntax-source #'str))]
                     [line (syntax-line #'str)]
                     [column (syntax-column #'str)])
         #'(printf (string-append "~a at ~a:~a " str)
                   (colorize file 'green)
                   (colorize line 'red)
                   (colorize column 'red)
                   x ...))]
      [(_ level:number message:str x ...)
       (if (>= (string->number verbose?)
               (syntax->datum #'level))
         #'(debug message x ...)
         #'(void))])
    #'(void)))

