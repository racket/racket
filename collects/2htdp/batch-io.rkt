#lang racket/base

(require racket/function
         racket/file
         racket/format
         racket/string
         racket/local
         (for-syntax racket/base
                     syntax/parse)
         htdp/error
         (rename-in lang/prim (first-order->higher-order f2h))
         "private/csv/csv.rkt")

;; todo?
;; -- export tokenization?

;; I am tryiing to use these lists to automate the documentation of the
;; functions but my scribble skills are insufficient and my time is running
;; out. 
(module devices racket/base
  (provide *input-devices* *output-devices*)
  (define *input-devices*  `((stdin ,current-input-port)   (standard-in ,current-input-port)))
  (define *output-devices* `((stdout ,current-output-port) (standard-out ,current-output-port))))

(require (submod "." devices))

;;---------------------------------------------------------------------------------------------------
(provide simulate-file) ;; syntax (simulate-file reader string ...)

(provide
 ;; all reader functions consume the name of a file f:
 ;; -- f must be a file name (string) in the same folder as the program 
 
 read-file ;; String -> String
 ;; read the specified file as a string
 
 read-1strings ;; String -> [Listof 1String]
 ;; read the specified file as a list of 1strings (characters)
 
 read-lines ;; String -> [Listof String]
 ;; read the specified file as a list of strings, one per line
 
 read-words ;; String -> [Listof String]
 ;; read the specified file as a list of white-space separated tokens
 
 read-words/line ;; String -> [Listof [Listof String]]
 ;; read the specified file as a list of lines, each line as a list of words
 
 read-words-and-numbers/line ;; String -> [Listof [Listof (Union Number String)]]
 ;; read the specified file as a list of lines, each line as a list of words and numbers 
 
 read-csv-file ;; String -> [Listof [Listof (U Any)]]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the specified file as a list of lists---one per line---of values (Any)
 
 read-csv-file/rows ;; String ([Listof Any] -> X) -> [Listof X]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the specified file as a file of comma-separated values, apply the second
 ;; argument to each row, i.e., list of CSV on one line 
 
 write-file ;; String String -> String 
 ;; (write-file filename str) writes str to filename; 
 ;; produces the file name as a confirmation that the write succeeded 
 
 ;; *input-devives*: symbols that redirect input from an input-port 
 ;; *output-devives*: symbols that redirect output from a output-port 
 )      

;; -----------------------------------------------------------------------------

(define-syntax-rule
  (def-reader (name f s ...) body ...)
  (define (name f s ...)
    (check-input-file f 'name)
    (let ()
      body ...)))

;; --- exported functions -- 

(def-reader (read-file f)
  (list->string (read-chunks f read-char drop-last-newline)))

(def-reader (read-1strings f)
  (map string (read-chunks f read-char drop-last-newline)))

(def-reader (read-lines f)
  (read-chunks f *read-line reverse))

(def-reader (read-words f)
  (read-words/line/internal f append))

(def-reader (read-words/line f)
  ;; String -> [Listof [Listof String]]
  ;; read the specified file as a list of lines, each line as a list of words
  (read-words/line/internal f cons))

(def-reader (read-words-and-numbers/line f)
  ;; String -> [Listof [Listof (U String Number)]]
  ;; read the specified file as a list of lines, each line as a list of words and numbers
  (read-words/line/internal f (lambda (line1 r)
                                (cons (for/list ((t (in-list line1))) (or (string->number t) t)) r))))

(define (read-words/line/internal f combine)
  (define lines (read-chunks f *read-line (lambda (x) x)))
  (foldl (lambda (f r)
           (define fst (filter (compose not (curry string=? "")) (split f)))
           (combine fst r))
         '() lines))

(def-reader (read-csv-file f)
  (read-csv-file/func f))

(def-reader (read-csv-file/rows f row)
  (check-proc 'read-csv-file row 1 "one argument" "row")
  (read-csv-file/func f row))

(define (*read-line)
  (read-line (current-input-port) 'any))

;; -----------------------------------------------------------------------------
;; tester 

(define-syntax (simulate-file stx)
  (syntax-case stx ()
    [(simulate-file) 
     (raise-syntax-error #f "expects at least one sub-expression" stx)]
    [(simulate-file reader str ...) #'(simulate-file/proc (f2h reader) str ...)]))
     
(define (simulate-file/proc reader . los)
  (define _1 (check-proc "simulate-file" reader 1 "reader" "one argument"))
  (define _2 
    (andmap 
     (lambda (f)
       (check-arg "simulate-file" (string? f) "sequence of strings" "" f))
     los))
  (define t (make-temporary-file "drracket-temporary-file-~a"))
  (dynamic-wind 
   (lambda ()
     (with-output-to-file t 
       (lambda () (for-each displayln los))
       #:mode 'text
       #:exists 'replace))
   (lambda () 
     (reader (path->string t)))
   (lambda ()
     (delete-file t))))

;; -----------------------------------------------------------------------------
;; writer 

(define (write-file f str)
  (check-output-file f 'write-file)
  (check-arg 'write-file (string? str) "string" "second" str)
  (define (wt) (printf "~a" str))
  (define device (assq f *output-devices*))
  (if device 
      (parameterize ((current-output-port [(cadr device)])) (wt))
      (with-output-to-file f wt #:mode 'text #:exists 'replace))
  f)

;; -----------------------------------------------------------------------------
;; auxiliaries 

;; String [([Listof X] -> Y)] -> [Listof Y]

;; String (-> X) ([Listof X] -> [Listof X]) -> [Listof X]
;; read a file as a list of X where process-accu is applied to accu when eof
(define (read-chunks f read-chunk process-accu) 
  (define (rd)
    (let loop ([accu '()])
        (define nxt (read-chunk))
        (if (eof-object? nxt) (process-accu accu) (loop (cons nxt accu)))))
  (define device (assq f *input-devices*))
  (if device 
      (parameterize ((current-input-port [(cadr device)])) (rd))
      (with-input-from-file f #:mode 'text rd)))

(define (read-csv-file/func f [row (lambda (x) x)])
  (define (reader o)
    (csv->list
     (make-csv-reader o '((strip-leading-whitespace?  . #t)
                          (strip-trailing-whitespace? . #t)))))
  (define device (assq f *input-devices*))
  (map row
       (if device 
           (reader [(cadr device)])
           (call-with-input-file f #:mode 'text reader))))

;; [Listof Char] -> [Listof Char]
(define (drop-last-newline accu)
  (reverse (if (and (pair? accu) (char=? (car accu) #\newline)) (cdr accu) accu)))

;; effect: ensure that f is a file in current directory or report error for t
(define (check-input-file f t)
  (define d? (assq f *input-devices*))
  (check-arg t (or (string? f) d?) (error-message (map car *input-devices*)) "first" f)
  (check-arg t (or d? (file-exists? f)) "name of file in program's folder" "first" f))

;; effect: ensure that f is a file in current directory or report error for t
(define (check-output-file f t)
  (define d? (assq f *output-devices*))
  (check-arg t (or (string? f) d?) (error-message (map car *output-devices*)) "first" f))

;; [Listof Symbol] -> String 
(define (error-message los)
  (string-append "string or one of: " (string-join (map ~e los) ", ")))

;; split : String [Regexp] -> [Listof String]
;; splits a string into a list of substrings using the given delimiter
;; (white space by default)
;;ELI: This shouldn't be needed now, it can use `string-split' as is
;; (also, the trimming doesn't make sense if the pattern is not a
;; space--?)
(define (split str [ptn #rx"[ ]+"])
  (regexp-split ptn (string-trim str)))

;; split-lines : String -> Listof[String]
;; splits a string with newlines into a list of lines
(define (split-lines str)
  (map string-trim (split str "\r*\n")))
