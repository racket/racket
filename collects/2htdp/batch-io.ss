#lang scheme

(require (for-syntax syntax/parse) srfi/13 htdp/error "private/csv/csv.ss")

;; todo?
;; -- export tokenization? 

;; -----------------------------------------------------------------------------
(provide
 ;; all reader functions consume the name of a file f:
 ;; -- f must be a file name (string) in the same folder as the program 

 read-file ;; String -> String
 ;; read the file f as a string
 
 read-file-as-lines ;; String -> [Listof String]
 ;; read the file f as a list of strings, one per line 
 
 read-file-as-words ;; String -> [Listof String]
 ;; read the file f as a list of white-space separated tokens
 
 read-file-as-1strings ;; String -> [Listof 1String]
 ;; read the file f as a list of 1strings (characters)

 read-file-as-csv ;; String -> [Listof [Listof (U Any)]]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the file f as a list of lists---one per line---of values (Any)
 
 read-file-as-csv/rows ;; String ([Listof Any] -> X) -> [Listof X]
 ;; -- f must be formated as a a file with comma-separated values (Any)
 ;; read the file f as a file of comma-separated values, apply the second
 ;; argument to each row, i.e., list of CSV on one line 

 write-file ;; String String -> Boolean
 ;; write the second argument to file f in the same folder as the program
 ;; produce false, if f exists
 ;; produce true, if f doesn't exist
 )      

;; -----------------------------------------------------------------------------

(define-syntax-rule
  (def-reader (name f s ...) body ...)
  (define (name f s ...)
    (check-file f 'name)
    (let ()
      body ...)))

;; --- exported functions -- 

(def-reader (read-file f)
  (list->string (read-chunks f read-char drop-last-newline)))

(def-reader (read-file-as-1strings f)
  (map string (read-chunks f read-char drop-last-newline)))

(def-reader (read-file-as-lines f)
  (read-chunks f read-line reverse))

(def-reader (read-file-as-words f)
  (define lines (read-chunks f read-line reverse))
  (foldr (lambda (f r) (append (split f) r)) '() lines))

(def-reader (read-file-as-csv f)
  (read-file-as-csv/func f))

(def-reader (read-file-as-csv/rows f row)
  (check-proc 'read-file-as-cvs row 1 "one argument" "row")
  (read-file-as-csv/func f row))

;; -----------------------------------------------------------------------------
;; writer 

(define (write-file f str)
  (check-arg 'write-file (string? f) "string (name of file)" "first" f)
  (check-arg 'write-file (string? str) "string" "second" str)
  (let ([result (not (file-exists? f))])
    (with-output-to-file f 
      (lambda () (printf "~a" str))
      #:exists 'truncate)
    result))

;; -----------------------------------------------------------------------------
;; auxiliaries 

;; String [([Listof X] -> Y)] -> [Listof Y]
(define (read-file-as-csv/func f [row (lambda (x) x)])
  (local ((define (reader o)
            (make-csv-reader o '((strip-leading-whitespace?  . #t)
                                 (strip-trailing-whitespace? . #t)))))
    (map row (call-with-input-file f (compose csv->list reader)))))

;; String (-> X) ([Listof X] -> [Listof X]) -> [Listof X]
;; read a file as a list of X where process-accu is applied to accu when eof
(define (read-chunks f read-chunk process-accu)
  (with-input-from-file f 
    (lambda ()
      (let loop ([accu '()])
        (define nxt (read-chunk))
        (if (eof-object? nxt) (process-accu accu) (loop (cons nxt accu)))))))

;; [Listof Char] -> [Listof Char]
(define (drop-last-newline accu)
  (reverse (if (char=? (car accu) #\newline) (cdr accu) accu)))

;; String[file name] Symbol -> Void
;; effect: ensure that f is a file in current directory or report error for t
(define (check-file f t)
  (check-arg t (string? f) "string" "first" f)
  (check-arg t (file-exists? f) "name of file in program's folder" "first" f))

;; split : String [Regexp] -> [Listof String]
;; splits a string into a list of substrings using the given delimiter
;; (white space by default)
(define (split str [ptn #rx"[ ]+"])
  (regexp-split ptn (string-trim-both str)))

;; split-lines : String -> Listof[String]
;; splits a string with newlines into a list of lines
(define (split-lines str)
  (map string-trim-both (split str "\r*\n")))
