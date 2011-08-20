#lang racket

(require (for-syntax syntax/parse)
         srfi/13 htdp/error
         (rename-in lang/prim (first-order->higher-order f2h))
         "private/csv/csv.rkt")

;; todo?
;; -- export tokenization?

;; -----------------------------------------------------------------------------
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
  (check-arg 'write-file (string? f) "string (name of file)" "first" f)
  (check-arg 'write-file (string? str) "string" "second" str)
  (let ([result (not (file-exists? f))])
    (with-output-to-file f 
      (lambda () (printf "~a" str))
      #:mode 'text
      #:exists 'replace)
    f))

;; -----------------------------------------------------------------------------
;; auxiliaries 

;; String [([Listof X] -> Y)] -> [Listof Y]
(define (read-csv-file/func f [row (lambda (x) x)])
  (local ((define (reader o)
            (make-csv-reader o '((strip-leading-whitespace?  . #t)
                                 (strip-trailing-whitespace? . #t)))))
    (map row (call-with-input-file f (compose csv->list reader)))))

;; String (-> X) ([Listof X] -> [Listof X]) -> [Listof X]
;; read a file as a list of X where process-accu is applied to accu when eof
(define (read-chunks f read-chunk process-accu)
  (with-input-from-file f 
    #:mode 'text
    (lambda ()
      (let loop ([accu '()])
        (define nxt (read-chunk))
        (if (eof-object? nxt) (process-accu accu) (loop (cons nxt accu)))))))

;; [Listof Char] -> [Listof Char]
(define (drop-last-newline accu)
  (reverse (if (and (pair? accu) (char=? (car accu) #\newline)) (cdr accu) accu)))

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
