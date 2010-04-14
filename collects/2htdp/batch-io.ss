#lang scheme

(require (for-syntax syntax/parse)
         htdp/error
         "private/csv/csv.ss")

;; todo
;; -- read files as "white space separated tokens"
;; -- tokenization? how? map-file? on a string? 

(provide 
 read-file ;; String -> String
 ;; read the file f (in current-directory) as a string
 
 read-file-as-lines ;; String -> [Listof String]
 ;; read the file f (in current-directory) as a list of strings
 
 read-file-as-1strings ;; String -> [Listof 1String]
 ;; read the file f (in current-directory) as a list of 1strings
 
 write-file ;; String String -> Boolean
 ;; write str to file f (in current-directory); 
 ;; false, if f exists
 ;; true, if f doesn't exist
 )

(provide 
 read-file-as-csv turn-row-into
 ;; (read-file-as-csv f:expr)
 ;; f must evaluate to the name of a file name (string) in the program's folder
 ;; read the file f as a file of comma-separated values; i.e., 
 ;; a list of rows, where each row is a list of strings, numbers, etc. 
 
 ;; (read-file-as-csv f:expr (turn-row-into row:expr))
 ;; row must evaluate to a function of one argument; the function is applied 
 ;; to each row, meaning the result is now a list of results produces by row
 )      

;; -----------------------------------------------------------------------------

(define (read-file f)
  (check-file f 'read-file)
  (list->string (read-chunks f read-char drop-last-newline)))

(define (read-file-as-1strings f)
  (check-file f 'read-file-as-1strings)
  (map string (read-chunks f read-char drop-last-newline)))

(define (read-file-as-lines f)
  (check-file f 'read-file-as-lines)
  (read-chunks f read-line reverse))

(define-syntax (turn-row-into stx)
  (raise-syntax-error 'turn-row-into "used out of context" stx))

(define-syntax (read-file-as-csv stx)
  (syntax-parse stx #:literals (turn-row-into)
    [(_ f:expr) #'(read-file-as-csv/func f (lambda (x) x))]
    [(_ f:expr (turn-row-into s:expr))
     #'(let ([row s]
             [error->syntax-error 
              (lambda (x)
                (raise-syntax-error 'turn-row-into (exn-message x) #'s))])
         (check-file f 'read-file-as-csv)
         (with-handlers ((exn? error->syntax-error))
           (check-proc 'read-file-as-cvs row 1 "one argument" "turn-row-into"))
         (read-file-as-csv/func f s))]))

;; String [([Listof X] -> Y)] -> [Listof Y]
(define (read-file-as-csv/func f row)  
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

;; -----------------------------------------------------------------------------

(define (write-file f str)
  (check-arg 'write-file (string? f) "name of file (string) in program's folder" "first" f)
  (check-arg 'write-file (string? str) "string" "second" str)
  (let ([result (not (file-exists? f))])
    (with-output-to-file f 
      (lambda () (printf "~a" str))
      #:exists 'truncate)
    result))

;; -----------------------------------------------------------------------------

;; String[file name] Symbol -> Void
;; effect: ensure that f is a file in current directory or report error for t
(define (check-file f t)
  (check-arg t (string? f) "string" "first" f)
  (check-arg t (file-exists? f) "name of file in program's folder" "first" f))

;; -----------------------------------------------------------------------------

;                                                                               
;                                                                               
;                                                                               
;                                                      ;                        
;                                              ;                                
;    ;;;;  ;   ;   ;; ;   ;; ;   ;;;    ;;;;  ;;;;;  ;;;     ;;;   ; ;;    ;;;; 
;   ;      ;   ;  ;  ;;  ;  ;;  ;   ;  ;       ;       ;    ;   ;  ;;  ;  ;     
;   ;      ;   ;  ;   ;  ;   ;  ;   ;  ;       ;       ;    ;   ;  ;   ;  ;     
;    ;;;   ;   ;  ;   ;  ;   ;  ;;;;;   ;;;    ;       ;    ;   ;  ;   ;   ;;;  
;       ;  ;   ;  ;   ;  ;   ;  ;          ;   ;       ;    ;   ;  ;   ;      ; 
;       ;  ;  ;;  ;  ;;  ;  ;;  ;          ;   ;       ;    ;   ;  ;   ;      ; 
;   ;;;;    ;; ;   ;; ;   ;; ;   ;;;;  ;;;;     ;;;    ;     ;;;   ;   ;  ;;;;  
;                     ;      ;                                                  
;                  ;;;    ;;;                                                   
;                                                                               

#|

For basic i/o, I find the following two functions extremely helpful to provide as a 
  teachpack along with what batch-io gives. Perhaps it would be possible to include 
  them in the standard teachpack?

 ;; split : String String -> Listof[String]
 ;; splits a string into a list of substrings using the given delimiter (space
 ;;   by default)
 (define (split str [ptn "[ ]+"])
   (unless (string? str)
     (error 'split "expects string as first argument"))
   (unless (string? ptn)
     (error 'split "expects string as second argument"))
   (regexp-split ptn (string-trim-both str)))

 ;; split : String -> Listof[String]
 ;; splits a string into a list of lines
 (define (split-lines str)
   (map string-trim-both (split str "\r*\n")))

These are some other functions that I've also found helpful... the first two
 because sometimes it's handy to be able to use the GUI dialog box to figure 
 out the complete pathname of a file; the third to be able to load images from 
 a program:

 (define (pick-a-file)
   (path->string (get-file)))

 (define (pick-a-save-file)
   (path->string (put-file)))

I realize that it might not be good to provide too much i/o stuff from the HtDP 
 perspective, because it could start to distract from the more important issues 
 that are to be taught/learned.

;; --- 


|#