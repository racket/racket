#lang scheme/base

(require htdp/error)

(define (read-file f)
  (check-arg 'read-file (string? f) "string" "first" f)
  (check-arg 'read-file (file-exists? f) "name of file in program's folder" "first" f)
  (list->string
    (with-input-from-file f 
      (lambda ()
	(let loop ([accu '()])
	  (define nxt (read-char))
	  (if (eof-object? nxt)
	      (reverse (if (char=? (car accu) #\newline) (cdr accu) accu))
	      (loop (cons nxt accu))))))))
 
(define (write-file f str)
  (check-arg 'read-file (string? f) "string" "first" f)
  (let ([result (not (file-exists? f))])
    (with-output-to-file f 
      (lambda () (printf "~a" str))
      #:exists 'truncate)
    result))

;; -----------------------------------------------------------------------------

(provide 
 read-file ;; String -> String
 ;; read the file f (in current-directory) as a string
 
 write-file ;; String String -> Boolean
 ;; write str to file f (in current-directory); 
 ;; false, if f exists
 ;; true, if f doesn't exist
 )


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

For basic i/o, I find the following two functions extremely helpful to provide as a teachpack along with what batch-io gives. Perhaps it would be possible to include them in the standard teachpack?

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

These are some other functions that I've also found helpful... the first two because sometimes it's handy to be able to use the GUI dialog box to figure out the complete pathname of a file; the third to be able to load images from a program:

 (define (pick-a-file)
   (path->string (get-file)))

 (define (pick-a-save-file)
   (path->string (put-file)))

 (define (read-image-file file-name)
   (make-object image-snip% file-name))

I realize that it might not be good to provide too much i/o stuff from the HtDP perspective, because it could start to distract from the more important issues that are to be taught/learned.

;; --- 

Why don't you incorporate this into the Teachpack?

#lang scheme

(require (planet neil/csv:1:2/csv))
(require lang/prim)

(provide-higher-order-primitive read-csv-file (_ convert-each-line))

(define (read-csv-file filename mapper)
 (with-input-from-file filename
   (lambda ()
     (csv-map mapper (current-input-port)))))

|#