#lang scheme

(require
 (rename-in srfi/13
            (reverse-list->string srfi13:reverse-list->string)
            (string->list srfi13:string->list)
            (string-concatenate srfi13:string-concatenate)
            (string-concatenate/shared srfi13:string-concatenate/shared)
            (string-concatenate-reverse srfi13:string-concatenate-reverse)
            (string-concatenate-reverse/shared srfi13:string-concatenate-reverse/shared)
            (string-join srfi13:string-join)
            (string-tokenize srfi13:string-tokenize))
 (only-in rnrs/base-6
          list->string)
 scheme/mpair)

(provide
    string-map string-map!
    string-fold       string-unfold
    string-fold-right string-unfold-right 
    string-tabulate string-for-each string-for-each-index
    string-every string-any
    string-hash string-hash-ci
    string-compare string-compare-ci
    string=    string<    string>    string<=    string>=    string<>
    string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<> 
    string-downcase  string-upcase  string-titlecase  
    string-downcase! string-upcase! string-titlecase! 
    string-take string-take-right
    string-drop string-drop-right
    string-pad string-pad-right
    string-trim string-trim-right string-trim-both
    string-filter string-delete
    string-index string-index-right 
    string-skip  string-skip-right
    string-count
    string-prefix-length string-prefix-length-ci
    string-suffix-length string-suffix-length-ci
    string-prefix? string-prefix-ci?
    string-suffix? string-suffix-ci?
    string-contains string-contains-ci
    string-copy! substring/shared
    string-reverse string-reverse! reverse-list->string
    string-concatenate string-concatenate/shared string-concatenate-reverse
    string-concatenate-reverse/shared
    string-append/shared
    xsubstring string-xcopy!
    string-null?
    string-join
    string-tokenize
    string-replace
    ; R5RS extended:
    string->list string-copy string-fill! 
    ; R5RS re-exports:
    string? make-string string-length string-ref string-set! 
    string string-append list->string
 )

(define string->list
  (case-lambda
    ((s start end)
     (list->mlist (srfi13:string->list s start end)))
    ((s start)
     (list->mlist (srfi13:string->list s start)))
    ((s)
     (list->mlist (srfi13:string->list s)))))

(define (reverse-list->string char-list)
  (srfi13:reverse-list->string (mlist->list char-list)))

(define (string-join string-list [delimiter " "] [grammar 'infix])
  (srfi13:string-join (mlist->list string-list) delimiter grammar))

(define (string-concatenate string-list)
  (srfi13:string-concatenate (mlist->list string-list)))

(define (string-concatenate/shared string-list)
  (srfi13:string-concatenate/shared (mlist->list string-list)))

(define (make-reverse-concatenator procedure)
  (case-lambda
    ((string-list final-string end)
     (procedure (mlist->list string-list) final-string end))
    ((string-list final-string)
     (procedure (mlist->list string-list) final-string))
    ((string-list)
     (procedure (mlist->list string-list)))))

(define string-concatenate-reverse
  (make-reverse-concatenator srfi13:string-concatenate-reverse))

(define string-concatenate-reverse/shared
  (make-reverse-concatenator srfi13:string-concatenate-reverse/shared))

(define string-tokenize
  (case-lambda
    ((s token-set start end)
     (list->mlist (srfi13:string-tokenize s token-set start end)))
    ((s token-set start)
     (list->mlist (srfi13:string-tokenize s token-set start)))
    ((s token-set)
     (list->mlist (srfi13:string-tokenize s token-set)))
    ((s)
     (list->mlist (srfi13:string-tokenize s)))))
