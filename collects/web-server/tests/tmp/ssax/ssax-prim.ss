; Module header is generated automatically
#cs(module ssax-prim mzscheme
(require "ssax-code.ss")

;=========================================================================
; This is a multi parser constructor function

;------------------------------------------------
; Some Oleg Kiselyov's features from SSAX:XML->SXML

; Returns 
(define (RES-NAME->SXML res-name)
  (string->symbol
   (string-append
    (symbol->string (car res-name))
    ":"
    (symbol->string (cdr res-name)))))

  
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings
(define (reverse-collect-str fragments)
  (if (null? fragments) '()	; a shortcut
      (let loop ((fragments fragments) (result '()) (strs '()))
        (cond
          ((null? fragments)
           (if (null? strs) result
               (cons (apply string-append strs) result)))
          ((string? (car fragments))
           (loop (cdr fragments) result (cons (car fragments) strs)))
          (else
           (loop (cdr fragments)
                 (cons
                  (car fragments)
                  (if (null? strs) result
                      (cons (apply string-append strs) result)))
                 '()))))))

  
; given the list of fragments (some of which are text strings)
; reverse the list and concatenate adjacent text strings
; We also drop "unsignificant" whitespace, that is, whitespace
; in front, behind and between elements. The whitespace that
; is included in character data is not affected.
(define (reverse-collect-str-drop-ws fragments)
  (cond 
    ((null? fragments) '())		; a shortcut
    ((and (string? (car fragments))	; another shortcut
          (null? (cdr fragments))	; remove trailing ws
          (string-whitespace? (car fragments))) '())
    (else
     (let loop ((fragments fragments) (result '()) (strs '())
                (all-whitespace? #t))
       (cond
         ((null? fragments)
          (if all-whitespace? result	; remove leading ws
              (cons (apply string-append strs) result)))
         ((string? (car fragments))
          (loop (cdr fragments) result (cons (car fragments) strs)
                (and all-whitespace?
                     (string-whitespace? (car fragments)))))
         (else
          (loop (cdr fragments)
                (cons
                 (car fragments)
                 (if all-whitespace? result
                     (cons (apply string-append strs) result)))
                '() #t)))))))


(provide (all-defined)))
