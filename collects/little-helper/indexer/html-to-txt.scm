;;; This lexer is a little tricky.
;;; HtmlPrag discards source location information, so
;;; we use HtmlPrag to get the content. Then we break
;;; the content up into terms. Regular expression
;;; matching is then used to recover the positions of
;;; the terms (line and column numbers are lost, but
;;; they aren't needed).

(module html-to-txt mzscheme
  (require scheme/mpair
           scheme/system
           (only scheme regexp-quote)
           (lib "match.ss")
           "planet/file.scm"
           "planet/htmlprag.ss" ; version 1.3
           "planet/intersperse.scm"
           (prefix txt: "lexer.scm"))
  
  (define (html-file->txt-file src-file dest-file)
    (system (format "w3m -dump 'a' >'a'" src-file dest-file)))
  
  (define (file->shtml file)
    ; read file and return parsed file
    (with-input-from-file file
      (lambda ()
        (html->shtml 
         (port->string 
          (current-input-port))))))
  
  (define (shtml->tokens s)
    ; extract the terms
    (match s
      [(t ...) (apply append (map shtml->tokens t))]
      [t       (if (string? t) (list t) '())]))
  
  ; freeze : mutable-tree -> immutabale-tree
  ;  convert mpairs to pairs
  (define (freeze o)
    (if (mlist? o)
        (map freeze (mlist->list o))
        o))
  
  (define (html-file->string file)
    (apply string-append
           (intersperse " "
                        (shtml->tokens
                         (freeze
                          (file->shtml file))))))
  
  #;(define (html-file->terms file)
      ; return a list of terms occuring in file
      (map car
           (txt:port->tokens 
            (open-input-string
             (apply string-append
                    (intersperse " "
                                 (shtml->tokens
                                  (freeze
                                   (file->shtml file)))))))))
  
  #;(define (terms->tokens terms file)
    ; use regexp-match-peek-positions to recover the 
    ; positions of the terms
    (let ([in (open-input-file file)])
      (let loop ([start-pos 0]
                 [terms terms]
                 [tokens '()])
        (cond [(null? terms)
               (reverse tokens)]
              [else
               (let ([term (car terms)])
                 (display (list start-pos term)) (newline)
                 (let ([pos (regexp-match-peek-positions (regexp-quote term) in start-pos)])
                   (if pos
                       (loop (cdr (car pos))
                             (cdr terms) 
                             (cons (list term (list 'line? 'col? (caar pos))) tokens))
                       (begin
                         (display (format "Skipped '~a'\n" term))
                         (loop start-pos (cdr terms) tokens)))))]))))
  
  ; (define file "/Applications/PLT Scheme v3.99.0.10/doc/reference/begin.html")
  
  #;(txt:port->tokens 
     (open-input-string
      (apply string-append
             (shtml->tokens
              (freeze
               (file->shtml
                "../test/html-example.html"))))))
  )