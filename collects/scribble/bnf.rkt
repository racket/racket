(module bnf mzscheme
  (require "struct.rkt"
           "decode.rkt"
           mzlib/kw)

  (provide BNF 
           nonterm
           BNF-seq
           BNF-alt  BNF-alt/close ; single-line alternatives
           BNF-etc
           BNF-group
           optional kleenestar kleeneplus kleenerange)

  (define spacer (make-element 'hspace (list " ")))
  (define equals (make-element 'tt (list spacer "::=" spacer)))
  (define alt (make-element 'tt (list spacer spacer "|" spacer spacer)))

  (define (as-flow i) (make-flow (list (make-paragraph (list i)))))

  (define (BNF . defns)
    (make-table
     #f
     (apply
      append
      (map (lambda (defn)
             (cons
              (list (as-flow spacer) (as-flow (car defn)) (as-flow equals) (as-flow (cadr defn)))
              (map (lambda (i)
                     (list (as-flow spacer) (as-flow " ") (as-flow alt) (as-flow i)))
                   (cddr defn))))
           defns))))

  (define (interleave l spacer)
    (make-element #f (cons (car l)
                           (apply append
                                  (map (lambda (i)
                                         (list spacer i))
                                       (cdr l))))))

  (define (BNF-seq . l)
    (if (null? l)
        ""
        (interleave l spacer)))

  (define (BNF-alt . l)
    (interleave l alt))

  (define (BNF-alt/close . l)
    (interleave l " | "))

  (define BNF-etc "...")

  (define/kw (nonterm #:body s)
    (make-element #f (append (list 'lang)
                             (list (make-element 'italic (decode-content s)))
                             (list 'rang))))

  (define/kw (optional #:body s)
    (make-element #f (append (list "[") (decode-content s) (list "]"))))

  (define/kw (BNF-group #:body s)
    (make-element #f (append (list "{") 
                             (list (apply BNF-seq (decode-content s)))
                             (list "}"))))

  (define/kw (kleenestar #:body s)
    (make-element #f (append (decode-content s) (list "*"))))

  (define/kw (kleeneplus #:body s)
    (make-element #f (append (decode-content s) (list (make-element 'superscript (list "+"))))))

  (define/kw (kleenerange a b #:body s)
    (make-element #f (append (decode-content s) 
                             (list (make-element 'superscript 
                                                 (list (format "{~a,~a}" a b))))))))
