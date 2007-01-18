
(module xml mzscheme
  (require (lib "class.ss")
           (lib "xml.ss" "xml")
           (lib "list.ss")
           "../wxmefile.ss"
           "nested.ss")

  (provide reader)

  (define reader
    (new (class nested-reader%
           (inherit read-nested-snip)
           (define/override (read-snip text? vers stream)
             (let ([elim-whitespace? (zero? (send stream read-integer "elim-whitespace?"))])
               (read-nested-snip text? vers stream elim-whitespace?)))

           (define/override (generate-special nested src line col pos)
             (let* ([port (nested-content-port nested)]
                    [xml (read-xml port)]
                    [xexpr (xml->xexpr (document-element xml))]
                    [clean-xexpr (if (readable-nested-data nested)
                                     (eliminate-whitespace-in-empty-tags xexpr)
                                     xexpr)])
               (list 'quasiquote clean-xexpr)))

           (super-new))))

  ;; FIXME! Copied from xml-snip-helpers.ss verbatim
  (define (eliminate-whitespace-in-empty-tags xexpr)
    (cond
     [(and (pair? xexpr)
           (symbol? (car xexpr)))
      (list* (car xexpr)
             (cadr xexpr)
             (map eliminate-whitespace-in-empty-tags
                  (eliminate-whitespace-in-list (cddr xexpr))))]
     [else xexpr]))
  (define (eliminate-whitespace-in-list xexprs)
    (cond
     [(andmap (lambda (x) (or (not (string? x))
                              (string-whitespace? x)))
              xexprs)
      (filter (lambda (x) (not (string? x))) xexprs)]
     [else xexprs]))
  (define (string-whitespace? str)
    (let loop ([i (string-length str)])
      (cond
       [(zero? i) #t]
       [(char-whitespace? (string-ref str (- i 1)))
        (loop (- i 1))]
       [else #f]))))
