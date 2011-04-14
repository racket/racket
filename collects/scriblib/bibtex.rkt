#lang racket/base
(require racket/function
         racket/list)

(define-syntax-rule
  (define-bibtex this-generate-bib this-cite bib-pth)
  (begin
    (define bibtex-db (path->bibdb bib-pth))
    (define this-generate-bib
      (curry generate-bib bibtex-db))
    (define this-cite
      (curry cite bibtex-db))))

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

(define-empty-tokens toks (AT LBRACE RBRACE COMMA EQUALS HASH EOF))
(define-tokens rtoks (NUM TAG STR))
(define bibtex-lex
  (lexer-src-pos
   ["@" (token-AT)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["," (token-COMMA)]
   ["=" (token-EQUALS)]
   ["#" (token-HASH)]
   [(:: "%" (complement (:: any-string #\newline any-string)))
    (return-without-pos (bibtex-lex input-port))]
   [(:+ (char-range "0" "9"))
    (token-NUM (string->number lexeme))]
   [(:: alphabetic (complement (:: any-string (:or whitespace (char-set "{},")) any-string)))
    (token-TAG (string-downcase lexeme))]
   #;[(:: #\" (complement (:: any-string #\" any-string)) #\")
    (token-STR (substring lexeme 1 (sub1 (string-length lexeme))))]
   [(eof) (token-EOF)]
   [any-char (return-without-pos (bibtex-lex input-port))]))

(define bibtex-parse
  (parser
   (src-pos)
   (tokens toks rtoks)
   (end EOF)
   (start db)
   (error
    (lambda (tok-ok? tok-name tok-value start-pos end-pos)
      (error 'bibtex-parse
             "Received ~a token ~a(~s) at ~a:~a-~a:~a"
             (if tok-ok? "valid" "invalid")
             tok-name tok-value
             (position-line start-pos) (position-col start-pos)
             (position-line end-pos) (position-col end-pos))))
   (grammar
    (db [() empty]
        [(entry db) (cons $1 $2)])
    (entry
     [(AT TAG LBRACE elems RBRACE)
      (vector $2 $4)])
    (elems
     [() empty]
     [(elem) (list $1)]
     [(elem COMMA elems) (cons $1 $3)])
    (elem
     [(TAG EQUALS val)
      (vector $1 $3)]
     [(TAG)
      $1]
     [(NUM)
      (number->string $1)])
    (val
     [(NUM) $1]
     [(STR) $1]
     [(TAG) $1]
     [(LBRACE bvals RBRACE)
      $2]
     [(val HASH val)
      (cons $1 $3)])
    (bval
     [(val) $1]
     [(COMMA) ","])
    (bvals
     [() empty]
     [(bval bvals) (cons $1 $2)]))))

(define (path->bibdb pth)
  (printf "~a\n"
          (with-input-from-file
              pth
            (λ ()
              (port-count-lines! (current-input-port))
              (bibtex-parse
               (λ () (bibtex-lex (current-input-port)))))))
  (error 'path->bibdb pth)
  #f)

(define (generate-bib db style)
  "XXX")

(define (cite db . keys)
  "XXX")

(provide define-bibtex)