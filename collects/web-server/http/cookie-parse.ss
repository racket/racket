#lang scheme
(require web-server/http/request-structs
         net/cookie
         web-server/private/util         
         scheme/contract)

(define-struct client-cookie 
  (name value domain path)
  #:transparent)

(provide/contract
 [struct client-cookie 
         ([name string?]
          [value string?]
          [domain (or/c false/c valid-domain?)]
          [path (or/c false/c string?)])]
 [request-cookies (request? . -> . (listof client-cookie?))])

;; ============================================================
;; utilities for retrieving cookies

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#|
   cookie          =       "Cookie:" cookie-version
                           1*((";" | ",") cookie-value)
   cookie-value    =       NAME "=" VALUE [";" path] [";" domain]
   cookie-version  =       "$Version" "=" value
   NAME            =       attr
   VALUE           =       value
   path            =       "$Path" "=" value
   domain          =       "$Domain" "=" value

   value          = token | quoted-string

   token          = 1*<any CHAR except CTLs or tspecials>

   quoted-string  = ( <"> *(qdtext) <"> )
   qdtext         = <any TEXT except <">>
|#
(define-lex-abbrevs
  (tspecial (:or (char-set "()<>@,;:\\\"/[]?={}") whitespace #\tab))
  (token-char (:- any-char tspecial iso-control)))

(define-tokens regular (TOKEN QUOTED-STRING))
(define-empty-tokens keywords (EQUALS SEMI COMMA PATH DOMAIN VERSION EOF))

(define cookie-lexer
  (lexer-src-pos
   [(eof) (token-EOF)]
   [whitespace (return-without-pos (cookie-lexer input-port))]
   ["=" (token-EQUALS)]
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   ["$Path" (token-PATH)]
   ["$Domain" (token-DOMAIN)]
   ["$Version" (token-VERSION)]
   [(:: #\" (:* (:or (:~ #\") "\\\"")) #\")
    (token-QUOTED-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ token-char) (token-TOKEN lexeme)]))

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define assoc-list-parser
  (parser (src-pos)
          (start cookie)
          (tokens regular keywords)
          (grammar (cookie [(VERSION EQUALS rhs separator items) $5]
                           [(items) $1])
                   (items [(item separator items) (cons $1 $3)]
                          [(item) (list $1)])
                   (separator
                    [(COMMA) #t]
                    [(SEMI) #t])
                   (item [(lhs EQUALS rhs) (cons $1 $3)]
                         ; This is not part of the spec. It is illegal
                         [(lhs EQUALS) (cons $1 "")])
                   (lhs [(DOMAIN) 'domain]
                        [(PATH) 'path]
                        [(TOKEN) $1])
                   (rhs [(TOKEN) $1]
                        [(QUOTED-STRING) $1]))
          (end EOF)
          (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (raise-syntax-error
                    'assoc-list-parser
                    (format 
                     (if tok-ok? 
                         "Did not expect token ~a"
                         "Invalid token ~a")
                     tok-name)
                    (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))))

(define (do-parse str)
  (with-handlers ([exn:fail? 
                   (λ (e) empty)])
    (with-input-from-string 
     str
     (λ () 
       (let ([ip (current-input-port)])
         (port-count-lines! ip)
         (parameterize ([current-source-name (object-name ip)])
           (raw->cookies (assoc-list-parser (λ () (cookie-lexer ip))))))))))

;; raw->cookies : flat-property-list -> (listof cookie)
(define (raw->cookies associations)
  
  ;; get-cookie-setter : symbol -> cookie string -> cookie
  ;; gets a setter for the given property
  (define (get-cookie-setter property-name)
    (case property-name
      [(domain)
       (λ (c x) 
         (struct-copy client-cookie c
                      [domain x]))]      
      [(path)
       (λ (c x) 
         (struct-copy client-cookie c
                      [path x]))]
      [else 
       (λ (c x) c)]))
  
  (unless (and (pair? associations) (string? (car (car associations))))
    (error 'raw->cookies "expected a non-empty association list headed by a cookie"))
  
  (let loop ([l (cdr associations)]
             [c (make-client-cookie (car (car associations))
                                    (cdr (car associations))
                                    #f #f)])
    (cond
      [(null? l) (list c)]
      [(string? (car (car l)))
       (cons c (loop (cdr l) (make-client-cookie
                              (car (car l))
                              (cdr (car l))
                              #f #f)))]
      [else
       (loop (cdr l)
             ((get-cookie-setter (car (car l))) c (cdr (car l))))])))

;; request-cookies* : request -> (listof cookie)
(define (request-cookies req)
  (define hdrs (request-headers/raw req))
  (apply append
         (map (compose do-parse bytes->string/utf-8 header-value)
              (filter (lambda (h)
                        (bytes-ci=? #"Cookie" (header-field h)))
                      hdrs))))
