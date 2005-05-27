(module quotes mzscheme
  (require (lib "url.ss" "net"))
  (require (lib "list.ss"))
  
  (provide no-quote? stock-quote no-quote-reason)
  
  (define-struct no-quote (company reason))
  
  #| ----------------------------------------------------------------------------
  Stock and Fund Quotes
  ---------------------
  
  procedures: 
  stock-quote : String -> Number 
  
  exceptions: 
  no-quote : (structure:no-quote String Symbol)
  
  The goal is to send a request to some quote server, 
  read the page line by line, and identify the quote. 
  
  -----------------------------------------------------
  
  First: where the information comes from and 
  how to indentify it within the page: 
  - the current source is Yahoo's page
  - we look for a table cell that looks like
  this: <td nowrap><b>DIGIT*.DIGITDIGIT</b></td>
  |#   
  
  ; String[format: String[company name]]
  (define SOURCE "http://finance.yahoo.com/q?s=~a&d=v1")
  
  ; String -> (union (list String String) #f)
  (define (find x)
    (regexp-match "<b>([0-9\\.]*)</b></big></td>" x))
  
  ; String -> Number
  (define (stock-quote company)
    (let* ([URL (string->url (format SOURCE company))]
           [PG  (call/input-url URL get-pure-port read-a-page)]
	   ; [_ (printf "~s~n" PG)] ; for debugging
           [LN  (filter find PG)]
           [QT  (if (pair? LN)
                    (cadr (find (car LN)))
                    (raise (make-no-quote company 'find)))])
      (cond
        [(string->number QT)]
        [else (raise (make-no-quote company 'no-number))])))
  
  
  ;; Iport -> (listof String)
  ;; reading a page as a list of lines
  (define (read-a-page ip)
    (letrec ([reader (lambda ()
                       (let ([next (read-line)])
                         (cond
                           [(eof-object? next) '()]
                           [else (cons next (reader))])))])
      (parameterize ([current-input-port ip])
        (reader))))
  
  #| TESTS:
  
  (map stock-quote '(; companies 
                     "BAC" "DELL" "IBM" 
                     ; funds 
                     "PRSCX" "NAESX" "FRBAX"))
  
  (with-handlers ([no-quote? (lambda (x) (eq? (no-quote-reason x) 'find))])
    (stock-quote "XXX"))
  
  |#
  
  )
