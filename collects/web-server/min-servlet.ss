;; This file is intended to include the minimum set of *utilities*
;; needed to write servlets. It is based on the *old* version of "servlet-sig.ss"
(module min-servlet mzscheme
  (require (lib "xml.ss" "xml")
           (rename "util.ss" translate-escapes translate-escapes))
  (provide response?
           (struct response/full (code message seconds mime extras body))
           (struct response/incremental ())
           (struct request (method uri headers host-ip client-ip))
           (rename request-bindings request-bindings/raw)
           (rename get-parsed-bindings request-bindings)
           translate-escapes)

  ; : TST -> bool
  (define (response? page)
    (or (response/full? page)
        ; this could fail for dotted lists - rewrite andmap
        (and (pair? page) (pair? (cdr page)) (andmap string? page))
                                        ; insist the xexpr has a root element
        (and (pair? page) (xexpr? page))))

  ; more here - these should really have a common super type, but I don't want to break
  ; the existing interface.
  (define-struct response/full (code message seconds mime extras body))
  (define-struct (response/incremental response/full) ())

  ; request = (make-request sym URL (listof (cons sym str)) (U str (listof (cons sym str))) str str)
  ; Outside this module, bindings looks like an association list (due to renaming request-bindings).
  ; Inside it is a string for normal requests, but for file uploads it is still an association list.
  ; more here - perhaps it should always be a string inside this module.
  (define-struct request (method uri headers bindings host-ip client-ip))

  ; get-parsed-bindings : request -> (listof (cons sym str))
  (define (get-parsed-bindings r)
    (let ([x (request-bindings r)])
      (if (list? x)
          x
          (parse-bindings x))))

  ; parse-bindings : (U #f String) -> (listof (cons Symbol String))
  (define (parse-bindings raw)
    (if (string? raw)
        (let ([len (string-length raw)])
          (let loop ([start 0])
            (let find= ([key-end start])
              (if (>= key-end len)
                  null
                  (if (eq? (string-ref raw key-end) #\=)
                      (let find-amp ([amp-end (add1 key-end)])
                        (if (or (= amp-end len) (eq? (string-ref raw amp-end) #\&))
                            (cons (cons (string->symbol (substring raw start key-end))
                                        (translate-escapes
                                         (substring raw (add1 key-end) amp-end)))
                                  (loop (add1 amp-end)))
                            (find-amp (add1 amp-end))))
                      (find= (add1 key-end)))))))
        null))
)
