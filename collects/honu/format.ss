(module format mzscheme

  (require (lib "contract.ss")
           (lib "list.ss")
           (lib "class.ss")
           (lib "plt-match.ss")
           (only "base.ss" null%)
           "ast.ss"
           )

  (provide/contract
   [honu-value->string (any/c . -> . string?)]
   [honu-type->string (ast:type? . -> . string?)]
   )

  (define (string-append-delimited pre mid post strings)
    (string-append
     pre
     (if (null? strings)
         ""
         (foldl (lambda (str prefix)
                  (string-append prefix mid str))
                (car strings)
                (cdr strings)))
     post))
  
  (define (honu-value->string value)
    (cond
     [(number? value) (format "~a" value)]
     [(char? value) (format "'~a'" value)]
     [(string? value) (format "~s" value)]
     [(boolean? value) (if value "true" "false")]
     [(procedure? value) "procedure"]
     [(null? value) "()"]
     [(list? value) ; Always non-empty
      (honu-tuple->string value)]
     [(is-a? value null%) "null"]
     [(object? value) ; Always non-null
      (honu-object->string value)]
     [else (error 'honu-value->string "Unknown value ~s" value)]))

  (define (honu-tuple->string tuple)
    (string-append-delimited "(" ", " ")" (map honu-value->string tuple)))

  (define (honu-object->string value)
    (send value format-class-name))

  (define (honu-type->string t)
    (match t
      [(struct ast:type:top (_))
       "(top type / any value)"]
      [(struct ast:type:bot (_))
       "(bottom type / no value)"]
      [(struct ast:type:primitive (_ name))
       (symbol->string name)]
      [(struct ast:type:tuple (_ args))
       (string-append-delimited "tuple(" ", " ")" (map honu-type->string args))]
      [(struct ast:type:partial/tuple (_ slot type))
       (format "tuple of size >= ~a where the type in position ~a is ~a"
               slot slot (honu-type->string type))]
      [(struct ast:type:function (_ arg ret))
       (if (ast:type:function? arg)
           (string-append "(" (honu-type->string arg) ") -> " (honu-type->string ret))
           (string-append (honu-type->string arg) " -> " (honu-type->string ret)))]
      [(struct ast:type:method (_ disp arg ret))
       (string-append "[" (honu-type->string disp) "] "
                      (honu-type->string arg) " -> " (honu-type->string ret))]
      [(struct ast:type:object:iface (_ name))
       (symbol->string (syntax-e name))]
      [(struct ast:type:object:any (_))
       "Any"]
      [(struct ast:type:object:null (_))
       "null"]))

  )
