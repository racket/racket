#lang racket/base
(require racket/contract
         racket/list
         web-server/http
         web-server/private/xexpr
         (only-in "lib.rkt"
                  formlet/c
                  pure
                  cross))

;; Convert UTF-8 bytes to string when needed.
(define (coerce-string/utf-8 bstr-or-str)
  (if (bytes? bstr-or-str)
    (bytes->string/utf-8 bstr-or-str)
    bstr-or-str))

; Low-level
(define (next-name i)
  (values (format "input_~a" i) (add1 i)))

(define (make-input*/forest render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (define wb (string->bytes/utf-8 w))
      (values (render w)
              (lambda (env) 
                (for/list ([b (in-list env)]
                           #:when (bytes=? wb (binding-id b)))
                  b))
              i))))

(define (make-input* render)
  (make-input*/forest
   (lambda (w)
     (list (render w)))))

(define (make-input render)
  (lambda (i)
    (let-values ([(w i) (next-name i)])
      (values (list (render w))
              (lambda (env) (bindings-assq (string->bytes/utf-8 w) env))
              i))))

(define binding:form-required
  (pure 
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         (error 'formlets "Missing required field")))))

(define (binding:form/default default)
  (pure
   (lambda (bf)
     (if (binding:form? bf)
         (binding:form-value bf)
         default))))

(provide/contract
 [make-input* ((string? . -> . pretty-xexpr/c) . -> . (formlet/c (listof binding?)))]
 [make-input ((string? . -> . pretty-xexpr/c) . -> . (formlet/c (or/c false/c binding?)))]
 #;[binding:form-required (formlet/c (binding? . -> . bytes?))]
 #;[binding:form/default (bytes? . -> . (formlet/c (binding? . -> . bytes?)))])

; HTML Spec
(define (input
         #:type [type "text"]
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (make-input
   (lambda (n)
     (list 'input
           (list* (list 'name n)
                  (list 'type type)
                  (append
                   (filter list?
                           (list (and value (list 'value (coerce-string/utf-8 value)))
                                 (and size (list 'size (number->string size)))
                                 (and max-length (list 'maxlength (number->string max-length)))
                                 (and read-only? (list 'readonly "true"))))
                   attrs))))))

(define (text-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (input
   #:type "text"
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (password-input 
         #:value [value #f]
         #:size [size #f]
         #:max-length [max-length #f]
         #:read-only? [read-only? #f]
         #:attributes [attrs empty])
  (input
   #:type "password"
   #:value value
   #:size size
   #:max-length max-length
   #:read-only? read-only?
   #:attributes attrs))

(define (checkbox value checked?
                  #:attributes [attrs empty])
  (input
   #:type "checkbox"
   #:value value
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (radio value checked?
               #:attributes [attrs empty])
  (input
   #:type "radio"
   #:value value
   #:attributes
   (if checked? (append (list (list 'checked "true")) attrs) attrs)))

(define (input-group l
                     #:kind kind
                     #:attributes [attrs (λ (x) empty)]
                     #:checked? [checked? (λ (x) #f)]
                     #:display [display (λ (x) x)])
  (define value->element (make-hasheq))
  (define i 0)
  (define (remember! e)
    (define this-i
      (begin0 i (set! i (add1 i))))
    (hash-set! value->element this-i e))
  (define (recall i)
    (hash-ref value->element i
              (λ () (error 'input-group "Invalid selection: ~e" i))))
  (for ([e l])
    (remember! e))
  (define (radio-first l)
    (if (string=? kind "radio")
        (first l)
        l))
  (cross
   (pure
    (lambda (bs)
      (radio-first
       (map (compose recall string->number
                     bytes->string/utf-8
                     binding:form-value)
            bs))))
   (make-input*/forest
    (lambda (name)
      (apply append
             (for/list ([vn (in-range i)])
                       (define e (hash-ref value->element vn))
                       (define v (number->string vn))
                       (list
                        `(input ([name ,name]
                                 [type ,kind]
                                 [value ,v]
                                 ,@(if (checked? e)
                                       '([checked "true"])
                                       empty)
                                 ,@(attrs e)))
                        (display e))))))))

(define (radio-group l 
                     #:attributes [attrs (λ (x) empty)]
                     #:checked? [checked? (λ (x) #f)]
                     #:display [display (λ (x) x)])
  (input-group l
               #:kind "radio"
               #:attributes attrs
               #:checked? checked?
               #:display display))

(define (checkbox-group l 
                     #:attributes [attrs (λ (x) empty)]
                     #:checked? [checked? (λ (x) #f)]
                     #:display [display (λ (x) x)])
  (input-group l
               #:kind "checkbox"
               #:attributes attrs
               #:checked? checked?
               #:display display))

(define (submit value
                #:attributes [attrs empty])
  (input
   #:type "submit"
   #:value value
   #:attributes attrs))

(define (reset value
               #:attributes [attrs empty])
  (input
   #:type "reset"
   #:value value
   #:attributes attrs))

(define (file-upload #:attributes [attrs empty])
  (input
   #:type "file"
   #:attributes attrs))

(define (hidden value #:attributes [attrs empty])
  (input
   #:type "hidden"
   #:value value
   #:attributes attrs))

(define (button type text
                #:disabled [disabled #f]
                #:value [value #f]
                #:attributes [attrs empty])
  (make-input
   (λ (n)
     (list 'button
           (list* (list 'name n)
                  (list 'type (coerce-string/utf-8 type))
                  (append 
                   (filter list?
                           (list (and disabled (list 'disabled (if disabled "true" "false")))
                                 (and value (list 'value (coerce-string/utf-8 value)))))
                   attrs))
           (coerce-string/utf-8 text)))))

(define (img alt src
             #:height [height #f]
             #:longdesc [ldesc #f]
             #:usemap [map #f]
             #:width [width #f]
             #:attributes [attrs empty])
  (make-input
   (λ (n)
     (list 'img
           (list* (list 'name n)
                  (list 'src (coerce-string/utf-8 src))
                  (list 'alt (coerce-string/utf-8 alt))
                  (append
                   (filter list?
                           (list (and height (list 'height (number->string height)))
                                 (and ldesc (list 'longdesc (coerce-string/utf-8 ldesc)))
                                 (and map (list 'usemap (coerce-string/utf-8 map)))
                                 (and width (list 'width (number->string width)))))
                   attrs))))))

(define (multiselect-input l
                           #:attributes [attrs empty]
                           #:multiple? [multiple? #t]
                           #:selected? [selected? (λ (x) #f)]
                           #:display [display (λ (x) x)])
  (define value->element (make-hasheq))
  (define i 0)
  (define (remember! e)
    (define this-i
      (begin0 i (set! i (add1 i))))
    (hash-set! value->element this-i e))
  (define (recall i)
    (hash-ref value->element i
              (λ () (error 'input-select* "Invalid selection: ~e" i))))
  (for ([e l])
    (remember! e))
  (cross
   (pure
    (lambda (bs)
      (map (compose recall string->number
                    bytes->string/utf-8
                    binding:form-value)
           bs)))
   (make-input*
    (lambda (name)
      `(select (,@(if multiple? '([multiple "true"]) empty)
                [name ,name]
                ,@attrs)
               ,@(for/list ([vn (in-range i)])
                   (define e (hash-ref value->element vn))
                   (define v (number->string vn))
                   `(option ([value ,v]
                             ,@(if (selected? e)
                                   '([selected "true"])
                                   empty))
                            ,(display e))))))))

(define (select-input l 
                      #:attributes [attrs empty]
                      #:selected? [selected? (λ (x) #f)]
                      #:display [display (λ (x) x)])
  (cross
   (pure first)
   (multiselect-input l
                      #:attributes attrs
                      #:multiple? #f
                      #:selected? selected?
                      #:display display)))

(define (textarea-input
         #:value [value #f]
         #:attributes [attrs empty]
         #:rows [rows #f]
         #:cols [cols #f])   
  (make-input
   (lambda (n)
     (list 'textarea
           (list* (list 'name n)
                  (append
                   (filter list?
                           (list (and rows (list 'rows (number->string rows)))
                                 (and cols (list 'cols (number->string cols)))))
                   attrs))
           (if value (coerce-string/utf-8 value) "")))))

(provide/contract
 [input (()
         (#:type string?
          #:value (or/c false/c bytes? string?)
          #:max-length (or/c false/c exact-nonnegative-integer?)
          #:read-only? boolean?
          #:attributes (listof (list/c symbol? string?)))
          . ->* .
          (formlet/c (or/c false/c binding?)))]
 [text-input (() 
              (#:value (or/c false/c bytes? string?)
                       #:size (or/c false/c exact-nonnegative-integer?)
                       #:max-length (or/c false/c exact-nonnegative-integer?)
                       #:read-only? boolean?
                       #:attributes (listof (list/c symbol? string?)))
              . ->* . 
              (formlet/c (or/c false/c binding?)))]
 [password-input (() 
                  (#:value (or/c false/c bytes? string?)
                           #:size (or/c false/c exact-nonnegative-integer?)
                           #:max-length (or/c false/c exact-nonnegative-integer?)
                           #:read-only? boolean?
                           #:attributes (listof (list/c symbol? string?)))
                  . ->* . 
                  (formlet/c (or/c false/c binding?)))]
 [checkbox (((or/c bytes? string?) boolean?)
            (#:attributes (listof (list/c symbol? string?)))
            . ->* .
            (formlet/c (or/c false/c binding?)))]
 [radio (((or/c bytes? string?) boolean?)
         (#:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [radio-group ((sequence?)
               (#:attributes 
                (-> any/c (listof (list/c symbol? string?)))
                #:checked? (any/c . -> . boolean?)
                #:display (any/c . -> . pretty-xexpr/c))
               . ->* .
               (formlet/c any/c))]
 [checkbox-group ((sequence?)
                  (#:attributes 
                   (-> any/c (listof (list/c symbol? string?)))
                   #:checked? (any/c . -> . boolean?)
                   #:display (any/c . -> . pretty-xexpr/c))
                  . ->* .
                  (formlet/c (listof any/c)))]
 [submit (((or/c bytes? string?))
          (#:attributes (listof (list/c symbol? string?)))
          . ->* .
          (formlet/c (or/c false/c binding?)))]
 [reset (((or/c bytes? string?))
         (#:attributes (listof (list/c symbol? string?)))
         . ->* .
         (formlet/c (or/c false/c binding?)))]
 [file-upload (()
               (#:attributes (listof (list/c symbol? string?)))
               . ->* .
               (formlet/c (or/c false/c binding?)))]
 [hidden (((or/c bytes? string?))
          (#:attributes (listof (list/c symbol? string?)))
          . ->* .
          (formlet/c (or/c false/c binding?)))]
 [img (((or/c bytes? string?) (or/c bytes? string?))
       (#:height (or/c false/c exact-nonnegative-integer?)
                 #:longdesc (or/c false/c (or/c bytes? string?))
                 #:usemap (or/c false/c (or/c bytes? string?))
                 #:width (or/c false/c exact-nonnegative-integer?)
                 #:attributes (listof (list/c symbol? string?)))
       . ->* .
       (formlet/c (or/c false/c binding?)))]
 [button (((or/c bytes? string?) (or/c bytes? string?))
          (#:disabled
           boolean?
           #:value (or/c false/c (or/c bytes? string?))
           #:attributes (listof (list/c symbol? string?)))
          . ->* .
          (formlet/c (or/c false/c binding?)))]
 [multiselect-input ((sequence?)
                     (#:attributes 
                      (listof (list/c symbol? string?))
                      #:multiple? boolean?
                      #:selected? (any/c . -> . boolean?)
                      #:display (any/c . -> . pretty-xexpr/c))
                     . ->* .
                     (formlet/c (listof any/c)))]
 [select-input ((sequence?)
                (#:attributes 
                 (listof (list/c symbol? string?))
                 #:selected? (any/c . -> . boolean?)
                 #:display (any/c . -> . pretty-xexpr/c))
                . ->* .
                (formlet/c any/c))]
 [textarea-input (()
                  (#:attributes 
                   (listof (list/c symbol? string?))
                   #:value (or/c false/c (or/c bytes? string?))
                   #:rows number?
                   #:cols number?)
                  . ->* .
                  (formlet/c (or/c false/c binding?)))])

; High-level
(define (required f)
  (cross binding:form-required f))

(define (default d f)
  (cross (binding:form/default d) f))

(define (to-string f)
  (cross (pure bytes->string/utf-8) f))

(define (to-number f)
  (cross (pure string->number) f))

(define (to-symbol f)
  (cross (pure string->symbol) f))

(define (to-boolean f)
  (cross (pure 
          (lambda (b)
            (bytes=? b #"on")))
         f))

(provide/contract
 [required ((formlet/c (or/c false/c binding?)) . -> . (formlet/c bytes?))]
 [default (bytes? (formlet/c (or/c false/c binding?)) . -> . (formlet/c bytes?))]
 [to-string ((formlet/c bytes?) . -> . (formlet/c string?))]
 [to-number ((formlet/c string?) . -> . (formlet/c number?))]
 [to-symbol ((formlet/c string?) . -> . (formlet/c symbol?))]
 [to-boolean ((formlet/c bytes?) . -> . (formlet/c boolean?))])

; OLD
(define input-string (to-string (required (text-input))))
(define input-int (to-number input-string))
(define input-symbol (to-symbol input-string))

(provide/contract
 [input-string (formlet/c string?)]
 [input-int (formlet/c integer?)]
 [input-symbol (formlet/c symbol?)])
