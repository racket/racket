
(module xexpr mzscheme
  (require (lib "unitsig.ss")
	  (lib "list.ss")
	  (lib "etc.ss"))

  (require "sig.ss")

  (provide xexpr@)

  (define xexpr@
    (unit/sig extra-xexpr^
      (import xml-structs^ writer^)
      ;; Xexpr ::= String
      ;;        |  (list* Symbol (listof Attribute-srep) (listof Xexpr))
      ;;        |  (cons Symbol (listof Xexpr))
      ;;        |  Symbol
      ;;        |  Nat
      ;;        |  Comment
      ;;        |  Processing-instruction
      ;; Attribute-srep ::= (list Symbol String)
      
      ;; sorting is no longer necessary, since xt3d uses xml->zxexpr, which sorts.
      
      ;; assoc-sort : (listof (list Symbol a)) -> (listof (list Symbol a))
      (define (assoc-sort to-sort)
	(quicksort to-sort (bcompose string<? (compose symbol->string car))))
      
      (define xexpr-drop-empty-attributes (make-parameter #f))

      #|
      ; : tst -> bool
      (define (xexpr? x)
	(or (string? x) (symbol? x) (number? x) (pcdata? x) (comment? x)
	    (and (cons? x) (symbol? (car x))
		 (or (and (cons? (cdr x)) (listof? xexpr-attribute? (cadr x))
			  (listof? xexpr? (cddr x)))
		     (listof? xexpr? (cdr x))))))
      |#

      (define (xexpr? x)
        (correct-xexpr? x (lambda () #t) (lambda (exn) #f)))
                                       

      (define (validate-xexpr x)
        (correct-xexpr? x (lambda () #t) (lambda (exn) (raise exn))))

      ;; ;; ;; ;; ;; ;; ;
      ;; ; xexpr? helpers

      (define-struct (exn:invalid-xexpr exn) (code))

      ;; correct-xexpr? : any (-> a) (exn -> a) -> a
      (define (correct-xexpr? x true false)
        (cond
          ((string? x) (true))
          ((symbol? x) (true))
          ((number? x) (true))
          ((comment? x) (true))
          ((pi? x) (true))
          ((list? x)
           (or (null? x)
               (if (symbol? (car x))
                 (if (has-attribute? x)
                   (and (attribute-pairs? (cadr x) true false)
                        (andmap (lambda (part)
                                  (correct-xexpr? part true false))
                                (cddr x))
                        (true))
                   (andmap (lambda (part)
                             (correct-xexpr? part true false))
                           (cdr x)))
                 (false (make-exn:invalid-xexpr
                          (string->immutable-string
                            (format
                              "Expected a symbol as the element name, given ~a"
                              (car x)))
                          (current-continuation-marks)
                          x)))))
          (else (false
                  (make-exn:invalid-xexpr
                    (string->immutable-string
                      (format
                        (string-append
                          "Expected a string, symbol, number, comment, "
                          "processing instruction, or list, given ~a")
                        x))
                    (current-continuation-marks)
                    x)))))

      ;; has-attribute? : List -> Boolean
      ;; True if the Xexpr provided has an attribute list.
      (define (has-attribute? x)
        (and (> (length x) 1)
             (list? (cadr x))
             (andmap (lambda (attr)
                       (pair? attr))
                     (cadr x))))

      ;; attribute-pairs? : List (-> a) (exn -> a) -> a
      ;; True if the list is a list of pairs.
      (define (attribute-pairs? attrs true false)
        (if (null? attrs)
          (true)
          (let ((attr (car attrs)))
            (if (pair? attr)
              (and (attribute-symbol-string? attr true false)
                   (attribute-pairs? (cdr attrs) true false )
                   (true))
              (false
                (make-exn:invalid-xexpr
                  (string->immutable-string
                    (format "Expected a pair, given ~a" attr))
                  (current-continuation-marks)
                  attr))))))

      ;; attribute-symbol-string? : List (-> a) (exn -> a) -> a
      ;; True if the list is a list of String,Symbol pairs.
      (define (attribute-symbol-string? attr true false)
        (if (symbol? (car attr))
          (if (string? (cadr attr))
            (true)
            (false (make-exn:invalid-xexpr
                     (string->immutable-string
                       (format "Expected a string, given ~a" (cadr attr)))
                     (current-continuation-marks)
                     (cadr attr))))
          (false (make-exn:invalid-xexpr
                   (string->immutable-string
                     (format "Expected a symbol, given ~a" (car attr)))
                   (current-continuation-marks)
                   (cadr attr)))))

      ;; ; end xexpr? helpers
      ;; ;; ;; ;; ;; ;; ;; ;;

     
      ; : (a -> bool) tst -> bool
      ; To check if l is a (listof p?)
      ; Don't use (and (list? l) (andmap p? l)) because l may be improper.
      (define (listof? p? l)
        (let listof-p? ([l l])
          (or (null? l)
              (and (cons? l) (p? (car l)) (listof-p? (cdr l))))))
      
      ; : tst -> bool
      (define (xexpr-attribute? b)
        (and (pair? b)
             (symbol? (car b))
             (pair? (cdr b))
             (string? (cadr b))
             (null? (cddr b))))

      ;; xml->xexpr : Content -> Xexpr
      ;; The contract is loosely enforced.
      (define (xml->xexpr x)
        (let* ([non-dropping-combine
                (lambda (atts body)
                  (cons (assoc-sort (map attribute->srep atts))
                        body))]
               [combine (if (xexpr-drop-empty-attributes)
                          (lambda (atts body)
                            (if (null? atts)
                              body
                              (non-dropping-combine atts body)))
                          non-dropping-combine)])
          (let loop ([x x])
            (cond
              [(element? x)
              (let ([body (map loop (element-content x))]
                          [atts (element-attributes x)])
                (cons (element-name x) (combine atts body)))]
              [(pcdata? x) (pcdata-string x)]
              [(entity? x) (entity-text x)]
              [(or (comment? x) (pi? x)) x]
              [(document? x) (error 'xml->xexpr "Expected content, given ~e~nUse document-element to extract the content." x)]
              [else ;(error 'xml->xexpr "Expected content, given ~e" x)
              x]))))

      ;; attribute->srep : Attribute -> Attribute-srep
      (define (attribute->srep a)
	(list (attribute-name a) (attribute-value a)))
      
      ;; srep->attribute : Attribute-srep -> Attribute
      (define (srep->attribute a)
	(unless (and (pair? a) (pair? (cdr a)) (null? (cddr a)) (symbol? (car a)) (string? (cadr a)))
	  (error 'srep->attribute "expected (list Symbol String) given ~e" a))
	(make-attribute 'scheme 'scheme (car a) (cadr a)))

      ;; xexpr->xml : Xexpr -> Content
      ;; The contract is enforced.
      (define (xexpr->xml x)
        (cond
          [(pair? x)
          (let ([f (lambda (atts body)
                     (unless (list? body)
                       (error 'xexpr->xml
                              "expected a list of xexprs for the body in ~e"
                              x))
                     (make-element 'scheme 'scheme (car x)
                                   atts
                                   (map xexpr->xml body)))])
            (if (and (pair? (cdr x))
                     (or (null? (cadr x))
                         (and (pair? (cadr x)) (pair? (caadr x)))))
              (f (map srep->attribute (cadr x)) (cddr x))
              (f null (cdr x))))]
          [(string? x) (make-pcdata 'scheme 'scheme x)]
          [(or (symbol? x) (and (integer? x) (>= x 0)))
          (make-entity 'scheme 'scheme x)]
          [(or (comment? x) (pi? x)) x]
          [else ;(error 'xexpr->xml "malformed xexpr ~s" x)
          x]))

      ;; xexpr->string : Xexpression -> String
      (define (xexpr->string xexpr)
        (let ([port (open-output-string)])
          (write-xml/content (xexpr->xml xexpr) port)
          (get-output-string port)))
      
      ;; bcompose : (a a -> c) (b -> a) -> (b b -> c)
      (define (bcompose f g)
	(lambda (x y) (f (g x) (g y)))))))
