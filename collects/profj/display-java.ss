(module display-java scheme/base
  
  (require scheme/class
           mred
           framework
           profj/libs/java/lang/Object
           profj/libs/java/lang/String
           profj/libs/java/lang/Throwable
           profj/libs/java/lang/array)
  
  (provide format-java-value make-format-style make-java-snip)
  
  
;                                                                                  
;                                                                                  
;                                                                                  
;   ######                                                     #                   
;    #   #                                   #       #                             
;    # #     &##&   ## $#$ ##*# *#*  $@#$:  #####   #####    :##    ##*##*   $#@ ##
;    ###    &+  +&   #$* :  #+*#$*#     -#   #       #         #     #+ *#  $+  +# 
;    # #    #    #   #      #  #  #  $##$#   #       #         #     #   #  #    # 
;    #      #    #   #      #  #  # @+   #   #       #         #     #   #  #    # 
;    #      &+  +&   #      #  #  # #-  +#   #* :$   #* :$     #     #   #  &+  +# 
;   ###      &##&   #####  ### ## # *##$ ##  *##$    *##$    #####  ### ###  $#@ # 
;                                                                               -@ 
;                                                                            $##$  
;                                                                                  
  
  (define-struct format-style (print-full? display multi-line?))
  
  ;format-java-value: value format-style -> (listof (U string snip%))
  (define (format-java-value value style)
    (internal-format value 
                     (format-style-print-full? style) 
                     (format-style-display style)
                     null
                     (format-style-multi-line? style)
                     0))
  
  ;internal-format: value boolean symbol (listof value) boolean int -> (listof (U string snip%))
  (define (internal-format value full-print? style already-printed newline? num-tabs)
    (cond
      ((null? value) '("null"))
      ((number? value) (list (format "~a" value)))
      ((char? value) (list (format "'~a'" value)))
      ((boolean? value) (list (if value "true" "false")))
      ((is-java-array? value)
       (if full-print?
           (format-array->list value (send value length) -1 #t style already-printed newline? num-tabs)
           (format-array->list value 3 (- (send value length) 3) #f style already-printed newline? num-tabs)))
      ((is-a? value String) (list (format "~v" (send value get-mzscheme-string))))
      ((string? value) (list (format "~v" value)))
      ((java:exception? value) (internal-format (java:exception-object value) full-print?
                                                style already-printed newline? num-tabs))
      ((or (is-a? value ObjectI) (supports-printable-interface? value))
       (cond 
         ((and (equal? "Image" (send value my-name))
               (object-method-arity-includes? value 'Image-constructor-dynamic 1)
               (object-method-arity-includes? value 'movePinhole-graphics.Posn 1))
          (list (cadr ((send value fields-for-display)))))
         (else
          (if (memq value already-printed)
              (list (send value my-name))
              (case style
                ((type) (list (send value my-name)))
                ((field)
                 (let* ((retrieve-fields (send value fields-for-display))
                        (st (format "~a(" (send value my-name)))
                        (new-tabs (+ num-tabs 3))
                        (fields null))
                   (let loop ((current (retrieve-fields)))
                     (let ((next (retrieve-fields)))
                       (when current
                         (set! fields 
                               (append fields 
                                       (cons
                                        (format "~a~a = "
                                                (if newline? (if (eq? fields null)
                                                                 (format "\n~a" (get-n-spaces new-tabs))
                                                                 (get-n-spaces new-tabs)) "")
                                                (car current))
                                        (append
                                         (if (memq (cadr current) already-printed)
                                             (internal-format (cadr current) full-print? 'type already-printed #f 0)
                                             (internal-format (cadr current) full-print? style 
                                                               (cons value already-printed) newline?
                                                               (if newline? 
                                                                   (+ new-tabs (if (string? (car current))
                                                                                   (string-length (car current)) 1) 3)
                                                                   num-tabs)))
                                         (list (format "~a~a" 
                                                       (if next "," "")
                                                       (if newline? "\n" " ")))))))
                         (loop next))))
                   (cons st 
                         (append
                          (if (> (length fields) 1) 
                              (reverse (cdr (reverse fields))) null) (list ")")))))
                (else (list (send value my-name))))))))
          (else (list value))))

  ;format-array->list: java-value int int bool symbol (list value) -> (list val)
  (define (format-array->list value stop restart full-print? style already-printed nl? nt)
    (letrec ((len (send value length))
             (make-partial-string
              (lambda (idx first-test second-test)
                (cond
                  ((first-test idx) (list ""))
                  ((second-test idx)
                   (append (internal-format (send value access idx) full-print? style already-printed nl? nt)
                           (make-partial-string (add1 idx) first-test second-test)))
                  (else
                   (append (internal-format (send value access idx) full-print? style already-printed nl? nt)
                           (if nl? (list "\n") (list " "))
                           (make-partial-string (add1 idx) first-test second-test)))))))
      (if (or full-print? (< restart stop))
          (append '("[") (make-partial-string 0 (lambda (i) (>= i len)) (lambda (i) (= i (sub1 len)))) '("]"))
          (append '("[")                      
                  (make-partial-string 0 (lambda (i) (or (>= i stop) (>= i len))) (lambda (i) (= i (sub1 stop))))
                  (if nl? (list "\n") (list ""))
                  '(" ... ")
                  (if nl? (list "\n") (list ""))
                  (make-partial-string restart (lambda (i) (>= i len)) (lambda (i) (= i (sub1 len))))
                  '("]")))))
  
  (define (get-n-spaces n)
    (cond
      ((= n 0) "")
      (else (string-append " " (get-n-spaces (sub1 n))))))
  
  (define (supports-printable-interface? o)
    (and (is-a? o object%)
         (method-in-interface? 'my-name (object-interface o))
         (method-in-interface? 'fields-for-display (object-interface o))))
;                                  
;                                  
;                                  
;    $#@*#             #           
;   @   :#                         
;   @+      ##*##*   :##    ##:#@  
;    $@##    #+ *#     #     #* -$ 
;       +$   #   #     #     #   # 
;        #   #   #     #     #   # 
;   #$+ :$   #   #     #     #: -$ 
;   #*@#$   ### ###  #####   # #@  
;                            #     
;                           ###    

  (define (make-java-snip value style)
    (let* ((formatted-java (format-java-value value style))
           (editor (new (editor:standard-style-list-mixin text%)))
           (snip (new editor-snip% (editor editor)
                      (with-border? #f))))
      (when (> (total-length formatted-java) 28)
        (set! formatted-java (format-java-value value 
                                                (make-format-style 
                                                 (format-style-print-full? style)
                                                 (format-style-display style)
                                                 #t))))
      (for-each (lambda (i)
                  (send editor insert i))
                formatted-java)
      snip))
  
  (define (total-length lst)
    (cond
      ((null? lst) 0)
      ((string? (car lst)) (+ (string-length (car lst))
                              (total-length (cdr lst))))
      (else (add1 (total-length (cdr lst))))))


  )
