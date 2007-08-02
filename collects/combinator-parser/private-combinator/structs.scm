(module structs mzscheme
  
  (provide (all-defined-except make-fail-type))
  
  (require (lib "force.ss" "lazy")
           (lib "lex.ss" "parser-tools"))
    
  ;fail-src: (list line col pos span loc)

  ;make-src-lst: position position -> src-list
  (define (make-src-lst start end)
    (list (!!! (position-line start))
          (!!! (position-col start))
          (!!! (position-offset start))
          (- (!!! (position-offset end))
             (!!! (position-offset start)))))
  
  ;(make-fail-type float fail-src string int int)
  (define-struct fail-type (chance src name used may-use) (make-inspector))
  ;(make-terminal-fail float fail-src string symbol 'a)
  (define-struct (terminal-fail fail-type) (kind found))
  ;(make-sequence-fail float fail-src string symbol (list string) string 'a boolean string)
  (define-struct (sequence-fail fail-type) (id kind correct expected found repeat? last-seen))
  ;(make-choice-fail float fail-src string int (list string) (list fail-type) boolean)
  (define-struct (choice-fail fail-type) (options names ended? messages) (make-inspector))
  ;(make-options-fail float #f #f (list fail-type))
  (define-struct (options-fail fail-type) (opts))
  
  (define (!!!-fail fail)
    (let*-values ([(chance src name used may-use)
                   (if (fail-type? fail)
                       (values (!!! (fail-type-chance fail))
                               (!!! (fail-type-src fail))
                               (!!! (fail-type-name fail))
                               (!!! (fail-type-used fail))
                               (!!! (fail-type-may-use fail)))
                       (values #f #f #f #f #f))])
      (cond
        [(terminal-fail? fail) 
         (make-terminal-fail chance src name used may-use
                             (!!! (terminal-fail-kind fail))
                             (!!! (terminal-fail-found fail)))]
        [(sequence-fail? fail) 
         (make-sequence-fail chance src name used may-use
                             (!!! (sequence-fail-id fail))
                             (!!! (sequence-fail-kind fail))
                             (!!! (sequence-fail-correct fail))
                             (!!! (sequence-fail-expected fail))
                             (!!!-fail (sequence-fail-found fail))
                             (!!! (sequence-fail-repeat? fail))
                             (!!! (sequence-fail-last-seen fail)))]
        [(choice-fail? fail) 
         (make-choice-fail chance src name used may-use
                           (!!! (choice-fail-options fail))
                           (!!! (choice-fail-names fail))
                           (!!! (choice-fail-ended? fail))
                           (map !!!-fail (!!! (choice-fail-messages fail))))]
        [(options-fail? fail)
         (make-options-fail chance src name used may-use
                            (map !!!-fail (!!! (options-fail-opts fail))))]         
        [else (!!! fail)])))
       
    
                         
  
  ;result = res | choice-res | repeat-res | (listof (U res choice-res))
  
  ;(make-res (U #f (listof 'b)) (listof 'a) (U string fail-type) (U string 'a) int) [U #f fail-type] token
  (define-struct res (a rest msg id used possible-error first-tok) (make-inspector))
  ;make-choice-res string (listof res fail-type)
  (define-struct choice-res (name matches errors) (make-inspector))
  ;(make-repeat-res answer (U symbol fail-type))
  (define-struct repeat-res (a stop) (make-inspector))
  
  (define (fail-res rst msg) (make-res #f rst msg "" 0 #f #f))
  
)
