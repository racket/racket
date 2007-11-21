; Module header is generated automatically
#cs(module lazy-ssax mzscheme
(require (lib "string.ss" "srfi/13"))
(require (lib "ssax.ss" "web-server/tmp/ssax"))
(require "sxpathlib.ss")
(require "sxml-tools.ss")
(require "sxpath-ext.ss")
(require "xpath-parser.ss")
(require "txpath.ss")
(require "xpath-ast.ss")
(require "xpath-context_xlink.ss")
(require "lazy-xpath.ss")

;; A specialized lazy XML->SXML parser
; Is heavily based on continuations

;-------------------------------------------------
; Preliminary helper functions

; A helper that forces all descendants of a given node or a nodeset
(define (lazy:force-descendants node)
  (cond
    ((lazy:promise? node)  ; force it
     (lazy:force-descendants (force node)))
    ((pair? node)  ; not null
     (for-each lazy:force-descendants node))
    (else  ; null or not pair
     #t  ; nothing to be done
   )))

; Returns the list containing of all members of the argument list except
; for the last member
(define (lazy:except-last lst)
  (if
    (or (null? lst)  ; this shouldn't happen
        (null? (cdr lst)))
    '()
    (cons (car lst) (lazy:except-last (cdr lst)))))
  
;-------------------------------------------------
;

; Returns the common part of the seed
(define (lazy:seed-common seed)
  ((if (null? (cdr seed))  ; a short seed
       car caddr)
   seed))

; A monad-like handler
; Replaces the common part of the seed
(define (lazy:replace-common seed new-common)
  (if (null? (cdr seed))  ; a short seed
      (list new-common)
      (list (car seed)
            (cadr seed)
            new-common
            (cadddr seed))))

; Produces a lazy SXML document, which corresponds to reading a source
; document in a stream-wise fashion
(define (lazy:xml->sxml port namespace-prefix-assig)
  (let ((namespaces
         (map (lambda (el)
                (cons* #f (car el) (ssax:uri-string->symbol (cdr el))))
              namespace-prefix-assig))
        (RES-NAME->SXML
         (lambda (res-name)
           (string->symbol
            (string-append
             (symbol->string (car res-name))
             ":"
             (symbol->string (cdr res-name)))))))
    ((lambda (result)
       ; We assume that nobody follows the document element       
       (if (null? namespace-prefix-assig)
           (cons '*TOP* (lazy:except-last result))
           (cons
            '*TOP*
            (cons
             `(@@ (*NAMESPACES*
                   ,@(map
                      (lambda (ns) (list (car ns) (cdr ns)))
                      namespace-prefix-assig)))
             (lazy:except-last result)))))
     (call-with-current-continuation   ; we grab the continuation to escape from parsing
      (lambda (result-k)
        ; seed ::= (list result-k state-k common-seed level)
        ; result-k - continuation on what to do with the current result portion
        ; state-k - continuation to return to SSAX state on this level of XML
        ;  tree hierarchy
        ; common-seed - general seed information
        ; level - level of a current node in a tree hierarchy
        ((ssax:make-parser
          NEW-LEVEL-SEED 
          (lambda (elem-gi attributes namespaces expected-content seed)
            ;(pp (cons elem-gi (cadddr seed)))
            (if
             (or (null? (cdr seed))  ; short seed
                 (> (cadddr seed) 3))   ; deep level
             (list '())  ; work like a conventional SSAX parser
             (let ((attrs
                    (attlist-fold
                     (lambda (attr accum)
                       (cons (list 
                              (if (symbol? (car attr)) (car attr)
                                  (RES-NAME->SXML (car attr)))
                              (cdr attr)) accum))
                     '() attributes)))
               (call-with-current-continuation
                (lambda (new-level-k)  ; how to parse next
                  ((car seed)  ; return the result
                   (let ((elem-content
                          ; A promise to continue parsing
                          (call-with-current-continuation  ; where to put the result
                           (lambda (elem-k)
                             (new-level-k
                              (list  ; now form a seed
                               elem-k  ; what to do with result
                               new-level-k   ; SSAX state on this level
                               '()  ; common-seed is empty
                               (+ (cadddr seed) 1)  ; increase level
                               ))))))
                     (append
                      ; Previous string content
                      (ssax:reverse-collect-str-drop-ws (caddr seed))
                      (list
                       (cons
                        (if (symbol? elem-gi) elem-gi
                            (RES-NAME->SXML elem-gi))
                        (if (null? attrs) elem-content
                            (cons (cons '@ attrs) elem-content)))
                       ; The following siblings of this element
                       (delay
                         (call-with-current-continuation  ; where to put the result
                          (lambda (foll-k)
                            ; First we force the parsing of the current element
                            (lazy:force-descendants elem-content)
                            ; Than continue parsing
                            ((cadr seed)  ; recover the parent level of nesting
                             (list
                              foll-k  ; what to do with result
                              (cadr seed)
                              '()  ; common-seed is empty
                              (cadddr seed)  ; the same level for siblings
                              ))))))))))))))
   
	     FINISH-ELEMENT
	     (lambda (elem-gi attributes namespaces parent-seed seed)
               (if
                (null? (cdr seed))  ; a short seed
                (let ((common (ssax:reverse-collect-str-drop-ws
                               (lazy:seed-common seed)))
                      (attrs
                       (attlist-fold
                        (lambda (attr accum)
                          (cons (list 
                                 (if (symbol? (car attr)) (car attr)
                                     (RES-NAME->SXML (car attr)))
                                 (cdr attr)) accum))
                        '() attributes)))
                  (lazy:replace-common
                   parent-seed
                   (cons
                    (cons 
                     (if (symbol? elem-gi) elem-gi
                         (RES-NAME->SXML elem-gi))
                     (if (null? attrs) common
                         (cons (cons '@ attrs) common)))
                    (lazy:seed-common parent-seed))))
                ; Otherwise - just return the remaining character content
                ((car seed)  ; continuation
                 (ssax:reverse-collect-str-drop-ws
                  (lazy:seed-common seed)))))
             
	     CHAR-DATA-HANDLER
	     (lambda (string1 string2 seed)
               ;(pp (list string1 string2 seed))
               (lazy:replace-common
                seed
                (if (string-null? string2)
                    (cons string1 (lazy:seed-common seed))
                    (cons* string2 string1 (lazy:seed-common seed)))))

	     DOCTYPE
	     (lambda (port docname systemid internal-subset? seed)
	       (when internal-subset?
		     (ssax:warn port
			   "Internal DTD subset is not currently handled ")
		     (ssax:skip-internal-dtd port))
	       (ssax:warn port "DOCTYPE DECL " docname " "
		     systemid " found and skipped")
	       (values #f '() namespaces seed))

	     UNDECL-ROOT
	     (lambda (elem-gi seed)
	       (values #f '() namespaces seed))

	     PI
	     ((*DEFAULT* .
		(lambda (port pi-tag seed)
                  (lazy:replace-common
                   seed
                   (cons
                    (list '*PI* pi-tag (ssax:read-pi-body-as-string port))
                    (lazy:seed-common seed))))))
	     )
	    port
            (list  ; form initial seed
             result-k   ; put the result
             (lambda (seed)  ; dummy top-level parser state that produces '()
               ((car seed)  ; where to put the result nodeset
                '()))
             '()
             1  ; level for the document element
             )))))))

(provide (all-defined)))
