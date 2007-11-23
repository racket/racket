; Module header is generated automatically
#cs(module id mzscheme
(require "common.ss")
(require "myenv.ss")
(require "access-remote.ss")
(require "sxpathlib.ss")

;; Creation and manipulation of the ID-index
;; Provides the DTD parser for extracting ID attribute declarations
;
; This software is in Public Domain.
; IT IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND.
;
; Please send bug reports and comments to:
;   lizorkin@hotbox.ru    Dmitry Lizorkin
;
; ID-index provides fast access to XML elements by their unique ID.
; ID-index has the following structure:
;  id-index = ( (id . element) (id . element) ... )
; i.e.
;  id-index = (list
;               (cons id element)
;               (cons id element)
;              ...)
; where
;  id - (a string) element's unique ID
;  element - an SXML presentation of an element
;
; Creation of an id-index generally consists of two steps.
; On the first step, a document declaration (internal and external DTD)
; is read and information of ID attributes is extracted.
; This is presented in a following form:
;  id-attrs = ( (elem-name attr-name attr-name attr-name ...)
;               (elem-name attr-name attr-name attr-name ...) ... )
; i.e.
;  id-attrs = (list
;               (cons
;                 elem-name
;                 (list attr-name attr-name attr-name ...)
;               (cons
;                 elem-name
;                 (list attr-name attr-name attr-name ...)
;               ...)
; where
;  elem-name - (a symbol) a name of the element
;  attr-name - (a symbol) element's attribute having an ID type
;
; On the second step, if an SXML presentation of the document is available,
; 'id-attrs' are used for forming an 'id-index'.
; If there is no SXML presentation for a document yet, both steps are
; performed as a single function call - to a specialized SSAX parser.
; This parser constructs an SXML presentation and an 'id-index'
; in a single pass
;
; ATTENTION:
; 1. Only non-qualified 'elem-name' and 'attr-name' are correctly supported
; 2. Parameter entity reference (PEReference) is NOT supported

;=========================================================================
; Functions which read XML document declaration

;------------------------------------------------
; Trivial functions that ignore symbols

; Function reads a whitespace (S production)
(define (id:process-s port)
  (let ((symb (peek-char port)))
    (cond((eof-object? symb) symb)
         ((char=? symb #\space) (read-char port)
                                (id:process-s port))
         ((char=? symb #\return) (read-char port)
                                 (id:process-s port))
         ((char=? symb #\newline)(read-char port)
                                 (id:process-s port))
         ((char=? symb #\tab)(read-char port)
                             (id:process-s port))
         (else symb))))


; Ignores all symbols until template-symbol
(define (id:ignore-until templ-sym port)
  (let loop ((symb (peek-char port)))
    (cond((eof-object? symb) symb)
         ((equal? symb templ-sym) (read-char port)
                                  symb)
         (else (read-char port)
               (loop (peek-char port))))))


;------------------------------------------------
; These functions perform reading from a file

; Read N symbols from a port
(define (id:read-n num port)
  (id:process-s port)
  (let loop ((num num) (res '()))
    (if(= num 0) 
       (list->string (reverse res))
       (let((symb (peek-char port)))
         (cond((eof-object? symb) symb)
              (else (read-char port)
                    (loop (- num 1) (cons symb res))))))))


; This function reads a name - a sequence of characters ending with
; a whitespace or '<'. '>', '(', ')', '[', ']', '|'
(define (id:read-name port)
  (id:process-s port)
  (let loop ((res ""))
    (let ((symb (peek-char port)))
      (cond((eof-object? symb) res)
           ((member symb '(#\space #\tab #\return #\newline
                           #\< #\> #\( #\) #\[ #\] #\|)) 
                    res)
           (else (loop (string-append res (string (read-char port)))))))))


; This function reads a literal
;  literal   ::=    ('"' [^"]* '"') | ("'" [^']* "'")  
; A string is returned 
(define (id:process-literal port)
  (id:process-s port)
  (let((quot (peek-char port)))
    (if(eof-object? quot)  ; an incorrect situaltion
       ""
       (let((quot (if (char=? (read-char port) #\") #\" #\')))
         (let loop ((res '()))
           (let((symb (peek-char port)))
             (cond
               ((eof-object? symb) (list->string (reverse res)))
               ((char=? symb quot)  ; end of the string
                (read-char port)
                (list->string (reverse res)))
               (else
                (read-char port)
                (loop (cons symb res))))))))))
         

;------------------------------------------------
; Miscellaneous

; Converts a string into small letters
(define (id:to-small str)
  (let loop ((arg (string->list str)) (res '()))
    (cond((null? arg) (list->string (reverse res)))
         ((char-upper-case? (car arg))
           (loop (cdr arg) (cons (char-downcase (car arg)) res)))
         (else (loop (cdr arg) (cons (car arg) res))))))


; Takes an 'id-attrs' which can contain equal element names
; Returns a new 'id-attrs' where all element names are unique
(define (id:unite-id-attrs id-attrs)
  (let loop ((id-attrs id-attrs)
             (new '()))
    (if
     (null? id-attrs)
     new
     (let rpt ((elem-name (caar id-attrs))
               (atts (cdar id-attrs))
               (rest (cdr id-attrs))
               (id-attrs '()))
       (cond
         ((null? rest) 
          (loop id-attrs (cons (cons elem-name atts) new)))
         ((equal? (caar rest) elem-name)
          (rpt elem-name 
               (append atts (cdar rest))
               (cdr rest)
               id-attrs))
         (else
          (rpt elem-name atts (cdr rest) (cons (car rest) id-attrs))))))))
    
  
;------------------------------------------------
; Parsing XML productions concerning document declaration
; These functions are not intendes for error detection, they assume that
; the document is correct

; This function ignores information related to a PI production [16]
; [16]    PI    ::=    '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>' 
; It looks for an ending '?>' template
(define (id:ignore-PI port)
  (id:ignore-until #\? port)
  (let ((symb (peek-char port)))
    (cond((eof-object? symb) symb)
         ((equal? symb #\>) (read-char port)
                            symb)
         (else (id:ignore-PI port)))))


; This function ignores information related to a Comment production [15]
; [15]    Comment    ::=    '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->' 
; The starting '<!' has been already processed
; The function looks for an ending '-->' template 
(define (id:ignore-comment port)
  (read-char port)   ; it is '-'
  (read-char port)   ; it is '-'
  (id:ignore-until #\- port)
  (let((sym1 (peek-char port)))
    (cond((eof-object? sym1) sym1)
         ((char=? sym1 #\-) (read-char port)
                            (let((sym2 (read-char port)))  ; must be '>'
                              sym2))
         (else (id:ignore-comment port)))))


; This function processes AttType production ([54]-[59] in XML specification)
; [54]  AttType    ::=    StringType | TokenizedType | EnumeratedType  
; [55]  StringType    ::=    'CDATA' 
; [56]  TokenizedType    ::=    'ID' | 'IDREF' | 'IDREFS' | 'ENTITY'
;                               | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS' 
; [57]  EnumeratedType    ::=    NotationType | Enumeration  
; [58]  NotationType    ::=  'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
; [59]  Enumeration    ::=  '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')' 
; The function returnd #t if the attribute has an ID type and #f otherwise
(define (id:AttType-ID? port)
  (let((type (id:to-small (id:read-name port))))
    (cond((string=? type "id") #t)
         ((string=? type "notation")
           (id:process-s port)
           (read-char port)  ; it is #\(
           (id:ignore-until #\) port)
           #f)
         ((and (string=? type "") (char=? (peek-char port) #\())   ; see [59]
           (id:ignore-until #\) port)
           #f)
         (else #f))))


; This function processes DefaultDecl production ([60] in XML specification)
; [60]    DefaultDecl    ::=    '#REQUIRED'
;                               | '#IMPLIED'
;                               | (('#FIXED' S)? AttValue)
; The result is always #t
(define (id:process-DefaultDecl port)
  (let((type (id:to-small (id:read-name port))))
    (cond((string=? type "#fixed")
           (id:read-name port)   ; reads a default value
           #t)  
         (else #t))))


; This function processes AttDef production ([53] in XML specification)
; [53]    AttDef    ::=    S Name S AttType S DefaultDecl
; If an attribute has an ID type, (list attribule-name) is returned
; (a list of one element). Otherwise, function returns an empty list
(define (id:process-AttDef port)
  (let((att-name (string->symbol (id:read-name port))))
    (let((bool (id:AttType-ID? port)))
      (id:process-DefaultDecl port)
      (if bool (list att-name) '()))))
                

; The function processes AttlistDecl production ([52] in XML specification)
; [52]    AttlistDecl    ::=    '<!ATTLIST' S Name AttDef* S? '>'
; The starting '<!ATTLIST' has been already processed
; 'id-attrs' are returned as a result
(define (id:process-AttlistDecl port)
  (let((element-name (string->symbol (id:read-name port))))
    (let loop ((atts '()))
      (id:process-s port)
      (cond((char=? (peek-char port) #\>) ; no more attributes will be declared
             (read-char port)
             (if(null? atts)
                '()
                (list (cons element-name atts))))
           (else
             (loop (append (id:process-AttDef port) atts)))))))


; This function processes a multiple markupdecl production [29]
; [29]    markupdecl    ::=    elementdecl | AttlistDecl | EntityDecl 
;                              | NotationDecl | PI | Comment
; 'id-attrs' are returned as a result
(define (id:process-markupdecl* port)
  (let loop ((id-attrs '()))
    (let((beg (id:read-n 2 port)))
      (cond((eof-object? beg) id-attrs)   ; the file is over
           ((string=? beg "]>") id-attrs)   ; the end of the markupdecl
           ((string=? beg "<?")   ; processing instruction
             (id:ignore-PI port)
             (loop id-attrs))
           ((and (string=? beg "<!") (char=? (peek-char port) #\-)) ; a comment
             (id:ignore-comment port)
             (loop id-attrs))
           ((string=? beg "<!")   ; AttlistDecl or any other declarations
             (let ((name (id:to-small (id:read-name port))))
               (cond((string=? name "attlist") 
                      (loop (append (id:process-AttlistDecl port) id-attrs)))
                    (else 
                      (id:ignore-until #\> port)
                      (loop id-attrs)))))
           (else   ; an error condition
             (cerr "Error in markupdecl production: unexpected " beg nl)
             (id:ignore-until #\> port)
             id-attrs)))))
                 

; This function processes a doctypedecl production ([75] in XML specification)
; [75]    ExternalID    ::=    'SYSTEM' S SystemLiteral 
;                              | 'PUBLIC' S PubidLiteral S SystemLiteral
; The function ignores a PubidLiteral
; 'id-attrs' are returned as a result
(define (id:process-ExternalID port)
  (let((system-literal
        (let((name (id:to-small (id:read-name port))))
          (cond
            ((string=? name "system")
             (id:process-literal port))
            ((string=? name "public")
             (id:process-literal port)
             (id:process-literal port))
            (else #f)))))
    (if(not system-literal)
       '()  ; an incorrect situation
       (let((external-port (open-input-resource system-literal)))
         (if(not external-port)
            '()   ; a failure
            (let((id-attrs (id:process-markupdecl* external-port)))
              (close-input-port external-port)
              id-attrs))))))
         

; This function processes a doctypedecl production ([28] in XML specification)
; [28]    doctypedecl    ::=    '<!DOCTYPE' S Name (S ExternalID)? 
;                               S? ('[' (markupdecl | DeclSep)* ']' S?)? '>'
; The function doesn't process a DeclSep (this is a PEReference which 
; this programme doesn't support)
; The starting '<!DOCTYPE' has been already processed
; 'id-attrs' are returned as a result
(define (id:process-doctypedecl port)
  (let((name (id:read-name port)))  ; root element's name
    (id:process-s port)
    (let((symb (peek-char port)))
      (cond
        ((eof-object? symb) '())  ; an incorrect situation
        ((char=? symb #\[)
         (read-char port)
         (id:process-markupdecl* port))
        (else
         (let((id-attrs (id:process-ExternalID port)))
           (id:process-s port)
           (let((symb (peek-char port)))
             (cond
               ((eof-object? symb) id-attrs)  ; an incorrect situation
               ((char=? symb #\[)
                (read-char port)
                (append id-attrs (id:process-markupdecl* port)))
               (else  ; an incorrect situation
                id-attrs)))))))))
                   

; This function processes a prolog production ([22] in XML specification)
; [1]    document    ::=    prolog element Misc*
; [22]    prolog    ::=    XMLDecl? Misc* (doctypedecl Misc*)? 
; [23]    XMLDecl    ::=    '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
; [27]    Misc    ::=    Comment | PI | S 
; 'id-attrs' are returned as a result
(define (id:process-prolog port)
  (let((beg (id:read-n 2 port)))
    (cond((eof-object? beg) '())  ; a file is over - strange...
         ((string=? beg "<?")   ; PI or XMLDecl
          (id:ignore-PI port)
          (id:process-prolog port))
         ((and (string=? beg "<!") (char=? (peek-char port) #\-))   ; a comment
          (id:ignore-comment port)
          (id:process-prolog port))
         ((string=? beg "<!")   ; doctypedecl expected
          (let ((name (id:to-small (id:read-name port))))
            (cond((string=? name "doctype") 
                  (id:process-doctypedecl port))
                 (else 
                  (cerr "doctypedecl production expected" nl)
                  '()))))
         (else   ; element begins, there was no doctypedecl
          '()))))



;=========================================================================
; Two-step id-index creation (user level functions)
; We use this variant when we already have an SXML presentation of the
; document

;------------------------------------------------
; The first step - creating 'id-attrs'

; Read the DTD
;  uri-string - a URI for the DTD location (a string) 
; 'id-attrs' are returned as a result
(define (id:read-external-dtd uri-string)
  (let((port (open-input-resource uri-string)))
    (if(not port)
       '()  ; a situation of an error
       (let((id-attrs (id:unite-id-attrs (id:process-markupdecl* port))))
         (close-input-port port)
         id-attrs))))


; Read the declaration from the document's prolog. 
; If prolog contains a reference to an external DTD, it is processed either
;  uri-string - a URI for the document location (a string)
; 'id-attrs' are returned as a result
(define (id:read-document-declaration uri-string)
  (let((port (open-input-resource uri-string)))
    (if(not port)
       '()  ; a situation of an error
       (let((id-attrs (id:unite-id-attrs (id:process-prolog port))))
         (close-input-port port)
         id-attrs))))


;------------------------------------------------
; The second step - creating an 'id-index' using 'id-attrs'

; This function forms an 'id-index' and inserts it in the document
;  document - a root node of the document (SXML presentation)
;  id-attrs - the result of the previous step
; A new SXML document is returned. It contains an auxiliary list with an
; 'id-index subtree. If the source document already contains such a
; subtree, it will be replaced. Other subtrees in an auxiliary list will
; remain unchanged.
(define (SXML->SXML+id document id-attrs)
  (let((aux-subtrees
        (let((aux ((select-kids (ntype?? '@@)) document)))
          (if(null? aux)
             '()
             (let rpt ((res '())
                       (to-see (cdar aux)))
               (cond
                 ((null? to-see) (reverse res))
                 ((equal? (caar to-see) 'id-index) (rpt res (cdr to-see)))
                 (else (rpt (cons (car to-see) res)
                            (cdr to-see)))))))))
    (let loop ((nodeset (list document))
               (id-index '()))
      (if(null? nodeset)
         (let((kids ((select-kids
                      (lambda (node)
                        (not (and (pair? node) (equal? (car node) '@@)))))
                     document)))
           (cons* '*TOP*
                  (cons* '@@
                         (cons 'id-index id-index)
                         aux-subtrees)
                  kids))         
         (let((cur-node (car nodeset)))
           (cond
             ((not (pair? cur-node))  ; a text node
              (loop (cdr nodeset) id-index))
             ((assoc (car cur-node) id-attrs)
              =>
              (lambda (lst)
                (let((id-values
                      ((select-kids (lambda (x) #t))
                       ((sxml:filter (lambda (x) (member (car x) (cdr lst))))
                        ((select-kids (lambda (x) #t))
                         ((select-kids (ntype?? '@)) cur-node))))))
                  (loop
                   (append 
                    ((select-kids (ntype?? '*)) (car nodeset))
                    (cdr nodeset))
                   (append
                    id-index
                    (map
                     (lambda (x) (cons x cur-node))
                     id-values))))))
             (else
              (loop
               (append ((select-kids (ntype?? '*)) (car nodeset)) (cdr nodeset))
               id-index))))))))



;=========================================================================
; Some stuff for a SSAX multi parser

;------------------------------------------------
; Id-related part of the seed
;  id:seed = (list  id-attrs  id-index)
;  id-attrs, id-index - see a head comment

; Mutator
(define (id:make-seed id-attrs id-index)
  (list id-attrs id-index))


; Accessors
(define (id:seed-attrs id:seed)
  (car id:seed))
        
(define (id:seed-index id:seed)
  (cadr id:seed))
        

;------------------------------------------------
; Handler units

; This function is called by the NEW-LEVEL-SEED handler
; A new 'id:seed' is returned
(define (id:new-level-seed-handler id:seed)
  id:seed)


; This function is called by the FINISH-ELEMENT handler
; A new 'id:seed' is returned
(define (id:finish-element-handler elem-gi attributes id:seed element)
  (cond
    ((assoc elem-gi (id:seed-attrs id:seed))
     => 
     (lambda (lst)
       (let loop ((atts attributes)
                  (id-index (id:seed-index id:seed)))
         (if
          (null? atts)
          (id:make-seed (id:seed-attrs id:seed) id-index)
          (let((att (car atts)))
            (cond
              ((pair? (car att))  ; namespace aware
               (loop (cdr atts) id-index))
              ((member (car att) (cdr lst))
               (loop (cdr atts)
                     (cons (cons (cdr att) element) id-index)))
              (else
               (loop (cdr atts) id-index))))))))
    (else
     id:seed)))

         
; This function is called by the DOCTYPE handler
; A new 'id:seed' is returned
(define (id:doctype-handler port systemid internal-subset?)
  (let((id-attrs
        (if
         (not systemid)
         '()   ; systemid not supplied
         (let((external-port (open-input-resource systemid)))
           (if
            (not external-port)
            '()   ; a failure
            (let((id-attrs (id:process-markupdecl* external-port)))
              (close-input-port external-port)
              id-attrs))))))
    (let((id-attrs
          (if
           internal-subset?
           (id:unite-id-attrs 
            (append id-attrs (id:process-markupdecl* port)))
           (id:unite-id-attrs id-attrs))))
      (id:make-seed id-attrs '()))))


; This function constructs the member of an axuiliary list
(define (id:ending-action id:seed)
  (let((id-index (id:seed-index id:seed)))
    (cons 'id-index id-index)))

                      


(provide (all-defined)))
