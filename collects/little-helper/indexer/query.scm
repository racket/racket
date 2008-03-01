(module query mzscheme
  (require (lib "match.ss")
           (lib "list.ss")
           (lib "67.ss" "srfi")
           
           "planet/bit-io.scm" ; (planet "bit-io.scm" ("soegaard" "bit-io.plt"))
           ; (all-except (planet "42.ss" ("soegaard" "srfi.plt" 1 2)) index)
           (all-except (file "planet/srfi.plt/1/2/42.ss") index)
           
           ; is = integer set
           ; (prefix is: (planet "set.scm" ("soegaard" "galore.plt" 2 2)))
           (prefix is: (file "planet/galore.plt/2/2/set.scm"))
           
           (prefix disk: "lexicon.scm")
           "indexer.scm"
           "lexer.scm"
           "config.scm")
  
  (provide (all-defined))
  
  ;;;
  ;;; BASIC QUERY
  ;;;
  
  ; The basic query is given a single term (byte-string) and an index,
  ; to lookup the term and return the corresponding inverted-list.
  
  ; The inverted list for a term with term number t is
  ;   (t n ((d1 f1) ... (dn fn))
  ; where t  is the term number,
  ;       n  is the number of documents the term occurs in,
  ;       di is a document number,
  ;       fi is the frequency of the term in document di

  (define (position-of-term index-name term)
    (disk:lookup term 
                 (lexicon-path index-name)
                 (lexicon-index-path index-name)
                 (current-lexicon-block-size)))
  
  ; search : byte-string -> inverted-list
  (define (search term)
    (let* ([index-name (current-index-name)]
           [pos (position-of-term index-name term)])
      (if pos
          (with-input-from-bit-file (inverted-path index-name)
            (λ ()
              (bit-file-position (current-input-bit-port) pos)
              (read-inverted-list)))
          #f)))
  
  (define (inverted-list->human index il)
    (if (not il)
        #f
        (match il
          [(t n dfs)
           (list ; (term-number->term index t)
                 n
                 (map (λ (df)
                        (match df
                          [(d f) (list (document-path (hash-table-get (index-documents index) d))
                                       f)]))
                      dfs))])))
  
  ;;;
  ;;; CONJUCTIVE QUERY
  ;;;
  
  ; search-or : (list terms) -> (list document-number)
  ;   find documents that contain at least one given term
  (define (search-or list-of-terms ils)
    ; inverted-list -> set of document numbers
    (define (il->is:ds il) (is:list->set integer-compare (map first (third il))))
    (cond
      ; 0 terms
      [(null? list-of-terms)
       '()]
      ; 1 term
      [(null? (cdr list-of-terms))
       (cond
         [(search (car list-of-terms))
          => inverted-list->document-numbers]
         [else
          '()])]
      [else
       ; 2 or more terms
       (let (; ignore terms with no documents
             [ils (filter (λ (x) x) ils)])
         ; calculate union 
         (is:elements
          (foldl (λ (il cds) (is:union cds (il->is:ds il)))
                 (il->is:ds (first ils))
                 (rest ils))))]))
  
  
  ; search-and : (list terms) -> (list document-number)
  ;   find documents containing all given terms
  (define (search-and list-of-terms ils)
    (define (il->is:ds il) (is:list->set integer-compare (map first (third il))))
    (cond
      [(null? list-of-terms)       
       '()]
      [(null? (cdr list-of-terms))
       (cond
         [(search (car list-of-terms))
          => inverted-list->document-numbers]
         [else
          '()])]
      [else
       ; for each term find the inverted list
       (cond
         [(not (andmap (λ (x) x) ils))
          '()]
         [else
          ; order the inverted list, so the rarest terms are first
          ; (this speeds up the intersection operation)
          (let ([ils (quicksort ils (λ (il1 il2) (< (second il1) (second il2))))])
            ; find the intersection of all document document numbers
            (is:elements
             (foldl (λ (il cds)
                      (is:intersection cds (il->is:ds il)))
                    (il->is:ds (first ils))
                    (rest ils))))])]))
  
  (define (inverted-list->document-numbers il)
    (map first (third il)))
  
  (define (bytes->string b)
    (bytes->string/utf-8 b (string-ref " " 0)))
  
  (define (split-in-terms query-string sensitivity)
    ; use whitespace to split the search string
    (let ([terms '()])
      (parameterize ([current-input-port   (open-input-string query-string)]
                     [token-case-sensitive sensitivity])
        (for-each-token
         (λ (t) (set! terms (cons (bytes->string (car t)) terms)))))
      (reverse terms)))

  
  ; The inverted list for a term with term number t is
  ;   (t n ((d1 f1) ... (dn fn))
  ; where t  is the term number,
  ;       n  is the number of documents the term occurs in,
  ;       di is a document number,
  ;       fi is the frequency of the term in document di
  
  (define (inverted-list-term-number il)
    (first il))
  
  (define (inverted-list-number-of-documents il)
    (second il))
  
  (define (inverted-list-document-frequencies il)
    (third il))
  
  
  (define (rank index ils)
    ; ils are the inverted lists for all terms t in the query
    ; ds is a list of document numbers
    
    ; Note: This hash-table relies on eq? on fixnums
    (let ([A (make-hash-table)]
          [N (exact->inexact (number-of-documents index))])
      (do-ec (:list il ils)
             (if il) ; il is #f if the term isn't in the index
             (:let t   (inverted-list-term-number il))
             (:let ft  (inverted-list-number-of-documents il))
             (:let dfs (inverted-list-document-frequencies il))
             (:let wt (+ 1.0 (log (/ N ft))))
             (:list d+f dfs)
             (:let d   (car d+f))
             (:let fdt (cadr d+f))
             (hash-table-put! A d (+ (hash-table-get A d (λ () 0.0))
                                     (* wt (log (+ 1.0 fdt))))))
      
      (let ([documents (index-documents index)])
        (hash-table-map A (λ (d score)
                            (let ([Wd (document-weight (hash-table-get documents d))])
                              (cons d (/ score Wd))))))))
  
  
  ; query-normal : ... -> (list document-number)
  (define (query-normal index query-string sensitive? contain-all-terms)
    ; handle and/or queries
    (let* ([terms (split-in-terms query-string sensitive?)]
           [ils   (map (λ (term) (search term)) terms)])
      (display terms (current-error-port))
      (newline (current-error-port))
      (let ([dss (mergesort (rank index ils)
                            (λ (p1 p2) 
                              (> (cdr p1) (cdr p2))))])
        (let ([result-ds
               (if contain-all-terms
                   ; and
                   (is:list->set integer-compare 
                                 (search-and terms ils))
                   ; or
                   (is:list->set integer-compare 
                                 (search-or terms ils)))])
          (filter (λ (ds)
                    (is:member? (car ds) result-ds))
                  dss)))))
  
  
  ; search-regular : (list terms) -> (list document-number)
  ;   find documents that contain at least one given term, that
  ;   match the regular expression
  (define (search-regular-expression index a-regexp)
    (let (; 1. Compile the regular expression
          [re (regexp a-regexp)]
          ; 2. Find all terms that match the pattern
          [ts '()])
      (display "pattern match: " (current-error-port))
      (display a-regexp (current-error-port)) 
      (newline (current-error-port))
      (display "search-regular-expression: for-each-term-in-lexicon\n" 
               (current-error-port))
      (time (disk:for-each-term-in-disk-lexicon 
             (index-name index)
             (lambda (t n)
               (when (regexp-match re t)
                 (set! ts (cons t ts))))))
      (set! ts (reverse ts))
      ; 3. Convert from terms to document numbers
      (display "search-regular-expression: search for all matches\n" 
               (current-error-port))
      (time (rank index
                  (foldl (lambda (t ils)
                           (let ([il (search t)])        
                             (if il
                                 (cons il ils)
                                 ils)))
                         '() ts)))
      ))
  
  (define (query-regular-expression index a-regexp)
    (search-regular-expression index a-regexp))
  
  (define (query index query-string sensitive? contain-all-terms type-normal)
    (parameterize ([current-index-name (index-name index)])
      ; type-normal = #t   => normal and/or search
      ; type-normal = #f   => regular expression search
      (let ([terms (split-in-terms query-string #t)])
        (values (cond
                  ; query is whitespace only
                  [(null? terms)
                   '()]
                  [type-normal
                   (query-normal index query-string sensitive? contain-all-terms)]
                  [else
                   (query-regular-expression index query-string)])
                terms))))
  
  )