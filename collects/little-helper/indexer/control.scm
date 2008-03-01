(module control mzscheme
  (provide delete-and-generate-scribblings-indices)
  
  (require scheme/file
           scheme/path
           scheme/system
           (only scheme filter)
           srfi/42
           "planet/basename.scm"
           "config.scm"
           "indexer.scm"
           "lexer.scm"
           "lexicon.scm"
           ; "query.scm" ; for debug
           (only "sort.scm" disk-sort) ; (planet "sort.scm" ("soegaard" "disk.plt"))
           )
  
  ;;;
  ;;; FILES
  ;;;
  
  (define (html-file? file)
    (equal? (filename-extension file) #"html"))
  
  (define (indexable-file? path)
    (define (special-format? file)
      (with-input-from-file file
        (lambda () (equal? (peek-string 3 0) "WXM"))))
    (let ([basename-str (path->string (basename path))]
          [ext          (filename-extension path)])
      (and (member ext '(#"ss" #"scm" #"txt" #"html"))
           (not (regexp-match "_html.txt" basename-str))
           (not (special-format? path)))))
  
  (define (indexable-files-in-dir dir)
    (filter indexable-file? (find-files file-exists? dir)))
  
  (define (html-file->txt-file src-file dest-file)
    ; TODO: Figure out whether w3m is in path, and write a fall back
    (system (format "w3m -dump '~a' >'~a'" src-file dest-file)))
  
  ;;;
  ;;; INDEX -> DISK-LEXICON
  ;;;
  
  ; in case the represenation changes
  (define write-term write)
  (define write-number write)
  (define read-term read)
  (define read-number read)
  
  (define (bytes->string b)
    (bytes->string/utf-8 b #\space))
  
  (define (index->disk-lexicon index file block-size)
    (let ([lexicon   (index-lexicon index)]
          [positions (index-positions index)])
      ; 1. Write all term/term-number pairs to file
      (call-with-output-file file
        (λ (out)
          (let ([numbers (lexicon-term-numbers lexicon)])
            (do-ec (: i 0 (vector-length numbers))
                   (begin
                     (write-term (bytes->string (vector-ref numbers i)) out)
                     (write-number (vector-ref positions i) out)))))
        'truncate)
      ; 2. Sort the file
      (let ()
        (define (read-record port)
          (let* ([t (read-term port)]
                 [n (read-number port)])
            (if (or (eof-object? t)
                    (eof-object? n))
                (read (open-input-string ""))  ; = #<eof>
                (cons t n))))
        (define (write-record record port)
          (write-term (car record) port)
          (write-number (cdr record) port))
        (define (record<? r1 r2)
          (string<? (car r1) (car r2)))
        (disk-sort file file block-size read-record write-record record<?))))
  
  ;;;
  ;;; GENERATE INDEX
  ;;;
  
  (when (not (directory-exists? the-index-dir))
    (make-directory* the-index-dir))
  
  (define (lexer file f)
    (let ([file-to-lex (if (html-file? file)
                           (path-add-suffix file ".txt")
                           file)])
      (when (html-file? file)
        (html-file->txt-file file file-to-lex))
      (for-each-token-in-file 
       file-to-lex (λ (term+pos) (f (car term+pos) (cadr term+pos))))))
  
  (define (generate-index dir-to-index index-name sensitive?)
    ; call to regenerate the index
    (parameterize ([token-case-sensitive sensitive?])
      (make-directory* (current-data-directory))
      (make-directory* (build-path (current-data-directory) index-name))
      
      (let ([index-path          (build-path (current-data-directory) index-name "index.txt")]
            [inverted-path       (build-path (current-data-directory) index-name "inverted.txt")]
            [lexicon-path        (build-path (current-data-directory) index-name "lexicon.txt")]
            [lexicon-index-path  (build-path (current-data-directory) index-name "lexicon-index.txt")])
        ; 1. Index directory, that is generate the inverted file
        (define the-index (time (index-files (indexable-files-in-dir dir-to-index) inverted-path index-name lexer)))
        ; 2. Generate the disk lexicon
        (index->disk-lexicon the-index lexicon-path (current-lexicon-block-size))
        ; 3. Generate the disk lexicon index
        (create-disk-index lexicon-path lexicon-index-path (current-lexicon-block-size))
        ; 4. Delete the during-construction lexicon from index, and save it
        (set-index-lexicon! the-index "")
        (set-index-positions! the-index "")
        (save-index index-path the-index 'replace))))
  
  (define (generate-scribblings-doc-data)
    (generate-index the-repository-path "sensitive" #t)
    (generate-index the-repository-path "insensitive" #f))
  
  (define (delete-and-generate-scribblings-indices)
    (delete-index the-index-path)
    (generate-scribblings-doc-data))  
  
  ; TEST QUERIES 
  #; (begin
       (print-struct #t)
       (define the-index (load-index (build-path (current-data-directory) (current-index-name) "index.txt")))
       (require (all-except "query.scm" bytes->string))
       
       (search "list")
       (define (s term-bytestring)
         (inverted-list->human the-index (search term-bytestring)))
       (display (s "list"))
       
       (define (q query-string)
         ; (query index query-string sensitive? contain-all-terms type-normal)
         (query the-index query-string #t #f #t))
       (q "list"))
  )