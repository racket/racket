(module linkage mzscheme
  
  (require "planet-shared.ss"
           "../config.ss"
           (lib "file.ss")
           (lib "match.ss"))

  (provide get/linkage
           get-linkage 
           add-linkage!
           remove-linkage-to!
           
           remove-all-linkage!)
  
  ; ==========================================================================================
  ; PHASE 1: LINKAGE
  ; The first check is to see if there is a valid linkage for the module.
  ; ==========================================================================================

  ;; get/linkage : pkg-getter [see ../resolver.ss]
  ;; getter for the linkage table
  (define (get/linkage module-specifier pkg-specifier success-k failure-k)
    (let ([linked-pkg (get-linkage module-specifier pkg-specifier)])
      (if linked-pkg
          (success-k linked-pkg)
          (failure-k 
           void
           (λ (pkg) (add-linkage! module-specifier pkg-specifier pkg))
           (λ (x) x)))))
  
  
  ;; NOTE :: right now we have a nasty situation with the linkage-table: it doesn't associate
  ;; keys to packages, which it seems it should. Instead it associates keys to the arguments
  ;; to the pkg-spec constructor; this is done to facilitate reading the data from disk but
  ;; causes ugliness in add-linkage! where we have the actual package but have to break it down
  ;; so the arguments needed to reconstitute it can be stored.
  
  
  ; LINKAGE-TABLE ::= hash-table[LINKAGE-KEY -> PKG-LOCATION]
  (define LT #f) 
  
  ; get-linkage-table : -> hash-table[LINKAGE-KEY -> PKG-LOCATION]
  (define (get-linkage-table)
    (unless (file-exists? (LINKAGE-FILE)) (with-output-to-file (LINKAGE-FILE) newline))
    (unless LT (set! LT (build-hash-table (with-input-from-file (LINKAGE-FILE) read-all))))
    LT)
  
  ; add-linkage! : (symbol | #f) FULL-PKG-SPEC PKG -> PKG
  ; unless the first argument is #f, associates the pair of the first two arguments 
  ; with the last in the linkage table. Returns the given package-location
  (define (add-linkage! module pkg-spec pkg)
    (when (and module (current-module-name-prefix))
      (let ((key (get-key module pkg-spec)))
        (hash-table-get 
         (get-linkage-table)
         key
         (lambda ()
           (let ((plist (pkg-as-list pkg)))
             (begin
               (hash-table-put! (get-linkage-table) key plist)
               (with-output-to-file (LINKAGE-FILE)
                 (lambda () (write (list key plist)))
                 'append)))))))
    pkg)
  
  ;; remove-linkage! pkg-spec -> void
  ;; eliminates linkage to the given package
  (define (remove-linkage-to! pkg)
    (let ((l (get-linkage-table)))
      
      ;; first remove bad entries from the in-memory hash table
      (hash-table-for-each 
       l
       (lambda (k v)
         (match v
           [(name route maj min _)
            (when (and (equal? name (pkg-name pkg))
                       (equal? route (pkg-route pkg))
                       (= maj (pkg-maj pkg))
                       (= min (pkg-min pkg)))
              (hash-table-remove! l k))]
           [_ (void)])))
      
      ;; now write the new table out to disk to keep it in sync
      (with-output-to-file (LINKAGE-FILE)
        (lambda ()
          (printf "\n")
          (hash-table-for-each 
           l
           (lambda (k v) (write (list k v)))))
        'truncate/replace)))
  
  ;; kill the whole linkage-table
  (define (remove-all-linkage!)
    (with-output-to-file (LINKAGE-FILE)
      (lambda () (printf "\n"))
      'truncate/replace)
    (set! LT #f))
  
  ;; pkg-as-list : PKG -> (list string string nat nat bytes[path])
  (define (pkg-as-list pkg)
    (list (pkg-name pkg)
          (pkg-route pkg)
          (pkg-maj pkg)
          (pkg-min pkg)
          (path->bytes (pkg-path pkg))))


  
  ; get-linkage : symbol FULL-PKG-SPEC -> PKG | #f
  ; returns the already-linked module location, or #f if there is none
  (define (get-linkage module-specifier pkg-specifier)
    (let ((pkg-fields (hash-table-get
                       (get-linkage-table)
                       (get-key module-specifier pkg-specifier)
                       (lambda () #f))))
      (if pkg-fields 
          (with-handlers ([exn:fail? (lambda (e) #f)])
            (match-let ([(name route maj min pathbytes) pkg-fields])
              (make-pkg name route maj min (bytes->path pathbytes))))
          #f)))
  
  ; get-key : symbol FULL-PKG-SPEC -> LINKAGE-KEY
  ; produces a linkage key for the given pair.
  (define (get-key module-specifier pkg-spec)
    (list* (get-module-id module-specifier pkg-spec)
           (pkg-spec-name pkg-spec)
           (pkg-spec-maj pkg-spec)
           (pkg-spec-minor-lo pkg-spec)
           (pkg-spec-minor-hi pkg-spec)
           (pkg-spec-path pkg-spec)))
  
  ; get-module-id : TST FULL-PKG-SPEC -> LINKAGE-MODULE-KEY
  ; gets a unique identifier naming the module that produced the pkg-spec.
  ; (strategy due to Matthew)
  (define (get-module-id ms pkg-spec)
    (cond
      [(full-filename-identifier? ms) 
       (module-specifier->key ms)]
      [(and 
        (pkg-spec-stx pkg-spec) ;; <-- I don't know about this
        (syntax-original? (pkg-spec-stx pkg-spec))
        (path? (syntax-source (pkg-spec-stx pkg-spec))))
       (path->key (desuffix (syntax-source (pkg-spec-stx pkg-spec))))]
      [(and (symbol? ms) (current-load-relative-directory))
       (path->key (build-path 
                   (current-load-relative-directory)
                   (symbol->string ms)))]
      [else #f]))
  
  
  ;; ----------------------------------------
  ;; ALL THE BELOW CODE IN THIS SECTION NEEDS
  ;; MAJOR MODIFICATION FOR v299
  
  ; path? : tst -> bool
  ;(define path? string?)
  
  ; full-filename-identifier? : TST -> bool
  ; determines if the given value represents a fully-resolved module identifier
  (define (full-filename-identifier? ms) 
    (and (symbol? ms)
         (regexp-match "^\\,.*" (symbol->string ms))))
  
  ; module-specifier->key : symbol -> LINKAGE-MODULE-KEY
  (define (module-specifier->key ms)
    (string->symbol (substring (symbol->string ms) 1)))
  
  ; path->key : string -> LINKAGE-MODULE-KEY
  (define (path->key p) (string->symbol (path->string p)))
  
  ; desuffix : path -> path
  ; removes the suffix from the given file
  (define (desuffix file)
    (path-replace-suffix file #"")))
