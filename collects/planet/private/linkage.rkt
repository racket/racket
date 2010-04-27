(module linkage mzscheme
  
  (require "planet-shared.ss"
           "../config.ss"
           mzlib/match)

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
  (define (get/linkage rmp pkg-specifier success-k failure-k)
    (let ([linked-pkg (get-linkage rmp pkg-specifier)])
      (if linked-pkg
          (success-k linked-pkg)
          (failure-k 
           void
           (λ (pkg) (add-linkage! rmp pkg-specifier pkg))
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
  
  ; add-linkage! : (resolved-module-path | #f) FULL-PKG-SPEC PKG -> PKG
  ; unless the first argument is #f, associates the pair of the first two arguments 
  ; with the last in the linkage table. Returns the given package-location
  (define (add-linkage! rmp pkg-spec pkg)
    (when rmp
      (let ((key (get-key rmp pkg-spec)))
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


  
  ; get-linkage : (resolved-module-path | #f) FULL-PKG-SPEC -> PKG | #f
  ; returns the already-linked module location, or #f if there is none
  (define (get-linkage rmp pkg-specifier)
    (cond
      [rmp
       (let ((pkg-fields (hash-table-get
                          (get-linkage-table)
                          (get-key rmp pkg-specifier)
                          (lambda () #f))))
         (if pkg-fields 
             (with-handlers ([exn:fail? (lambda (e) #f)])
               (match-let ([(name route maj min pathbytes) pkg-fields])
                 (make-pkg name route maj min (bytes->path pathbytes))))
             #f))]
      [else #f]))
  
  ; get-key : resolved-module-path? FULL-PKG-SPEC -> LINKAGE-KEY
  ; produces a linkage key for the given pair.
  (define (get-key rmp pkg-spec)
    (list* (get-module-id rmp)
           (pkg-spec-name pkg-spec)
           (pkg-spec-maj pkg-spec)
           (pkg-spec-minor-lo pkg-spec)
           (pkg-spec-minor-hi pkg-spec)
           (pkg-spec-path pkg-spec)))
  
  ; get-module-id : resolved-module-path? -> LINKAGE-MODULE-KEY
  ; key suitable for marshalling that represents the given resolved-module-path
  (define (get-module-id rmp)
    (path->string (resolved-module-path-name rmp)))
  
  )

