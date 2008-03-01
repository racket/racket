(module raw-set-signature mzscheme
  (provide provide-raw-set)
  
  (define-syntax provide-raw-set
    (syntax-rules ()
      [(provide-raw-set)
       (provide 
        ; a combiner is a (element element -> element)
        difference            ; compare set set -> set
        empty                 ; set
        empty?                ; set -> boolean
        equal=?               ; set set -> boolean
        elements              ; set -> list
        find-min              ; set -> element
        fold                  ; (element alpha -> alpha) alpha set -> alpha
        get                   ; compare element set -> (union element #f)
        intersection          ; compare set set -> set
        intersection/combiner ; compare set set -> set
        insert                ; compare element set -> set
        insert/combiner       ; compare element set combiner-> set
        insert*               ; compare (list element) set -> set
        insert*/combiner      ; compare (list element) set combiner-> set
        list->set             ; compare list -> set
        list->set/combiner    ; compare list combiner -> set
        member?               ; compare element set -> boolean
        remove                ; compare element set -> set
        remove*               ; compare (list element) set -> set
        set                   ; element ... -> set
        select                ; non-empty-set -> element
        singleton             ; object -> set
        size                  ; set -> integer
        subset?               ; compare set set -> boolean
        union                 ; compare set set -> set
        union/combiner        ; compare set set combiner-> set
        )])))
