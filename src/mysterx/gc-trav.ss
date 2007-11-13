#lang scheme/base

(printf "/* Generated from gc-trav.ss */\n\n")

(define types
  '([mx_com_object_type MX_COM_Object (types)]
    [mx_com_type_type MX_COM_Type ()]
    [mx_browser_type MX_Browser_Object ()]
    [mx_document_type MX_Document_Object ()]
    [mx_element_type MX_Element ()]
    [mx_event_type MX_Event ()]
    [mx_com_cy_type MX_COM_Data_Object ()]
    [mx_com_date_type MX_COM_Data_Object ()]
    [mx_com_scode_type MX_COM_Data_Object ()]
    [mx_com_iunknown_type MX_COM_Data_Object ()]
    [mx_com_omit_type MX_OMIT ()]
    [mx_com_typedesc_type MX_TYPEDESC ()]
    [mx_tbl_entry_type MX_TYPE_TBL_ENTRY (pTypeDesc next)]))

(for-each (lambda (type)
            (let ([tag (car type)]
                  [ctype (cadr type)]
                  [ptr-fields (caddr type)])
              (define (print-one prefix do-field)
                (printf "static int ~a_~a(void *_p) {\n" prefix tag)
                (when (and do-field
                           (pair? ptr-fields))
                  (printf "  ~a *p = (~a *)_p;\n" ctype ctype)
                  (for-each (lambda (ptr-field)
                              (printf "  ~a(p->~a);\n" do-field ptr-field))
                            ptr-fields))
                (printf "  return gcBYTES_TO_WORDS(sizeof(~a));\n" ctype)
                (printf "}\n"))
              (print-one "size" #f)
              (print-one "mark" "gcMARK")
              (print-one "fixup" "gcFIXUP")))
          types)


(printf "\nstatic void register_traversers(void) {\n")
(for-each (lambda (type)
            (let ([tag (car type)]
                  [ctype (cadr type)]
                  [ptr-fields (caddr type)])
              (printf "  GC_register_traversers(~a, size_~a, mark_~a, fixup_~a, 1, ~a);\n"
                      tag tag tag tag (if (null? ptr-fields) "1" "0"))))
          types)
(printf "}\n")

