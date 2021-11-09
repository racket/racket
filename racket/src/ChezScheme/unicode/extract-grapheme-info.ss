(import (scheme) (unicode-data))

(include "extract-common.ss")
(include "bitfields.ss")
(include "codepoint-range.ss")

(define ptr-bytes 4) ; Does this have any effect?
(define code-point-limit #x110000)

;; Since we only need 4 bits per code point, we can use the low
;; 4 bits of a byte for code point n and the high bits for n+1
(define-table (make-table table-ref table-set! table-ref-code)
  (make-bytevector bytevector-u8-ref bytevector-u8-set!)
  (quotient code-point-limit 2) #x40 #x40)

(define (getprop tbl i)
  (fxand
   (fxsrl (table-ref tbl (fxsrl i 1))
	  (fxsll (fxand i 1) 2))
   #x0F))

(define (setprop! tbl i prop)
  (let ([b (table-ref tbl (fxsrl i 1))])
    (table-set! tbl
		(fxsrl i 1)
		(fxior b (fxsll prop (fxsll (fxand i 1) 2))))))

(define-bitfields
  ;; 4 bits
  (enumeration gcb-property ; "gcb" stands for "grapheme cluster break"
	       gcb-other gcb-prepend gcb-cr gcb-lf gcb-control
	       gcb-extend gcb-regional-indicator gcb-spacing-mark
	       gcb-l gcb-v gcb-t gcb-lv gcb-lvt gcb-zwj
	       gcb-extended-pictographic))

(define (name->gcb-prop name)
  (case (string->symbol name)
    [(Prepend) (fxsll gcb-prepend gcb-property-shift)]
    [(CR) (fxsll gcb-cr gcb-property-shift)]
    [(LF) (fxsll gcb-lf gcb-property-shift)]
    [(Control) (fxsll gcb-control gcb-property-shift)]
    [(Extend) (fxsll gcb-extend gcb-property-shift)]
    [(Regional_Indicator) (fxsll gcb-regional-indicator gcb-property-shift)]
    [(SpacingMark) (fxsll gcb-spacing-mark gcb-property-shift)]
    [(L) (fxsll gcb-l gcb-property-shift)]
    [(V) (fxsll gcb-v gcb-property-shift)]
    [(T) (fxsll gcb-t gcb-property-shift)]
    [(LV) (fxsll gcb-lv gcb-property-shift)]
    [(LVT) (fxsll gcb-lvt gcb-property-shift)]
    [(ZWJ) (fxsll gcb-zwj gcb-property-shift)]
    [else (error 'name->gcb-prop "unexpected property value" name)]))

(define (add-gcb-property-to-table! tbl)
  (for-each
   (lambda (record)
     (let* ([range (extract-range (list-ref record 0))]
	    [prop (name->gcb-prop (list-ref record 1))])
       (let loop ([i (car range)])
	 (unless (> i (cdr range))
	   (unless (= (getprop tbl i) 0)
	     (error 'add-gcb-property-to-table! "properties not disjoint"))
	   (setprop! tbl i prop)
	   (loop (add1 i))))))
   (get-unicode-data "UNIDATA/GraphemeBreakProperty.txt")))

(define (add-extended-pictographic-property-to-table! tbl)
  (for-each
   (lambda (record)
     (let ([range (extract-range (list-ref record 0))]
	   [name (list-ref record 1)])
       (when (equal? name "Extended_Pictographic")
	 (let loop ([i (car range)])
	   (unless (> i (cdr range))
	     (unless (= (getprop tbl i) 0)
	       (error 'add-extended-pictographic-property-to-table!
		      "properties not disjoint"))
	     (setprop! tbl i (fxsll gcb-extended-pictographic  gcb-property-shift))
	     (loop (add1 i)))))))
   (get-unicode-data "UNIDATA/emoji-data.txt")))

(let ([tbl (make-table 0)])
  (add-gcb-property-to-table! tbl)
  (add-extended-pictographic-property-to-table! tbl)
  (commonize* tbl)

  (with-output-to-file* "unicode-grapheme-info.ss"
    (lambda ()
      (parameterize ([print-graph #t] [print-vector-length #f])
	(pretty-print
	 `(module ($char-grapheme-break-property)
	    (define unicode-grapheme-info-table ',tbl)
	    (define table-ref ,table-ref-code)

	    (define (getprop n)
	      (fxand
	       (fxsrl (table-ref unicode-grapheme-info-table (fxsrl n 1))
		      (fxsll (fxand n 1) 2))
	       #x0F))

	    (define gcb-property-names
	      ',(list->vector
		 (map (lambda (sym)
			(let ([str (symbol->string sym)])
			  (string->symbol
			   (substring str 4 (string-length str)))))
		      gcb-property-members)))

	    (define ($char-grapheme-break-property c)
	      (vector-ref gcb-property-names (getprop (char->integer c))))))))))
	    
(printf "Happy Happy Joy Joy ~a\n" (sizeof cache))
