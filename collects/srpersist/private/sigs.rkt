;; sigs.ss for srpersist collection

(module sigs mzscheme

  (require mzlib/unitsig)

  (provide srpersist:odbc^)

  (define-signature srpersist:odbc-1.0^

   ; version info

    (srp-version
     compiled-odbc-version

   ; utility

     make-length
     free-length!
     read-length
     make-boxed-uint	
     read-boxed-uint	
     free-boxed-uint!
     make-indicator
     free-indicator!
     read-indicator
     set-indicator!
     make-row-status
     read-row-status
     free-row-status!
     make-buffer
     free-buffer!
     read-buffer
     write-buffer!

     ; ODBC procedures 

     alloc-connect
     alloc-env
     alloc-stmt
     bind-col
     cancel
     columns
     connect
     data-sources
     describe-col
     disconnect
     sql-error
     exec-direct
     sql-execute
     fetch
     free-connect
     free-env
     free-stmt
     get-connect-option
     get-cursor-name
     get-data
     get-functions
     get-info
     get-stmt-option
     get-type-info
     num-result-cols
     param-data
     prepare
     put-data
     row-count
     set-connect-option
     set-cursor-name
     set-param
     set-stmt-option
     special-columns
     statistics
     tables
     transact
     driver-connect
     browse-connect
     col-attributes
     column-privileges
     describe-param
     extended-fetch
     foreign-keys
     more-results
     native-sql
     num-params
     param-options
     primary-keys
     procedure-columns
     procedures
     set-pos
     table-privileges
     set-scroll-options

     ; implementation of ODBC macro

     len-binary-attr

     ; structures

     struct:sql-date
     make-sql-date
     sql-date?
     sql-date-year
     set-sql-date-year!
     sql-date-month
     set-sql-date-month!
     sql-date-day
     set-sql-date-day!
     struct:sql-time
     make-sql-time
     sql-time?
     sql-time-hour
     set-sql-time-hour!
     sql-time-minute
     set-sql-time-minute!
     sql-time-second
     set-sql-time-second!
     struct:sql-timestamp
     make-sql-timestamp
     sql-timestamp?
     sql-timestamp-year
     set-sql-timestamp-year!
     sql-timestamp-month
     set-sql-timestamp-month!
     sql-timestamp-day
     set-sql-timestamp-day!
     sql-timestamp-hour
     set-sql-timestamp-hour!
     sql-timestamp-minute
     set-sql-timestamp-minute!
     sql-timestamp-second
     set-sql-timestamp-second!
     sql-timestamp-fraction
     set-sql-timestamp-fraction!
     
     ; exceptions

     struct:exn-not-implemented
     make-exn-not-implemented
     exn-not-implemented?
     struct:exn-with-info
     make-exn-with-info
     exn-with-info?
     exn-with-info-val
     set-exn-with-info-val!
     struct:exn-invalid-handle
     make-exn-invalid-handle
     exn-invalid-handle?
     struct:exn-error
     make-exn-error
     exn-error?
     struct:exn-need-data
     make-exn-need-data
     exn-need-data?
     exn-need-data-val
     set-exn-need-data-val!
     struct:exn-still-executing
     make-exn-still-executing
     exn-still-executing?
     struct:exn-no-data
     make-exn-no-data
     exn-no-data?))
  
  (define-signature srpersist:odbc-2.0^

    ;; ODBC procedures

    ((open srpersist:odbc-1.0^)
     bind-parameter
     drivers))


  (define-signature srpersist:odbc-3.0^

    ((open srpersist:odbc-2.0^)

     ;; utility 

     read-op-parms

     ;; ODBC procedures

     alloc-handle
     bind-param
     bulk-operations
     close-cursor
     col-attribute
     copy-desc
     end-tran
     fetch-scroll
     free-handle
     get-connect-attr
     get-desc-field
     get-desc-rec
     get-diag-field
     get-diag-rec
     get-env-attr
     get-stmt-attr
     set-connect-attr
     set-desc-field
     set-desc-rec
     set-env-attr
     set-stmt-attr
     
     ;; structures

     struct:sql-numeric
     make-sql-numeric
     sql-numeric?
     sql-numeric-precision
     set-sql-numeric-precision!
     sql-numeric-scale
     set-sql-numeric-scale!
     sql-numeric-sign
     set-sql-numeric-sign!
     sql-numeric-val
     set-sql-numeric-val!
     struct:sql-year-interval
     make-sql-year-interval
     sql-year-interval?
     sql-year-interval-sign
     set-sql-year-interval-sign!
     sql-year-interval-year
     set-sql-year-interval-year!
     struct:sql-month-interval
     make-sql-month-interval
     sql-month-interval?
     sql-month-interval-sign
     set-sql-month-interval-sign!
     sql-month-interval-month
     set-sql-month-interval-month!
     struct:sql-day-interval
     make-sql-day-interval
     sql-day-interval?
     sql-day-interval-sign
     set-sql-day-interval-sign!
     sql-day-interval-day
     set-sql-day-interval-day!
     struct:sql-hour-interval
     make-sql-hour-interval
     sql-hour-interval?
     sql-hour-interval-sign
     set-sql-hour-interval-sign!
     sql-hour-interval-hour
     set-sql-hour-interval-hour!
     struct:sql-minute-interval
     make-sql-minute-interval
     sql-minute-interval?
     sql-minute-interval-sign
     set-sql-minute-interval-sign!
     sql-minute-interval-minute
     set-sql-minute-interval-minute!
     struct:sql-second-interval
     make-sql-second-interval
     sql-second-interval?
     sql-second-interval-sign
     set-sql-second-interval-sign!
     sql-second-interval-second
     set-sql-second-interval-second!
     struct:sql-year-to-month-interval
     make-sql-year-to-month-interval
     sql-year-to-month-interval?
     sql-year-to-month-interval-sign
     set-sql-year-to-month-interval-sign!
     sql-year-to-month-interval-year
     set-sql-year-to-month-interval-year!
     sql-year-to-month-interval-month
     set-sql-year-to-month-interval-month!
     struct:sql-day-to-hour-interval
     make-sql-day-to-hour-interval
     sql-day-to-hour-interval?
     sql-day-to-hour-interval-sign
     set-sql-day-to-hour-interval-sign!
     sql-day-to-hour-interval-day
     set-sql-day-to-hour-interval-day!
     sql-day-to-hour-interval-hour
     set-sql-day-to-hour-interval-hour!
     struct:sql-day-to-minute-interval
     make-sql-day-to-minute-interval
     sql-day-to-minute-interval?
     sql-day-to-minute-interval-sign
     set-sql-day-to-minute-interval-sign!
     sql-day-to-minute-interval-day
     set-sql-day-to-minute-interval-day!
     sql-day-to-minute-interval-hour
     set-sql-day-to-minute-interval-hour!
     sql-day-to-minute-interval-minute
     set-sql-day-to-minute-interval-minute!
     struct:sql-day-to-second-interval
     make-sql-day-to-second-interval
     sql-day-to-second-interval?
     sql-day-to-second-interval-sign
     set-sql-day-to-second-interval-sign!
     sql-day-to-second-interval-day
     set-sql-day-to-second-interval-day!
     sql-day-to-second-interval-hour
     set-sql-day-to-second-interval-hour!
     sql-day-to-second-interval-minute
     set-sql-day-to-second-interval-minute!
     sql-day-to-second-interval-second
     set-sql-day-to-second-interval-second!
     struct:sql-hour-to-minute-interval
     make-sql-hour-to-minute-interval
     sql-hour-to-minute-interval?
     sql-hour-to-minute-interval-sign
     set-sql-hour-to-minute-interval-sign!
     sql-hour-to-minute-interval-hour
     set-sql-hour-to-minute-interval-hour!
     sql-hour-to-minute-interval-minute
     set-sql-hour-to-minute-interval-minute!
     struct:sql-hour-to-second-interval
     make-sql-hour-to-second-interval
     sql-hour-to-second-interval?
     sql-hour-to-second-interval-sign
     set-sql-hour-to-second-interval-sign!
     sql-hour-to-second-interval-hour
     set-sql-hour-to-second-interval-hour!
     sql-hour-to-second-interval-minute
     set-sql-hour-to-second-interval-minute!
     sql-hour-to-second-interval-second
     set-sql-hour-to-second-interval-second!
     struct:sql-minute-to-second-interval
     make-sql-minute-to-second-interval
     sql-minute-to-second-interval?
     sql-minute-to-second-interval-sign
     set-sql-minute-to-second-interval-sign!
     sql-minute-to-second-interval-minute
     set-sql-minute-to-second-interval-minute!
     sql-minute-to-second-interval-second
     set-sql-minute-to-second-interval-second!))
  
  (define-signature srpersist:odbc-3.5^
    
    ((open srpersist:odbc-3.0^)
     
     struct:sql-guid
     make-sql-guid
     sql-guid?
     sql-guid-data1
     set-sql-guid-data1!
     sql-guid-data2
     set-sql-guid-data2!
     sql-guid-data3
     set-sql-guid-data3!
     sql-guid-data4
     set-sql-guid-data4!))

  (define-signature srpersist:odbc^
    
    ((open srpersist:odbc-3.5^))))













