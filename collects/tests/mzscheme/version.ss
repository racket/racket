
(load-relative "loadtest.ss")

(Section 'version)

(require version/utils)

(test #t valid-version? (version))
(for-each (lambda (v+i) (test (cadr v+i) version->integer (car v+i)))
          '(;; legacy version scheme
            ["372"           372000000]
            ["372.0"         #f] ; should be just "372"
            ["372.1"         372001000]
            ["372.12"        372012000]
            ["123.4"         123004000]
            ["49"             49000000] ; oldest legacy-version supported
            ["103"           103000000]
            ;; new version scheme
            ["4.0"           400000000]
            ["4"             #f] ; must have one decimal digit
            ["4.1"           401000000]
            ["4.0.1"         400001000]
            ["4.0.2"         400002000]
            ["4.1.2.3"       401002003]
            ["4.0.0.99"      400000099]
            ["4.12.123.999"  412123999]
            ["4.99.99.99"    499099099]
            ["4.99.99.099"   #f] ; leading zeroes forbidden
            ["4.99.099.99"   #f]
            ["4.09.99.99"    #f]
            ["04.9.99.0"     #f]
            ["4.9.99.00"     #f]
            ["00.0"          #f]
            ["4.99.099.099"  #f]
            ["4.99.0"        #f] ; single zero forbidden at end
            ["4.99.99.0"     #f]
            ["0.0"                    0] ; smallest new-scheme version
            ["10.0"          1000000000]
            ["10.0.1"        1000001000]
            ["48.99.999.999" 4899999999] ; largest new-scheme version allowed
            ["4.100"         #f] ; second component should be < 100
            ["4.999"         #f]
            ["1.2.3.4.5"     #f] ; only four parts
            ["1..2"          #f] ; other random junk
            [".1"            #f]
            ["1."            #f]
            ["foo"           #f]
            ["x.y"           #f]
            ["0"             #f]
            ))

(report-errs)
