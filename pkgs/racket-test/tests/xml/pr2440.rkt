#lang racket/base
(require xml
         xml/plist)

(module+ test
  (require rackunit)

  (define example
    #<<END
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist SYSTEM "file://localhost/System/Library/DTDs/PropertyList.dtd">
<plist version="0.9">
 <dict>
  <key>Genre</key>
  <string>Gospel &#38; Religious</string>
 </dict>
</plist>
END
    )  

  (check-equal?
   (read-plist (open-input-string example))
   '(dict (assoc-pair "Genre" "Gospel & Religious"))))
