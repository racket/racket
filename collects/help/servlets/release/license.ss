(module license mzscheme
  (require "../private/util.ss"
           "../private/headelts.ss"
           (lib "uri-codec.ss" "net")
	   (lib "dirs.ss" "setup"))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (make-item ss)
    `(UL
      (LI
       ,@(map (lambda (s)
                `(DIV ,s (BR)))
              ss))))
  
  (define copyright-year 2006)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    
    `(HTML
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "License"))
      (BODY
       (A ((NAME "lic") (VALUE "License")))
       (B "PLT Software") (BR)
       (B ,(format "Copyright (c) ~a PLT Scheme Inc." copyright-year))
       (P)
       "PLT software is distributed under the GNU Library General Public "
       " License (LGPL).  This means you can link PLT software (such as "
       "MzScheme or MrEd) into proprietary applications, provided you follow "
       "the specific rules stated in the LGPL.  You can also modify PLT "
       "software; if you distribute a modified version, you must distribute it "
       "under the terms of the LGPL, which in particular means that you must "
       "release the source code for the modified software. See "
       (A ((HREF ,(format "/servlets/doc-anchor.ss?name=COPYING.LIB&caption=Copying PLT software&file=~a"
                          (uri-encode
                           (path->string
                            (simplify-path 
                             (build-path (find-doc-dir) "release-notes" "COPYING.LIB")))))))
          "COPYING.LIB")
       " for more information."
       (P)
       "PLT software includes or extends the following copyrighted material:"
       (P)
       ,@(map make-item 
              `(("DrScheme"
                 "Copyright (c) 1995-2003 PLT"
                 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("MrEd"
                 "Copyright (c) 1995-2003 PLT"
		 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("MzScheme" 
                 "Copyright (c) 1995-2003 PLT"
		 ,(format "Copyright (c) 2004-~a PLT Scheme Inc." copyright-year)
                 "All rights reserved.")
                ("libscheme" 
                 "Copyright (c) 1994 Brent Benson"
                 "All rights reserved.")
                ("wxWindows"
                 "Copyright (c) 1994 Artificial Intelligence Applications Institute, The University of Edinburgh"
                 "All rights reserved.")
                ("wxWindows Xt"
                 "Copyright (c) 1994 Artificial Intelligence Applications Institute, The University of Edinburgh"
                 "Copyright (c) 1995 GNU (Markus Holzem)"
                 "All rights reserved.")
                ("Conservative garbage collector"
                 "Copyright (c) 1988, 1989 Hans-J. Boehm, Alan J. Demers"
                 "Copyright (c) 1991-1996 Xerox Corporation"
                 "Copyright (c) 1996-1999 Silicon Graphics"
                 "Copyright (c) 1999-2001 by Hewlett-Packard Company"
                 "All rights reserved.")
                ("Collector C++ extension by Jesse Hull and John Ellis"
                 "Copyright (c) 1994 Xerox Corporation"
                 "All rights reserved.")
                ("The A List"
                 "Copyright (c) 1997-2000 Kyle Hammond."
                 "All rights reserved.")
                ("Independent JPEG Group library"
                 "Copyright (c) 1991-1998 Thomas G. Lane."
                 "All rights reserved.")
                ("libpng"
		 "Copyright (c) 2000-2002 Glenn Randers-Pehrson"
                 "All rights reserved.")
		("zlib"
		 "Copyright (c) 1995-2002 Jean-loup Gailly and Mark Adler"
		 "All rights reserved.")
		("GNU MP Library"
		 "Copyright (c) 1992, 1993, 1994, 1996 by Free Software Foundation, Inc.")
		("GNU lightning"
		 "Copyright (c) 1994, 1995, 1996, 1999, 2000, 2001, 2002 Free Software Foundation, Inc.")
		("GNU Classpath"
		 "Gnu Public Licence with special exception")))))))
