#lang racket/base

(define template #<<EOS
HKCR
{
	MzCOM.MzObj.@|VERSION| = s 'MzObj Class'
	{
		CLSID = s '{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}'
	}
	MzCOM.MzObj = s 'MzObj Class'
	{
		CLSID = s '{A3B0AF9E-2AB0-11D4-B6D2-0060089002FE}'
		CurVer = s 'MzCOM.MzObj.@|VERSION|'
	}
	NoRemove CLSID
	{
		ForceRemove {A3B0AF9E-2AB0-11D4-B6D2-0060089002FE} = s 'MzObj Class'
		{
			ProgID = s 'MzCOM.MzObj.@|VERSION|'
			VersionIndependentProgID = s 'MzCOM.MzObj'
			ForceRemove 'Programmable'
			LocalServer32 = s '%MODULE%'
			val AppID = s '{A604CB9D-2AB5-11D4-B6D3-0060089002FE}'
			'TypeLib' = s '{A604CB9C-2AB5-11D4-B6D3-0060089002FE}'
		}
	}
}

EOS
)

(define content (regexp-replace* #rx"@[|]VERSION[|]" template (version)))

(define file  "mzobj.rgs")

(unless (and (file-exists? file)
	     (equal? content
		     (call-with-input-file*
		      file
		      (lambda (i)
			(read-string (add1 (string-length content)) i)))))
  (call-with-output-file*
   file
   #:exists 'truncate
   (lambda (o)
     (display content o))))
