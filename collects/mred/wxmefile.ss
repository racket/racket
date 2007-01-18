
(module wxmefile mzscheme
  (require "wxme/wxme.ss")
  (provide (all-from-except "wxme/wxme.ss"
                            wxme:read
                            wxme:read-syntax)))
