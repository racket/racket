# Methods that should not be called by random testing
wx:media-edit% save-file
wx:media-pasteboard% save-file
wx:multi-text% save-file
wx:text-window% save-file
wx:bitmap% save-file
'wxsGlobal wx:write-resource
'wxsGlobal wx:exit
'wxsGlobal wx:concat-files
'wxsGlobal wx:copy-file
wx:media-buffer% get-file
wx:media-buffer% put-file
wx:media-edit% get-file
wx:media-edit% put-file
wx:media-pasteboard% get-file
wx:media-pasteboard% put-file
wx:style-list clear
