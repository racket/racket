(module |74| mzscheme
  (require mzlib/include
           srfi/26
           srfi/60
           srfi/66)
  (provide
   endianness
   blob?
   make-blob
   blob-length
   blob-u8-ref blob-s8-ref
   blob-u8-set! blob-s8-set!
   blob-uint-ref blob-sint-ref
   blob-uint-set! blob-sint-set!
   blob-u16-ref blob-s16-ref
   blob-u16-native-ref blob-s16-native-ref
   blob-u16-set! blob-s16-set! 
   blob-u16-native-set! blob-s16-native-set!
   blob-u32-ref blob-s32-ref
   blob-u32-native-ref blob-s32-native-ref
   blob-u32-set! blob-s32-set!
   blob-u32-native-set! blob-s32-native-set!
   blob-u64-ref blob-s64-ref
   blob-u64-native-ref blob-s64-native-ref
   blob-u64-set! blob-s64-set!
   blob-u64-native-set! blob-s64-native-set! 
   blob=?
   blob-copy! blob-copy
   blob->u8-list u8-list->blob
   blob->uint-list blob->sint-list
   uint-list->blob sint-list->blob) 
  (include (lib "blob.scm" "srfi" "74")))
