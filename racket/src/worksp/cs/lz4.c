extern void LZ4_decompress_safe(void);
extern void LZ4_compressBound(void);
extern void LZ4_compress_default(void);

void lz4_stub(void) {
  LZ4_decompress_safe();
  LZ4_compressBound();
  LZ4_compress_default();
}
