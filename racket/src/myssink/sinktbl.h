// sinktbl.h

#ifndef _SINKTBL_
#define _SINKTBL_
#endif

#define MAXINVOKEARGS 128

typedef struct _myssink_table_ {
  void (*psink_release_handler)(void *);
  void (*psink_release_arg)(void *);
  void (*psink_apply)(void *, int argc, void **argv);
  void *(*psink_variant_to_scheme)(VARIANTARG *p);
  void (*psink_unmarshal_scheme)(void *obj, VARIANTARG *p);
  void *(*pmake_scode)(SCODE);

} MYSSINK_TABLE;

#define sink_release_handler (myssink_table->psink_release_handler)
#define sink_release_arg (myssink_table->psink_release_arg)
#define sink_apply (myssink_table->psink_apply)
#define sink_variant_to_scheme (myssink_table->psink_variant_to_scheme)
#define sink_unmarshal_scheme (myssink_table->psink_unmarshal_scheme)
#define make_scode (myssink_table->pmake_scode)
