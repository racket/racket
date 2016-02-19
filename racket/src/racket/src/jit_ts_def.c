#define define_ts_siS_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g178, int g179, Scheme_Object** g180) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s("[" #id "]", src_type, id, g178, g179, g180); \
  else \
    return id(g178, g179, g180); \
}
#define define_ts_iSs_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g181, Scheme_Object** g182, Scheme_Object* g183) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s("[" #id "]", src_type, id, g181, g182, g183); \
  else \
    return id(g181, g182, g183); \
}
#define define_ts_s_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g184) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s("[" #id "]", src_type, id, g184); \
  else \
    return id(g184); \
}
#define define_ts_n_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Native_Lambda* g185) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s("[" #id "]", src_type, id, g185); \
  else \
    return id(g185); \
}
#define define_ts__s(id, src_type) \
static Scheme_Object* ts_ ## id() \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall__s("[" #id "]", src_type, id); \
  else \
    return id(); \
}
#define define_ts_ss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g186, Scheme_Object* g187) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s("[" #id "]", src_type, id, g186, g187); \
  else \
    return id(g186, g187); \
}
#define define_ts_ssi_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g188, Scheme_Object* g189, int g190) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ssi_s("[" #id "]", src_type, id, g188, g189, g190); \
  else \
    return id(g188, g189, g190); \
}
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g191, const Scheme_Object* g192) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g191, g192); \
  else \
    return id(g191, g192); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g193, Scheme_Object* g194) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g193, g194); \
  else \
    return id(g193, g194); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g195, intptr_t g196) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g195, g196); \
  else \
    return id(g195, g196); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(intptr_t g197) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g197); \
  else \
    return id(g197); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g198, Scheme_Object* g199, int g200) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g198, g199, g200); \
  else \
     id(g198, g199, g200); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g201, int g202, Scheme_Object** g203) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g201, g202, g203); \
  else \
     id(g201, g202, g203); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g204, Scheme_Object* g205) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g204, g205); \
  else \
     id(g204, g205); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g206) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g206); \
  else \
     id(g206); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g207, intptr_t g208) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g207, g208); \
  else \
    return id(g207, g208); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g209, Scheme_Object** g210) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g209, g210); \
  else \
    return id(g209, g210); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g211) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g211); \
  else \
    return id(g211); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g212) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g212); \
  else \
     id(g212); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g213, Scheme_Object** g214, int g215) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g213, g214, g215); \
  else \
    return id(g213, g214, g215); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g216, int g217, Scheme_Object** g218) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g216, g217, g218); \
  else \
     id(g216, g217, g218); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g219) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g219); \
  else \
    return id(g219); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g220, int g221) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g220, g221); \
  else \
    return id(g220, g221); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g222, int g223, Scheme_Object* g224) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g222, g223, g224); \
  else \
     id(g222, g223, g224); \
}
#define define_ts_ss_i(id, src_type) \
static int ts_ ## id(Scheme_Object* g225, Scheme_Object* g226) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_i("[" #id "]", src_type, id, g225, g226); \
  else \
    return id(g225, g226); \
}
#define define_ts_iSp_v(id, src_type) \
static void ts_ ## id(int g227, Scheme_Object** g228, void* g229) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iSp_v("[" #id "]", src_type, id, g227, g228, g229); \
  else \
     id(g227, g228, g229); \
}
#define define_ts_sss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g230, Scheme_Object* g231, Scheme_Object* g232) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sss_s("[" #id "]", src_type, id, g230, g231, g232); \
  else \
    return id(g230, g231, g232); \
}
#define define_ts__v(id, src_type) \
static void ts_ ## id() \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall__v("[" #id "]", src_type, id); \
  else \
     id(); \
}
#define define_ts_iS_v(id, src_type) \
static void ts_ ## id(int g233, Scheme_Object** g234) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iS_v("[" #id "]", src_type, id, g233, g234); \
  else \
     id(g233, g234); \
}
