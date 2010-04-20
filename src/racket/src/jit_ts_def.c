#define define_ts_siS_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g7, int g8, Scheme_Object** g9) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s("[" #id "]", src_type, id, g7, g8, g9); \
  else \
    return id(g7, g8, g9); \
}
#define define_ts_iSs_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g10, Scheme_Object** g11, Scheme_Object* g12) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s("[" #id "]", src_type, id, g10, g11, g12); \
  else \
    return id(g10, g11, g12); \
}
#define define_ts_s_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g13) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s("[" #id "]", src_type, id, g13); \
  else \
    return id(g13); \
}
#define define_ts_n_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Native_Closure_Data* g14) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s("[" #id "]", src_type, id, g14); \
  else \
    return id(g14); \
}
#define define_ts__s(id, src_type) \
static Scheme_Object* ts_ ## id() \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall__s("[" #id "]", src_type, id, ); \
  else \
    return id(); \
}
#define define_ts_ss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g15, Scheme_Object* g16) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s("[" #id "]", src_type, id, g15, g16); \
  else \
    return id(g15, g16); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g17, Scheme_Object* g18) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g17, g18); \
  else \
    return id(g17, g18); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g19, long g20) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g19, g20); \
  else \
    return id(g19, g20); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(long g21) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g21); \
  else \
    return id(g21); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g22, Scheme_Object* g23, int g24) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g22, g23, g24); \
  else \
     id(g22, g23, g24); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g25, int g26, Scheme_Object** g27) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g25, g26, g27); \
  else \
     id(g25, g26, g27); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g28, Scheme_Object* g29) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g28, g29); \
  else \
     id(g28, g29); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g30) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g30); \
  else \
     id(g30); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g31, long g32) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g31, g32); \
  else \
    return id(g31, g32); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g33, Scheme_Object** g34) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g33, g34); \
  else \
    return id(g33, g34); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g35) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g35); \
  else \
    return id(g35); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g36) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g36); \
  else \
     id(g36); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g37, Scheme_Object** g38, int g39) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g37, g38, g39); \
  else \
    return id(g37, g38, g39); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g40, int g41, Scheme_Object** g42) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g40, g41, g42); \
  else \
     id(g40, g41, g42); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g43) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g43); \
  else \
    return id(g43); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g44, int g45) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g44, g45); \
  else \
    return id(g44, g45); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g46, int g47, Scheme_Object* g48) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g46, g47, g48); \
  else \
     id(g46, g47, g48); \
}
