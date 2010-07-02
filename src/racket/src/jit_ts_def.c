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
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g17, const Scheme_Object* g18) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g17, g18); \
  else \
    return id(g17, g18); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g19, Scheme_Object* g20) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g19, g20); \
  else \
    return id(g19, g20); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g21, long g22) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g21, g22); \
  else \
    return id(g21, g22); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(long g23) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g23); \
  else \
    return id(g23); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g24, Scheme_Object* g25, int g26) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g24, g25, g26); \
  else \
     id(g24, g25, g26); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g27, int g28, Scheme_Object** g29) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g27, g28, g29); \
  else \
     id(g27, g28, g29); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g30, Scheme_Object* g31) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g30, g31); \
  else \
     id(g30, g31); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g32) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g32); \
  else \
     id(g32); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g33, long g34) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g33, g34); \
  else \
    return id(g33, g34); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g35, Scheme_Object** g36) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g35, g36); \
  else \
    return id(g35, g36); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g37) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g37); \
  else \
    return id(g37); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g38) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g38); \
  else \
     id(g38); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g39, Scheme_Object** g40, int g41) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g39, g40, g41); \
  else \
    return id(g39, g40, g41); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g42, int g43, Scheme_Object** g44) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g42, g43, g44); \
  else \
     id(g42, g43, g44); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g45) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g45); \
  else \
    return id(g45); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g46, int g47) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g46, g47); \
  else \
    return id(g46, g47); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g48, int g49, Scheme_Object* g50) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g48, g49, g50); \
  else \
     id(g48, g49, g50); \
}
