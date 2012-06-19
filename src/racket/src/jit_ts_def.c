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
#define define_ts_ssi_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g17, Scheme_Object* g18, int g19) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ssi_s("[" #id "]", src_type, id, g17, g18, g19); \
  else \
    return id(g17, g18, g19); \
}
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g20, const Scheme_Object* g21) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g20, g21); \
  else \
    return id(g20, g21); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g22, Scheme_Object* g23) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g22, g23); \
  else \
    return id(g22, g23); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g24, intptr_t g25) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g24, g25); \
  else \
    return id(g24, g25); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(intptr_t g26) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g26); \
  else \
    return id(g26); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g27, Scheme_Object* g28, int g29) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g27, g28, g29); \
  else \
     id(g27, g28, g29); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g30, int g31, Scheme_Object** g32) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g30, g31, g32); \
  else \
     id(g30, g31, g32); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g33, Scheme_Object* g34) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g33, g34); \
  else \
     id(g33, g34); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g35) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g35); \
  else \
     id(g35); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g36, intptr_t g37) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g36, g37); \
  else \
    return id(g36, g37); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g38, Scheme_Object** g39) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g38, g39); \
  else \
    return id(g38, g39); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g40) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g40); \
  else \
    return id(g40); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g41) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g41); \
  else \
     id(g41); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g42, Scheme_Object** g43, int g44) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g42, g43, g44); \
  else \
    return id(g42, g43, g44); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g45, int g46, Scheme_Object** g47) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g45, g46, g47); \
  else \
     id(g45, g46, g47); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g48) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g48); \
  else \
    return id(g48); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g49, int g50) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g49, g50); \
  else \
    return id(g49, g50); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g51, int g52, Scheme_Object* g53) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g51, g52, g53); \
  else \
     id(g51, g52, g53); \
}
#define define_ts_ss_i(id, src_type) \
static int ts_ ## id(Scheme_Object* g54, Scheme_Object* g55) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_i("[" #id "]", src_type, id, g54, g55); \
  else \
    return id(g54, g55); \
}
#define define_ts_iSp_v(id, src_type) \
static void ts_ ## id(int g56, Scheme_Object** g57, void* g58) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iSp_v("[" #id "]", src_type, id, g56, g57, g58); \
  else \
     id(g56, g57, g58); \
}
#define define_ts_sss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g59, Scheme_Object* g60, Scheme_Object* g61) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sss_s("[" #id "]", src_type, id, g59, g60, g61); \
  else \
    return id(g59, g60, g61); \
}
