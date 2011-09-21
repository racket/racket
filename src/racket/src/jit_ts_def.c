#define define_ts_siS_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g5, int g6, Scheme_Object** g7) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_siS_s("[" #id "]", src_type, id, g5, g6, g7); \
  else \
    return id(g5, g6, g7); \
}
#define define_ts_iSs_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g8, Scheme_Object** g9, Scheme_Object* g10) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSs_s("[" #id "]", src_type, id, g8, g9, g10); \
  else \
    return id(g8, g9, g10); \
}
#define define_ts_s_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g11) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_s_s("[" #id "]", src_type, id, g11); \
  else \
    return id(g11); \
}
#define define_ts_n_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Native_Closure_Data* g12) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_n_s("[" #id "]", src_type, id, g12); \
  else \
    return id(g12); \
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
static Scheme_Object* ts_ ## id(Scheme_Object* g13, Scheme_Object* g14) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_s("[" #id "]", src_type, id, g13, g14); \
  else \
    return id(g13, g14); \
}
#define define_ts_ssi_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g15, Scheme_Object* g16, int g17) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ssi_s("[" #id "]", src_type, id, g15, g16, g17); \
  else \
    return id(g15, g16, g17); \
}
#define define_ts_tt_s(id, src_type) \
static Scheme_Object* ts_ ## id(const Scheme_Object* g18, const Scheme_Object* g19) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_tt_s("[" #id "]", src_type, id, g18, g19); \
  else \
    return id(g18, g19); \
}
#define define_ts_ss_m(id, src_type) \
static MZ_MARK_STACK_TYPE ts_ ## id(Scheme_Object* g20, Scheme_Object* g21) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_m("[" #id "]", src_type, id, g20, g21); \
  else \
    return id(g20, g21); \
}
#define define_ts_Sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g22, intptr_t g23) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_Sl_s("[" #id "]", src_type, id, g22, g23); \
  else \
    return id(g22, g23); \
}
#define define_ts_l_s(id, src_type) \
static Scheme_Object* ts_ ## id(intptr_t g24) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_l_s("[" #id "]", src_type, id, g24); \
  else \
    return id(g24); \
}
#define define_ts_bsi_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g25, Scheme_Object* g26, int g27) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_bsi_v("[" #id "]", src_type, id, g25, g26, g27); \
  else \
     id(g25, g26, g27); \
}
#define define_ts_iiS_v(id, src_type) \
static void ts_ ## id(int g28, int g29, Scheme_Object** g30) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iiS_v("[" #id "]", src_type, id, g28, g29, g30); \
  else \
     id(g28, g29, g30); \
}
#define define_ts_ss_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g31, Scheme_Object* g32) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_ss_v("[" #id "]", src_type, id, g31, g32); \
  else \
     id(g31, g32); \
}
#define define_ts_b_v(id, src_type) \
static void ts_ ## id(Scheme_Bucket* g33) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_b_v("[" #id "]", src_type, id, g33); \
  else \
     id(g33); \
}
#define define_ts_sl_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g34, intptr_t g35) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sl_s("[" #id "]", src_type, id, g34, g35); \
  else \
    return id(g34, g35); \
}
#define define_ts_iS_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g36, Scheme_Object** g37) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iS_s("[" #id "]", src_type, id, g36, g37); \
  else \
    return id(g36, g37); \
}
#define define_ts_S_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object** g38) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_S_s("[" #id "]", src_type, id, g38); \
  else \
    return id(g38); \
}
#define define_ts_s_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g39) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_s_v("[" #id "]", src_type, id, g39); \
  else \
     id(g39); \
}
#define define_ts_iSi_s(id, src_type) \
static Scheme_Object* ts_ ## id(int g40, Scheme_Object** g41, int g42) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_iSi_s("[" #id "]", src_type, id, g40, g41, g42); \
  else \
    return id(g40, g41, g42); \
}
#define define_ts_siS_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g43, int g44, Scheme_Object** g45) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_siS_v("[" #id "]", src_type, id, g43, g44, g45); \
  else \
     id(g43, g44, g45); \
}
#define define_ts_z_p(id, src_type) \
static void* ts_ ## id(size_t g46) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_z_p("[" #id "]", src_type, id, g46); \
  else \
    return id(g46); \
}
#define define_ts_si_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g47, int g48) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_si_s("[" #id "]", src_type, id, g47, g48); \
  else \
    return id(g47, g48); \
}
#define define_ts_sis_v(id, src_type) \
static void ts_ ## id(Scheme_Object* g49, int g50, Scheme_Object* g51) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_sis_v("[" #id "]", src_type, id, g49, g50, g51); \
  else \
     id(g49, g50, g51); \
}
#define define_ts_ss_i(id, src_type) \
static int ts_ ## id(Scheme_Object* g52, Scheme_Object* g53) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_ss_i("[" #id "]", src_type, id, g52, g53); \
  else \
    return id(g52, g53); \
}
#define define_ts_iSp_v(id, src_type) \
static void ts_ ## id(int g54, Scheme_Object** g55, void* g56) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
     scheme_rtcall_iSp_v("[" #id "]", src_type, id, g54, g55, g56); \
  else \
     id(g54, g55, g56); \
}
#define define_ts_sss_s(id, src_type) \
static Scheme_Object* ts_ ## id(Scheme_Object* g57, Scheme_Object* g58, Scheme_Object* g59) \
   XFORM_SKIP_PROC \
{ \
  if (scheme_use_rtcall) \
    return scheme_rtcall_sss_s("[" #id "]", src_type, id, g57, g58, g59); \
  else \
    return id(g57, g58, g59); \
}
