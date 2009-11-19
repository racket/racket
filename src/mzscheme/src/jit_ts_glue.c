 Scheme_Object* rtcall_siS_s(prim_siS_s f, Scheme_Object* g37, int g38, Scheme_Object** g39)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_s0 = g37;
    future->arg_i1 = g38;
    future->arg_S2 = g39;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_s_s(prim_s_s f, Scheme_Object* g40)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_s0 = g40;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall__s(prim__s f, )
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
  
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_ss_s(prim_ss_s f, Scheme_Object* g41, Scheme_Object* g42)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_s0 = g41;
    future->arg_s1 = g42;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_lS_s(prim_lS_s f, long g43, Scheme_Object** g44)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_l0 = g43;
    future->arg_S1 = g44;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_l_s(prim_l_s f, long g45)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_l0 = g45;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_bsi_v(prim_bsi_v f, Scheme_Bucket* g46, Scheme_Object* g47, int g48)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_b0 = g46;
    future->arg_s1 = g47;
    future->arg_i2 = g48;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_s_s(prim_s_s f, Scheme_Object* g49)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_s0 = g49;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_iiS_v(prim_iiS_v f, int g50, int g51, Scheme_Object** g52)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_i0 = g50;
    future->arg_i1 = g51;
    future->arg_S2 = g52;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_ss_v(prim_ss_v f, Scheme_Object* g53, Scheme_Object* g54)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_s0 = g53;
    future->arg_s1 = g54;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_b_v(prim_b_v f, Scheme_Bucket* g55)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_b0 = g55;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_sl_s(prim_sl_s f, Scheme_Object* g56, long g57)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_s0 = g56;
    future->arg_l1 = g57;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_iS_s(prim_iS_s f, int g58, Scheme_Object** g59)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_i0 = g58;
    future->arg_S1 = g59;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_s_v(prim_s_v f, Scheme_Object* g60)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_s0 = g60;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} Scheme_Object* rtcall_iSi_s(prim_iSi_s f, int g61, Scheme_Object** g62, int g63)
{
  START_XFORM_SKIP;
  future_t *future;
  Scheme_Object* retval;

  future = current_ft;
      future->arg_i0 = g61;
    future->arg_S1 = g62;
    future->arg_i2 = g63;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_s;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
} void rtcall_siS_v(prim_siS_v f, Scheme_Object* g64, int g65, Scheme_Object** g66)
{
  START_XFORM_SKIP;
  future_t *future;
  void retval;

  future = current_ft;
      future->arg_s0 = g64;
    future->arg_i1 = g65;
    future->arg_S2 = g66;
  future_do_runtimecall((void*)f, 0, NULL);
  future = current_ft;
  retval = future->retval_v;
  future->prim_data.retval = NULL;
  return retval;
  END_XFORM_SKIP;
}