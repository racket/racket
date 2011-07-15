#define SIG_siS_s 10
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g169, int g170, Scheme_Object** g171);
#define SIG_iSs_s 11
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g172, Scheme_Object** g173, Scheme_Object* g174);
#define SIG_s_s 12
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g175);
#define SIG_n_s 13
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Closure_Data*);
Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g176);
#define SIG__s 14
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f );
#define SIG_ss_s 15
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g177, Scheme_Object* g178);
#define SIG_ssi_s 16
typedef Scheme_Object* (*prim_ssi_s)(Scheme_Object*, Scheme_Object*, int);
Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g179, Scheme_Object* g180, int g181);
#define SIG_tt_s 17
typedef Scheme_Object* (*prim_tt_s)(const Scheme_Object*, const Scheme_Object*);
Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g182, const Scheme_Object* g183);
#define SIG_ss_m 18
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g184, Scheme_Object* g185);
#define SIG_Sl_s 19
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, intptr_t);
Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g186, intptr_t g187);
#define SIG_l_s 20
typedef Scheme_Object* (*prim_l_s)(intptr_t);
Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g188);
#define SIG_bsi_v 21
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g189, Scheme_Object* g190, int g191);
#define SIG_iiS_v 22
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g192, int g193, Scheme_Object** g194);
#define SIG_ss_v 23
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g195, Scheme_Object* g196);
#define SIG_b_v 24
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g197);
#define SIG_sl_s 25
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, intptr_t);
Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g198, intptr_t g199);
#define SIG_iS_s 26
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g200, Scheme_Object** g201);
#define SIG_S_s 27
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g202);
#define SIG_s_v 28
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g203);
#define SIG_iSi_s 29
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g204, Scheme_Object** g205, int g206);
#define SIG_siS_v 30
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g207, int g208, Scheme_Object** g209);
#define SIG_z_p 31
typedef void* (*prim_z_p)(size_t);
void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g210);
#define SIG_si_s 32
typedef Scheme_Object* (*prim_si_s)(Scheme_Object*, int);
Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g211, int g212);
#define SIG_sis_v 33
typedef void (*prim_sis_v)(Scheme_Object*, int, Scheme_Object*);
void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g213, int g214, Scheme_Object* g215);
#define SIG_ss_i 34
typedef int (*prim_ss_i)(Scheme_Object*, Scheme_Object*);
int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g216, Scheme_Object* g217);
#define SIG_iSp_v 35
typedef void (*prim_iSp_v)(int, Scheme_Object**, void*);
void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g218, Scheme_Object** g219, void* g220);
#define SIG_sss_s 36
typedef Scheme_Object* (*prim_sss_s)(Scheme_Object*, Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_sss_s(const char *who, int src_type, prim_sss_s f, Scheme_Object* g221, Scheme_Object* g222, Scheme_Object* g223);
