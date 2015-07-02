#define SIG_siS_s 11
typedef Scheme_Object* (*prim_siS_s)(Scheme_Object*, int, Scheme_Object**);
Scheme_Object* scheme_rtcall_siS_s(const char *who, int src_type, prim_siS_s f, Scheme_Object* g183, int g184, Scheme_Object** g185);
#define SIG_iSs_s 12
typedef Scheme_Object* (*prim_iSs_s)(int, Scheme_Object**, Scheme_Object*);
Scheme_Object* scheme_rtcall_iSs_s(const char *who, int src_type, prim_iSs_s f, int g186, Scheme_Object** g187, Scheme_Object* g188);
#define SIG_s_s 13
typedef Scheme_Object* (*prim_s_s)(Scheme_Object*);
Scheme_Object* scheme_rtcall_s_s(const char *who, int src_type, prim_s_s f, Scheme_Object* g189);
#define SIG_n_s 14
typedef Scheme_Object* (*prim_n_s)(Scheme_Native_Closure_Data*);
Scheme_Object* scheme_rtcall_n_s(const char *who, int src_type, prim_n_s f, Scheme_Native_Closure_Data* g190);
#define SIG__s 15
typedef Scheme_Object* (*prim__s)();
Scheme_Object* scheme_rtcall__s(const char *who, int src_type, prim__s f );
#define SIG_ss_s 16
typedef Scheme_Object* (*prim_ss_s)(Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_ss_s(const char *who, int src_type, prim_ss_s f, Scheme_Object* g191, Scheme_Object* g192);
#define SIG_ssi_s 17
typedef Scheme_Object* (*prim_ssi_s)(Scheme_Object*, Scheme_Object*, int);
Scheme_Object* scheme_rtcall_ssi_s(const char *who, int src_type, prim_ssi_s f, Scheme_Object* g193, Scheme_Object* g194, int g195);
#define SIG_tt_s 18
typedef Scheme_Object* (*prim_tt_s)(const Scheme_Object*, const Scheme_Object*);
Scheme_Object* scheme_rtcall_tt_s(const char *who, int src_type, prim_tt_s f, const Scheme_Object* g196, const Scheme_Object* g197);
#define SIG_ss_m 19
typedef MZ_MARK_STACK_TYPE (*prim_ss_m)(Scheme_Object*, Scheme_Object*);
MZ_MARK_STACK_TYPE scheme_rtcall_ss_m(const char *who, int src_type, prim_ss_m f, Scheme_Object* g198, Scheme_Object* g199);
#define SIG_Sl_s 20
typedef Scheme_Object* (*prim_Sl_s)(Scheme_Object**, intptr_t);
Scheme_Object* scheme_rtcall_Sl_s(const char *who, int src_type, prim_Sl_s f, Scheme_Object** g200, intptr_t g201);
#define SIG_l_s 21
typedef Scheme_Object* (*prim_l_s)(intptr_t);
Scheme_Object* scheme_rtcall_l_s(const char *who, int src_type, prim_l_s f, intptr_t g202);
#define SIG_bsi_v 22
typedef void (*prim_bsi_v)(Scheme_Bucket*, Scheme_Object*, int);
void scheme_rtcall_bsi_v(const char *who, int src_type, prim_bsi_v f, Scheme_Bucket* g203, Scheme_Object* g204, int g205);
#define SIG_iiS_v 23
typedef void (*prim_iiS_v)(int, int, Scheme_Object**);
void scheme_rtcall_iiS_v(const char *who, int src_type, prim_iiS_v f, int g206, int g207, Scheme_Object** g208);
#define SIG_ss_v 24
typedef void (*prim_ss_v)(Scheme_Object*, Scheme_Object*);
void scheme_rtcall_ss_v(const char *who, int src_type, prim_ss_v f, Scheme_Object* g209, Scheme_Object* g210);
#define SIG_b_v 25
typedef void (*prim_b_v)(Scheme_Bucket*);
void scheme_rtcall_b_v(const char *who, int src_type, prim_b_v f, Scheme_Bucket* g211);
#define SIG_sl_s 26
typedef Scheme_Object* (*prim_sl_s)(Scheme_Object*, intptr_t);
Scheme_Object* scheme_rtcall_sl_s(const char *who, int src_type, prim_sl_s f, Scheme_Object* g212, intptr_t g213);
#define SIG_iS_s 27
typedef Scheme_Object* (*prim_iS_s)(int, Scheme_Object**);
Scheme_Object* scheme_rtcall_iS_s(const char *who, int src_type, prim_iS_s f, int g214, Scheme_Object** g215);
#define SIG_S_s 28
typedef Scheme_Object* (*prim_S_s)(Scheme_Object**);
Scheme_Object* scheme_rtcall_S_s(const char *who, int src_type, prim_S_s f, Scheme_Object** g216);
#define SIG_s_v 29
typedef void (*prim_s_v)(Scheme_Object*);
void scheme_rtcall_s_v(const char *who, int src_type, prim_s_v f, Scheme_Object* g217);
#define SIG_iSi_s 30
typedef Scheme_Object* (*prim_iSi_s)(int, Scheme_Object**, int);
Scheme_Object* scheme_rtcall_iSi_s(const char *who, int src_type, prim_iSi_s f, int g218, Scheme_Object** g219, int g220);
#define SIG_siS_v 31
typedef void (*prim_siS_v)(Scheme_Object*, int, Scheme_Object**);
void scheme_rtcall_siS_v(const char *who, int src_type, prim_siS_v f, Scheme_Object* g221, int g222, Scheme_Object** g223);
#define SIG_z_p 32
typedef void* (*prim_z_p)(size_t);
void* scheme_rtcall_z_p(const char *who, int src_type, prim_z_p f, size_t g224);
#define SIG_si_s 33
typedef Scheme_Object* (*prim_si_s)(Scheme_Object*, int);
Scheme_Object* scheme_rtcall_si_s(const char *who, int src_type, prim_si_s f, Scheme_Object* g225, int g226);
#define SIG_sis_v 34
typedef void (*prim_sis_v)(Scheme_Object*, int, Scheme_Object*);
void scheme_rtcall_sis_v(const char *who, int src_type, prim_sis_v f, Scheme_Object* g227, int g228, Scheme_Object* g229);
#define SIG_ss_i 35
typedef int (*prim_ss_i)(Scheme_Object*, Scheme_Object*);
int scheme_rtcall_ss_i(const char *who, int src_type, prim_ss_i f, Scheme_Object* g230, Scheme_Object* g231);
#define SIG_iSp_v 36
typedef void (*prim_iSp_v)(int, Scheme_Object**, void*);
void scheme_rtcall_iSp_v(const char *who, int src_type, prim_iSp_v f, int g232, Scheme_Object** g233, void* g234);
#define SIG_sss_s 37
typedef Scheme_Object* (*prim_sss_s)(Scheme_Object*, Scheme_Object*, Scheme_Object*);
Scheme_Object* scheme_rtcall_sss_s(const char *who, int src_type, prim_sss_s f, Scheme_Object* g235, Scheme_Object* g236, Scheme_Object* g237);
#define SIG__v 38
typedef void (*prim__v)();
void scheme_rtcall__v(const char *who, int src_type, prim__v f );
#define SIG_iS_v 39
typedef void (*prim_iS_v)(int, Scheme_Object**);
void scheme_rtcall_iS_v(const char *who, int src_type, prim_iS_v f, int g238, Scheme_Object** g239);
