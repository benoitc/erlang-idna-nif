/*

This file is part of hackney-idna released under the MIT license.
See the NOTICE for more information.

Copyright (c) 2014 Benoit Chesneau

*/

#ifdef DARWIN
#define U_HIDE_DRAFT_API 1
#define U_DISABLE_RENAMING 1
#endif

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "erl_nif.h"

#include "unicode/utypes.h"
#include "unicode/ustring.h"
#include "unicode/uidna.h"
#include "unicode/putil.h"


static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

typedef struct {
    ErlNifEnv* env;
    int error;
    UIDNA* uts46;
} ctx_t;

typedef struct {
    UIDNA** uts46s;
    int uts46StackTop;
    int numUTS46s;
    ErlNifMutex* uts46Mutex;
} priv_data_t;

typedef int32_t
(U_EXPORT2 *IDNAFunc) (const UIDNA *idna, const UChar *src, int32_t srcLength,
        UChar *dest, int32_t destCapacity,
        UIDNAInfo *pInfo, UErrorCode *status);


static ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
static ERL_NIF_TERM make_ok(ErlNifEnv* env, ERL_NIF_TERM value);
static ERL_NIF_TERM make_error(ErlNifEnv* env, const char* error);
static ERL_NIF_TERM to_ascii(ErlNifEnv *, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM to_unicode(ErlNifEnv *, int, const ERL_NIF_TERM []);
static ERL_NIF_TERM idna_binary(priv_data_t* pData, ctx_t* ctx,
        ERL_NIF_TERM term, IDNAFunc func);
static int on_load(ErlNifEnv*, void**, ERL_NIF_TERM);
static void on_unload(ErlNifEnv*, void*);
static __inline void reserve_uts46(priv_data_t*, ctx_t*);
static __inline void release_uts46(priv_data_t*, ctx_t*);
int on_reload(ErlNifEnv*, void**, ERL_NIF_TERM);
int on_upgrade(ErlNifEnv*, void**, void**, ERL_NIF_TERM);

/* functions to manage uts46 reserved instance */

void
reserve_uts46(priv_data_t* pData, ctx_t *ctx)
{
    if (ctx->uts46 == NULL) {
        enif_mutex_lock(pData->uts46Mutex);
        assert(pData->uts46StackTop < pData->numUTS46s);
        ctx->uts46 = pData->uts46s[pData->uts46StackTop];
        pData->uts46StackTop += 1;
        enif_mutex_unlock(pData->uts46Mutex);
    }
}

void
release_uts46(priv_data_t* pData, ctx_t *ctx)
{
    if (ctx->uts46 != NULL) {
        enif_mutex_lock(pData->uts46Mutex);
        pData->uts46StackTop -= 1;
        assert(pData->uts46StackTop >= 0);
        enif_mutex_unlock(pData->uts46Mutex);
    }
}

/* util functions */

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

ERL_NIF_TERM
make_ok(ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, ATOM_OK, value);
}

ERL_NIF_TERM
make_error(ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, ATOM_ERROR, make_atom(env, error));
}


/* main functions */


static ERL_NIF_TERM
to_ascii(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {

    ERL_NIF_TERM term = argv[0];
    ERL_NIF_TERM result;
    ctx_t ctx;
    priv_data_t* pData;
    IDNAFunc func = uidna_labelToASCII;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    /* init the conext */
    ctx.env = env;
    ctx.uts46 = NULL;
    pData = (priv_data_t*) enif_priv_data(env);

    /* convert to ascii */
    result = idna_binary(pData, &ctx, term, func);

    /* release the uts46 instance */
    release_uts46(pData, &ctx);
    /* return the result */
    return result;
}

static ERL_NIF_TERM
to_unicode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM term = argv[0];

    int result;
    ctx_t ctx;
    priv_data_t* pData;
    IDNAFunc func = uidna_labelToUnicode;

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    ctx.env = env;
    ctx.uts46 = NULL;
    pData = (priv_data_t*) enif_priv_data(env);

    result = idna_binary(pData, &ctx, term, func);

    release_uts46(pData, &ctx);

    /* return the result */
    return result;
}



ERL_NIF_TERM
idna_binary(priv_data_t* pData, ctx_t* ctx, ERL_NIF_TERM term, IDNAFunc func) {
    ErlNifBinary bin, to;
    UChar src[1000];
    UChar* dest =  malloc(sizeof(UChar) * 64);
    int32_t oLen = u_strlen(dest);
    int32_t destLen = 0;
    int32_t srcLen = 0;
    UErrorCode status;
    UIDNAInfo info = UIDNA_INFO_INITIALIZER;

    if(!enif_inspect_binary(ctx->env, term, &bin)) {
        return enif_make_badarg(ctx->env);
    }

    /* convert binary to uchar */
    u_charsToUChars((const char *)bin.data, src, (uint32_t)bin.size);
    srcLen = u_strlen(src);

    /* grab a UIDNA instance */
    reserve_uts46(pData, ctx);

    do {
        dest = realloc(dest, (sizeof(UChar *) * (oLen + destLen)));
        status = U_ZERO_ERROR;
        destLen =  func(ctx->uts46, src, srcLen, dest, destLen, &info,
                &status);
    } while ((status == U_BUFFER_OVERFLOW_ERROR && info.errors == 0));

    if (U_SUCCESS(status) && info.errors == 0) {
        enif_alloc_binary(destLen, &to);
        u_UCharsToChars(dest, (char *)to.data, destLen);
        return make_ok(ctx->env, enif_make_binary(ctx->env, &to));
    }

    return make_error(ctx->env, "noidna");
}

/* ------------------------------------------------------------------------
 * nif module declarations
 *  ----------------------------------------------------------------------- */

int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    UErrorCode status = U_ZERO_ERROR;
    priv_data_t* pData = (priv_data_t*)enif_alloc(sizeof(priv_data_t));
    int i, j;

    /* Initialize the structure */
    pData->uts46s = NULL;
    pData->uts46StackTop = 0;
    pData->numUTS46s = 0;
    pData->uts46Mutex = NULL;

    if (!enif_get_int(env, info, &(pData->numUTS46s) )) {
        enif_free((char*)pData);
        return 1;
    }

    if (pData->numUTS46s < 1) {
        enif_free((char*)pData);
        return 2;
    }

    pData->uts46Mutex = enif_mutex_create((char *)"uts46_mutex");

    if (pData->uts46Mutex == NULL) {
        enif_free((char*)pData);
        return 3;
    }

    pData->uts46s = enif_alloc(sizeof(UIDNA*) * pData->numUTS46s);

    if (pData->uts46s == NULL) {
        enif_mutex_destroy(pData->uts46Mutex);
        enif_free((char*)pData);
        return 4;
    }

    for (i = 0; i < pData->numUTS46s; i++) {

        pData->uts46s[i] = uidna_openUTS46(UIDNA_CHECK_BIDI, &status);

        if (U_FAILURE(status)) {
            for (j = 0; j < i; j++) {
                uidna_close(pData->uts46s[j]);
            }

            enif_free(pData->uts46s);
            enif_mutex_destroy(pData->uts46Mutex);
            enif_free((char*)pData);
            return 5;
        }
    }

    ATOM_OK = enif_make_atom(env, "ok");
    ATOM_ERROR = enif_make_atom(env, "error");

    *priv_data = pData;

    return 0;
}


void
on_unload(ErlNifEnv* env, void* priv_data)
{
    priv_data_t* pData = (priv_data_t*)priv_data;
    if (pData->uts46s != NULL) {
        int i;

        for (i = 0; i < pData->numUTS46s; i++) {
            uidna_close(pData->uts46s[i]);
        }

        enif_free(pData->uts46s);
    }

    if (pData->uts46Mutex != NULL) {
        enif_mutex_destroy(pData->uts46Mutex);
    }

    enif_free((char*)pData);
}

int
on_reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM info)
{
    return 0;
}

int
on_upgrade(ErlNifEnv* env, void** priv_data, void** old_data, ERL_NIF_TERM info)
{
    if (*old_data != NULL) {
        priv_data_t* pData = (priv_data_t*)old_data;

        if (pData->uts46s != NULL) {
            int i;

            for (i = 0; i < pData->numUTS46s; i++) {
                uidna_close(pData->uts46s[i]);
            }

            enif_free(pData->uts46s);
        }

        if (pData->uts46Mutex != NULL) {
            enif_mutex_destroy(pData->uts46Mutex);
        }

        enif_free((char*)pData);
    }

    return on_load(env, priv_data, info);
}


static ErlNifFunc funcs[] =
{
    {"to_ascii_nif", 1, to_ascii},
    {"to_unicode_nif", 1, to_unicode}
};

ERL_NIF_INIT(idna_nif, funcs, &on_load, &on_reload, &on_upgrade, &on_unload)
