#ifndef __fvm_h__

#define __fvm_h__

#include <flipper/types.h>

#include <fmr/fmr.h>

extern const struct _target fvm;

extern const struct _bus fdb;

#ifdef __private_include__

/* ~ FVM definitions. ~ */

enum { _fvm_configure, _fvm_invoke, _fvm_call, _fvm_push, _fvm_pull };

extern void fvm_configure(const struct _bus *bus);

extern uint32_t fvm_call(void);

extern uint32_t fvm_invoke(uint8_t object, uint8_t index, uint8_t argc, ...);

extern uint32_t fvm_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...);

extern void fvm_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...);

/* ~ FDB definitions. ~ */

enum { _fdb_configure, _fdb_enable, _fdb_disable, _fdb_ready, _fdb_put, _fdb_get, _fdb_push, _fdb_pull };

extern void fdb_configure(void *configuration);

extern void fdb_enable(void);

extern void fdb_disable(void);

extern bool fdb_ready(void);

extern void fdb_put(uint8_t byte);

extern uint8_t fdb_get();

extern void fdb_push(void *source, uint32_t length);

extern void fdb_pull(void *destination, uint32_t length);

#endif

#endif