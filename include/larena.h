#ifndef LARENA_H
#define LARENA_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Arena structure - matches assembly layout exactly
typedef struct LArena {
    char* memory;      // +0  (8 bytes)
    size_t allocated;  // +8  (8 bytes)
    size_t size;       // +16 (8 bytes)
} LArena;              // Total: 24 bytes

// Constants
#define LARENA_CACHE_LINE_SIZE 64
#define LARENA_DEFAULT_ALIGNMENT 16
#define LARENA_MIN_ARENA_SIZE 4096

// Pure assembly functions
extern LArena* larena_create_asm(size_t size);
extern void* larena_alloc_asm(LArena* arena, size_t size);
extern void* larena_alloc_aligned_asm(LArena* arena, size_t size, size_t align);
extern void* larena_calloc_asm(LArena* arena, size_t count, size_t size);
extern char* larena_alloc_string_asm(LArena* arena, const char* str);
extern bool larena_resize_asm(LArena* arena, size_t new_size);
extern size_t larena_getfree_asm(LArena* arena);
extern void larena_reset_asm(LArena* arena);
extern void larena_destroy_asm(LArena* arena);

// Utility functions
extern size_t larena_align_up_asm(size_t size, size_t align);
extern size_t larena_strlen_asm(const char* str);
extern void larena_memcpy_asm(void* dest, const void* src, size_t n);

// C wrapper functions (optional - call assembly directly for max performance)
static inline LArena* larena_create(size_t size) {
    return larena_create_asm(size);
}

static inline void* larena_alloc(LArena* arena, size_t size) {
    return larena_alloc_asm(arena, size);
}

static inline void* larena_alloc_aligned(LArena* arena, size_t size, size_t align) {
    return larena_alloc_aligned_asm(arena, size, align);
}

static inline void* larena_calloc(LArena* arena, size_t count, size_t size) {
    return larena_calloc_asm(arena, count, size);
}

static inline char* larena_alloc_string(LArena* arena, const char* str) {
    return larena_alloc_string_asm(arena, str);
}

static inline bool larena_resize(LArena* arena, size_t new_size) {
    return larena_resize_asm(arena, new_size);
}

static inline size_t larena_getfree(LArena* arena) {
    return larena_getfree_asm(arena);
}

static inline void larena_reset(LArena* arena) {
    larena_reset_asm(arena);
}

static inline void larena_destroy(LArena* arena) {
    larena_destroy_asm(arena);
}

#ifdef __cplusplus
}
#endif

#endif  // LARENA_H
