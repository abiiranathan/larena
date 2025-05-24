#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../include/larena.h"

// Error logging (can be disabled)
#ifndef NDEBUG
#define LOG_ERROR(fmt, ...) fprintf(stderr, "[LARENA ERROR] " fmt "\n", ##__VA_ARGS__)
#else
#define LOG_ERROR(fmt, ...) ((void)0)
#endif

// C wrapper with error checking for create
LArena* larena_create_safe(size_t size) {
    if (size < LARENA_MIN_ARENA_SIZE) {
        size = LARENA_MIN_ARENA_SIZE;
    }
    return larena_create_asm(size);
}

// Debug function to print arena stats
void larena_debug_print(const LArena* arena) {
    if (!arena) {
        printf("Arena: NULL\n");
        return;
    }

    printf("Arena Debug Info:\n");
    printf("  Memory:    %p\n", arena->memory);
    printf("  Size:      %zu bytes\n", arena->size);
    printf("  Allocated: %zu bytes\n", arena->allocated);
    printf("  Free:      %zu bytes\n", arena->size - arena->allocated);
    printf("  Usage:     %.1f%%\n", arena->size > 0 ? (100.0 * arena->allocated) / arena->size : 0.0);
}

// Bulk string allocation
char** larena_alloc_strings(LArena* arena, const char** strings, size_t count) {
    if (!arena || !strings || count == 0) {
        return NULL;
    }

    // Allocate array of pointers
    char** result = larena_alloc_asm(arena, count * sizeof(char*));
    if (!result) {
        return NULL;
    }

    // Allocate each string
    for (size_t i = 0; i < count; i++) {
        result[i] = larena_alloc_string_asm(arena, strings[i]);
        if (!result[i] && strings[i]) {
            // Allocation failed - arena state is consistent but incomplete
            return NULL;
        }
    }
    return result;
}
