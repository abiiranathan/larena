#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../include/larena.h"

void test_basic_allocation() {
    printf("Testing basic allocation...\n");

    LArena* arena = larena_create(4096);
    assert(arena != NULL);

    void* ptr1 = larena_alloc(arena, 100);
    assert(ptr1 != NULL);

    void* ptr2 = larena_alloc(arena, 200);
    assert(ptr2 != NULL);
    assert(ptr2 != ptr1);

    larena_destroy(arena);
    printf("✓ Basic allocation tests passed\n");
}

void test_string_allocation() {
    printf("Testing string allocation...\n");

    LArena* arena = larena_create(4096);

    char* str1 = larena_alloc_string(arena, "Hello, World!");
    assert(str1 != NULL);
    assert(strcmp(str1, "Hello, World!") == 0);

    char* str2 = larena_alloc_string(arena, "");
    assert(str2 != NULL);
    assert(strlen(str2) == 0);

    larena_destroy(arena);
    printf("✓ String allocation tests passed\n");
}

void test_alignment() {
    printf("Testing alignment...\n");

    LArena* arena = larena_create(4096);

    void* ptr1 = larena_alloc_aligned(arena, 100, 32);
    assert(ptr1 != NULL);
    assert(((uintptr_t)ptr1 % 32) == 0);

    void* ptr2 = larena_alloc_aligned(arena, 200, 64);
    assert(ptr2 != NULL);
    assert(((uintptr_t)ptr2 % 64) == 0);

    larena_destroy(arena);
    printf("✓ Alignment tests passed\n");
}

void test_calloc() {
    printf("Testing calloc...\n");

    LArena* arena = larena_create(4096);

    int* numbers = larena_calloc(arena, 10, sizeof(int));
    assert(numbers != NULL);

    for (int i = 0; i < 10; i++) {
        printf("Number at %d=%d\n", i, numbers[i]);
        assert(numbers[i] == 0);
    }

    larena_destroy(arena);
    printf("✓ Calloc tests passed\n");
}

void test_reset_and_reuse() {
    printf("Testing reset and reuse...\n");

    LArena* arena = larena_create(4096);

    void* ptr1 = larena_alloc(arena, 1000);
    assert(ptr1 != NULL);

    size_t used_before = arena->allocated;
    assert(used_before >= 1000);

    larena_reset(arena);
    assert(arena->allocated == 0);

    void* ptr2 = larena_alloc(arena, 1000);
    assert(ptr2 == ptr1);  // Should reuse same memory

    larena_destroy(arena);
    printf("✓ Reset and reuse tests passed\n");
}

void test_memory_copy() {
    char dst[4096] = {0};
    char src[1024] = {};
    strcpy(src, "Hello from source\n");

    larena_memcpy_asm(dst, src, sizeof(src));
    assert(strcmp(src, dst) == 0);
    memset(dst, 0, sizeof(dst));
    larena_memcpy_asm(dst, src, sizeof(src));
    assert(strcmp(src, dst) == 0);

    // copy 1MB
    char s[1 * 1024 * 1024]  = {0};
    char s2[1 * 1024 * 1024] = {0};

    larena_memcpy_asm(s2, s, sizeof(s2));
    printf("Copied large string buffers\n");
}

void benchmark_allocation() {
    printf("Running allocation benchmark...\n");
    const size_t num_allocs = 100000;
    const size_t alloc_size = 1024;

    // Benchmark arena allocation
    LArena* arena = larena_create(num_allocs * alloc_size * 2);

    clock_t start = clock();
    for (size_t i = 0; i < num_allocs; i++) {
        void* ptr = larena_alloc(arena, alloc_size);
        assert(ptr != NULL);
    }
    clock_t arena_time = clock() - start;

    larena_destroy(arena);

    // Benchmark malloc
    void** ptrs = malloc(num_allocs * sizeof(void*));
    start       = clock();
    for (size_t i = 0; i < num_allocs; i++) {
        ptrs[i] = malloc(alloc_size);
        assert(ptrs[i] != NULL);
    }
    clock_t malloc_time = clock() - start;

    for (size_t i = 0; i < num_allocs; i++) {
        free(ptrs[i]);
    }
    free(ptrs);

    printf("Arena allocations: %.2f ms\n", (double)arena_time / CLOCKS_PER_SEC * 1000);
    printf("Malloc allocations: %.2f ms\n", (double)malloc_time / CLOCKS_PER_SEC * 1000);
    printf("Speedup: %.1fx\n", (double)malloc_time / arena_time);
}

int main() {
    printf("Linear Arena Allocator Test Suite\n");
    printf("=================================\n");

    test_basic_allocation();
    test_string_allocation();
    test_alignment();
    test_calloc();
    test_reset_and_reuse();
    test_memory_copy();

    printf("\n");
    benchmark_allocation();

    printf("\n✓ All tests passed!\n");
    return 0;
}
