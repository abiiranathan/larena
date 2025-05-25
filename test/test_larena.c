#include <assert.h>
#include <stdint.h>
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

    larena_memcpy(dst, src, sizeof(src));
    assert(strcmp(src, dst) == 0);
    memset(dst, 0, sizeof(dst));
    larena_memcpy(dst, src, sizeof(src));
    assert(strcmp(src, dst) == 0);

    // copy 1MB
    char s[1 * 1024 * 1024]  = {"Hello world from S"};
    char s2[1 * 1024 * 1024] = {"Hello World from S2"};

    larena_memcpy(s2, s, sizeof(s2));  // s2 is overwritten
    printf("Copied large string buffers\n");
    assert(memcmp(s2, s, sizeof(s2)) == 0);
}

void test_arena_strlen() {
    // Short strings
    const char* s1 = "Lorem ipsum dolor!";
    size_t len     = larena_strlen(s1);
    assert(len == 18);

    // Long strings > 32 bytes (uses SIMD - AVX2)
    s1  = "hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh";
    len = larena_strlen(s1);
    assert(len == 62);

    // make sure we handle NULL
    assert(larena_strlen(NULL) == 0);
}

void test_larena_memset() {
    printf("Testing larena_memset...\n");

    char buffer[256];
    memset(buffer, 0xCD, sizeof(buffer));  // poison

    larena_memset(buffer, 0xAB, sizeof(buffer));
    for (int i = 0; i < 256; i++) {
        assert((unsigned char)buffer[i] == 0xAB);
    }

    printf("✓ larena_memset test passed\n");
}

void test_larena_strncpy() {
    printf("Testing larena_strncpy...\n");

    char dst[128];
    const char* src = "AVX2-powered strncpy test";

    char* out = larena_strncpy(dst, src, sizeof(dst));
    assert(out == dst);
    assert(strcmp(dst, src) == 0);

    // Copy smaller string into larger buffer — padding should occur
    memset(dst, 0xCD, sizeof(dst));  // poison it first
    out = larena_strncpy(dst, "Short", 16);
    assert(strcmp(dst, "Short") == 0);
    for (int i = 5; i < 16; i++) {
        assert(dst[i] == '\0');
    }

    // Copy with exact n, no null terminator expected
    const char* long_str = "This is exactly 31 chars....";
    assert(strlen(long_str) == 28);
    memset(dst, 0xCD, sizeof(dst));  // reset poison
    out = larena_strncpy(dst, long_str, 28);
    assert(memcmp(dst, long_str, 28) == 0);
    // Check no overflow
    dst[28] = 0xAA;
    assert(dst[28] == (char)0xAA);

    printf("✓ larena_strncpy tests passed\n");
}

void test_larena_memcmp() {
    printf("Testing larena_memcmp...\n");

    char a[] = "Hello, world!";
    char b[] = "Hello, world!";
    char c[] = "Hello, worle!";  // one byte off

    assert(larena_memcmp(a, b, strlen(a)) == 0);
    assert(larena_memcmp(a, c, strlen(a)) != 0);
    assert(larena_memcmp(a, c, 11) == 0);  // first 11 bytes same

    char x[] = {0x00, 0x01, 0x02};
    char y[] = {0x00, 0x01, 0x03};

    assert(larena_memcmp(x, y, 3) < 0);  // 0x02 < 0x03

    printf("✓ larena_memcmp tests passed\n");
}

void test_larena_memchr() {
    printf("Testing larena_memchr...\n");

    char data[] = "Search for this char!";
    char* found = larena_memchr(data, 'f', strlen(data));
    assert(found != NULL && *found == 'f');

    found = larena_memchr(data, 'z', strlen(data));
    assert(found == NULL);  // not present

    found = larena_memchr(data, 'S', strlen(data));
    assert(found == data);  // first character

    found = larena_memchr(data, '\0', 100);  // should find terminator
    assert(found == data + strlen(data));

    printf("✓ larena_memchr tests passed\n");
}

// Function prototype (assumed)
void* larena_memmem(const void* haystack, size_t haystack_len, const void* needle, size_t needle_len);

// Test helper to print test results
static void print_test_result(const char* test_name, int passed) {
    printf("%s: %s\n", test_name, passed ? "PASSED" : "FAILED");
}

// Test 1: Basic string match
static void test_basic_string_match(void) {
    const char* haystack = "Hello, World!";
    const char* needle   = "World";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    printf("result: %s\n", (char*)result);
    assert(result == haystack + 7);  // "World" starts at offset 7
    print_test_result("Basic string match", result == haystack + 7);
}

// Test 2: No match
static void test_no_match(void) {
    const char* haystack = "Hello, World!";
    const char* needle   = "NotFound";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == NULL);
    print_test_result("No match", result == NULL);
}

// Test 3: Empty needle
static void test_empty_needle(void) {
    const char* haystack = "Hello, World!";
    const char* needle   = "";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = 0;

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == haystack);  // Empty needle typically returns start of haystack
    print_test_result("Empty needle", result == haystack);
}

// Test 4: Empty haystack
static void test_empty_haystack(void) {
    const char* haystack = "";
    const char* needle   = "Test";
    size_t haystack_len  = 0;
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == NULL);
    print_test_result("Empty haystack", result == NULL);
}

// Test 5: Needle longer than haystack
static void test_needle_longer_than_haystack(void) {
    const char* haystack = "Short";
    const char* needle   = "ThisIsTooLong";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == NULL);
    print_test_result("Needle longer than haystack", result == NULL);
}

// Test 6: Binary data match
static void test_binary_data_match(void) {
    unsigned char haystack[] = {0x00, 0x01, 0x02, 0x03, 0x04, 0x05};
    unsigned char needle[]   = {0x02, 0x03, 0x04};
    size_t haystack_len      = 6;
    size_t needle_len        = 3;

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == haystack + 2);
    print_test_result("Binary data match", result == haystack + 2);
}

// Test 7: Partial match at end
static void test_partial_match_at_end(void) {
    const char* haystack = "Hello, Wo";
    const char* needle   = "World";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == NULL);
    print_test_result("Partial match at end", result == NULL);
}

// Test 8: Multiple occurrences (finds first)
static void test_multiple_occurrences(void) {
    const char* haystack = "abaaba";
    const char* needle   = "aba";
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, haystack_len, needle, needle_len);
    assert(result == haystack);  // First "aba" at offset 0
    print_test_result("Multiple occurrences", result == haystack);
}

// Test 9: NULL haystack
static void test_null_haystack(void) {
    const char* needle = "Test";
    size_t needle_len  = strlen(needle);

    void* result = larena_memmem(NULL, 10, needle, needle_len);
    assert(result == NULL);
    print_test_result("NULL haystack", result == NULL);
}

// Test 10: NULL needle
static void test_null_needle(void) {
    const char* haystack = "Hello, World!";
    size_t haystack_len  = strlen(haystack);

    void* result = larena_memmem(haystack, haystack_len, NULL, 5);
    assert(result == NULL);
    print_test_result("NULL needle", result == NULL);
}

// Test 11: Single byte match
static void test_single_byte_match(void) {
    const char* haystack = "abcde";
    const char needle    = 'c';
    size_t haystack_len  = strlen(haystack);
    size_t needle_len    = 1;

    void* result = larena_memmem(haystack, haystack_len, &needle, needle_len);
    assert(result == haystack + 2);
    print_test_result("Single byte match", result == haystack + 2);
}

// Test 12: Zero haystack length
static void test_zero_haystack_length(void) {
    const char* haystack = "Non-empty";
    const char* needle   = "Test";
    size_t needle_len    = strlen(needle);

    void* result = larena_memmem(haystack, 0, needle, needle_len);
    assert(result == NULL);
    print_test_result("Zero haystack length", result == NULL);
}

// ========================================
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
    test_arena_strlen();

    test_larena_memset();
    test_larena_strncpy();
    test_larena_memcmp();
    test_larena_memchr();

    // memmem tests
    //==============
    printf("Running larena_memmem tests...\n");
    test_basic_string_match();
    test_no_match();
    test_empty_needle();
    test_empty_haystack();
    test_needle_longer_than_haystack();
    test_binary_data_match();
    test_partial_match_at_end();
    test_multiple_occurrences();
    test_null_haystack();
    test_null_needle();
    test_single_byte_match();
    test_zero_haystack_length();
    // ===============================

    printf("\n");
    benchmark_allocation();

    printf("\n✓ All tests passed!\n");
    return 0;
}
