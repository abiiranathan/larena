#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "../include/larena.h"  // Your assembly function's header

// High-resolution timer
long long get_nanoseconds() {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000000000L + ts.tv_nsec;
}

// Function to fill a buffer with random printable ASCII characters
void fill_random_printable_ascii(char* buffer, size_t len) {
    if (len == 0)
        return;
    for (size_t i = 0; i < len; ++i) {
        buffer[i] = (rand() % (126 - 32 + 1)) + 32;  // ASCII 32-126
    }
    buffer[len - 1] = '\0';  // Ensure null termination for safety if used as C-string, though not for memmem
}

void run_benchmark(size_t haystack_len, size_t needle_len, int needle_occurrence_type) {
    char* haystack = (char*)malloc(haystack_len);
    char* needle   = (char*)malloc(needle_len + 1);  // +1 for potential null terminator if debugging

    if (!haystack || !needle) {
        fprintf(stderr, "Memory allocation failed\n");
        free(haystack);
        free(needle);
        return;
    }

    fill_random_printable_ascii(haystack, haystack_len);
    fill_random_printable_ascii(needle, needle_len);

    void* expected_ptr = NULL;
    size_t needle_pos  = (size_t)-1;  // Position where needle is placed, or -1 if not found

    switch (needle_occurrence_type) {
        case 0:  // Needle not present (modify slightly to ensure it's not there by chance)
            if (needle_len > 0) {
                char original_char     = needle[needle_len - 1];
                needle[needle_len - 1] = (original_char == 'a' ? 'b' : 'a');  // Flip last char
                // Standard memmem will find nothing
                expected_ptr           = memmem(haystack, haystack_len, needle, needle_len);
                needle[needle_len - 1] = original_char;  // Restore for our function
            } else {
                expected_ptr = memmem(haystack, haystack_len, needle, needle_len);  // Empty needle
            }
            break;
        case 1:  // Needle at the beginning
            if (needle_len > 0 && needle_len <= haystack_len) {
                memcpy(haystack, needle, needle_len);
                needle_pos = 0;
            }
            // fall through
        case 2:  // Needle in the middle
            if (needle_occurrence_type == 2 && needle_len > 0 && haystack_len > needle_len) {
                needle_pos = (haystack_len - needle_len) / 2;
                memcpy(haystack + needle_pos, needle, needle_len);
            }
            // fall through
        case 3:  // Needle at the end
            if (needle_occurrence_type == 3 && needle_len > 0 && haystack_len >= needle_len) {
                needle_pos = haystack_len - needle_len;
                memcpy(haystack + needle_pos, needle, needle_len);
            }
            break;
    }
    if (needle_len == 0)
        needle_pos = 0;  // Empty needle always "found" at start

    const int NUM_ITERATIONS = 100;  // Number of times to run for averaging
    long long time_larena    = 0;
    long long time_std       = 0;
    void *res_larena = NULL, *res_std = NULL;

    // Benchmark larena_memmem
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        long long start = get_nanoseconds();
        res_larena      = larena_memmem(haystack, haystack_len, needle, needle_len);
        long long end   = get_nanoseconds();
        time_larena += (end - start);
    }

    // Benchmark standard memmem
    for (int i = 0; i < NUM_ITERATIONS; ++i) {
        long long start = get_nanoseconds();
        res_std         = memmem(haystack, haystack_len, needle, needle_len);
        long long end   = get_nanoseconds();
        time_std += (end - start);
    }

    // Verification
    if (needle_pos != (size_t)-1) {  // If needle was intentionally placed
        expected_ptr = haystack + needle_pos;
    } else if (needle_len == 0) {
        expected_ptr = haystack;
    } else {  // Needle not present
        expected_ptr = NULL;
    }

    if (res_larena != res_std) {
        fprintf(stderr,
                "Mismatch! Haystack len: %zu, Needle len: %zu, Type: %d\n",
                haystack_len,
                needle_len,
                needle_occurrence_type);
        fprintf(stderr, "  Larena found: %p, Standard found: %p, Expected: %p\n", res_larena, res_std, expected_ptr);
        if (res_larena)
            fprintf(stderr, "  Larena offset: %ld\n", (char*)res_larena - haystack);
        if (res_std)
            fprintf(stderr, "  Std offset: %ld\n", (char*)res_std - haystack);
        if (expected_ptr)
            fprintf(stderr, "  Expected offset: %ld\n", (char*)expected_ptr - haystack);
    }
    assert(res_larena == res_std);  // Critical check

    const char* type_str;
    switch (needle_occurrence_type) {
        case 0:
            type_str = "Not Found";
            break;
        case 1:
            type_str = "At Start ";
            break;
        case 2:
            type_str = "In Middle";
            break;
        case 3:
            type_str = "At End   ";
            break;
        default:
            type_str = "Unknown  ";
            break;
    }
    if (needle_len == 0)
        type_str = "Empty Ndl";

    printf("| %10zu | %10zu | %-10s | %12.3f | %12.3f | %6.2fx |\n",
           haystack_len,
           needle_len,
           type_str,
           (double)time_larena / NUM_ITERATIONS,
           (double)time_std / NUM_ITERATIONS,
           (time_std > 0 && time_larena > 0) ? (double)time_std / time_larena : 0.0);

    free(haystack);
    free(needle);
}

int main() {
    srand(time(NULL));  // Seed random number generator

    printf("Benchmarking memmem implementations (%d iterations per test)\n", 1000 /* Must match NUM_ITERATIONS */);
    printf("| Haystack   | Needle     | Occurrence | Larena (ns)  | Standard (ns)| Speedup |\n");
    printf("|------------|------------|------------|--------------|--------------|---------|\n");

    // --- Test Cases ---
    // Empty needle
    run_benchmark(100, 0, 0);
    run_benchmark(10000, 0, 0);

    // Small needles (should use .small_needle_path or similar)
    size_t small_needles[] = {1, 4, 8, 15, 16};
    for (size_t sn_idx = 0; sn_idx < sizeof(small_needles) / sizeof(small_needles[0]); ++sn_idx) {
        size_t nl = small_needles[sn_idx];
        run_benchmark(100, nl, 1);   // Start
        run_benchmark(1000, nl, 2);  // Middle
        run_benchmark(5000, nl, 3);  // End
        run_benchmark(5000, nl, 0);  // Not found
    }

    // // Large needles
    // size_t large_needles[] = {256, 512, 1024};
    // for (size_t ln_idx = 0; ln_idx < sizeof(large_needles) / sizeof(large_needles[0]); ++ln_idx) {
    //     size_t nl = large_needles[ln_idx];
    //     run_benchmark(10000, nl, 1);
    //     run_benchmark(100000, nl, 2);
    //     run_benchmark(500000, nl, 3);
    //     run_benchmark(500000, nl, 0);
    // }

    // // Needle almost as large as haystack
    // run_benchmark(1000, 900, 2);
    // run_benchmark(1000, 900, 0);

    // Edge case: Haystack just large enough for needle
    // run_benchmark(64, 64, 1);
    // run_benchmark(65, 64, 1);  // needle at start
    // run_benchmark(65, 64, 3);  // needle at end [haystack+1]

    printf("\nNote: Timings are in nanoseconds (ns) per call.\n");
    printf("Speedup is Standard_Time / Larena_Time. >1.0x means larena_memmem is faster.\n");

    return 0;
}
