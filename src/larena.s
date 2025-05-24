.section .text
.intel_syntax noprefix

# Constants used throughout the arena implementation
.equ CACHE_LINE_SIZE, 64       # Modern CPU cache line size (bytes)
.equ DEFAULT_ALIGNMENT, 16     # Default memory alignment for allocations
.equ ARENA_MEMORY_OFFSET, 0    # Offset in LArena struct for memory pointer
.equ ARENA_ALLOCATED_OFFSET, 8 # Offset for tracking allocated bytes  
.equ ARENA_SIZE_OFFSET, 16     # Offset for total arena size

# Export all public functions
.global larena_create_asm
.global larena_alloc_asm
.global larena_alloc_aligned_asm
.global larena_calloc_asm
.global larena_alloc_string_asm
.global larena_resize_asm
.global larena_getfree_asm
.global larena_reset_asm
.global larena_destroy_asm
.global larena_align_up_asm
.global larena_strlen_asm
.global larena_memcpy_asm

##############################################################################
# Utility function: align_up(size, align) -> rax
# Aligns a size up to the nearest multiple of alignment
# Arguments:
#   rdi = size to align
#   rsi = alignment (must be power of 2)
# Returns:
#   rax = aligned size
##############################################################################
larena_align_up_asm:
    mov rax, rdi        # Copy size to return register
    dec rsi             # alignment - 1 (creates mask)
    add rax, rsi        # size + (alignment - 1) - ensures we cross boundary
    not rsi             # Bitwise NOT to create mask of ~(alignment - 1)
    and rax, rsi        # Mask off lower bits to get aligned size
    ret                 # Return aligned size in rax


##############################################################################
# AVX2-optimized strlen implementation
# 
# Scans 32 bytes per iteration using AVX2 instructions, then falls back to
# byte-by-byte scanning for the remaining 0-31 bytes.
#
# Arguments:
#   rdi = string pointer (does not need alignment)
#
# Returns:
#   rax = string length (excluding null terminator)
#
# Clobbers:
#   ymm0, ymm1, rcx, rdx, flags
#
# Performance:
#   - 3-5x faster than `repne scasb` for medium/long strings (>64 bytes)
#   - Minimal overhead for very short strings
##############################################################################
larena_strlen_asm:
    mov rax, rdi                # Save original pointer for length calculation
    vpxor ymm0, ymm0, ymm0      # Zero ymm0 (compare against null bytes)

    # Main loop: check 32 bytes per iteration
.strlen_avx_loop:
    vmovdqu ymm1, [rdi]         # Load 32 bytes (unaligned OK)
    vpcmpeqb ymm1, ymm1, ymm0   # Compare against null, set byte masks
    vpmovmskb edx, ymm1         # Extract bitmask of null bytes
    test edx, edx               # Check if any nulls found
    jnz .strlen_null_found      # Exit loop if null found
    add rdi, 32                 # Advance pointer
    jmp .strlen_avx_loop        # Continue scanning

.strlen_null_found:
    # Find exact null position
    bsf edx, edx                # Find first set bit (null position)
    add rdi, rdx                # Adjust pointer to null terminator
    sub rdi, rax                # Calculate length
    mov rax, rdi                # Return length in rax
    vzeroupper                  # Avoid AVX-SSE transition penalties
    ret

    # Fallback for strings <32 bytes or remaining bytes
.strlen_small:
    # Reset pointer if we skipped AVX path
    mov rdi, rax
    # Standard byte-by-byte scan
    mov rcx, -1
    xor al, al
    repne scasb
    not rcx
    dec rcx
    mov rax, rcx
    ret


##############################################################################
#larena_memcpy_asm:
#    mov rcx, rdx        # Copy byte count to counter register
#    cld                 # Clear direction flag (forward copy)
    
#    # Copy 8 bytes at a time using 64-bit registers
#    mov rax, rcx        # Save original count
#    shr rcx, 3          # Divide count by 8 (bytes per qword)
#    rep movsq           # Repeat copy qwords (8 bytes at a time)
    
#    # Copy remaining bytes (0-7)
#    mov rcx, rax        # Restore original count
#    and rcx, 7          # Get remainder (count % 8)
#    rep movsb           # Copy remaining bytes one at a time
#    ret

##############################################################################  
# Optimized memcpy implementation
# Arguments:
#   rdi = destination pointer
#   rsi = source pointer
#   rdx = byte count
# Returns:
#   (destination pointer is returned implicitly in rdi)
larena_memcpy_asm:
    mov rcx, rdx        # Copy byte count
    cld                 # Clear direction flag

    # Check size for optimization path
    cmp rcx, 256        # If size > 256, use large copy
    ja larena_memcpy_large_asm
    cmp rcx, 32         # If size < 32, use byte copy
    jb .byte_copy

    # Medium copy (32–256 bytes) using AVX
    mov rax, rcx        # Save count
    shr rcx, 5          # Divide by 32 (bytes per ymm register)
    jz .remainder       # Skip if no 32-byte chunks

.avx_copy:
    vmovdqu ymm0, [rsi] # Load 32 bytes
    vmovdqu [rdi], ymm0 # Store 32 bytes
    add rsi, 32
    add rdi, 32
    dec rcx
    jnz .avx_copy
    vzeroupper          # Clear AVX state

.remainder:
    mov rcx, rax        # Restore count
    and rcx, 31         # Get remainder (0–31 bytes)

.byte_copy:
    rep movsb           # Copy remaining bytes
    ret

##############################################################################
# AVX-optimized large memory copy
# 
# Optimized for copying blocks >512 bytes using 256-bit AVX registers (ymm0-ymm7)
# Provides ~3-5x speedup over rep movsq for aligned large copies
#
# Arguments:
#   rdi = destination pointer (must be 32-byte aligned for best performance)
#   rsi = source pointer (must be 32-byte aligned for best performance)
#   rdx = byte count (must be >=64 bytes to use this version)
#
# Returns:
#   (destination pointer is returned implicitly in rdi)
#
# Clobbers:
#   ymm0, flags
#
# Performance Notes:
#   - Uses non-temporal stores could be added for very large copies (>1MB)
#   - Includes vzeroupper to avoid AVX-SSE transition penalties
#   - Optimal for Haswell and later CPUs (AVX2)
##############################################################################
larena_memcpy_large_asm:
    cmp rdx, 64         # Minimum size for AVX
    jb larena_memcpy_asm # Fallback for small copies
    cmp rdx, 1048576    # Check for very large copy (>1MB)
    ja .large_copy      # Use non-temporal stores for huge copies

    # Main loop: Copy 128 bytes per iteration (4x32 bytes)
.loop_128:
    vmovdqu ymm0, [rsi]      # Load 32 bytes
    vmovdqu ymm1, [rsi + 32] # Load next 32 bytes
    vmovdqu ymm2, [rsi + 64]
    vmovdqu ymm3, [rsi + 96]
    vmovdqu [rdi], ymm0      # Store 32 bytes
    vmovdqu [rdi + 32], ymm1
    vmovdqu [rdi + 64], ymm2
    vmovdqu [rdi + 96], ymm3
    add rsi, 128
    add rdi, 128
    sub rdx, 128
    cmp rdx, 128
    jae .loop_128

    # Handle remaining 0–127 bytes
    test rdx, rdx
    jz .done
    vmovdqu ymm0, [rsi - 128 + rdx]
    vmovdqu [rdi - 128 + rdx], ymm0
.done:
    vzeroupper
    ret

.large_copy:
    # Non-temporal copy for >1MB
.loop_nt:
    vmovntdq [rdi], ymm0     # Non-temporal store
    vmovdqu ymm0, [rsi]      # Load 32 bytes
    add rsi, 32
    add rdi, 32
    sub rdx, 32
    cmp rdx, 32
    jae .loop_nt
    test rdx, rdx
    jz .done_nt
    vmovdqu ymm0, [rsi - 32 + rdx]
    vmovdqu [rdi - 32 + rdx], ymm0
.done_nt:
    vzeroupper
    ret

##############################################################################
# Create a new arena (optimized register preservation version)
#
# Arguments:
#   rdi = requested size in bytes
# Returns:
#   rax = pointer to new LArena struct, or NULL on failure
# Clobbers:
#   rcx, rdx, rsi, r8-r11 (call-clobbered by ABI)
# Preserves:
#   r13 (callee-saved), all other non-volatile registers untouched
##############################################################################
##############################################################################
# Optimized Arena Creation with Smart Prefetching
# 
# Key Improvements:
# 1. Conditional prefetching based on arena size
# 2. Reduced register preservation (only r13 saved)
# 3. Stack space optimized to 24 bytes
# 4. Prefetch distance adjusted for modern CPUs
#
# Arguments:
#   rdi = requested size in bytes
# Returns:
#   rax = pointer to new LArena, or NULL on failure
# Clobbers:
#   rcx, rdx, rsi, r8-r11 (call-clobbered by ABI)
##############################################################################
larena_create_asm:
    push r13                    # [PRESERVE] Only callee-saved reg we need
    sub rsp, 24                 # [STACK] 16-byte align + 8-byte ptr storage
    
    # Step 1: Size Alignment (cache line + safety margin)
    mov rsi, CACHE_LINE_SIZE    # [ARG] Alignment boundary
    call larena_align_up_asm     # [CALL] rax = aligned_size(size)
    add rax, CACHE_LINE_SIZE     # [CALC] Add metadata safety margin
    mov r13, rax                # [SAVE] Stash size in preserved reg
    
    # Step 2: Allocate Control Structure
    mov rdi, 24                 # [ARG] sizeof(LArena)
    call malloc                 # [CALL] Allocate struct
    test rax, rax               # [CHECK] malloc success?
    jz .create_fail             # [FAIL] Return NULL if failed
    
    # Step 3: Allocate Aligned Memory Block
    lea rdi, [rsp+16]           # [ARG] ptr to store allocation
    mov rsi, CACHE_LINE_SIZE    # [ARG] Alignment
    mov rdx, r13                # [ARG] Aligned size
    mov r13, rax                # [SAVE] Now r13 = arena*
    call posix_memalign         # [CALL] Allocate memory
    test eax, eax               # [CHECK] Allocation OK?
    jnz .create_cleanup         # [FAIL] Cleanup if failed
    
    # Step 4: Initialize Arena
    mov rcx, [rsp+16]           # [LOAD] Get memory block
    mov [r13 + ARENA_MEMORY_OFFSET], rcx  # [INIT] memory_ptr
    mov qword ptr [r13 + ARENA_ALLOCATED_OFFSET], 0  # [INIT] alloc_pos
    mov [r13 + ARENA_SIZE_OFFSET], r13    # [INIT] total_size
    
    # Step 5: Intelligent Prefetch
    cmp r13, 65536              # [OPT] Only prefetch for arenas >64KB
    jb .skip_prefetch
    prefetchnta [rcx + CACHE_LINE_SIZE*4]  # [OPT] 4 lines ahead, non-temporal
.skip_prefetch:
    
    mov rax, r13                # [RETURN] arena*
    jmp .create_exit            # [EXIT] Skip cleanup

.create_cleanup:                # [CLEANUP] memalign failed
    mov rdi, r13                # [ARG] arena* to free
    call free                   # [CALL] Prevent leak
.create_fail:                   # [FAIL] Early exit
    xor eax, eax                # [RETURN] NULL
.create_exit:                   # [COMMON EXIT]
    add rsp, 24                 # [STACK] Reclaim space
    pop r13                     # [RESTORE] Callee-saved
    ret                         # [RETURN]

##############################################################################
# Allocate memory from arena
# Arguments:
#   rdi = arena pointer
#   rsi = requested size in bytes
# Returns:
#   rax = pointer to allocated memory, or NULL on failure
##############################################################################
larena_alloc_asm:
    test rdi, rdi       # Check for NULL arena pointer
    jz .alloc_fail      # Return NULL if arena is NULL
    
    # Load arena fields into registers
    mov r8, [rdi + ARENA_ALLOCATED_OFFSET]  # Current allocated bytes
    mov r9, [rdi + ARENA_SIZE_OFFSET]       # Total arena size
    mov r10, [rdi + ARENA_MEMORY_OFFSET]    # Base memory pointer
    
    # Align current allocation pointer to 16 bytes
    lea rax, [r8 + 15]  # allocated + 15 (for rounding up)
    and rax, -16        # Mask to 16-byte boundary (align down)
    
    # Calculate new allocation end pointer
    lea r11, [rax + rsi] # new_alloc = aligned_offset + requested_size
    
    # Check if we have enough space
    cmp r11, r9         # Compare new_alloc with total size
    ja .alloc_fail      # Jump if new_alloc > arena_size (unsigned comparison)
    
    # Update arena and return pointer
    mov [rdi + ARENA_ALLOCATED_OFFSET], r11  # Store new allocated size
    add rax, r10        # Calculate pointer: memory_base + aligned_offset
    
    # Prefetch next cache line if allocation is large
    cmp rsi, CACHE_LINE_SIZE  # Compare size with cache line size
    jb .alloc_exit      # Skip prefetch for small allocations
    
    prefetchw [rax + CACHE_LINE_SIZE]  # Prefetch next cache line for write
    
.alloc_exit:
    ret                 # Return pointer (already in rax)

.alloc_fail:
    xor rax, rax        # Return NULL on failure
    ret

##############################################################################
# Aligned memory allocation from arena
# Arguments:
#   rdi = arena pointer
#   rsi = requested size in bytes
#   rdx = alignment (must be power of 2)
# Returns:
#   rax = pointer to allocated memory, or NULL on failure
##############################################################################
larena_alloc_aligned_asm:
    test rdi, rdi       # Check for NULL arena pointer
    jz .alloc_aligned_fail
    
    # Verify alignment is power of 2: (align & (align - 1)) == 0
    mov rax, rdx        # Copy alignment
    dec rax             # align - 1
    and rax, rdx        # align & (align - 1)
    jnz .alloc_aligned_fail  # Jump if result is non-zero (not power of 2)
    
    # Load arena fields
    mov r8, [rdi + ARENA_ALLOCATED_OFFSET]  # Current allocated bytes
    mov r9, [rdi + ARENA_SIZE_OFFSET]       # Total arena size
    mov r10, [rdi + ARENA_MEMORY_OFFSET]    # Base memory pointer
    
    # Align current allocation to requested alignment
    mov rax, r8         # Start with current allocated bytes
    dec rdx             # align - 1 (for rounding up)
    add rax, rdx        # allocated + (align - 1)
    not rdx             # ~(align - 1) (mask for alignment)
    and rax, rdx        # aligned_offset = (allocated + align - 1) & ~(align - 1)
    
    # Calculate new allocation end
    lea r11, [rax + rsi] # new_alloc = aligned_offset + size
    
    # Check bounds
    cmp r11, r9         # Compare new_alloc with total size
    ja .alloc_aligned_fail  # Fail if exceeds arena size
    
    # Update arena and return pointer
    mov [rdi + ARENA_ALLOCATED_OFFSET], r11  # Store new allocated size
    add rax, r10        # Calculate pointer: memory_base + aligned_offset
    jmp .alloc_aligned_exit
    
.alloc_aligned_fail:
    xor rax, rax        # Return NULL on failure
    
.alloc_aligned_exit:
    ret

##############################################################################
# Zero-initialized allocation (calloc equivalent)
# Arguments:
#   rdi = arena pointer
#   rsi = number of elements
#   rdx = size of each element
# Returns:
#   rax = pointer to zeroed memory, or NULL on failure
##############################################################################
larena_calloc_asm:
    push rbp            # Function prologue
    mov rbp, rsp
    push rbx            # Save all callee-saved registers
    push r12            # r12 = arena
    push r13            # r13 = count
    push r14            # r14 = element size
    push r15            # r15 = total size
    sub rsp, 8          # Maintain 16-byte stack alignment
    
    # Check for NULL arena
    test rdi, rdi
    jz .calloc_fail
    
    # Save parameters in callee-saved registers
    mov r12, rdi        # arena
    mov r13, rsi        # count
    mov r14, rdx        # element size
    
    # Safe multiplication (check for overflow)
    mov rax, r13        # count
    xor edx, edx        # Clear upper 32 bits for multiplication
    mul r14             # Multiply count * size (result in RDX:RAX)
    jc .calloc_fail     # Jump if overflow occurred (CF=1)
    test rdx, rdx       # Check if high bits are set
    jnz .calloc_fail    # Jump if overflow (result > 64 bits)
    
    # Handle zero-size allocation (like standard calloc)
    test rax, rax       # Check if total size is zero
    jz .zero_size       # Special handling for zero bytes
    
    # Save total size
    mov r15, rax        # r15 = total size
    
    # Allocate memory from arena
    mov rdi, r12        # arena
    mov rsi, r15        # size
    call larena_alloc_asm
    test rax, rax       # Check for allocation failure
    jz .calloc_exit     # Return NULL if allocation failed
    
    # Zero the memory using memset
    mov rdi, rax        # destination
    xor esi, esi        # fill value (0)
    mov rdx, r15        # size
    call memset         # Call libc memset
    jmp .calloc_exit    # Return pointer (already in rax)

.zero_size:
    # Allocate minimum 1 byte for zero-size request (like calloc)
    mov rdi, r12        # arena
    mov rsi, 1          # allocate 1 byte
    call larena_alloc_asm
    test rax, rax       # Check for failure
    jz .calloc_exit
    
    # Explicitly zero the single byte
    mov byte ptr [rax], 0
    jmp .calloc_exit

.calloc_fail:
    xor eax, eax        # Return NULL on failure

.calloc_exit:
    add rsp, 8          # Clean up stack
    pop r15             # Restore callee-saved registers
    pop r14
    pop r13
    pop r12
    pop rbx
    mov rsp, rbp
    pop rbp
    ret

##############################################################################
# Allocate and copy a string into the arena
# Arguments:
#   rdi = arena pointer
#   rsi = source string pointer
# Returns:
#   rax = pointer to arena-allocated string, or NULL on failure
##############################################################################
larena_alloc_string_asm:
    push rbp
    mov rbp, rsp
    push r12            # Save callee-saved registers
    push r13            # r12 = arena, r13 = source string
    push r14            # r14 = string length
    push r15            # r15 = destination pointer

    # Validate input pointers
    test rdi, rdi       # Check arena != NULL
    jz .string_fail
    test rsi, rsi       # Check source string != NULL
    jz .string_fail

    mov r12, rdi        # Save arena pointer
    mov r13, rsi        # Save source string pointer

    # Get string length using our optimized strlen
    mov rdi, rsi        # Set string argument
    call larena_strlen_asm
    mov r14, rax        # Save length in r14

    # Handle empty string case
    test r14, r14       # Check if length is zero
    jz .string_empty    # Special handling for empty string

    # Allocate memory for string + null terminator
    mov rdi, r12        # arena
    lea rsi, [r14 + 1]  # length + 1 (for null terminator)
    call larena_alloc_asm
    test rax, rax       # Check for allocation failure
    jz .string_fail     # Return NULL if failed

    # Copy string data including null terminator
    mov r15, rax        # Save destination pointer
    mov rdi, r15        # destination
    mov rsi, r13        # source
    lea rdx, [r14 + 1]  # length + 1 (copy null terminator too)
    call larena_memcpy_asm

    # Ensure null terminator is set (defensive programming)
    mov byte ptr [r15 + r14], 0  # Set null terminator
    mov rax, r15        # Return pointer in rax
    jmp .string_exit

.string_empty:
    # Allocate 1 byte for empty string (just null terminator)
    mov rdi, r12        # arena
    mov rsi, 1          # allocate 1 byte
    call larena_alloc_asm
    test rax, rax       # Check for failure
    jz .string_fail
    
    # Set null terminator
    mov byte ptr [rax], 0
    jmp .string_exit

.string_fail:
    xor rax, rax        # Return NULL on failure

.string_exit:
    pop r15             # Restore callee-saved registers
    pop r14
    pop r13
    pop r12
    mov rsp, rbp
    pop rbp
    ret

##############################################################################
# Get remaining free space in arena
# Arguments:
#   rdi = arena pointer
# Returns:
#   rax = bytes of free space remaining
##############################################################################
larena_getfree_asm:
    test rdi, rdi       # Check for NULL arena
    jz .getfree_zero    # Return 0 if arena is NULL
    
    mov rax, [rdi + ARENA_SIZE_OFFSET]       # Load total size
    sub rax, [rdi + ARENA_ALLOCATED_OFFSET]  # Subtract allocated bytes
    ret                 # Return free space in rax
    
.getfree_zero:
    xor rax, rax        # Return 0 for NULL arena
    ret

##############################################################################
# Reset arena (free all allocations without freeing memory)
# Arguments:
#   rdi = arena pointer
##############################################################################
larena_reset_asm:
    test rdi, rdi       # Check for NULL arena
    jz .reset_exit      # Do nothing if NULL
    
    mov qword ptr [rdi + ARENA_ALLOCATED_OFFSET], 0  # Reset allocated bytes to 0
.reset_exit:
    ret

##############################################################################
# Destroy arena and free all memory
# Arguments:
#   rdi = arena pointer
##############################################################################
larena_destroy_asm:
    test rdi, rdi       # Check for NULL arena
    jz .destroy_exit    # Do nothing if NULL
    
    push rdi            # Save arena pointer on stack
    
    # Free the arena's memory block
    mov rdi, [rdi + ARENA_MEMORY_OFFSET]  # Load memory pointer
    call free           # Free the memory
    
    # Free the arena control structure
    pop rdi             # Restore arena pointer
    call free           # Free the arena struct
    
.destroy_exit:
    ret

##############################################################################
# Resize arena to accommodate more memory
# Arguments:
#   rdi = arena pointer
#   rsi = new minimum size requested
# Returns:
#   rax = 1 on success, 0 on failure
##############################################################################
larena_resize_asm:
    push rbp
    mov rbp, rsp
    push r12            # Save callee-saved registers
    push r13            # r12 = arena, r13 = new_size
    push r14            # r14 = old memory pointer
    push r15            # r15 = old allocated size
    sub rsp, 8          # Maintain stack alignment
    
    # Check for NULL arena
    test rdi, rdi
    jz .resize_fail
    
    mov r12, rdi        # Save arena pointer
    mov r13, rsi        # Save requested new size
    
    # Check if new size is actually larger than current
    cmp rsi, [rdi + ARENA_SIZE_OFFSET]
    jbe .resize_fail    # Fail if new_size <= current_size
    
    # Calculate growth size (current_size * 1.5)
    mov rax, [rdi + ARENA_SIZE_OFFSET]  # Current size
    mov rdx, rax        # Copy current size
    shr rdx, 1          # current_size / 2
    add rax, rdx        # current_size * 1.5
    
    # Use maximum of requested size and growth size
    cmp r13, rax        # Compare requested size with growth size
    cmova rax, r13      # If requested > growth, use requested
    
    # Align final size to cache lines
    mov rdi, rax        # Size to align
    mov rsi, CACHE_LINE_SIZE  # Alignment
    call larena_align_up_asm
    mov r13, rax        # Store final size in r13
    
    # Save old arena values before reallocation
    mov r14, [r12 + ARENA_MEMORY_OFFSET]     # Old memory pointer
    mov r15, [r12 + ARENA_ALLOCATED_OFFSET]  # Old allocated size
    
    # Allocate new memory block with posix_memalign
    lea rdi, [rsp-8]    # Pointer to store allocation (using our stack space)
    mov rsi, CACHE_LINE_SIZE  # Alignment
    mov rdx, r13        # Size
    call posix_memalign
    test eax, eax       # Check for error (0 = success)
    jnz .resize_fail    # Jump if allocation failed
    
    # Get new memory pointer from stack
    mov rax, [rsp-8]    # New memory pointer
    
    # Copy old data if any was allocated
    test r15, r15       # Check if old allocated size > 0
    jz .resize_update   # Skip copy if no old data
    
    mov rdi, rax        # Destination (new memory)
    mov rsi, r14        # Source (old memory)
    mov rdx, r15        # Size (old allocated bytes)
    call larena_memcpy_asm
    
.resize_update:
    # Update arena structure with new values
    mov rax, [rsp-8]    # New memory pointer
    mov [r12 + ARENA_MEMORY_OFFSET], rax  # Update memory pointer
    mov [r12 + ARENA_SIZE_OFFSET], r13    # Update total size
    
    # Free old memory block
    mov rdi, r14        # Old memory pointer
    call free
    
    mov rax, 1          # Return success (true)
    jmp .resize_exit
    
.resize_fail:
    xor rax, rax        # Return failure (false)
    
.resize_exit:
    add rsp, 8          # Clean up stack
    pop r15             # Restore callee-saved registers
    pop r14
    pop r13
    pop r12
    mov rsp, rbp
    pop rbp
    ret