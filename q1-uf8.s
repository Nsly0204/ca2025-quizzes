.data
fail_1:   .string "Failed test1: value < 16\n"
fail_2:   .string "Failed test2: value = 127\n"
success:  .string "All test passes!\n"
.text
main:
#    li      a0, 0x8FFFFFFF
#    jal     ra, clz
#    li      a7, 10                 # System call code for exiting the program
#    ecall                          # Make the exit system call
    add     s1, x0, x0              # error = s1 = 0
# test value < 16
    li      s0, 15
    mv      a0, s0
    jal     ra, uf8_decode
    jal     ra, uf8_encode
    beq     a0, s0, test2
    # test1 failed
    addi    s1, s1, 1
    la      a0, fail_1              # Load the address of the string 
    li      a7, 4                   # System call code for printing a string
    ecall                           # Print the string

test2:
    li      s0, 128
    mv      a0, s0
    jal     ra, uf8_decode
    jal     ra, uf8_encode
    # 
    li a7, 1                        # System call code for printing an integer
    ecall                           # Print the integer (X)

    beq     a0, s0, end_main
    # test 2 failed
    addi    s1, s1, 1
    la      a0, fail_2              # Load the address of the string 
    li      a7, 4                   # System call code for printing a string
    ecall                           # Print the string
end_main:
    bne     x0, s1, Exit
    la      a0, success             # Load the address of the string 
    li      a7, 4                   # System call code for printing a string
    ecall                        

Exit:
    li      a7, 10                  # System call code for exiting the program
    ecall                           # Make the exit system call

############################################
clz:
# input: a0 = x
# output: a0 = Count Leading Zeros
##
# t0 = n
# t1 = c
# t2 = x
# t3 = y 
    
# callee save
    addi    sp, sp, -4
    sw      ra, 0(sp)

    addi    t0, x0, 32
    addi    t1, x0, 16
    mv      t2, a0
clz_loop:
    srl     t3, t2, t1                      # y = x >> c
    beq     t3, x0, dont_found_one          # if (y==0), go to next half
    sub     t0, t0, t1                      # n = n - c
    mv      t2, t3                          # x = y
dont_found_one:
    srli    t1, t1, 1                       # c >> 1, search next half of x
    bne     t1, x0, clz_loop                # while c != 0, keep searching

    sub     a0, t0, t2                      # return (n - x)
# retrieve ra and callee save
    lw      ra, 0(sp)
    addi    sp, sp, 4
    ret

   
############################################
# input: a0 = fl (uint8_t)
# output: a0 = decoded integer ranging [0,1,015,792]
##
# t0 = mantissa
# t1 = exponent
# t2 = offset
uf8_decode:
# callee save
    addi    sp, sp, -4
    sw      ra, 0(sp)
#
    andi    t0, a0, 0x0F                    # (m)antissa = fl & 0x0f;
    srli    t1, a0, 4                       # (e)xponent = fl >> 4 = fl/16
    li      t2, 0x7FFF
    addi    t3, t1, -15                     # t3 = exponent -15
    sub     t3, x0, t3                      # t3 = 15 - exponent
    srl     t2, t2, t3                      # offset = 0x7FFF >> 15 - exponent
    slli    t2, t2, 4                       # offset = 0x7FFF * 2^(15-e) * 16
#
    sll     a0, t0, t1
    add     a0, a0, t2

# retrieve ra and callee save
    lw      ra, 0(sp)
    addi    sp, sp, 4
    ret

############################################
# input: a0 = integer ranging [0,1,015,792]
# output: a0 = encoded uf8
###
# s0 = value
# s1 = exponent
# s2 = overflow 
# s3 = mantissa
uf8_encode:
    # callee save
    addi    sp, sp, -20
    sw      ra, 0(sp)
    sw      s0, 4(sp)
    sw      s1, 8(sp)
    sw      s2, 12(sp)
    sw      s3, 16(sp)
# initailization
    mv      s0, a0                          # s0 = value
    add     s1, x0, x0                      # s1 = exponent = 0
    add     s2, x0, x0                      # s2 = overflow = 0
# value < 16, don't need to encode
    addi    t0, x0, 16
    blt     s0, t0, find_exact_exp          # return a0 = value
# find msb from clz
    # a0 = value
    jal     ra, clz                         # a0 = clz(value)
    addi    t0, x0, 31                      # t0 = 31
    sub     t0, t0, a0                      # t0 = msb = 31 - lz
    addi    t1, x0, 5                       # t1 = 5
    blt     t0, t1, find_exact_exp          # msb < 5, don't need estimate
exp_estimate:
    # rule of thumb
    add     s1, x0, t0                      # s1 exponent = msb
    addi    s1, s1, -4                      # s1 exponent = msb - 4
    addi    t0, t0, 15                      # t0 = 15
    bge     t0, s1, cal_overflow            # exponent <= 15, jump
    addi    s1, s1, 15
    # Calculate overflow for estimated exponent
    # t0 = e
    # s2 = overflow starts from 0
cal_overflow:
    add     t0, x0, x0                      # e = 0
overflow_loop:
    bge     t0, s1, adjust_est              # e < exponent then keep looping, esle adjust estimation
    slli    s2, s2, 1                       # overflow << 1
    addi    s2, s2, 16
end_overflow_loop:
    addi    t0, t0, 1                       # e++
    jal     x0, overflow_loop               # go back to for loop

adjust_est:
    bge     x0, s1, find_exact_exp          # exponent <= 0, end adjust
    bge     s0, s2, find_exact_exp          # value >= overflow, end adjust
    addi    s2, s2, -16
    srli    s2, s2, 1                       # overflow = (overflow - 16) >> 1
    addi    s1, s1, -1                      # exponent --
    jal     x0, adjust_est

find_exact_exp:
    # t0 = 15
    # t1 = next overflow
    addi    t0, x0, 15
    bge     s1, t0, encode_result           # while (exponent < 15), exp >= 15 jump
    slli    t1, s2, 1                       #
    addi    t1, t1, 16                      # next_overflow = (overflow << 1) + 16
    blt     s0, t1, encode_result           # if (value < next_overflow), break
    add     s2, x0, t1                      # overflow = next_overflow
    addi    s1, s1, 1                       # exponent ++
    jal     x0, find_exact_exp

encode_result:
    sub     s3, s0, s2                      # mantissa = (value - overflow)
    srl     s3, s3, s1                      # mantissa = (value - overflow) >> exponent
    slli    a0, s1, 4
    or      a0, a0, s3                      # return (exponent << 4) | mantissa

end_encode:
# retrieve ra and callee save
    lw      s3, 16(sp)
    lw      s2, 12(sp)
    lw      s1, 8(sp)
    lw      s0, 4(sp)
    lw      ra, 0(sp)
    addi    sp, sp, 20
    ret