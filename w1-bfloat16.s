.data
Inf_pos:        .word   0x7F800000, 0x7F80
Inf_neg:        .word   0xFF800000, 0xFF80
NaN:            .word   0xFFC00000, 0xFFC0
normal:         .word   0x40490fd0, 0x4049
denormal:       .word   0x40000fd0

pass_convert:   .string "Convert Test Passed\n"
fail_msg:       .string "Test Failed !!!!! \n"
end_msg:        .string "System Pause"

BF16_SIGN_MASK: .word   0x8000
BF16_EXP_MASK:  .word   0x7F80
BF16_MANT_MASK: .word   0x007F
BF16_EXP_BIAS:  .word   127

BF16_NAN:       .word   0x7FC0
BF16_ZERO:      .word   0x0

.text
main:


mul_test:
        li      a0, 0x4040              # 3
        li      a1, 0x4040              # 3
        jal     ra, bf16_mul 
        li      a7, 10                  # System call code for exiting the program
        ecall                           # Make the exit system call
div_test:
        li      a0, 0x4110              # 9
        li      a1, 0x4040              # 3
        jal     ra, bf16_div
        li      a7, 10                  # System call code for exiting the program
        ecall                           # Make the exit system call
    # f32_to_bf16 test 
        # load argument
        # s0 = test argument
        la      s0, normal              # s0 = @M[f32]
        lw      s1, 0(s0)               # s1 = f32
        mv      a0, s1                  # passing a0 as argument
convert_test:
        jal     ra, f32_to_bf16         
        mv      t0, a0                  # get return value
        lw      t1, 4(s0)               # get answer
        beq     t0, t1, convert_pass    # if answer equal to return value then exit

    #fail message
        la      a0, fail_msg            # Load the address of the string ("Test Failed !!!!! \n")
        li      a7, 4                   # System call code for printing a string
        ecall                           # Print the string
        jal     x0, add_test
convert_pass:
        la      a0, pass_convert        # Load the address of the string 
        li      a7, 4                   # System call code for printing a string
        ecall                           # Print the string
add_test:
        li      a0, 1
        li      a1, 2
        jal     ra, bf16_add

Exit:
        la      a0, end_msg             # Load the address of the tring ("System Pause")
        li      a7, 4                   # System call code for printing a string
        ecall

        li      a7, 10                  # System call code for exiting the program
        ecall                           # Make the exit system call



########################################
f32_to_bf16:
    ####
    ## input argument
    # a0 = f32
    ## output argument
    # a0 = traslated bf16
    ####

    # callee save
    addi    sp, sp, -8
    sw      ra, 0(sp)
    sw      s0, 4(sp)

    # get input argument
    mv      s0, a0
    # NaN or Inf check
    li      t0, 0xFF                    # t0 = 0xFF (exp mask)
    srli    t2, s0, 23                  # (f32bits >> 23)
    and     t1, t2, t0                  # (exp of fp32) t1 = (f32bits >> 23) & 0xFF
    beq     t1, t0, exception           # if (f32bits >> 23) & 0xFF == 0xFF
    # convert
    srli    t0, s0, 16                  # t0 = fp32 >> 16
    # round to even
    addi    t1, x0, 1                   # t1 = 1
    and     t0, t0, t1                  # t0 = (fp32 >> 16) & 1, the 16-th bit in mantissa
    li      t1, 0x7FFF                  # t1 = 0x7FFF
    add     t0, t0, t1                  # t0 = ((fp32 >> 16) & 1) + 0x7FFF, t0 = carry
    add     t0, s0, t0                  # t0 = fp32 + carry
    srli    a0, t0, 16                  # a0 = (fp32 + carry) >> 16
    beq     x0, x0, end_f32_to_bf16


exception:
    srli    a0, s0, 16                  # a0 = fp32 >> 16

end_f32_to_bf16:
    # retrieve ra and callee save
    lw      ra, 0(sp)
    lw      s0, 4(sp)
    addi    sp, sp, 8
    ret

########################################
bf16_to_f32:
    ####
    ## input argument
    # a0 = bf16
    ## output argument
    # a0 = traslated fp32
    ####
    srli    a0, a0, 16
    ret

########################################
bf16_add:
    ####
    ## input argument
    # a0 = (bf16) a
    # a1 = (bf16) b
    ## output argument
    # a0 = (bf16) a+b
    ####

    # callee save
    addi    sp, sp, -28
    sw      ra, 0(sp)
    sw      s0, 4(sp)
    sw      s1, 8(sp)
    sw      s2, 12(sp)
    sw      s3, 16(sp)
    sw      s4, 20(sp)
    sw      s5, 24(sp)
## if a is +-inf / NaN
    srli    s0, a0, 7                   # s0 = a >> 7
    andi    s0, s0, 0xFF                # s0 = exponent a
    srli    s1, a1, 7                   # s1 = b >> 7
    andi    s1, s1, 0xFF                # s1 = exponent b

    andi    s2, a0, 0x7F                # s2 = mantissa a
    andi    s3, a1, 0x7F                # s3 = mantissa b

    srli    s4, a0, 15                  # s4 = sign a
    srli    s5, a1, 15                  # s5 = sign b
    li      t6, 0xFF                    # t6 = 0xFF
    bne     t6, s0, a_actual_num        # if a_exp != 0xFF jump
### a exp = 0xFF ###
# handle NaN for a
    beq     s3, x0, end_add             # if a = NAN return a 
# handle +-inf for a
    beq     s1, t6, a_b_exp_FF          # if b exp = 0xFF, jump to handling section
    jal     x0, end_add                 # if b is actual number, return a
### a b exp = 0xFF ###
a_b_exp_FF: 
    bne     s3, x0, return_NAN          # if b is NAN, return NAN
# handle inf +- inf
    beq     s4, s5, end_add             # if a b have same sign return a
    jal     x0, return_NAN              # else return NaN
    
a_actual_num:
    beq     s1, t6, add_return_b        # b exp = 0xFF, return b
# b = 0
    slli    t1, a1, 1                   # eliminate sign bit of b
    beq     x0, t1, end_add             # if b = 0, return a
# a = 0
    slli    t1, a0, 1                   # eliminate sign bit of a
    beq     x0, t1, add_return_b        # if a = 0, return b
### a b are both actual numbers ###
# fraction part adjustment
add_adjust_mant_a:
    beq     x0, s0, add_adjust_mant_b       # a exp == 0,no need to adjust
    addi    s2, s2, 0x80                # retrieve hidden 1. for mantissa a
add_adjust_mant_b:
    beq     x0, s1, add_main            # b exp == 0,no need to adjust
    addi    s3, s3, 0x80                # retrieve hidden 1. for mantissa b

##### ADD MAIN ##### 
# s0 = result exp
# s1 = result mantissa
# s2 = mantissa a
# s3 = mantissa b
# s4 = result sign
###
# t6 = 0xFF
add_main:
### fraction alignment ###
    sub     t0, s0, s1                  # t0 = a_exp - b_exp
    li      t1, 8                       # t1 = 8
    mv      t3, s2                      # put mant_a in buffer
    mv      t4, s3                      # put mant_b in buffer
# 8 <= exp_diff (exp_diff > 8)
    blt     t1, t0, end_add             # |a| is too big, return a
# 8 <= -exp_diff (exp_diff < -8)
    sub     t2, x0, t0                  # t2 = -exp_diff
    blt     t1, t2, add_return_b            # |b| is too big, return b
# exp_diff == 0
    beq     t0, x0, true_add            # s0 = a_exp already, just jump
# 8 > exp_diff > 0
    srl     t4, s3, t0                  # t4 = mant_b >>= exp_diff;
    blt     t0, x0, true_add            # mant_b aligned with s0(result exp) = a_exp, jump
# -8 < exp_diff < 0
    sub     t0, x0, t0                  # t0 = -exp_diff (make positive)
    srl     t3, s2, t0                  # t3 = mant_a >>= -exp_diff;
    mv      s0, s1                      # s0 (result exp) = b exp
    blt     t0, x0, true_add            # mant_a aligned with s0(result exp) = b_exp, jump


true_add:
    mv      s2, t3                      # move aligned t3 (mant_a) to s2
    mv      s3, t4                      # move aligned t4 (mant_b) to s3
    bne     s4, s5, add_diff_sign

add_same_sign:
    slli    s4, s4, 15                  ## s4(result sign) = a sign << 15
    add     s1, s2, s3                  ## s1(result mantissa) = mantissa (a + b) 
    andi    t0, s1, 0x100               # t0 = overflow bit
    beq     t0, x0, add_result          # if no overflow (t0==0), get result
add_handle_overflow:
    srli    s1, s1, 1                   # s1(result_mant) >>= 1
    addi    s0, s0, 1                   # s0(esult_exp) += 1
    blt     s0, t6, add_result          # s0(esult_exp) < 0xFF, result number is normal, get result
    mv      s0, t6                      # else set s0(esult_exp) = 0xFF
    add     s1, x0, x0                  # s1(result mantissa) = 0
    jal     x0, add_result              # overflow => return inf with according sign

add_diff_sign:
# assume mantissa a(s2) < b(s3), set result to sign_b
    slli    s4, s5, 15                  # s4(result sign) = sign_b << 15
    sub     s1, s3, s2                  # s1(result mantissa) = s3(mant_b) - s2(mant_a)
    blt     s2, s3, add_handle_zero     # assumption is true, handle zero condition
# otherwise mantissa a >= b, set result sign = sign a
    slli    s4, s4, 15                  # s4(result sign) = sign_a << 15
    sub     s1, s2, s3                  # s1(result mantissa) = s2(mant_a) - s3(mant_b)

# check the result of substraction of aligned mantissa not be zero 
# otherwise error exists when we use a loop to adjust mantissa
add_handle_zero:
    beq     s1, x0, add_return_zero     #
add_adjust_mantissa:
    andi    t1, s1, 0x80                # t1 is the first bit of result_mant
    bne     t1, x0, add_result          # if first mantissa bit = 1, done / else shift until 1 is found
    slli    s1, s1, 1                   # s1(result_mant) <<= 1
    addi    s0, s0 -1                   # s0(result_exp) -= 1
    blt     x0, s0, add_return_zero         # if 0 >= result_exp, underflow
    jal     x0,  add_adjust_mantissa
###
# s0 = result exp
# s1 = result mantissa
# s2 = mantissa a
# s3 = mantissa b
# s4 = result sign
add_result:
    andi    s0, s0, 0xFF                # mask out the logic bit out for neg exp
    slli    s0, s0, 7                   # left shift to match the correct format
    andi    s1, s1, 0x7F                # mask out the first bit of mantissa (1.XX)
    or      a0, s4, s0                  # result_sign | result exp
    or      a0, a0, s1                  # result_sign | result exp | result mantissa
    jal     x0, end_add
### end of bf16_add function
add_return_zero:
    mv      a0, x0                      #
    jal     x0, end_add
add_return_b:
    mv      a0, a1
    jal     x0, end_add
return_NAN:
    lw      a0, BF16_NAN
    jal     x0, end_add
end_add:
    # retrieve ra and callee save
    lw      s5, 24(sp)
    lw      s4, 20(sp)
    lw      s3, 16(sp)
    lw      s2, 12(sp)
    lw      s1, 8(sp)    
    lw      s0, 4(sp)
    lw      ra, 0(sp)
    addi    sp, sp, 28
    ret

########################################
bf16_sub:
    ####
    ## input argument
    # a0 = (bf16) a
    # a1 = (bf16) b
    ## output argument
    # a0 = (bf16) a-b
    ####

    # callee save
    addi    sp, sp, -4
    sw      ra, 0(sp)

    la      t0, BF16_SIGN_MASK          #
    xor     a0, a0, t0                  # b = -b
    jal     ra, bf16_add

    # retrieve ra and callee save
    lw      ra, 0(sp)
    addi    sp, sp, 4
    ret
########################################
bf16_mul:
    ####
    ## input argument
    # a0 = (bf16) a
    # a1 = (bf16) b
    ## output argument
    # a0 = (bf16) a+b
    ####

    # callee save
    addi    sp, sp, -28
    sw      ra, 0(sp)
    sw      s0, 4(sp)                   # s0 = exponent a
    sw      s1, 8(sp)                   # s1 = exponent b
    sw      s2, 12(sp)                  # s2 = mantissa a
    sw      s3, 16(sp)                  # s3 = mantissa b
    sw      s4, 20(sp)                  # s4 = sign a
    sw      s5, 24(sp)                  # s5 = sign b

# Decomposite a and b into sign / exp / mantissa
    srli    s0, a0, 7                   # s0 = a >> 7
    andi    s0, s0, 0xFF                # s0 = exp_a = ((a.bits >> 7) & 0xFF)
    srli    s1, a1, 7                   # s1 = b >> 7
    andi    s1, s1, 0xFF                # s1 = exp_b = ((b.bits >> 7) & 0xFF)

    andi    s2, a0, 0x7F                # s2 = mant_a = a.bits & 0x7F
    andi    s3, a1, 0x7F                # s3 = mant_b = b.bits & 0x7F

    srli    s4, a0, 15                  # s4 = sign a
    srli    s5, a1, 15                  # s5 = sign b

#####
# t3 = result mant
# t4 = result exp
# t5 = result sign
# t6 = 0xFF
    xor     t5, s4, s5                  # t5(esult sign) = sign_a ^ sign_b
    addi    t6, x0, 0xFF                # t6 = 0xFF
# check a exp
    bne     s0, t6, mul_check_exp_b     # if exp_a != 0xFF, jump
    bne     s0, x0, end_mul             # if a = NaN, return a  
# a is +-inf
    li      t0, 0x7FFF                  # sign filter
    and     t1, a1, t0                  # t1 = b | 0x7FFF
    beq     t1, x0, mul_return_NAN      # if b = +- 0, inf * 0 = NaN
# return inf with correct sign
    slli    t5, t5, 15                  #
    li      t0, 0x7F80
    or      a0, t5, t0                  # (result_sign << 15) | 0x7F80
    jal     x0, end_mul
mul_check_exp_b:
    bne     s0, t6, mul_check_zero      # if exp_b != 0xFF, jump
    bne     s0, x0, mul_return_b        # if b = NaN, return b
# b is +-inf
    li      t0, 0x7FFF                  # sign filter
    and     t1, a0, t0                  # t1 = a | 0x7FFF
    beq     t1, x0, mul_return_NAN      # if a = +- 0, 0 * inf = NaN
# return inf with correct sign
    slli    t5, t5, 15                  #
    li      t0, 0x7F80
    or      a0, t5, t0                  # (result_sign << 15) | 0x7F80
    jal     x0, end_mul

### a, b both are actual numbers
# check 0 * 0 = 0
mul_check_zero:
    or      t0, s0, s1                  # t0 = exp_a | exp_b
    or      t0, t0, s2                  # t0 = exp_a | exp_b | mant_a
    or      t0, t0, s3                  # t0 = exp_a | exp_b | mant_a | mant_b
    bne     t0, x0, mul_main            # if t0 != 0, is not 0*0 case, jump
# return zero with correct sign
    slli    a0, t5, 15                  # a0 = result_sign << 15
    jal     x0, end_mul    

### a, b both are non zero actual numbers
# aligned the mentissa part and then do the multiple
mul_main:
    add     t0, x0, x0                  # t0 = adjust exp = 0
mul_adjust_a:    
    beq     s0, x0, mul_denormal_a      # if s0(exp_a) = 0, a is denormal number
    # a is non-zero normal number
    ori     s2, s2, 0x80                # retrieve 1.XXX in mant_a
    jal     x0, mul_adjust_b
mul_denormal_a:
    # find the first set bit
    andi    t1, s2, 0x80                # t1 = s2(mant_a) & 0x80
    bne     t1, x0, mul_mant_a_aligned  # while t1(first bit) is not found, loop
    slli    s2, s2, 1                   # left shift s2(mant_a) to find the first set bit
    addi    t0, t0, -1                  # exp_adjust--
    jal     x0, mul_denormal_a
mul_mant_a_aligned:
    addi    s0, x0, 1                   # first set bit is found, exp_a should change from 0 to 1
mul_adjust_b:
    beq     s1, x0, mul_denormal_b      # if s1(exp_b) = 0, b is denormal number
    # a is non-zero normal number
    ori     s3, s3, 0x80                # retrieve 1.XXX in s3(mant_b)
    jal     x0, result_exp
mul_denormal_b:
    # find the first set bit
    andi    t1, s3, 0x80                # t1 = s3(mant_b) & 0x80
    bne     t1, x0, mul_mant_a_aligned  # while t1(first bit) is not found, loop
    slli    s3, s3, 1                   # left shift s3(mant_b) to find the first set bit
    addi    t0, t0, -1                  # exp_adjust--
    jal     x0, mul_denormal_b
# mantissas are non-zero positive integer, just multiple
# t0 = adjust_exp       s0 = exponent a
# t3 = result mant      s1 = exponent b
# t4 = result exp       s2 = mantissa a
# t5 = result sign      s3 = mantissa b
# t6 = 0xFF             s4 = sign a
#                       s5 = sign b
result_exp:
    add     t4, s0, s1                  # t4 (result exp) = exp_a + exp_b
    addi    t4, t4, -127                # result exp = exp_a + exp_b - 127
    add     t4, t4, t0                  # result exp = exp_a + exp_b - 127 + adjust_exp
    # result_mant = (uint32_t) mant_a * mant_b;
    # while(mant_b != 0)
    #   iif(mant_b & 1) result += mant_a
    #   mant_a << 1, mant_b >> 1
true_mul:
    beq     x0, s3, mul_get_result      # while (mant_b != 0)
    andi    t2, s3, 1                   # t2 = mant_b & 1 = lsb bit
    beq     x0, t2, mul_skip            # if lsb bit of mant_b is 0, do not add to result
    add     t3, t3, s2                  # result_mant += (shifted) mant_a
mul_skip:
    srli    s3, s3, 1                   # mant_b >> 1
    slli    s2, s2 ,1                   # mant_a << 1
    jal     x0, true_mul                # go back to while

mul_get_result:
    # check result mentissa overflow
    li      t0, 0x8000                  # overflow mask
    and     t0, t0, t3                  # t0 =  0x8000 & t3 (result_mant)
    beq     t0, x0, mul_result_mant_adjust
    # overflow happened
    srli    t3, t3, 8                   # result_mant >> 8
    andi    t3, t3, 0x7F                # result_mant = (result_mant >> 8) & 0x7F
    addi    t4, t4, 1                   # result_exp ++
    jal     x0, mul_result_exp_adjust
mul_result_mant_adjust:
    # no mantissa overflow 
    srli    t3, t3, 7                   # t3 (result_mant) >> 7
    andi    t3, t3, 0x7F                # t3 (result_mant) = (result_mant >> 7) & 0x7F
mul_result_exp_adjust:
    # check exp again after checking mantissa overflow
    blt     t4, t6, mul_result_check_denormal
    # return inf with correct sign
    slli    t5, t5, 15                  # t5 (result_sign << 15)
    li      t0, 0x7F80
    or      a0, t5, t0                  # (result_sign << 15) | 0x7F80
    jal     x0, end_mul

mul_result_check_denormal:
    # 0 < result_exp , not denoraml   
    blt     x0, t4, mul_result
    li      t0, -6
    bge     t4, t0, mul_result_handle_denormal
    # return zero with correct sign
    slli    a0, t5, 15                  # a0 = result_sign << 15
    jal     x0, end_mul    
mul_result_handle_denormal:
    addi    t0, t4, -1                  # t0 = s4(result_exp) -1
    sub     t0, x0, t0                  # t0 = 1 - s4(result_exp) 
    srl     t3, t3, t0                  # ts (result_mant) >>= (1 - result_exp)
    add     t4, x0, x0                  # t4 (result_exp) = 0
mul_result:
    slli    t5, t5, 15                  # t5 (result_sign << 15)
    and     t4, t4, t6                  # t4 (result_exp) |= 0xFF
    slli    t4, t4, 7                   # (t4 (result_exp) | 0xFF) << 7
    andi    t3, t3, 0x7F                # result_mant & 0x7F
    or      a0, t5, t4
    or      a0, a0, t3
    jal     x0, end_mul

mul_return_b:
    mv      a0, a1
    jal     x0, end_mul

mul_return_NAN:
    lw      a0, BF16_NAN
    jal     x0, end_mul

end_mul:
    # retrieve ra and callee save
    lw      s5, 24(sp)
    lw      s4, 20(sp)
    lw      s3, 16(sp)
    lw      s2, 12(sp)
    lw      s1, 8(sp)    
    lw      s0, 4(sp)
    lw      ra, 0(sp)
    addi    sp, sp, 28
    ret

########################################
bf16_div:
    ####
    ## input argument
    # a0 = (bf16) a
    # a1 = (bf16) b
    ## output argument
    # a0 = (bf16) a/b
    ####

    # callee save
    addi    sp, sp, -28
    sw      ra, 0(sp)
    sw      s0, 4(sp)                   # s0 = exponent a
    sw      s1, 8(sp)                   # s1 = exponent b
    sw      s2, 12(sp)                  # s2 = mantissa a
    sw      s3, 16(sp)                  # s3 = mantissa b
    sw      s4, 20(sp)                  # s4 = sign a
    sw      s5, 24(sp)                  # s5 = sign b

# Decomposite a and b into sign / exp / mantissa
    srli    s0, a0, 7                   # s0 = a >> 7
    andi    s0, s0, 0xFF                # s0 = exp_a = ((a.bits >> 7) & 0xFF)
    srli    s1, a1, 7                   # s1 = b >> 7
    andi    s1, s1, 0xFF                # s1 = exp_b = ((b.bits >> 7) & 0xFF)

    andi    s2, a0, 0x7F                # s2 = mant_a = a.bits & 0x7F
    andi    s3, a1, 0x7F                # s3 = mant_b = b.bits & 0x7F

    srli    s4, a0, 15                  # s4 = sign a
    srli    s5, a1, 15                  # s5 = sign b

# t0 = adjust_exp       s0 = exponent a
# t3 = result mant      s1 = exponent b
# t4 = result exp       s2 = mantissa a
# t5 = result sign      s3 = mantissa b
# t6 = 0xFF             s4 = sign a
#                       s5 = sign b
    xor     t5, s4, s5                  # t5(esult sign) = sign_a ^ sign_b
    addi    t6, x0, 0xFF                # t6 = 0xFF
# (exp_b == 0xFF) 
div_check_exp_b:
    bne     s1, t6, div_check_zero_b    # if s1 (exp_b) != 0xFF, jump
    bne     s1, x0, div_return_b        # if b = NaN, return b  
    # inf / inf = NAN
    # => a != inf , return_sign_zero
    # => exp_a != 0xFF, return_sign_zero
    # exp_a == 0xFF and mant_a != 0, return_sign_zero
    bne     s0, t0, div_return_sign_zero
    bne     s2, x0, div_return_sign_zero
    jal     x0, div_return_NAN

div_check_zero_b:
    # (!exp_b && !mant_b)
    bne     s1, x0, div_check_exp_a     # if s1 (exp_b)  != 0, b!=0, pass
    bne     s3, x0, div_check_exp_a     # if s3 (mant_b) != 0, b!=0, pass
    # iif (!exp_a && !mant_a) / a=b=0, return NAN
    # elsee return sign inf
    bne     s0, x0, div_return_sign_inf # if s0 (exp_a)  != 0, a!=0, a/0=inf
    bne     s2, x0, div_return_sign_inf # if s2 (mant_a) != 0, a!=0, a/0=inf
    # 0/0 = NAN
    jal     x0, div_return_NAN
div_check_exp_a:
    bne     s0, t6, div_check_zero_a    # if s0 (exp_a) != 0xFF, jump
    bne     s1, x0, end_div             # if a = NaN, return a  
    
    jal     x0, div_return_sign_inf     # else inf/b = inf
div_check_zero_a:
    # (!exp_b && !mant_b)
    bne     s0, x0, div_get_mant_a      # if s0 (exp_a)  != 0, a!=0, pass
    bne     s2, x0, div_get_mant_a      # if s2 (mant_a) != 0, a!=0, pass
    # we have already handle 0/0 in the previous code
    jal     x0, div_return_sign_zero    # else 0/b = 0

###
# During devision, there is no need to aligned the mantissa with ccorrect exp 
div_get_mant_a:
    beq     x0, s2, div_get_mant_b      # denormal a, pass
    ori     s2, s2, 0x80                # retrieve 1.XX
div_get_mant_b:
    beq     x0, s3, div_main            # denormal b, pass
    ori     s3, s3, 0x80                # retrieve 1.XX

# t1 = i                s0 = exponent a
# t2 =                  s1 = exponent b
# t3 = result mant      s2 = mantissa a
# t4 = result exp       s3 = mantissa b
# t5 = result sign      s4 = divident
# t6 = quotient         s5 = divisor

div_main:
    slli    s4, s2, 15                  # dividend = (uint32_t) mant_a << 15
    add     s5, x0, s3                  # divisor = mant_b
    add     t6, x0, x0                  # initial quotient = 0

    addi    t0, x0, 16                  # t0 = 16 (maximum persision)
    add     t1, x0, x0                  # t1 = i = 0
true_div:
    bge     t1, t0, div_get_result      # if t1 (i) >= 16 end loop
    slli    t6, t6, 1                   # t6 (quotient) <<= 1
    addi    t2, x0, 15                  # t2 = 15
    sub     t2, t2, t1                  # t2 = 15 - i
    sll     t2, s5, t2                  # divisor << (15 - i)
    blt     s4, t2, div_i_minus_one     ## dividend < (divisor << (15 - i)), no quotient
    sub     s4, s4, t2                  # dividend -= (divisor << (15 - i))
    ori     t6, t6, 1                   # quotient |= 1
div_i_minus_one:
    addi    t1, t1, 1                   # i++
    jal     x0, true_div
###
div_get_result:
    sub     t4, s0, s1                  # t4 (result exp) = exp_a - exp_b
    addi    t4, t4, 127                 # t4 (result exp) = exp_a - exp_b + BF16_EXP_BIAS
div_zero_exp_a_correction:
    ## if (!exp_a), result_exp--
    bne     x0, s0, div_zero_exp_b_correction
    addi    t4, t4, -1
div_zero_exp_b_correction:
    ## if (!exp_b), result_exp++
    bne     x0, s1, div_check_quotient
    addi    t4, t4, 1
div_check_quotient:
    li      t0, 0x8000                  # t0 = 0x8000
    and     t0, t6, t0                  # t0 = quotient & 0x8000
    bne     t0, x0, div_result_mant_shift

div_result_mant_adjust:
    ## find the first set bit
    # t0 = quotient & 0x8000
    li      t0, 0x8000                  # t0 = 0x8000
    and     t0, t6, t0                  
    bne     t0, x0, div_result_mant_shift
    # result_exp-1 <= 0, jump
    addi    t1, t4, -1                  # t1 = result_exp - 1
    bge     x0, t1, div_result_mant_shift
    slli    t6, t6, 1                   # t6 (quotient) << 1
    addi    t4, t4, -1                  # result_exp --
    
div_result_mant_shift:
    srli    t6, t6, 8                   # quotient >>= 8

    li      t0, 0xFF
    bge     t4, t0, div_return_sign_inf
    bge     x0, t4, div_return_sign_zero
div_result:
    slli    t5, t5, 15                  # t5 (result_sign << 15)
    and     t4, t4, t0                  # t4 (result_exp) |= 0xFF
    slli    t4, t4, 7                   # (t4 (result_exp) | 0xFF) << 7
    andi    t6, t6, 0x7F                # quotient & 0x7F
    or      a0, t5, t4
    or      a0, a0, t6
    jal     x0, end_mul

####
div_return_sign_zero:
    slli    a0, t5, 15                  # result_sign << 15
    jal     x0, end_div
div_return_sign_inf:
    slli    a0, t5, 15                  # result_sign << 15
    li      t0, 0x7F80                  # t0 = 0x780
    or      a0, a0, t0                  # result_sign << 15 | 0x7F80
    jal     x0, end_div
div_return_NAN:
    lw      a0, BF16_NAN
    jal     x0, end_mul
div_return_b:
    mv      a0, a1
    jal     x0, end_div
end_div:
    # retrieve ra and callee save
    lw      s5, 24(sp)
    lw      s4, 20(sp)
    lw      s3, 16(sp)
    lw      s2, 12(sp)
    lw      s1, 8(sp)    
    lw      s0, 4(sp)
    lw      ra, 0(sp)
    addi    sp, sp, 28
    ret
