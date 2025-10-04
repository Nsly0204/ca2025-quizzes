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
        ####
        # s0 = fp32
        # s1 = traslated bf16
        ####

    # Callee Save
        addi    sp, sp, -8
        sw      s0, 0(sp)
        sw      s1, 4(sp)

    # f32_to_bf16 test 
        # load argument
        # s0 = test argument
        la      s0, normal              # s0 = @M[f32]
        lw      s1, 0(s0)               # s1 = f32
        mv      a0, s1                  # passing a0 as argument
        
        # call function f32_to_bf16
        # caller save
        addi    sp, sp, -4
        sw      ra, 0(sp)
        # jump to func f32_to_bf16
        jal     ra, f32_to_bf16         
        mv      t0, a0                  # get return value
        lw      t1, 4(s0)               # get answer
        beq     t0, t1, convert_pass    # if answer equal to return value then exit

    #fail message
        la      a0, fail_msg            # Load the address of the string ("Test Failed !!!!! \n")
        li      a7, 4                   # System call code for printing a string
        ecall                           # Print the string
convert_pass:
        la      a0, pass_convert        # Load the address of the string 
        li      a7, 4                   # System call code for printing a string
        ecall                           # Print the string

Add_test:
        li      a0, 1
        li      a1, 2
        jal     ra, bf16_add

Exit:
    # retrieve ra and callee save
        lw      s1, 4(sp)
        lw      s0, 0(sp)
        addi    sp, sp, 8

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
    addi    sp, sp, 28
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
    jal     x0, end_add                     # if b is actual number, return a
### a b exp = 0xFF ###
a_b_exp_FF: 
    bne     s3, x0, return_NAN          # if b is NAN, return NAN
# handle inf +- inf
    beq     s4, s5, end_add             # if a b have same sign return a
    jal     x0, return_NAN              # else return NaN
    
a_actual_num:
    beq     s1, t6, return_b            # b exp = 0xFF, return b
# b = 0
    slli    t1, a1, 1                   # eliminate sign bit of b
    beq     x0, t1, end_add             # if b = 0, return a
# a = 0
    slli    t1, a0, 1                   # eliminate sign bit of a
    beq     x0, t1, return_b            # if a = 0, return b
### a b are both actual numbers ###
# fraction part adjustment
adjust_mant_a:
    beq     x0, s0, adjust_mant_b       # a exp == 0,no need to adjust
    addi    s2, s2, 0x80                # retrieve hidden 1. for mantissa a
adjust_mant_b:
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
    blt     t1, t2, return_b            # |b| is too big, return b
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
    bne     s4, s5, diff_sign

same_sign:
    slli    s4, s4, 15                  ## s4(result sign) = a sign << 15
    add     s1, s2, s3                  ## s1(result mantissa) = mantissa (a + b) 
    andi    t0, s1, 0x100               # t0 = overflow bit
    beq     t0, x0, get_result          # if no overflow (t0==0), get result
handle_overflow:
    srli    s1, s1, 1                   # s1(result_mant) >>= 1
    addi    s0, s0, 1                   # s0(esult_exp) += 1
    blt     s0, t6, get_result          # s0(esult_exp) < 0xFF, result number is normal, get result
    mv      s0, t6                      # else set s0(esult_exp) = 0xFF
    add     s1, x0, x0                  # s1(result mantissa) = 0
    jal     x0, get_result              # overflow => return inf with according sign

diff_sign:
# assume mantissa a(s2) < b(s3), set result to sign_b
    slli    s4, s5, 15                  # s4(result sign) = sign_b << 15
    sub     s1, s3, s2                  # s1(result mantissa) = s3(mant_b) - s2(mant_a)
    blt     s2, s3, handle_zero         # assumption is true, handle zero condition
# otherwise mantissa a >= b, set result sign = sign a
    slli    s4, s4, 15                  # s4(result sign) = sign_a << 15
    sub     s1, s2, s3                  # s1(result mantissa) = s2(mant_a) - s3(mant_b)

# check the result of substraction of aligned mantissa not be zero 
# otherwise error exists when we use a loop to adjust mantissa
handle_zero:
    beq     s1, x0, return_zero         #
adjust_mantissa:
    andi    t1, s1, 0x80                # t1 is the first bit of result_mant
    bne     t1, x0, get_result          # if first mantissa bit = 1, done / else shift until 1 is found
    slli    s1, s1, 1                   # s1(result_mant) <<= 1
    addi    s0, s0 -1                   # s0(result_exp) -= 1
    blt     x0, s0, return_zero         # if 0 >= result_exp, underflow
    jal     x0,  adjust_mantissa
###
# s0 = result exp
# s1 = result mantissa
# s2 = mantissa a
# s3 = mantissa b
# s4 = result sign
get_result:
    andi    s0, s0, 0xFF                # mask out the logic bit out for neg exp
    slli    s0, s0, 7                   # left shift to match the correct format
    andi    s1, s1, 0x7F                # mask out the first bit of mantissa (1.XX)
    or      a0, s4, s0                  # result_sign | result exp
    or      a0, a0, s1                  # result_sign | result exp | result mantissa
    jal     x0, end_add
### end of bf16_add function
return_zero:
    mv      a0, x0                      #
    jal     x0, end_add
return_b:
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
    addi    sp, sp, 4
    ret

########################################
bf16_sub:

########################################
bf16_mul:

########################################
bf16_div:




