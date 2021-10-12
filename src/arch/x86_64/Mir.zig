//! Machine Intermediate Representation.
//! This data is produced by x86_64 Codegen and consumed by x86_64 Isel.
//! These instructions have a 1:1 correspondence with machine code instructions
//! for the target. MIR can be lowered to source-annotated textual assembly code
//! instructions, or it can be lowered to machine code.
//! The main purpose of MIR is to postpone the assignment of offsets until Isel,
//! so that, for example, the smaller encodings of jump instructions can be used.

const Mir = @This();
const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

instructions: []const Inst,
/// The meaning of this data is determined by `Inst.Tag` value.
extra: []const u32,

pub const Inst = struct {
    tag: Tag,
    /// This is 3 fields, and the meaning of each depends on `tag`.
    /// reg1: Register
    /// reg2: Register
    /// flags: u2
    ops: u16,
    /// The meaning of this depends on `tag` and `ops`.
    data: Data,

    pub const Tag = enum(u16) {
        /// ops flags:  form:
        ///       0b00  reg1, reg2
        ///       0b00  reg1, imm32
        ///       0b01  reg1, [reg2 + imm32]
        ///       0b10  [reg1 + imm32], reg2
        ///       0b10  [reg1 + 0], imm32
        ///       0b11  [reg1 + imm32], imm32
        /// Notes:
        ///  * If reg2 is `none` then it means Data field `imm` is used as the immediate.
        ///  * When two imm32 values are required, Data field `payload` points at `ImmPair`.
        adc,

        /// form: reg1, [reg2 + scale*rcx + imm32]
        /// ops flags  scale
        ///      0b00      1
        ///      0b01      2
        ///      0b10      4
        ///      0b11      8
        adc_scale_src,

        /// form: [reg1 + scale*rax + imm32], reg2
        /// form: [reg1 + scale*rax + 0], imm32
        /// ops flags  scale
        ///      0b00      1
        ///      0b01      2
        ///      0b10      4
        ///      0b11      8
        /// Notes:
        ///  * If reg2 is `none` then it means Data field `imm` is used as the immediate.
        adc_scale_dst,

        /// form: [reg1 + scale*rax + imm32], imm32
        /// ops flags  scale
        ///      0b00      1
        ///      0b01      2
        ///      0b10      4
        ///      0b11      8
        /// Notes:
        ///  * Data field `payload` points at `ImmPair`.
        adc_scale_imm,

        // The following instructions all have the same encoding as `adc`.

        add,
        add_scale_src,
        add_scale_dst,
        add_scale_imm,
        sub,
        sub_scale_src,
        sub_scale_dst,
        sub_scale_imm,
        xor,
        xor_scale_src,
        xor_scale_dst,
        xor_scale_imm,
        @"and",
        and_scale_src,
        and_scale_dst,
        and_scale_imm,
        @"or",
        or_scale_src,
        or_scale_dst,
        or_scale_imm,
        rol,
        rol_scale_src,
        rol_scale_dst,
        rol_scale_imm,
        ror,
        ror_scale_src,
        ror_scale_dst,
        ror_scale_imm,
        rcl,
        rcl_scale_src,
        rcl_scale_dst,
        rcl_scale_imm,
        rcr,
        rcr_scale_src,
        rcr_scale_dst,
        rcr_scale_imm,
        shl,
        shl_scale_src,
        shl_scale_dst,
        shl_scale_imm,
        sal,
        sal_scale_src,
        sal_scale_dst,
        sal_scale_imm,
        shr,
        shr_scale_src,
        shr_scale_dst,
        shr_scale_imm,
        sar,
        sar_scale_src,
        sar_scale_dst,
        sar_scale_imm,
        sbb,
        sbb_scale_src,
        sbb_scale_dst,
        sbb_scale_imm,
        cmp,
        cmp_scale_src,
        cmp_scale_dst,
        cmp_scale_imm,
        mov,
        mov_scale_src,
        mov_scale_dst,
        mov_scale_imm,
        lea,
        lea_scale_src,
        lea_scale_dst,
        lea_scale_imm,

        /// ops flags: 0bX0:
        /// - Uses the `inst` Data tag as the jump target.
        /// - reg1 and reg2 are ignored.
        /// ops flags: 0bX1:
        /// - reg1 is the jump target.
        /// - reg2 and data are ignored.
        jmp,

        /// ops flags:  form:
        ///       0bX0   reg1
        ///       0bX1   [reg1 + imm32]
        push,
        /// ops flags:  form:
        ///       0b00  retf imm16
        ///       0b01  retf
        ///       0b10  retn imm16
        ///       0b11  retn
        ret,
    };

    /// The position of an MIR instruction within the `Mir` instructions array.
    pub const Index = u32;

    /// All instructions have a 4-byte payload, which is contained within
    /// this union. `Tag` determines which union field is active, as well as
    /// how to interpret the data within.
    pub const Data = union {
        /// Another instruction.
        inst: Index,
        /// A 32-bit immediate value.
        imm: i32,
        /// Index into `extra`. Meaning of what can be found there is context-dependent.
        payload: u32,
    };

    // Make sure we don't accidentally make instructions bigger than expected.
    // Note that in Debug builds, Zig is allowed to insert a secret field for safety checks.
    comptime {
        if (builtin.mode != .Debug) {
            assert(@sizeOf(Inst) == 8);
        }
    }
};

pub const Register = enum(u7) {
    // zig fmt: off
    // 0 through 15, 64-bit registers. 8-15 are extended.
    // id is just the int value.
    rax, rcx, rdx, rbx, rsp, rbp, rsi, rdi,
    r8,  r9,  r10, r11, r12, r13, r14, r15,

    // 16 through 31, 32-bit registers. 24-31 are extended.
    // id is int value - 16.
    eax, ecx,  edx,  ebx,  esp,  ebp,  esi,  edi, 
    r8d, r9d, r10d, r11d, r12d, r13d, r14d, r15d,

    // 32-47, 16-bit registers. 40-47 are extended.
    // id is int value - 32.
    ax,  cx,  dx,   bx,   sp,   bp,   si,   di,
    r8w, r9w, r10w, r11w, r12w, r13w, r14w, r15w,
    
    // 48-63, 8-bit registers. 56-63 are extended.
    // id is int value - 48.
    al,  cl,   dl,  bl,   ah,   ch,   dh,   bh,
    r8b, r9b, r10b, r11b, r12b, r13b, r14b, r15b,
    // zig fmt: on

    none,
};

pub const ImmPair = struct {
    dest_off: i32,
    operand: i32,
};
