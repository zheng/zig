//! Code generation.
//! State used for lowering AIR into MIR.

air: Air,
liveness: Liveness,
mir_instructions: std.ArrayListUnmanaged(Air.Inst) = .{},
mir_extra: std.ArrayListUnmanaged(u32) = .{},

const Codegen = @This();
const Air = @import("../../Air.zig");
const Liveness = @import("../../Liveness.zig");

const std = @import("std");
const math = std.math;
const mem = std.mem;

const InnerError = error{
    OutOfMemory,
    CodegenFail,
};

const MCValue = union(enum) {
    /// Control flow will not allow this value to be observed.
    unreach,
    /// No more references to this value remain.
    dead,
    /// The value is undefined.
    undef,
    /// A pointer-sized integer that fits in a register.
    /// If the type is a pointer, this is the pointer address in virtual address space.
    immediate: u64,
    /// The value is in a target-specific register.
    register: Register,
    /// The value is in memory at a hard-coded address.
    /// If the type is a pointer, it means the pointer address is at this memory location.
    memory: u64,
    /// The value is one of the stack variables.
    /// If the type is a pointer, it means the pointer address is in the stack at this offset.
    stack_offset: u32,
    /// The value is a pointer to one of the stack variables (payload is stack offset).
    ptr_stack_offset: u32,
    /// The value is in the compare flags assuming an unsigned operation,
    /// with this operator applied on top of it.
    compare_flags_unsigned: math.CompareOperator,
    /// The value is in the compare flags assuming a signed operation,
    /// with this operator applied on top of it.
    compare_flags_signed: math.CompareOperator,

    fn isMemory(mcv: MCValue) bool {
        return switch (mcv) {
            .embedded_in_code, .memory, .stack_offset => true,
            else => false,
        };
    }

    fn isImmediate(mcv: MCValue) bool {
        return switch (mcv) {
            .immediate => true,
            else => false,
        };
    }

    fn isMutable(mcv: MCValue) bool {
        return switch (mcv) {
            .none => unreachable,
            .unreach => unreachable,
            .dead => unreachable,

            .immediate,
            .embedded_in_code,
            .memory,
            .compare_flags_unsigned,
            .compare_flags_signed,
            .ptr_stack_offset,
            .ptr_embedded_in_code,
            .undef,
            => false,

            .register,
            .stack_offset,
            => true,
        };
    }
};

pub fn genFunction(cg: *Codegen) !void {
    const cc = cg.fn_type.fnCallingConvention();
    if (cc != .Naked) {
        // We want to subtract the aligned stack frame size from rsp here, but we don't
        // yet know how big it will be. Later we will come back and replace the instruction
        // with the appropriate one when we know the size.
        // TODO During semantic analysis, check if there are no function calls. If there
        // are none, here we can omit the part where we subtract and then add rsp.
        try cg.addPushReg(.rbp);
        try cg.addMovRegReg(.rbp, .rsp);
        const reloc_index = cg.mir_instructions.items.len;
        try cg.addSubRegImm(.rsp, 0);

        try cg.dbgSetPrologueEnd();
        try cg.genBody(cg.air.getMainBody());

        const stack_end = cg.max_end_stack;
        if (stack_end > math.maxInt(i32))
            return cg.failSymbol("too much stack used in call parameters", .{});
        const aligned_stack_end = mem.alignForward(stack_end, cg.stack_align);
        cg.mir_instructions.items[reloc_index].data.imm = aligned_stack_end;

        const epilogue_index = @intCast(u32, cg.mir_instructions.items.len);
        for (cg.exitlude_jump_relocs.items) |jmp_reloc| {
            cg.mir_instructions.items[jmp_reloc].data.inst = epilogue_index;
        }
        try cg.dbgSetEpilogueBegin();

        try cg.addAddRegImm(.rsp, aligned_stack_end);
        try cg.addPopReg(.rbp);
        try cg.addRet();
    } else {
        try cg.dbgSetPrologueEnd();
        try cg.genBody(cg.air.getMainBody());
        try cg.dbgSetEpilogueBegin();
    }
}

fn genBody(cg: *Codegen, body: []const Air.Inst.Index) InnerError!void {
    const air_tags = cg.air.instructions.items(.tag);

    for (body) |inst| {
        const old_air_bookkeeping = cg.air_bookkeeping;
        try cg.ensureProcessDeathCapacity(Liveness.bpi);

        switch (air_tags[inst]) {
            // zig fmt: off
            .add, .ptr_add   => try cg.airAdd(inst),
            .addwrap         => try cg.airAddWrap(inst),
            .add_sat         => try cg.airAddSat(inst),
            .sub, .ptr_sub   => try cg.airSub(inst),
            .subwrap         => try cg.airSubWrap(inst),
            .sub_sat         => try cg.airSubSat(inst),
            .mul             => try cg.airMul(inst),
            .mulwrap         => try cg.airMulWrap(inst),
            .mul_sat         => try cg.airMulSat(inst),
            .div             => try cg.airDiv(inst),
            .rem             => try cg.airRem(inst),
            .mod             => try cg.airMod(inst),
            .shl, .shl_exact => try cg.airShl(inst),
            .shl_sat         => try cg.airShlSat(inst),

            .cmp_lt  => try cg.airCmp(inst, .lt),
            .cmp_lte => try cg.airCmp(inst, .lte),
            .cmp_eq  => try cg.airCmp(inst, .eq),
            .cmp_gte => try cg.airCmp(inst, .gte),
            .cmp_gt  => try cg.airCmp(inst, .gt),
            .cmp_neq => try cg.airCmp(inst, .neq),

            .bool_and => try cg.airBoolOp(inst),
            .bool_or  => try cg.airBoolOp(inst),
            .bit_and  => try cg.airBitAnd(inst),
            .bit_or   => try cg.airBitOr(inst),
            .xor      => try cg.airXor(inst),
            .shr      => try cg.airShr(inst),

            .alloc           => try cg.airAlloc(inst),
            .ret_ptr         => try cg.airRetPtr(inst),
            .arg             => try cg.airArg(inst),
            .assembly        => try cg.airAsm(inst),
            .bitcast         => try cg.airBitCast(inst),
            .block           => try cg.airBlock(inst),
            .br              => try cg.airBr(inst),
            .breakpoint      => try cg.airBreakpoint(),
            .fence           => try cg.airFence(),
            .call            => try cg.airCall(inst),
            .cond_br         => try cg.airCondBr(inst),
            .dbg_stmt        => try cg.airDbgStmt(inst),
            .fptrunc         => try cg.airFptrunc(inst),
            .fpext           => try cg.airFpext(inst),
            .intcast         => try cg.airIntCast(inst),
            .trunc           => try cg.airTrunc(inst),
            .bool_to_int     => try cg.airBoolToInt(inst),
            .is_non_null     => try cg.airIsNonNull(inst),
            .is_non_null_ptr => try cg.airIsNonNullPtr(inst),
            .is_null         => try cg.airIsNull(inst),
            .is_null_ptr     => try cg.airIsNullPtr(inst),
            .is_non_err      => try cg.airIsNonErr(inst),
            .is_non_err_ptr  => try cg.airIsNonErrPtr(inst),
            .is_err          => try cg.airIsErr(inst),
            .is_err_ptr      => try cg.airIsErrPtr(inst),
            .load            => try cg.airLoad(inst),
            .loop            => try cg.airLoop(inst),
            .not             => try cg.airNot(inst),
            .ptrtoint        => try cg.airPtrToInt(inst),
            .ret             => try cg.airRet(inst),
            .ret_load        => try cg.airRetLoad(inst),
            .store           => try cg.airStore(inst),
            .struct_field_ptr=> try cg.airStructFieldPtr(inst),
            .struct_field_val=> try cg.airStructFieldVal(inst),
            .array_to_slice  => try cg.airArrayToSlice(inst),
            .int_to_float    => try cg.airIntToFloat(inst),
            .float_to_int    => try cg.airFloatToInt(inst),
            .cmpxchg_strong  => try cg.airCmpxchg(inst),
            .cmpxchg_weak    => try cg.airCmpxchg(inst),
            .atomic_rmw      => try cg.airAtomicRmw(inst),
            .atomic_load     => try cg.airAtomicLoad(inst),
            .memcpy          => try cg.airMemcpy(inst),
            .memset          => try cg.airMemset(inst),
            .set_union_tag   => try cg.airSetUnionTag(inst),
            .get_union_tag   => try cg.airGetUnionTag(inst),
            .clz             => try cg.airClz(inst),
            .ctz             => try cg.airCtz(inst),

            .atomic_store_unordered => try cg.airAtomicStore(inst, .Unordered),
            .atomic_store_monotonic => try cg.airAtomicStore(inst, .Monotonic),
            .atomic_store_release   => try cg.airAtomicStore(inst, .Release),
            .atomic_store_seq_cst   => try cg.airAtomicStore(inst, .SeqCst),

            .struct_field_ptr_index_0 => try cg.airStructFieldPtrIndex(inst, 0),
            .struct_field_ptr_index_1 => try cg.airStructFieldPtrIndex(inst, 1),
            .struct_field_ptr_index_2 => try cg.airStructFieldPtrIndex(inst, 2),
            .struct_field_ptr_index_3 => try cg.airStructFieldPtrIndex(inst, 3),

            .switch_br       => try cg.airSwitch(inst),
            .slice_ptr       => try cg.airSlicePtr(inst),
            .slice_len       => try cg.airSliceLen(inst),

            .array_elem_val      => try cg.airArrayElemVal(inst),
            .slice_elem_val      => try cg.airSliceElemVal(inst),
            .ptr_slice_elem_val  => try cg.airPtrSliceElemVal(inst),
            .ptr_elem_val        => try cg.airPtrElemVal(inst),
            .ptr_elem_ptr        => try cg.airPtrElemPtr(inst),
            .ptr_ptr_elem_val    => try cg.airPtrPtrElemVal(inst),

            .constant => unreachable, // excluded from function bodies
            .const_ty => unreachable, // excluded from function bodies
            .unreach  => cg.finishAirBookkeeping(),

            .optional_payload           => try cg.airOptionalPayload(inst),
            .optional_payload_ptr       => try cg.airOptionalPayloadPtr(inst),
            .unwrap_errunion_err        => try cg.airUnwrapErrErr(inst),
            .unwrap_errunion_payload    => try cg.airUnwrapErrPayload(inst),
            .unwrap_errunion_err_ptr    => try cg.airUnwrapErrErrPtr(inst),
            .unwrap_errunion_payload_ptr=> try cg.airUnwrapErrPayloadPtr(inst),

            .wrap_optional         => try cg.airWrapOptional(inst),
            .wrap_errunion_payload => try cg.airWrapErrUnionPayload(inst),
            .wrap_errunion_err     => try cg.airWrapErrUnionErr(inst),
            // zig fmt: on
        }
        if (std.debug.runtime_safety) {
            if (cg.air_bookkeeping < old_air_bookkeeping + 1) {
                std.debug.panic("in codegen.zig, handling of AIR instruction %{d} ('{}') did not do proper bookkeeping. Look for a missing call to finishAir.", .{ inst, air_tags[inst] });
            }
        }
    }
}

fn airAdd(cg: *Codegen, inst: Air.Inst.Index) !void {
    const bin_op = cg.air.instructions.items(.data)[inst].bin_op;
    if (cg.liveness.isUnused(inst)) {
        return finishAir(cg, inst, .dead, .{ bin_op.lhs, bin_op.rhs, .none });
    }
        .x86_64 => try cg.genX8664BinMath(inst, bin_op.lhs, bin_op.rhs),
        .arm, .armeb => try cg.genArmBinOp(inst, bin_op.lhs, bin_op.rhs, .add),
        else => return cg.fail("TODO implement add for {}", .{cg.target.cpu.arch}),
    };
    return finishAir(cg, inst, result, .{ bin_op.lhs, bin_op.rhs, .none });
}

/// Perform "binary" operators, excluding comparisons.
/// Currently, the following ops are supported:
/// ADD, SUB, XOR, OR, AND
fn genX8664BinMath(self: *Self, inst: Air.Inst.Index, op_lhs: Air.Inst.Ref, op_rhs: Air.Inst.Ref) !MCValue {
    // We'll handle these ops in two steps.
    // 1) Prepare an output location (register or memory)
    //    This location will be the location of the operand that dies (if one exists)
    //    or just a temporary register (if one doesn't exist)
    // 2) Perform the op with the other argument
    // 3) Sometimes, the output location is memory but the op doesn't support it.
    //    In this case, copy that location to a register, then perform the op to that register instead.
    //
    // TODO: make this algorithm less bad

    try self.code.ensureUnusedCapacity(8);

    const lhs = try self.resolveInst(op_lhs);
    const rhs = try self.resolveInst(op_rhs);

    // There are 2 operands, destination and source.
    // Either one, but not both, can be a memory operand.
    // Source operand can be an immediate, 8 bits or 32 bits.
    // So, if either one of the operands dies with this instruction, we can use it
    // as the result MCValue.
    var dst_mcv: MCValue = undefined;
    var src_mcv: MCValue = undefined;
    var src_inst: Air.Inst.Ref = undefined;
    if (self.reuseOperand(inst, op_lhs, 0, lhs)) {
        // LHS dies; use it as the destination.
        // Both operands cannot be memory.
        src_inst = op_rhs;
        if (lhs.isMemory() and rhs.isMemory()) {
            dst_mcv = try self.copyToNewRegister(inst, lhs);
            src_mcv = rhs;
        } else {
            dst_mcv = lhs;
            src_mcv = rhs;
        }
    } else if (self.reuseOperand(inst, op_rhs, 1, rhs)) {
        // RHS dies; use it as the destination.
        // Both operands cannot be memory.
        src_inst = op_lhs;
        if (lhs.isMemory() and rhs.isMemory()) {
            dst_mcv = try self.copyToNewRegister(inst, rhs);
            src_mcv = lhs;
        } else {
            dst_mcv = rhs;
            src_mcv = lhs;
        }
    } else {
        if (lhs.isMemory()) {
            dst_mcv = try self.copyToNewRegister(inst, lhs);
            src_mcv = rhs;
            src_inst = op_rhs;
        } else {
            dst_mcv = try self.copyToNewRegister(inst, rhs);
            src_mcv = lhs;
            src_inst = op_lhs;
        }
    }
    // This instruction supports only signed 32-bit immediates at most. If the immediate
    // value is larger than this, we put it in a register.
    // A potential opportunity for future optimization here would be keeping track
    // of the fact that the instruction is available both as an immediate
    // and as a register.
    switch (src_mcv) {
        .immediate => |imm| {
            if (imm > math.maxInt(u31)) {
                src_mcv = MCValue{ .register = try self.copyToTmpRegister(Type.initTag(.u64), src_mcv) };
            }
        },
        else => {},
    }

    // Now for step 2, we perform the actual op
    const inst_ty = self.air.typeOfIndex(inst);
    const air_tags = self.air.instructions.items(.tag);
    switch (air_tags[inst]) {
        // TODO: Generate wrapping and non-wrapping versions separately
        .add, .addwrap => try self.genX8664BinMathCode(inst_ty, dst_mcv, src_mcv, 0, 0x00),
        .bool_or, .bit_or => try self.genX8664BinMathCode(inst_ty, dst_mcv, src_mcv, 1, 0x08),
        .bool_and, .bit_and => try self.genX8664BinMathCode(inst_ty, dst_mcv, src_mcv, 4, 0x20),
        .sub, .subwrap => try self.genX8664BinMathCode(inst_ty, dst_mcv, src_mcv, 5, 0x28),
        .xor, .not => try self.genX8664BinMathCode(inst_ty, dst_mcv, src_mcv, 6, 0x30),

        .mul, .mulwrap => try self.genX8664Imul(inst_ty, dst_mcv, src_mcv),
        else => unreachable,
    }

    return dst_mcv;
}
