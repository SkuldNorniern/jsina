use std::collections::HashMap;

use crate::ir::bytecode::{BytecodeChunk, ConstEntry, Opcode};
use crate::runtime::Value;

use super::error::BackendError;
use super::lower::bytecode_to_lamina_trivial;
use super::runtime::CompiledChunk;

enum CacheEntry {
    Unknown,
    NativeCompiled(CompiledChunk),
    EvalCompiled,
    Rejected,
}

pub struct JitSession {
    cache: Vec<CacheEntry>,
    compilation_attempt_count: usize,
    eval_result_cache: HashMap<(usize, Vec<i64>), i64>,
}

impl Default for JitSession {
    fn default() -> Self {
        Self::new()
    }
}

impl JitSession {
    pub fn new() -> Self {
        Self {
            cache: Vec::new(),
            compilation_attempt_count: 0,
            eval_result_cache: HashMap::new(),
        }
    }

    #[inline(always)]
    fn ensure_slot(&mut self, chunk_index: usize) {
        if chunk_index >= self.cache.len() {
            self.cache
                .resize_with(chunk_index + 1, || CacheEntry::Unknown);
        }
    }

    pub fn try_invoke_compiled_for_call(
        &mut self,
        chunk_index: usize,
        args: &[Value],
        program_chunks: &[BytecodeChunk],
    ) -> Option<i64> {
        match self.cache.get(chunk_index)? {
            CacheEntry::NativeCompiled(compiled) => {
                if args.is_empty() {
                    Some(compiled.invoke())
                } else {
                    None
                }
            }
            CacheEntry::EvalCompiled => {
                let eval_args = values_to_i64_args(args)?;
                let mut eval_stack_cache: HashMap<(usize, Vec<i64>), i64> = HashMap::new();
                self.evaluate_cached(
                    chunk_index,
                    &eval_args,
                    program_chunks,
                    0,
                    &mut eval_stack_cache,
                )
            }
            CacheEntry::Unknown | CacheEntry::Rejected => None,
        }
    }

    pub fn try_compile_for_call(
        &mut self,
        chunk_index: usize,
        chunk: &BytecodeChunk,
        args: &[Value],
        program_chunks: &[BytecodeChunk],
    ) -> Result<Option<i64>, BackendError> {
        self.ensure_slot(chunk_index);
        match &self.cache[chunk_index] {
            CacheEntry::NativeCompiled(_) | CacheEntry::EvalCompiled | CacheEntry::Rejected => {
                return Ok(self.try_invoke_compiled_for_call(chunk_index, args, program_chunks));
            }
            CacheEntry::Unknown => {}
        }

        self.compilation_attempt_count = self.compilation_attempt_count.saturating_add(1);

        if let Some(module) = bytecode_to_lamina_trivial(chunk) {
            let compiled = CompiledChunk::from_module(&module)?;
            self.cache[chunk_index] = CacheEntry::NativeCompiled(compiled);
            return Ok(self.try_invoke_compiled_for_call(chunk_index, args, program_chunks));
        }

        if supports_eval_subset(chunk) {
            self.cache[chunk_index] = CacheEntry::EvalCompiled;
            return Ok(self.try_invoke_compiled_for_call(chunk_index, args, program_chunks));
        }

        self.cache[chunk_index] = CacheEntry::Rejected;
        Ok(None)
    }

    pub fn try_invoke_compiled(&mut self, chunk_index: usize) -> Option<i64> {
        let program_chunks: [BytecodeChunk; 0] = [];
        self.try_invoke_compiled_for_call(chunk_index, &[], &program_chunks)
    }

    pub fn try_compile(
        &mut self,
        chunk_index: usize,
        chunk: &BytecodeChunk,
    ) -> Result<Option<i64>, BackendError> {
        self.try_compile_for_call(chunk_index, chunk, &[], std::slice::from_ref(chunk))
    }

    pub fn has_compiled(&self, chunk_index: usize) -> bool {
        matches!(
            self.cache.get(chunk_index),
            Some(CacheEntry::NativeCompiled(_) | CacheEntry::EvalCompiled)
        )
    }

    pub fn invoke_compiled(&mut self, chunk_index: usize) -> Result<i64, BackendError> {
        self.try_invoke_compiled(chunk_index)
            .ok_or_else(|| BackendError::Process("chunk not compiled".to_string()))
    }

    pub fn compilation_attempt_count(&self) -> usize {
        self.compilation_attempt_count
    }

    fn evaluate_cached(
        &mut self,
        chunk_index: usize,
        args: &[i64],
        program_chunks: &[BytecodeChunk],
        depth: u32,
        eval_stack_cache: &mut HashMap<(usize, Vec<i64>), i64>,
    ) -> Option<i64> {
        const MAX_EVAL_DEPTH: u32 = 2048;
        if depth > MAX_EVAL_DEPTH {
            return None;
        }

        let key = (chunk_index, args.to_vec());
        if let Some(cached) = eval_stack_cache.get(&key) {
            return Some(*cached);
        }
        if let Some(cached) = self.eval_result_cache.get(&key) {
            return Some(*cached);
        }

        let chunk = program_chunks.get(chunk_index)?;
        let result =
            self.evaluate_chunk(chunk, args, program_chunks, depth + 1, eval_stack_cache)?;
        eval_stack_cache.insert(key.clone(), result);
        self.eval_result_cache.insert(key, result);
        Some(result)
    }

    fn evaluate_chunk(
        &mut self,
        chunk: &BytecodeChunk,
        args: &[i64],
        program_chunks: &[BytecodeChunk],
        depth: u32,
        eval_stack_cache: &mut HashMap<(usize, Vec<i64>), i64>,
    ) -> Option<i64> {
        if chunk.rest_param_index.is_some() || !chunk.handlers.is_empty() {
            return None;
        }

        let mut locals = vec![EvalValue::Int(0); chunk.num_locals as usize];
        let copy_len = args.len().min(locals.len());
        for (index, value) in args.iter().copied().take(copy_len).enumerate() {
            locals[index] = EvalValue::Int(value);
        }

        let mut stack: Vec<EvalValue> = Vec::with_capacity(16);
        let mut pc = 0usize;
        let code = &chunk.code;

        while pc < code.len() {
            let op = *code.get(pc)?;
            pc += 1;
            match op {
                x if x == Opcode::PushConst as u8 => {
                    let idx = *code.get(pc)? as usize;
                    pc += 1;
                    let value = match chunk.constants.get(idx)? {
                        ConstEntry::Int(n) => EvalValue::Int(*n),
                        _ => return None,
                    };
                    stack.push(value);
                }
                x if x == Opcode::LoadLocal as u8 => {
                    let local = *code.get(pc)? as usize;
                    pc += 1;
                    stack.push(*locals.get(local)?);
                }
                x if x == Opcode::StoreLocal as u8 => {
                    let local = *code.get(pc)? as usize;
                    pc += 1;
                    let value = stack.pop()?;
                    *locals.get_mut(local)? = value;
                }
                x if x == Opcode::Pop as u8 => {
                    stack.pop()?;
                }
                x if x == Opcode::Dup as u8 => {
                    let top = *stack.last()?;
                    stack.push(top);
                }
                x if x == Opcode::Swap as u8 => {
                    let len = stack.len();
                    if len < 2 {
                        return None;
                    }
                    stack.swap(len - 1, len - 2);
                }
                x if x == Opcode::Add as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(lhs.as_i64().saturating_add(rhs.as_i64())));
                }
                x if x == Opcode::Sub as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(lhs.as_i64().saturating_sub(rhs.as_i64())));
                }
                x if x == Opcode::Mul as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(lhs.as_i64().saturating_mul(rhs.as_i64())));
                }
                x if x == Opcode::Div as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    let divisor = rhs.as_i64();
                    if divisor == 0 {
                        return None;
                    }
                    stack.push(EvalValue::Int(lhs.as_i64() / divisor));
                }
                x if x == Opcode::Mod as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    let divisor = rhs.as_i64();
                    if divisor == 0 {
                        return None;
                    }
                    stack.push(EvalValue::Int(lhs.as_i64() % divisor));
                }
                x if x == Opcode::Lt as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(lhs.as_i64() < rhs.as_i64()));
                }
                x if x == Opcode::Lte as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(lhs.as_i64() <= rhs.as_i64()));
                }
                x if x == Opcode::Gt as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(lhs.as_i64() > rhs.as_i64()));
                }
                x if x == Opcode::Gte as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(lhs.as_i64() >= rhs.as_i64()));
                }
                x if x == Opcode::StrictEq as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(lhs.strict_eq(rhs)));
                }
                x if x == Opcode::StrictNotEq as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Bool(!lhs.strict_eq(rhs)));
                }
                x if x == Opcode::Not as u8 => {
                    let value = stack.pop()?;
                    stack.push(EvalValue::Bool(!value.is_truthy()));
                }
                x if x == Opcode::BitwiseAnd as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int((lhs.as_i32() & rhs.as_i32()) as i64));
                }
                x if x == Opcode::BitwiseOr as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int((lhs.as_i32() | rhs.as_i32()) as i64));
                }
                x if x == Opcode::BitwiseXor as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int((lhs.as_i32() ^ rhs.as_i32()) as i64));
                }
                x if x == Opcode::BitwiseNot as u8 => {
                    let value = stack.pop()?;
                    stack.push(EvalValue::Int((!value.as_i32()) as i64));
                }
                x if x == Opcode::LeftShift as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(
                        lhs.as_i32().wrapping_shl(rhs.as_i32() as u32) as i64,
                    ));
                }
                x if x == Opcode::RightShift as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(
                        lhs.as_i32().wrapping_shr(rhs.as_i32() as u32) as i64,
                    ));
                }
                x if x == Opcode::UnsignedRightShift as u8 => {
                    let (lhs, rhs) = pop2_eval(&mut stack)?;
                    stack.push(EvalValue::Int(
                        ((lhs.as_i32() as u32).wrapping_shr(rhs.as_i32() as u32)) as i64,
                    ));
                }
                x if x == Opcode::JumpIfFalse as u8 => {
                    let offset = read_i16(code, pc)? as isize;
                    pc += 2;
                    let value = stack.pop()?;
                    if !value.is_truthy() {
                        pc = ((pc as isize) + offset) as usize;
                    }
                }
                x if x == Opcode::Jump as u8 => {
                    let offset = read_i16(code, pc)? as isize;
                    pc += 2;
                    pc = ((pc as isize) + offset) as usize;
                }
                x if x == Opcode::Call as u8 => {
                    let callee_idx = *code.get(pc)? as usize;
                    let argc = *code.get(pc + 1)? as usize;
                    pc += 2;
                    let call_args = pop_i64_args(&mut stack, argc)?;
                    let call_result = self.evaluate_cached(
                        callee_idx,
                        &call_args,
                        program_chunks,
                        depth + 1,
                        eval_stack_cache,
                    )?;
                    stack.push(EvalValue::Int(call_result));
                }
                x if x == Opcode::Return as u8 => {
                    return Some(stack.pop().unwrap_or(EvalValue::Int(0)).as_i64());
                }
                _ => return None,
            }
        }

        Some(stack.pop().unwrap_or(EvalValue::Int(0)).as_i64())
    }
}

#[derive(Clone, Copy)]
enum EvalValue {
    Int(i64),
    Bool(bool),
}

impl EvalValue {
    fn as_i64(self) -> i64 {
        match self {
            Self::Int(v) => v,
            Self::Bool(v) => i64::from(v),
        }
    }

    fn as_i32(self) -> i32 {
        match self {
            Self::Int(v) => v as i32,
            Self::Bool(v) => i32::from(v),
        }
    }

    fn is_truthy(self) -> bool {
        match self {
            Self::Int(v) => v != 0,
            Self::Bool(v) => v,
        }
    }

    fn strict_eq(self, rhs: Self) -> bool {
        match (self, rhs) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs == rhs,
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

fn supports_eval_subset(chunk: &BytecodeChunk) -> bool {
    if chunk.rest_param_index.is_some() || !chunk.handlers.is_empty() {
        return false;
    }
    let mut pc = 0usize;
    while pc < chunk.code.len() {
        let op = chunk.code[pc];
        pc += 1;
        match op {
            x if x == Opcode::PushConst as u8
                || x == Opcode::LoadLocal as u8
                || x == Opcode::StoreLocal as u8 =>
            {
                if pc >= chunk.code.len() {
                    return false;
                }
                pc += 1;
            }
            x if x == Opcode::Jump as u8 || x == Opcode::JumpIfFalse as u8 => {
                if pc + 1 >= chunk.code.len() {
                    return false;
                }
                pc += 2;
            }
            x if x == Opcode::Call as u8 => {
                if pc + 1 >= chunk.code.len() {
                    return false;
                }
                pc += 2;
            }
            x if x == Opcode::Return as u8
                || x == Opcode::Pop as u8
                || x == Opcode::Dup as u8
                || x == Opcode::Swap as u8
                || x == Opcode::Add as u8
                || x == Opcode::Sub as u8
                || x == Opcode::Mul as u8
                || x == Opcode::Div as u8
                || x == Opcode::Mod as u8
                || x == Opcode::Lt as u8
                || x == Opcode::Lte as u8
                || x == Opcode::Gt as u8
                || x == Opcode::Gte as u8
                || x == Opcode::StrictEq as u8
                || x == Opcode::StrictNotEq as u8
                || x == Opcode::Not as u8
                || x == Opcode::LeftShift as u8
                || x == Opcode::RightShift as u8
                || x == Opcode::UnsignedRightShift as u8
                || x == Opcode::BitwiseAnd as u8
                || x == Opcode::BitwiseOr as u8
                || x == Opcode::BitwiseXor as u8
                || x == Opcode::BitwiseNot as u8 => {}
            _ => return false,
        }
    }
    true
}

fn read_i16(code: &[u8], pc: usize) -> Option<i16> {
    let lo = *code.get(pc)?;
    let hi = *code.get(pc + 1)?;
    Some(i16::from_le_bytes([lo, hi]))
}

fn pop2_eval(stack: &mut Vec<EvalValue>) -> Option<(EvalValue, EvalValue)> {
    let rhs = stack.pop()?;
    let lhs = stack.pop()?;
    Some((lhs, rhs))
}

fn pop_i64_args(stack: &mut Vec<EvalValue>, argc: usize) -> Option<Vec<i64>> {
    if argc == 0 {
        return Some(Vec::new());
    }
    let start = stack.len().checked_sub(argc)?;
    let args = stack.split_off(start);
    Some(args.into_iter().map(EvalValue::as_i64).collect())
}

fn values_to_i64_args(args: &[Value]) -> Option<Vec<i64>> {
    let mut out = Vec::with_capacity(args.len());
    for value in args {
        match value {
            Value::Int(v) => out.push(*v as i64),
            Value::Bool(v) => out.push(i64::from(*v)),
            Value::Number(v) => out.push(*v as i64),
            _ => return None,
        }
    }
    Some(out)
}
