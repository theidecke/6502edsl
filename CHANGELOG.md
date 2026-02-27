# Revision history for 6502edsl

## 0.2.0.0 -- 2026-02-27

### Debug and assertions
* VICE monitor symbol file export (`exportViceLabels` in `Target.C64.Debug`)
* Code annotation with `annotate` for naming regions (`Asm.Mos6502.Debug`)
* `assembleWithLabels` returns collected labels alongside bytes (`Asm.Monad`)
* Assembly-time size assertion with `fitsIn` (`Asm.Mos6502.Debug`)
* Page-boundary assertion with `samePage` (`Asm.Mos6502.Memory`)

### Control flow and 16-bit operations
* Structured control flow: `if_eq`, `if_ne`, `if_cs`, `if_cc`, `if_pl`, `if_mi`, `if_`, `while_`, `for_x`, `for_y`, `loop_` (`Asm.Mos6502.Control`)
* 16-bit arithmetic: `load16`, `inc16`, `dec16`, `add16`, `sub16`, `mov16`, `cmp16` (`Asm.Mos6502.Ops16`)
* Memory alignment with `align` and `alignPage` (`Asm.Mos6502.Memory`)

### Addressing mode sugar
* `#` operator for immediate mode (`lda # 0x42`)
* Bare `Word8`/`Word16` for zero-page/absolute addressing
* Tuple syntax for indexed modes (`(addr, X)`, `(addr, Y)`)
* `!` operator for indirect modes (`addr ! X`, `addr ! Y`)
* `A` singleton for accumulator mode (`asl A`)
* Typed zero-page variables: `Var8`, `Var16`, `Ptr` with `allocVar8`, `allocVar16`, `allocPtr`

### C64 memory map
* Full VIC-II, SID, CIA1, CIA2, KERNAL, and system memory map constants (`Target.C64.Mem`)

### Data embedding
* `byte`, `word`, `petscii`, `pstring`, `charToPetscii` (`Target.C64.Data`)

## 0.1.0.0 -- 2026-02-26

### Core
* Assembler monad (`ASM`) with `emit`, `label`, `assemble`
* `MonadFix` instance for forward label references via `mdo`
* Zero-page allocation with `allocZP`
* `lo`/`hi` byte helpers
* 6502 ISA: all 56 official opcodes with full addressing mode support
* C64 target configuration with subsystem-aware free zero-page tracking

### Output formats
* `.prg` file generation (`toPRG` in `Target.C64.PRG`)
* `.d64` disk image generation (`toD64` in `Target.C64.D64`)

### Testing
* QuickCheck test suite with deterministic seed
