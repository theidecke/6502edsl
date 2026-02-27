# Code Review — 6502edsl v0.2.0.0

Thorough review of the full codebase as of 2026-02-27.

## Summary

The project is in excellent shape for its stage. Clean architecture, zero compiler warnings, comprehensive test suite, and idiomatic Haskell throughout. The DSL design is clever — the `MonadFix`-based single-pass assembly, the type-driven addressing mode selection, and the `#`/`!` operator sugar combine to make 6502 assembly feel natural in Haskell.

The findings below are organized by severity: correctness issues first, then robustness concerns, design suggestions, and minor style notes.

---

## Correctness Issues

### 1. Branch offset calculation silently wraps on out-of-range targets

**File**: `src/Asm/Mos6502.hs:321-323`

```haskell
emitBranch :: Opcode -> Word16 -> ASM ()
emitBranch opc target = do
    pc <- label
    let offset = fromIntegral (target - pc - 2) :: Word8
    emit [opcodeFor opc MRelative, offset]
```

The 6502 branch offset is a signed `Int8` (-128 to +127), but this code casts to `Word8` without checking the range. If `target - pc - 2` falls outside [-128, +127], the offset silently wraps and the branch goes to the wrong address. The `doc/6502-edsl-plan.md` explicitly calls this out as needing validation.

**Recommendation**: Add a range check and `error` if the offset exceeds signed byte range. This is an assembly-time error, consistent with how `fitsIn` and `samePage` already work.

### 2. `cmp16` does not set carry before `SBC` on high byte

**File**: `src/Asm/Mos6502/Ops16.hs:47-50`

```haskell
cmp16 :: Var16 -> Var16 -> ASM ()
cmp16 a b = do
    lda (lo16 a); cmp (lo16 b)
    lda (hi16 a); sbc (hi16 b)
```

The `CMP` on the low byte sets carry correctly for unsigned comparison, and the `SBC` on the high byte borrows from that carry — this is actually the standard 6502 idiom for 16-bit unsigned comparison. However, the initial carry is not explicitly set. If the code preceding `cmp16` leaves carry in an unknown state, the `CMP` instruction always sets carry based on its comparison (carry is an *output* of `CMP`, not an input), so the low byte compare is fine. The `SBC` on the high byte then correctly uses the carry/borrow from `CMP`. This is actually correct as-is, matching the well-known 6502 pattern. No fix needed.

### 3. PETSCII mapping inconsistency between `Target.C64.Data` and `Target.C64.D64`

**File**: `src/Target/C64/Data.hs:33-38` vs `src/Target/C64/D64.hs:97-103`

`charToPetscii` in `Data.hs` maps lowercase `a-z` to `$C1-$DA` (PETSCII lowercase codes in unshifted mode) and errors on unsupported characters.

`toPETSCII` in `D64.hs` maps lowercase to uppercase (`- 32`, so `a` becomes `A` = `$41`) and falls back to space for unknown characters.

Both are valid PETSCII mappings for different contexts (screen codes vs. disk names), but the naming collision and inconsistent behavior could cause confusion. The D64 version is specifically for disk/file names where uppercase is conventional, so the different behavior makes sense.

**Recommendation**: Add a brief comment to `D64.hs`'s `toPETSCII` clarifying it's disk-name-specific and differs from `Target.C64.Data.charToPetscii`.

---

## Robustness Concerns

### 4. `findContiguous` can skip valid runs due to `drop runLen` logic

**File**: `src/Asm/Monad.hs:123-129`

```haskell
go (a:as)
    | runLen >= n = a
    | otherwise   = go (drop runLen (a:as))
  where
    runLen = 1 + length (takeWhile id (zipWith (\x y -> x + 1 == y) (a:as) as))
```

When a run is too short, it drops `runLen` elements from `(a:as)` and tries again. This is correct — it skips past the entire contiguous run to start fresh. However, if the free set has addresses like `[0x02, 0x04, 0x05, 0x06]` and `n=3`, it correctly finds `0x04`. No bug here, but the logic is subtle enough to warrant a clarifying comment.

### 5. `allocZP` error messages don't say which allocation failed

**File**: `src/Asm/Monad.hs:107-114`

When `allocZP` fails, the error says how many bytes were needed but not what the calling context was. In a program with many allocations, this makes debugging harder.

**Recommendation**: Consider a variant that takes a label string for better diagnostics, e.g., `allocZPNamed :: String -> Int -> ASM Word8`.

### 6. `opcodeFor` uses `error` on invalid mode

**File**: `src/Asm/Mos6502.hs:299-304`

This is fine for assembly-time errors, but `error` produces unhelpful stack traces. With GHC's `HasCallStack`, adding a constraint would give better error locations:

```haskell
opcodeFor :: HasCallStack => Opcode -> Mode -> Word8
```

This is low priority since the opcode table covers all valid combinations and the DSL functions dispatch through `emitOp` which always provides the correct mode.

### 7. D64 `toD64` operates on `[Word8]` — performance on large programs

**File**: `src/Target/C64/D64.hs`

The entire D64 module works with `[Word8]` (linked lists). For a 174,848-byte disk image, this means ~700K cons cells. The image is assembled by concatenating 683 sectors. For the current use case (small C64 programs), this is fine. If larger images or batch processing become relevant, `ByteString` or `Vector` would be more appropriate.

---

## Design Observations

### 8. No branch range relaxation

The plan document (Part 1) mentions branch relaxation (expanding branches to branch-over-JMP sequences) as a future consideration. Currently, out-of-range branches silently produce wrong code (see item 1). For now, just adding range validation is the right call. True relaxation would require a multi-pass approach that conflicts with the current `MonadFix` design.

### 9. `ISA.Mos6502` types are not used by the assembler

`Instruction` and `AddressingMode` in `ISA.Mos6502` define a clean AST for the 6502 ISA, but the assembler in `Asm.Mos6502` uses its own `Mode` enum and `Operand` typeclass instead. The `Instruction` type is exported but never constructed anywhere. The `AddressingMode` type carries payloads (`Immediate Word8`, etc.) which would duplicate what the `Operand` newtypes already do.

This is fine if `ISA.Mos6502` is intended as a reference/future disassembler foundation. If not, it could be trimmed to just `Opcode`.

### 10. Var16 `Num` instance is missing

`Var8` and `Ptr` have `Num` instances (for arithmetic on ZP addresses like `ptr + 1`), but `Var16` does not. This means you can't do `lo16 (v + 1)` style pointer math on `Var16`. This is likely intentional since `Var16` address arithmetic is less common, but it's worth noting.

### 11. No BASIC stub generator

The plan document (Part 6) describes a `basicStub` function that emits the standard BASIC autorun line (`10 SYS 2064`). This would allow programs to auto-start instead of requiring manual `SYS` commands. Currently the example program loads at `$C000` and needs `SYS 49152`.

### 12. No `org` / segment support

The plan document (Part 6) describes multi-segment support with `org` directives. Currently, programs are assembled as a single contiguous block from the origin address. This is adequate for most C64 programs but would be needed for more complex layouts (e.g., code at `$0801` + sprites at `$2000` + music at `$C000`).

---

## Test Suite

### Strengths

- **Comprehensive coverage**: ~100 properties covering every module
- **Deterministic**: Fixed seed (42) makes tests reproducible
- **Mix of strategies**: Unit-style `checkOnce` for exact byte sequences + parametric QuickCheck for general properties
- **Structural properties**: Tests for monad laws (PC tracking), instruction size determinism (critical for MonadFix), and round-trip (D64 encode → chain-follow → original data)
- **Error path testing**: `prop_samePageFails` and `prop_fitsInFails` verify that assertions error correctly

### Suggestions

- **No negative tests for invalid addressing modes**: Could verify that e.g. `sta (Imm 0x42)` is a compile-time type error (would need a separate compilation-failure test).
- **Branch offset edge cases untested**: No test for a branch at exactly +127 or -128 offset, or for out-of-range branches (expected to fail — see item 1).
- **D64 test scale**: `prop_d64RoundTrip` limits payload to <2000 bytes. A single test with a larger payload (~30KB, filling many tracks) would exercise multi-track allocation.
- **Control flow with non-trivial bodies**: The `if_eq`/`while_` tests use `nop` bodies. A test with multi-byte bodies that cross branch range boundaries would be valuable.
- **Orphan instance pragma**: `test/Main.hs` uses `{-# OPTIONS_GHC -fno-warn-orphans #-}` for `Arbitrary` instances on operand newtypes. This is reasonable for a test module.

---

## Style and Minor Notes

### Clean build
The project compiles with `-Wall` and **zero warnings** across all 19 library modules, the executable, and the test suite. This is commendable.

### Naming consistency
- Module naming is consistent and well-organized: `ISA.*`, `Asm.*`, `Target.*`
- Memory map constants follow clear prefix conventions
- `jmp_ind` / `jmpInd` alias is slightly redundant — two names for the same function

### Code organization
- Modules are well-sized (none over 500 lines; most under 150)
- Clear section separators with comment banners
- Export lists are explicit on all modules (good practice)

### Documentation
- README is thorough with a complete reference table
- CHANGELOG documents both releases with categorized changes
- The plan document in `doc/` provides valuable architectural context
- Inline comments are sparse but present where the logic is non-obvious (e.g., MonadFix instance, findContiguous)

### Potential cleanup
- `doc/` directory exists with the plan; the new `docs/` directory (where this review lives) creates a split. Consider consolidating to one documentation directory.
- `.gitignore` excludes `*.d64` and `*.vs` but the repo currently contains `hello.d64`, `hello.vs`, and `setcolors.d64` (these appear to be generated artifacts that slipped in before the gitignore was added).

---

## Prioritized Recommendations

1. **Add branch range validation** (item 1) — prevents silent wrong code generation
2. **Comment the D64 PETSCII function** (item 3) — prevents future confusion
3. **Add `HasCallStack` to `opcodeFor`** (item 6) — improves error diagnostics
4. **Add branch edge-case tests** — strengthens the test suite
5. **Consider BASIC stub** (item 11) — high usability impact, small implementation
6. **Consider `org`/segment support** (item 12) — needed for more complex programs
