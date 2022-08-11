# Revision history for libBF-hs

## 0.6.4 -- 2022.08.11

* Fix the build on Windows with GHC 9.4, which bundles a Clang-based C
  toolchain instead of a GCC-based one.

## 0.6.3 -- 2021-10-14

* Fix an additional compile issue on 32-bit systems. Previously
the argument to fmul2Exp was incorrectly specified as an `Int64`.

## 0.6.2 -- 2021-02-19

* Fix a bug related to incorrect marshalling of String to C in
  `setString` and `bfFromString`.

## 0.6.1 -- 2021-02-16

* Fix a linker issue on Windows when loading via the GHC dynamic linker.

## 0.6 -- 2021-01-29

* Fix a bug with `frint` and `bfRoundInt` that was causing incorrect
rounding modes to be selected.

* Implement additional operations on `BigFloat` values, including
some missing predicates, conversions to and from binary form
and fused-multiply-add.

* Fix a compile issue on 32-bit systems.

## 0.5.2 -- 2021-01-12

* Use `install-includes` over `c-sources` for header files to avoid linker
issues.

## 0.5.1 -- 2020-07-13

* Add header files to `c-sources` field to include them in `sdist`.

## 0.5.0 -- 2020-07-01

* First version. Released on an unsuspecting world.
