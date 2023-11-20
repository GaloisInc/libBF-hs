# LibBF bindings for Haskell

This repository defines a high-level Haskell bindings for
[LibBF](https://bellard.org/libbf/), a C library for handling
arbitrary-precision floating-point numbers. These bindings are reasonably
portable (with the caveats listed in the "Limitations" section) and have been
tested to work on Linux, macOS, and Windows.

## LibBF version

By default, this library builds a copy of the LibBF source code that is bundled
with the source distribution, which means that you can build this library
without needing to install any external C dependencies. If you wish, you may
also build this library with the `+system-libbf` flag enabled to instead link
against your operating system's version of LibBF.

The most recent release on the [LibBF website](https://bellard.org/libbf/)
dates back to 2020, which does not support non-x86 architectures (e.g.,
AArch64). For this reason, we instead bundle a more recent of LibBF (written in
2021) that is bundled with the [`quickjs`](https://bellard.org/libbf/) library.
We may also choose to backport other critical bugfixes if they are needed.

## Limitations

* At present, this library only supports 64-bit architectures. If you would
  like to help get this library running on 32-bit, pull requests are welcome!

* Not all LibBF operations are currently exposed. For instance, we do not
  yet expose transcendental functions.
