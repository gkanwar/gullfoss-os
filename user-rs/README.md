Let's try something crazy: redux
================================
After the first experiment, it seems clear that we need a few features from the
language we choose to support for user-space code:

0. Security through memory safety.
1. Simple or no runtime.
2. Ability to execute efficient tight loops (i.e. should compile or interpret
   to nearly bare-metal ops when needed).
3. Simple and expressive FFI.

After some searching around, it seems like Rust checks all the boxes. Caveat:
rust code allows unsafe operations within `unsafe` blocks. These must be
disallowed to ensure safety when executing in a single shared memory space at
ring-0 privilege level. Not being able to use `unsafe` prevents a small amount
of last-mile optimization, but the benefits should outweigh the cost if the
kernel takes full advantage of a shared memory space and operation level.

A safe package system
=====================
Rust code compiles down to raw binaries, which means there are no guarantees on
Rust safety if the user runs arbitrary pre-compiled packages. Requiring
compilation on the user's system also seems non-ideal: it's a slow process, it's
a heavy process requiring dependencies to be downloaded and compiled, and
different compiler settings and version might cause compilation to unexpectedly
fail.

Instead, we can use a _trusted package system_. Up to implementation details,
the OS maintainers should offer a signing service that allows trusted binaries
to be signed. Only signed binaries should be executed by a production-mode
system. A weak form of the system might leave compilation up to the package
distributors, so long as they attest to running only safe code. A stronger form
would require a compile-and-sign service from the OS maintainers, given raw
source from the distributors.

To alleviate some pressure on a central system, a more exotic community-driven
compile-and-sign service could work as long as deterministic build environments
were possible. This relies on open-source distribution of packages with build
instructions, which is exactly the format used by public Cargo packages as I
understand it. Community members could then compile static package binaries
locally for target systems and cross-check the publicly-hosted results.
