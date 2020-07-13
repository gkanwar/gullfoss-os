Let's try something crazy
=========================
I hate the idea of writing a "gimmick-y" hobby OS, but I hate the idea of poorly
rewriting the Linux kernel even more. This is all a learning experience, so no
worries about the inability to reasonably port existing software or provide many
useful features. I'm curious to see if this idea is at all feasible while:
(1) Being relatively efficient
(2) Maintaining security
(3) Keeping the spirit of the idea

Idea: a functional OS
=====================
In particular, an OS with Haskell user-space. There have been attempts to write
a kernel in Haskell, which is an interesting direction in its own right, but the
idea here is to write a standard-looking C/C++/ASM kernel that _supports_
user-space code written in Haskell.

Haskell of course provides lots of standard looking functionality to users by
binding impure code (i.e. with side effects) through monads like IO. The idea
here is to go a step beyond. Rather than just binding POSIX-y interfaces to
monadic actions, let's actually design and provide interfaces centered around
pure functional user code. This is the  _primary design challenge_.

Example: TODO
=============
TODO: before diving into coding, it would be useful to have some prototypical
examples of cases where providing a nice functional-friendly OS interface can
reduce app complexity, or improve reliability, or even performance.


Kernel simplicity
=================
We can also exploit the high-level nature of all user code to forego a lot of
the usual protections between user programs in standard OSes. No need for
identical, independent virtual memory spaces for each program. No need to even
run code in ring 3; we may as well run in ring 0, since user code doesn't have
any mechanisms for direct hardware access in the language. With a single flat
virtual memory space, system calls and IPC can directly reference kernel code,
kernel data, and data owned by other programs.

To do this, the kernel must offer as a core feature a _kernel-level_ JIT
compiler for any user Haskell code. Existing compilers assume target OS virtual
memory structures and produce non-relocatable code. We would instead take pure
source and let the OS JIT compile, possibly caching to ELF shadow executables,
with an assumption about kernel structures and other programs in a shared memory
space.

The tradeoff is therefore that at the cost of producing a completely trusted
Haskell JIT compiler, we get to skip lots of the usual OS implementation
challenges and overhead. The _primary implementation challenge_ then is to
guarantee a bug-free JIT compiler.


High-level virtual machines
===========================
Since most of our application developers will not be running in the OS itself (I
might go as far as to say, _all_ of our application developers...), we need the
ability to test in some sort virtualized environment. But in this design, the OS
is presented to user-space code entirely through Haskell libraries and monad
runners. This means that we are free to mock out these libraries and the runner
to write a virtual machine with absolutely no reference to virtualized CPU
architecture. Of course, part of the interface is accessing particular other bits of
hardware like the screen, keyboard, mouse, disks, etc, so these do need to be
virtualized. However, our goal will be to choose these interfaces well, so that
we can virtualize them at a conveniently high level without sacrificing
opportunities for performance or offloading too much logic into the kernel.


Portability
===========
In line with the ability to run in high-level virtual machines, porting the OS
to different hardware (in particular, different ISAs) "only" requires porting
the kernel. User-space code is not allowed to know about low-level instructions
or memory layout, so it cannot depend on the underlying ISA. Of course, it can
possibly depend on specifics of screen resolution, graphics capabilities,
available keyboard keys, etc., but the idea is that these interfaces should be
sufficiently high-level so dependence can be handled by sufficiently general use
of the presented hardware.


Formalism
=========
Below is some scratch work towards designing a useful interface formally.
```
old_world_state :: RealWorld
new_world_state :: RealWorld
f :: RealWorld -> RealWorld
new_world_state = f(old_world_state)
```
pure functions do not depend on world_state,
```
pure :: a, b, c, ... -> a', b', c', ...
```
OS provides impure functions which are abstractly "purified" as fns
of the world state,
```
sys_f :: a, b, c, ... RealWorld -> a', b', c', ... RealWorld
```

All would be well and good, and we could just use the IO monad at the
highest level, except that we also want some sort of multitasking and
probably real concurrency too. Pure functions by definition only depend
on their current inputs, so reduce to two cases

1. Completely defined pure functions, in which case there is no demand
   on data consistency in inputs and we can evaluate inputs at varying
   times (so long as the input datatypes themselves are satisfied).
2. Partially defined pure functions, which expect inputs in their domain
   of definition. In this case, program correctness somehow depends on
   their inputs being consistently grabbed from a single real world.

A "simple" solution is to define only total functions and never encounter this
case. However, promoting functions to total functions cannot solve everything,
because it might just mean that your user code is returning useless values like
`Nothing` based on a race condition (instead of crashing). This still prevents
usable execution of your application from the perspective of the user. (Sidebar:
this is one of the things I hate most about Mathematica's language. Just because
outer functions can continue to apply to garbage results from inner functions
doesn't mean it's useful to do so, and short-circuiting failure e.g. in the
context of the Maybe monad is a really nice thing. However, this should still be
considered failure in some cases.)

Let's come up with examples, because it should make solving the design a lot
easier.
