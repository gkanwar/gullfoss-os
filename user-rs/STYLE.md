Rust style tweaks
=================
Idiomatic Rust is mostly a nice thing. In a few instances, however, it makes
reading code harder. A few suggested tweaks for in-house style:

1. Type suffixes for numeric literals should use `_` for separation. E.g.
```rust
// Bad numeric literal
let x = 0x1234usize;
// Good numeric literal
let y = 0x1234_usize;
```
2. ~~Short names for modules should be used over wildcard imports.~~
*EDIT:* I've come around on this one. Concise wildcard imports can be good.
E.g.
```rust
// Bad wildcard import
use blah::blah::*; // who knows what we just pulled into our namespace?
// Good import
use blah::blah as b;
fn function() {
  b::fn_in_blah_blah(); // much clearer where this comes from
}
```
3. Bare expressions for returns should be avoided. Short lambdas and short code
   without early returns are reasonable exceptions. E.g.
```rust
fn clearer(a: u32, b: u32) -> Option<u32> {
  if a > 100 {
    return None;
  }
  if b < 100 {
    return None;
  }
  let c = a - b;
  return Some(c); // better parallels the early returns, doesn't dangle
}
fn ok_lambda(a: u32, b: u32) {
  let mut v = vec![a, b];
  // short lambda is just an expression with bound variables, bare returns ok
  v.sort_by(|a, b| {
    if a < b {
      Ordering::Less
    }
    else {
      Ordering::Greater
    }
  })
}
```
