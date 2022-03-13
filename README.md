# calc example from `Learn LLVM 12` in rust

```
cargo run -- samples/with.calc | llc -filetype=obj -o expr.obj
clang expr.obj rtcalc.c -o expr
./expr
```