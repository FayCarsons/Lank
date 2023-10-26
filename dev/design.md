# Mutability
No global mutability per se, but this is a livecoding oriented language so 
global variables can be re-evaluated at any time. If *absolutely necessary* I 
may add an "atom" type(something like Arc+Mutex) that holds mutable values, 
because I would like people to be able to write complex algorithms with
this language and recognize that may be more ergonomic. That said, a REPL should
allow for everything you would need.

Looping sequencers that can have their environment reference replaced may be 
difficult without mutability, at least internally.

