Implementation of Part II of https://craftinginterpreters.com/ in Haskell.

## Things to improve in future

- Error handling in `Parse.hs` isn't great, because I throw `LoxError 1 "" msg` in
  many places for lack of a better error to throw if there's no relevant token to
  associate with the error. But this shouldn't actually happen if the scanner works
  correctly.

## Significant milestones

- Going from [this monstrosity](https://github.com/mantariksh/hlox/blob/35cd6cbd7f7c7b6e3b6b8ccd9214d5b91a20ec55/Scanner.hs)
  which could only parse consectuve integers, to [this much cleaner version](https://github.com/mantariksh/hlox/blob/45109968ae104b0dceb9ac24e5f858567dba96a9/src/Scan.hs)
  which was a full-fledged scanner. The latter probably wasn't perfect by any measure but it was still a huge
  improvement. Many thanks to https://haskell.mooc.fi/ for their excellent material on monads.
- [Refactoring the interpreter](https://github.com/mantariksh/hlox/commit/16b7f90422eeebbe1c2168015a748220660259bc) towards
  using monads to thread the environment through the statements, instead of passing the environment around as an argument
  everywhere. It took me several existential crises to wrap my head around the monadic construct that I needed but I got
  there in the end (I think).
