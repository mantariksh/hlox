Implementation of Part II of https://craftinginterpreters.com/ in Haskell, up until
most of Chapter 10 (Functions).

I started this project with the goal of learning a functional programming language,
and stopped when I felt that I had sufficiently achieved that goal (and when I started
getting tired of it).

## Usage

1. [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
2. In the project root, run `stack run` for the REPL or `stack run test.lox` to
   interpret a file

## Interesting bugs / decisions

### Catching runtime errors from blocks

Notice this block in the `interpret'` function in `Interpret.hs`:

```haskell
(mapM_ interpret' stmts) `catchError` \err -> do
  env' <- get
  put (popCtx env')
  throwError err
```

I spent hours figuring out that the `catchError` needed to be there.

The problem I was facing was that after implementing `return` statements,
variable scope would break once the call stack got deep enough, e.g. I would start
getting negative numbers from the Fibonacci sequence in `test.lox`.

While debugging, I observed that after a function ended with a return statement,
not all of its contexts would be popped from the context stack. I was
confused because I thought each call to `pushCtx` was matched by another
call to `popCtx`, so this shouldn't have been happening.

Eventually I figured out that the problem was as follows:

- I was using exceptions to exit the call stack whenever I found a `return`
  statement, similar to how the book did it.
- These exceptions were being caught at the function call level, and after
  capturing the return value, I would pop the context of the function.
- The problem was that each block within the function was adding its own
  context as well. The return exceptions were bypassing the call to `popCtx` in
  the block statement handler, and were only being caught further up in `handleCall`.
- The fix was to ensure errors were caught wherever statements were being executed
  after a `pushCtx`, and the relevant context was popped before re-throwing the error.

### Open vs closed abstractions

At the beginning, it was easy to define tokens as follows:

```haskell
-- Expr.hs
data Expr =
    LitNum Double
  | LitStr String
  | LitBool Bool
  | LitNil
  | Grouping Expr
  -- etc
```

This is a pattern called closed abstraction, where it's easy to add new functionality
(just write a new function which pattern-matches all the existing constructors) but
hard to add new types (all existing functions which pattern-match on the constructors
have to be updated).

This made sense at the time because the book deals with interpreters in layers (scanning,
parsing, interpreting etc.), so each layer gets the full set of functionality (e.g.
handling all expression types) in each chapter.

A more realistic way to implement the compiler might have been to use an open abstraction, e.g.

```haskell
data LitNum = LitNum Double
data LitStr = LitStr String

class Expr a where
  evaluate :: a -> ExprOut

instance Expr LitNum where
  evaluate (LitNum n) = NumOut n
```

This would have made it easier to add new types incrementally, at the cost of having
more boilerplate. It would probably also have completely changed the code structure.

## Things to improve in future

- The REPL doesn't keep state between lines.
- Adding synchronisation. I didn't bother with this part of the implementation.
- Continuing to parse after the function argument limit is exceeded. The book
  implementation reports an error and carries on, but this implementation doesn't
  check for the limit at all, because it wasn't straightforward to add an error-reporting
  side effect to the otherwise pure parse function.
- Error handling in `Parse.hs` isn't great, because I throw `LoxError 1 "" msg` in
  a couple of places for lack of a better error to throw if there's no relevant token to
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
  there in the end (I think). (edit: though I had to combine the Interpret and Eval monads in the end because once the interpreter
  could process function calls, expressions now had side-effects)
- Likewise for [refactoring the parser](https://github.com/mantariksh/hlox/commit/688926c0ca9029244a4fd94d04e2169e607789ea).
  Super satisfying to get rid of all the extra args and see all the types line up between parsing of statements and expressions.
