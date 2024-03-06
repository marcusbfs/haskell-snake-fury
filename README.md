# snake-fury

This is my solution to [snake-fury Haskell challenge](https://github.com/lsmor/snake-fury).

- [snake-fury](#snake-fury)
  - [Introduction](#introduction)
    - [note about not using monads](#note-about-not-using-monads)
  - [Start coding: Building a MVP](#start-coding-building-a-mvp)
  - [Continue coding: Refactors](#continue-coding-refactors)
  - [Solution branches](#solution-branches)
  - [Architecture](#architecture)
  - [Contributions and Feedback](#contributions-and-feedback)

## Run

```bash
# To move the snake, either
#   use arrow keys: ← ↑ → ↓
#   use vim motions: hjkl
#   use gamer motions: wasd

# cabal users
cabal build
cabal run snake-fury -- height width fps # A common set up is a 10x10 board running at 6 fps

# stack users
stack build --stack-yaml stack-ghc-<your.system.version>.yaml
stack run snake-fury -- height width fps # A common set up is a 10x10 board running at 6 fps
```

