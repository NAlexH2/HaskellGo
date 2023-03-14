# HaskellGo
The game of "Go" is ancient and the first instances of the game being played
goes back around 4,000 years ago in China. It has stood the test of time and
is still enjoyed by many people world wide.

This repository is an attempt to re-create the game using **Haskell** as the
language of choice while utilizing 
[The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/) which comes
packaged with the following:
 
 - GHC
 - Haskell Language Server
 - Stack
 - Cabal


# [Rules of Go](https://en.wikipedia.org/wiki/Rules_of_Go)
With a game as old as Go, there have been a number of rules set in place. For
this project (being that it was my first foyer working with Haskell and required
a lot of learning) I limited myself to the following basic rules:

- Board is empty at the start of the game.
- Black makes the first move, then white at which point it alternates between the two.
- A move consists of placing one stone of one’s own color on an empty intersection on the board.
- A player may pass their turn any time.
  - Two consecutive passes end the game.
- A stone of solidly connected group of stones of one color is captured and 
  removed from the board when all the intersections directly adjacent to it are 
  occupied by the enemy.
- The player with the most stones captured wins!


### On Ko and Komi - The rule 
**Ko** is the rule that a previous instance of the board may not be repeated.
This eliminates the possibility of an infinite loop of occurring. For more
information see 
[this article on Ko fights](https://en.wikipedia.org/wiki/Ko_fight).

**Komi** is the rule that enables a handicap. This wasn't something I had a
strong desire to implement. Feel free to read 
[this article about Komi](https://en.wikipedia.org/wiki/Komi_(Go)).


# Setup
Depending on what machine you are on, the easiest and fastest way to get started
to run this is by typing one of the following...
> ## Windows PowerShell (Non-admin)
```PowerShell
Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true } catch { Write-Error $_ }
```
> ## Linux, macOS, FreeBSD or WSL2 (Non-root user)
```Bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

## How to run

1. Clone this repository.
2. In your shell of choice, navigate to the directory it is located in.
3. Type `stack build` and let the project compile and download packages.
4. Type `stack run main` to play! Carefully read the instructions once the game
   starts.


# Major Challenges and Their Solutions (And the Files You Can Find Them In)
## Captured Units - CaptureGo.hs
This was the task that took me the longest to figure out. I could not manage to
find a way for sometime on what solution would work best. At a certain point I
ran into the issue of having 3-5 functions attempting to tackle this issue. I
took step back and re-examined the issue. 

Each "stone" in a unit must lose all its liberties to be considered captured.
Once I realized all I had to do was examine each stone to check its liberties
and then check all the other stones in that unit for the same instance of losing
all their liberties. From there, the code returned `True` or `False` for each
stone - `True` if it lost all its liberties, `False` if any of its liberties
were not occupied - if *any* were `False`, then the unit had not been captured,
if all were `True` then the unit had been captured.

## Identifying Units - CaptureGo.hs
This was the 2nd most difficult task, weirdly enough. It had felt (at first) as
if it was going to take me the entire duration of the project but I found myself
noticing a pattern in the code I had already wrote.

This pattern actually saved me a lot of time trying to generate a solution and
realized that my arrays had something along these lines of overlapping values:
```Haskell
[[0,1,3],[2,3],[3,5,6],[4,7],[5,8]]
```
Once recognizing this pattern, and a bit more research, ```` a `union` b ````
could do ***a lot*** of heavy lifting in identifying my overlapping values.
There's a lot of summarizing happening I'll admit, but with some fancy logic
and a bit of function composition the above array came out to a nice neat list
of lists like this:

```Haskell
[[0,1,2,3,5,6,8],[4,7]]
```
Where the first was all the `Black` stones in a unit, and the second was all the
`White` stones in a unit. This part wasn't explicitly ordered this way, but the
example comes from a real list I had generated while working on the problem with
`testBoard2` in `GoTests.hs`

This then allowed me to start working on identifying which stones needed to be
captured. Single stones were trivial in comparison.