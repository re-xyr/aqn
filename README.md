# aqn

Toy dependent glued normalization-by-evaluation elaborator [à la sixty](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784).

## Performance

I have tried multiple effects libraries in the implementation because I do not want to resort to bare `IO`. These are the benchmark numbers, resulted from single runs:

| lib           | forcing 10m nat | conversion checking 10m nat | branch          |
| ------------- | --------------- | --------------------------- | --------------- |
| smalltt       | 1148ms          | 1681ms                      | -               |
| freer-simple  | 2261ms          | 5287ms                      | `master`        |
| mtl*          | 2648ms          | 9907ms                      | `mtl`           |
| fused-effects | ~4s             | ~13s                        | unavailable yet |
| mtl**         | 7249ms          | 8441ms                      | `mtl-inlinable` |
| mtl***        | 20710ms         | 28497ms                     | `mtl-no-spec`   |
| effectful     | 8317ms          | 48308ms                     | `effectful`     |

*\*: Specialization by `SPECIALIZE`, \*\*: Specialization by `INLINABLE`, \*\*\*: No specialization*

The effects were simply `Fresh` + `State` via `IORef` + `Error UnifyError`, and I was pretty confident that pure means of `State` on boxed values won't perform better. The compiler arguments were `-O2 -fllvm` and rtsopts were `-A1G`. RAM was 16G DDR3 @ 1600MHz and CPU was Intel i7-4870HQ @ 3.7GHz.

Amazingly `freer-simple` managed to beat `fused-effects` and all other competitors, probably due to better specialization. I was surprised by how bad `mtl` performed, and was only nearly on par with `freer-simple` after specialization (which involves writing a hard ton of signatures). `ReaderT`-pattern based `effectful` performed even worse, which is disappointing because it self-claimed to be being very efficient.

Note that `freer-simple` was only somewhere from 1x to 3x slower than bare `IO` in this microbenchmark.
