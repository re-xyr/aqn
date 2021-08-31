# aqn

Toy dependent glued normalization-by-evaluation elaborator [à la sixty](https://gist.github.com/AndrasKovacs/a0e0938113b193d6b9c1c0620d853784).

## Performance

I have tried multiple effects libraries in the implementation because i do not want to resort to bare `IO`. These are the benchmark numbers, resulted from single runs:

| lib            | forcing 10m nat | conversion checking 10m nat |
| -------------- | --------------- | --------------------------- |
| baseline (stt) | 1148ms          | 1681ms                      |
| freer-simple   | 2261ms          | 5287ms                      |
| fused-effects  | ~4s             | ~13s                        |
| mtl            | -               | ~33s                        |
| effectful      | -               | ~167s                       |

The compiler arguments were `-O2 -fllvm` and rtsopts were `-A1G`.

I was surprised by how bad `mtl` performed even with specialization, and `ReaderT`-pattern based `effectful` performed even worse. Amazingly `freer-simple` managed to beat `fused-effects`, probably due to better specialization.

Note that `freer-simple` was only somewhere from 1x to 3x slower than bare `IO` in this microbenchmark.
