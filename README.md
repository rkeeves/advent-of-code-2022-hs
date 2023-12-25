# advent-of-code-2022-hs

Advent of Code 2022 because 2 very special someones did not finish it last year.

[Base](https://youtu.be/7PCkvCPvDXk?t=159) + containers.

```shell
cd src
ghc -O2 d01.hs
cat d01.inp | ./d01
```

[Let's bother Haskell!](https://youtu.be/eu0KsZ_MVBc?t=11)

### Notes

_Yep, should've `newtype`-d, but at least I aliased..._

_Long `>>>` chains because I like to see da' pipe._

_Ton of small functions instead of one huge blob, because I can separately call them to debug even in ghci._

_Stepwise functions thrown into `unfoldr` / `foldl` etc, instead of manual recursions, because I like it... and you can quickly debug it with `mapAccumL` etc._

_Not too much MonadLogger, MonadPizza, MonadCheeseOnTop. Mostly just purish functions, which are impure... only due to being partial. E.g. parsing, or we assume some stupid nonsense constraint based on the stupid nonsense problem statement... so forgive me Haskell, [I am an imperative little soldier](https://youtu.be/1MoTZJBiQrk?t=11)._
