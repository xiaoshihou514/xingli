**型理** (xíng lǐ, reason of types) is an educational typer for lambda calculus, covering basic curry types, $\Lambda_N$, $\Lambda_{NR}$, ML type.

Accompanying blog post:

- Basic curry types, p1: https://xiaoshihou514.github.io/blogs/2025-11-3.html
- Basic curry types, p2: https://xiaoshihou514.github.io/blogs/2025-11-8.html
- $\Lambda_N$ and $\Lambda_{NR}$: https://xiaoshihou514.github.io/blogs/2025-11-18.html
- ML: https://xiaoshihou514.github.io/blogs/2025-12-2.html

型理（类型之理）是一个教学用λ演算类型推导器，支持基本柯里类型, $\Lambda_N$, $\Lambda_{NR}$, ML类型。

相关博客文章：

- 基础柯里类型（上）：https://xiaoshihou514.github.io/zh/blogs/2025-11-3.html
- 基础柯里类型（下）：https://xiaoshihou514.github.io/zh/blogs/2025-11-8.html
- 赋值和递归篇：https://xiaoshihou514.github.io/zh/blogs/2025-11-18.html
- ML类型：https://xiaoshihou514.github.io/zh/blogs/2025-12-2.html

```
$ cabal run
xingli - educational lambda calculus typer

Usage: xingli ((-f|--file FILENAME) | --stdin) [--lang language]

  Type LC, LCN, LCNR or ML

Available options:
  -f,--file FILENAME       Input file
  --stdin                  Read from stdin
  --lang language          Language (lc, lcn, lcnr, ml)
  -h,--help                Show this help text
```

```
$ cabal run
型理 - 教学用λ演算类型推导器

Usage: xingli ((-f|--file 文件名) | --stdin) [--lang 语言]

  选择表达式的语言

Available options:
  -f,--file 文件名            输入文件
  --stdin                  使用标准输入
  --lang 语言                语言（LC, LCN, LCNR, ML）
  -h,--help                Show this help text
```

**Credits**
**鸣谢**

- Steffen van Bakel & David Davies (For the theoretical stuff)
- Steffen van Bakel & David Davies（理论部分）
- Jamie Willis (For the Haskell stuff)
- Jamie Willis（Haskell部分）
