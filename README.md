# RNpy
This package enables to read and write npy files created by numpy into R. It is implemented using only R features, so we do not need Rcpp.

```{R}
library(RNpy)
x <- read.npy("file.npy")
```

## Limitation
It only implements reading up to 3-dimensional array and writing up to 2-dimensional matrix.
