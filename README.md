
# WofostR
## R implementation of Wofost crop simulation model

See [De Wit et al. (2019)](https://doi.org/10.1016/j.agsy.2018.06.018)
for details.

Python version available at
[ajdewit/pcse](https://github.com/ajwdewit/pcse.git) as part of the
Python Crop Simulation Environment (PCSE/WOFOST).

***
### Installation

Install with:
```R
devtools::install_github('lucabutikofer/WofostR', build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = T)
```

Variable `build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = T` insures that vignettes are built both in Mac and Windows environments.

***
### Documentation

A tutorial is available as vingette. To access the vignette type `browseVignettes('WofostR')`
