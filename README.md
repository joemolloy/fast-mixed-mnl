
# MIXL

The *mixl* package presents a new approach to estimating complex choice models which is both many times faster than existing approaches, and simplier to use. The interface remains in R, while the loglikelihood function is compiled into to high performance C++ code in the background. Users need no knowledge of C++, and can specify thier utility functions and random variables as they would on paper. Where the appropriate parallel computing resources are available (on linux machines), the loglikelihood estimation can be easily sped up by simply specifying the number of cores available.

## Motivation
The development of this package was motivated by the need at the [Institute for Transport Planning and Systems (IVT)](https://www.ivt.ethz.ch/) at ETH Zürich to 
estimate complicated mixed multinomial logit models of time use and travel behaviour. Other approaches, such as [biogeme](https://biogeme.epfl.ch/) and other R implementations were found to be very memory intensive when a large number of random draws were needed. 

Additionally, we found that the learning curve for some implementations was very steep, and changes to model specifications involved understanding of the underlying code base, on top of random utility and choice modeling fundamentals. 

In R, until now there has been two approaches for specifying utility functions. The first, popularised by the mlogit package uses the R formula notation to allow succint yet expressive definition of a utility function. This approach falls short however, when the user wants to specify multiple utility functions, which is standard practice for more complicated models. This approach also doesn't handle complicated mixed models well. The alternative involves specifying the utility functions as operations on datatable columns, which is much more flexible, but inefficient to run, and error prone to write.

From a technical perspective, the utility functions can be seen as the 'inner loop' of the Maximum likelihood Estimation (mle) operation. If there are p parameters, then for every iteration of the mle the utility functions are calculated at least 2p+1 times. It follows that any attempt at high performance mnl estimation should focus here. 

This is where approaches in R fall short. The speedups possible by coding inner loops using C++ with the Rcpp package are already well [widely documented](https://dirk.eddelbuettel.com/papers/useR2014_keynote.pdf), however, every change to the utility function would require changes to code in C++, not something your trained choice modeller should be spending thier time on. 

## Aim
Hence, we invisaged a package where one could write utility functions as they would on paper, which could be parsed into C++ and complied 'behind the scenes' into a high performant loglikelihood function accessible from R.

The second goal was to optimise the memory consumption. The straightforward approach is to merge the dataset with the  random draw dataset, duplicating every data row by the number of requested random draws. However, this leads to an polynomial increase in the memory required. And is particular inefficient for large panel datasets. However, if the data and random draw datasets are kept separate, then the memory usage is only linear in the number of draws.

As such, this package, called *mixl*, allows for advanced choice modelling through an intuitive interface. The number of random draws is no longer limited by the computer memory available. Indeed, we have been able to estimate models with dozens of parameters and 10,000 random draws. Importantly, this performance does not require advanced programming skills from the modeller. models can be speficied as they would be on paper. The code is also parallelised, see section ________ for more details.

For more details, see the readme and Vingettes

## Installation
Under the hood, the *mixl* package compiles a loglikelihood function based on your model specification. This requires a c++ compiler. Somewhat complicatedly, the correct compiler (for C++ with openMP support) is only available by defauly on linux machines. On windows and OSX machines, *mixl* will installed successfully, but attempts to specify a model will fail unless the following steps are taken.

### Windows
On windows machines, the easiest way is to install Rtools, an official extension for R. As of writing, the Version 4.0 the most recent. It is available here: [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/). Download and run the installer. The default location (`C:/Rtools`) is fine.

### OSX (Mac)
On Mac, things are a little more complicated. OSX has a compiler pre-installed. However, the compiler (clang) doesn't come with openMP enabled. Hence, the `specify_model` function will fail. The easist way to install a suitable compiler is as follows. Note, these commands require using the Terminal (command line). This can be accessed from the search functionality in OSX, or directly in R Studio itself. The following code assumes that you don't have a Makevars for your R environment set up already.

* Install [homebrew](https://brew.sh/)
* Install gcc `brew install gcc`
* Make a note of which gcc version is provided by brew. i.e. gcc-14
* Add the following to ~/.R/Makevars, replacing `xx` with your gcc version:
``` 
CC=gcc-xx
CXX=g++-xx
CXX1X=g++-xx
CXX11=g++-xx
SHLIB_CXXLD=g++-xx
FC=gfortran-xx
F77=gfortran-xx

SHLIB_OPENMP_CFLAGS=-fopenmp
SHLIB_OPENMP_CXXFLAGS=-fopenmp
SHLIB_OPENMP_FCFLAGS=-fopenmp
SHLIB_OPENMP_FFLAGS=-fopenmp
```

