---
title: "Mixl User Guide"
author: "Joe Molloy"
date: "2020-10-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{"Mixl User Guide"}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



# Introduction
The *mixl* package presents a new approach to estimating complex choice models which is both many times faster than existing approaches, and simplier to use. The interface remains in R, while the loglikelihood function is compiled into to high performance C++ code in the background. Users need no knowledge of C++, and can specify thier utility functions and random variables as they would on paper. Where the appropriate parallel computing resources are available (on linux machines), the loglikelihood estimation can be easily sped up by simply specifying the number of cores available.

##Installation
Under the hood, the *mixl* package compiles a loglikelihood function based on your model specification. This requires a c++ compiler. Somewhat complicatedly, the correct compiler (for C++ with openMP support) is only available by defauly on linux machines. On windows and OSX machines, *mixl* will installed successfully, but attempts to specify a model will fail unless the following steps are taken.

### Windows
On windows machines, the easiest way is to install Rtools, an official extension for R. As of writing, the Version 4.0 the most recent. It is available here: [https://cran.r-project.org/bin/windows/Rtools/](https://cran.r-project.org/bin/windows/Rtools/). Download and run the installer. The default location (`C:/Rtools`) is fine.

### OSX (Mac)
On Mac, things are a little more complicated. OSX has a compiler pre-installed. However, the compiler (clang) doesn't come with openMP enabled. Hence, the `specify_model` function will fail. The easist way to install a suitable compiler is as follows. Note, these commands require using the Terminal (command line). This can be accessed from the search functionality in OSX, or directly in R Studio itself. The following code assumes that you don't have a Makevars for your R environment set up already.

* Install [homebrew](https://brew.sh/)
* In the terminal, run the following commands 
``` 
brew install gcc@9 
mkdir -p ~/.R
curl -L -o ~/.R/Makevars https://git.io/JTRql
```
* 

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

# Defining a model
To both simplify the transformation of the utility functions into C++ code and aid the readability of the scripts, a small amount of syntax has been introduced:

* Variables from the dataset must be prefixed with a \$, 
* coefficients with a @
* Every line must end with a ; 
* Intermediate variables that are calculated don’t get prefixed by anything
* The utility functions are prefixed by U_. “U_bus” or “U_4” are valid, but U_geer.erg is not.
* Draws are prefixed by “draw”. If you pass nDraws into the estimate function, a suitable set of halton draws will be generated automatically. If you pass in a draws matrix, it needs to be large enough. there is a helper function to do this: 
``` mixl::create_halton_draws(Nindividuals, Ndraws, draw_dimensions) ```

* For powers, you need to use pow (x,y) instead of x^y

## Availabilities
The availabilities must to be provided as a matrix of $n*k$ where $n$ is the number of rows in the data, and $k$ is the number of utility functions. The helper functions \code{extract_av_cols} and \code{av_matrix} can help here

## The mixl input file
Below are two sample model definitions to get you started. The package automatically detects mixing, and generates the loglikelihood function appropriately. A simple mnl model, using the Train dataset could look like the following:
```
U_A =          @B_price * $price_A / 1000 + @B_timeA * $time_A / 60 + @B_change * $change_A; 
U_B = @ASC_B + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
```
Here, the coefficients ASC_B, B_price, B_time, B_timeB and B_change would be estimated. 

If we extend this to a simple mixed model, the additional random coefficients SIGMA_B and SIGMA_B2 are estimated. the declaration of ASC_B_RND helps the readability of the utility function:
```
ASC_B_RND = @ASC_B + draw_1 * @SIGMA_B  + draw_2 * @SIGMA_B2;

U_A =             @B_price * $price_A / 1000 + @B_timeA * $time_A / 60 + @B_change * $change_A; 
U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
```  

## Input and constraints
As clean as this interface is, there are some limits to what the package can handle. The modeller needs to make sure that their dataset has two columns, ID and CHOICE. 

* ID is the id of the individual, NOT the choice, and needs to be congurent from $1..n$ where n is the number of individuals. 
* For each row in the data, CHOICE must be an integer from $1..k$, where k is the number of available alternatives. 
* A utility function must be specified for each alternative. If none is required, ```U_x = 1``` will give a utility of zero.
* The availabilities need to be a numeric matrix of 1 or 0s, with the number of rows equal to number of data rows, and the number of columns equal to the number of alternatives.

#M maximum simulated  likelihood estimation
A set choice model generated through this package is estimated using the ```maximumLikelihood``` function. It wraps the maxLik procedure from the maxLik package, which then accepts the optimised likelihood function generated from a utility script. The maxLik package is stable and well tested. To improve the covariance estimates, the Hessian function from the numDeriv package, rather than the default from the maxLik package is used. The BFGS (Broyden–Fletcher–Goldfarb–Shanno algorithm) method is used by deafault. This has found to converge faster and more reliably than other methods such as Newton-Rhapson.

Even for simple MNL models, the numerical gradient of the loglikelihood function is used, as it is envisaged that this package will primarily be used for estimating mixed models, where simulation is necessary due to the use of random draws. 

# Technical Details

## The precompiler
### What is validated

## Performance

### Speed Comparisons

### openMP and parallel computing
Parallelisation is provided through the [openMP](https://www.openmp.org/). Where this functionality is available (primarily on linux, but it can be enabled on OSX with some effort) the loglikelihood function can be run in parallel with near linear speedups. The parallel performance is largely due to the 'embarassingly parallel' nature of the problem. For each observation, only $k$ utilities are calculated, and each observation is independent. The input data, namely the variables, coefficients and random draws are modified during the calculation. This means that the only variable jointly written by each processing core is the output matrix of probabilities. Since the update operation is only an (associative) addition, this operation can be performed atomically, requiring no locking sections of code.

# Advanced Topics

## Probit
Probit models are currently not supported, though that is planned for the near future.

## Hybrid Choice
The package also supports hybrid choice models. Declared variables with the prefix ```P_indic_``` will be considered as probability indicators for each observation. The compiler detects these automatically, and automatically includes  the code to include the product of the probability indicators in the loglikelihood. The model estimation will return both the choice loglikelihood and the model loglikelihood. In psuedo code, the inclusion of the indicators looks like this: 

```
p_choice = log(chosen_utility / sum(utilities) );
p_indic_total = P_indic_1 * P_indic_2 * .... P_indic_k;

p_choice = p_choice + (1/num_obs)*log(p_indic_total);
```
One extra column is required in the dataset to enable hybrid choice, namely a ```count``` column with the total number of observations for the individual making the choice.

### Using probability distributions
To give maximum flexibility to the user, there are no restrictions on which probability distributions can be used in the probability indicators. In R, it is common to use functions such as ```rnorm``` or ```dnorm``` from the stats package. These can still be used, but the syntax is a little different, as they must be called from the C++ code. hence, where in R one would call the following 

```dnorm(x, mean, sd)```

in the utility script, one must write

```R::dnorm(x, mean, sd, 0);```

For a list of distributions which can be used, please refer to the [Rcpp documentation](https://dirk.eddelbuettel.com/code/rcpp/html/namespaceR.html)
