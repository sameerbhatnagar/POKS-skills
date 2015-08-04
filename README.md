# Which skills should be learned before others?

#### Author: Michel C. Desmarais, Polytechnique Montreal

Using the Partial Order Knowledge Structure (POKS) algorithm, the R code in this repository is can help derive which latent skills should be learned before others. Two inputs are required:
- a student-item response matrix
- a Q-matrix, which maps each item to the latent skills required to succeed on that item

The `pdf` file describes the process (`Rnw` file weaved using `knitr`. The workhorse functions are in `lib-poks-3.R`.
 
 