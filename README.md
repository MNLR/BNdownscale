BNdownscale
==========

**BNdownscale** aims to apply Discrete Bayesian Networks to climate statistical downscaling using the statistical software  [`R`](https://www.r-project.org/). There are two main functions,

1.  build.downscalingBN() builds the Bayesian Network for a data set, and 
2.  downscale.BN() uses the network from build.downscalingBN() to perform downscaling.

They are specifically prepared to work with the climate data set structures provided by the [`loadeR` bundle](https://github.com/SantanderMetGroup/loadeR) for data access, and use [`bnlearn` package](http://www.bnlearn.com/) for Bayesian Networks.

***

The folder contains two examples: EX.VALUE.GER.R and EX.VALUE.IB.R, which perform downscaling over Germany and the Iberian penynsula, respectivelly. Aditional examples showing different combinations of the options available can be found in DBN.examples.R. The folder functions contains all the functions implemented, in particular the two above mentioned, divided into the subfolders


* downscaling folder contains the functions that perform the two phases (train and predict) of the downscaling process.
* local.bnlearning contain wrappers for the local learning algorithms, which
* validation folder contains different functions to compute scores and perform cross-validation to the model.
* plot.graph.functions contains several functions to plot downscaling bayesian networks.
* generalaux contains functions to process climate datasets.



