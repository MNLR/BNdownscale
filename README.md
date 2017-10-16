BNdownscale
==========

**BNdownscale** aims to apply Discrete Bayesian Networks to climate statistical downscaling using the statistical software  [`R`](https://www.r-project.org/). There are two main functions,

1.  build.downscalingBN() builds the Bayesian Network for a data set, and 
2.  downscale.BN() uses the network from build.downscalingBN() to perform downscaling.

They are specifically prepared to work with the climate data set structures provided by the [`loadeR` bundle](https://github.com/SantanderMetGroup/loadeR) for data access, and use [`bnlearn` package](http://www.bnlearn.com/) for Bayesian networks.

***

The folder contains two examples: EX.VALUE.GER.R and EX.VALUE.IB.R, which perform downscaling over Germany and the Iberian penynsula, respectivelly. Aditional examples showing different combinations of the options available can be found in DBN.examples.R. The folder **functions** contains all the functions implemented, in particular the two above mentioned, divided into the subfolders:


* downscaling: Contains the functions that perform the two phases (train and predict) of the downscaling process.
* local.bnlearning: Contains wrappers for the local learning algorithms, which
* validation: Contains different functions to compute scores and perform cross-validation to the model.
* plot.graph.functions: Contains several functions to plot downscaling bayesian networks.
* generalaux: Contains functions to process climate datasets.

**exampleplots** has several bayesian network and scores plots and **data** contains a few example datasets obtained using [`loadeR` bundle](https://github.com/SantanderMetGroup/loadeR). The file Worked_example.html briefly describes the main functions and explains with several examples how to perform downscaling using all the functions developed.
