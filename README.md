BNdownscale
==========

**BNdownscale** aims to apply Discrete Bayesian Networks to climate statistical downscaling. There are two main functions,

1.  build.downscalingBN() builds the Bayesian Network for a data set, and 
2.  downscale.BN() uses the network from build.downscalingBN() to perform downscaling.

They are specifically prepared to work with the climate data set structures provided by the [`loadeR` bundle](https://github.com/SantanderMetGroup/loadeR) for data access.

***
