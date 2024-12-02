# A Comparative Study of Imputation Methods for Multivariate Ordinal Data

## Abstract
Missing data remains a very common problem in large datasets, including survey and census data containing many ordinal responses, such as political polls and opinion surveys. Multiple imputation (MI) is usually the go-to approach for analyzing such incomplete datasets, and there are indeed several implementations of MI, including methods using generalized linear models, tree-based models, and Bayesian non-parametric models. However, there is limited research on the statistical performance of these methods for multivariate ordinal data. In this article, we perform an empirical evaluation of several MI methods, including MI by chained equations (MICE) using multinomial logistic regression models, MICE using proportional odds logistic regression models, MICE using classification and regression trees, MICE using random forest, MI using Dirichlet process (DP) mixtures of products of multinomial distributions, and MI using DP mixtures of multivariate normal distributions. We evaluate the methods using simulation studies based on ordinal variables selected from the 2018 American Community Survey (ACS). Under our simulation settings, the results suggest that MI using proportional odds logistic regression models, classification and regression trees and DP mixtures of multinomial distributions generally outperform the other methods. In certain settings, MI using multinomial logistic regression models and DP mixtures of multivariate normal distributions, are able to achieve comparable performance, depending on the missing data mechanism and amount of missing data.

**Keywords**: Multiple imputation; Missing data; Mixtures; Nonresponse; Tree methods

## Objectives

There have been empirical comparisons of several MI methods for multivariate continuous data, multivariate nominal data, or both. However, there are no rigorous empirical comparisons of MI methods for multivariate ordinal data. Specifically, to the best of our knowledge, there are no studies that provide practical guidance on potential advantages and disadvantages of using nominal models over the ordinal versions, when dealing with ordinal data. Therefore, in this work, we compare six main MI methods for multivariate ordinal data, including (i) MICE using multinomial logistic regression models (MI-Multireg), (ii) MICE using proportional odds logistic regression models (MI-Polr), (iii) MICE using CART (MI-Cart), (iv) MICE using random forests (MI-Forest), (v) MI using DP mixtures of products of multinomial distributions (MI-DPMPM), and (vi) MI using DP mixtures of multivariate normal distributions (MI-DPMMVN). We compare all methods using evaluation metrics that assess how each method preserves and recovers both marginal features and joint relationships in the data. We base our empirical comparisons on hypothetical populations comprising data from the 2018 American Community Survey (ACS).

## Dataset

We conduct our empirical comparison of the methods using data from the 2018 American Community Survey (ACS) Public Use Microdata Sample (PUMS) files. As is generally the case for single-year ACS PUMS, the 2018 ACS PUMS contains sample data from about 1 in every 40 households in the United States. The data includes variables collected both at the individual level (e.g., income, age, sex, educational attainment) and at the household level (e.g., lot size, and number of rooms, vehicles, and persons). The data can be downloaded from [the United States Bureau of the Census](https://www2.census.gov/programs-surveys/acs/data/pums/2018/).

## Software and analytical approaches

For MI-Multireg, MI-Polr, and MI-Cart, we use the mice package in R. For MI-Forest, we use the missForest package in R. For MI-DPMPM, we use the NPBayesImputeCat package in R. For MI-DPMMVN, we adapt the R code in the supplementary material for DeYoreo & Kottas (2014). 

We also evaluate the performance of a deep learning imputation method like GAIN which is based on generative adversarial networks (GAN) framework. We modify the authors’ code from Yoon et al. (2018), made available through their [GitHub repository](https://github.com/jsyoon0823/GAIN). For our comparisons, we consider two implementations of GAIN. The first uses MSE as the reconstruction loss while the second uses cross-entropy loss as our reconstruction loss for categorical variables and also takes into account different missing rates in different features while training.

## Key file description
* **[Analysis](https://github.com/ChayutWo/Nonparam-oridinal-nominal/tree/master/Analysis)**: Folder that contains all codes for generating subsampled datasets, performing MI on each of them, and analyzing the imputed datasets. The names of subfolders are self-explanatory.
* **[Dataset](https://github.com/ChayutWo/Nonparam-oridinal-nominal/tree/master/Datasets)**: Folder that contains all 500 subsampled datasets for different missing data mechanisms.
* **[GAIN](https://github.com/ChayutWo/Nonparam-oridinal-nominal/tree/master/GAIN)**: Folder that contains our two different implementation of GAIN: gain.py and gain_categorical.py.
* **[utils/models](https://github.com/ChayutWo/Nonparam-oridinal-nominal/tree/master/utils/models)**: Folder that contains R codes for running different MI methods. The names of R files are self-explanatory.

## Publications
Chayut Wongkamthong, Olanrewaju Akande, A Comparative Study of Imputation Methods for Multivariate Ordinal Data, Journal of Survey Statistics and Methodology, Volume 11, Issue 1, February 2023, Pages 189–212, https://doi.org/10.1093/jssam/smab028

Wongkamthong, C., & Akande, O. (2020). A Comparative Study of Imputation Methods for Multivariate Ordinal Data. arXiv: Methodology. [arXiv](https://arxiv.org/abs/2010.10471)

**Author**

Chayut Wongkamthong - Social Science Research Institute (SSRI), Duke University

**Advisor**

Professor Olanrewaju M. Akande - Social Science Research Institute (SSRI), and Statistical Science, Duke University
