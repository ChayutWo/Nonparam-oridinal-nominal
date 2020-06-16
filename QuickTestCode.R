

####Load packages needed for MCMC ####
rm(list = ls())
library(truncnorm)
library(mvtnorm)
library(MCMCpack)
library(mnormt)
library(abind)
library(tidyverse)
library(dplyr)


####Define data####
y  <- read.csv("Datasets/PUMS_missing/MAR_30percent.csv",header=T)
dim(y)
head(y)


####Run MCMC ####
source("probitBayes_by_cluster.R")
Results <- probitBayesImputation(y, N = 50, Mon = 2000, B = 5000, thin.int = 5, seed = 0)


library(coda)
Results$alpha
plot(mcmc(Results$alpha))
plot(mcmc(Results$M.cur))
