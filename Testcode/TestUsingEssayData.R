

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
essay=read.csv("essay.csv",header=FALSE)
grades=essay[,1:5]
y=matrix(0,nrow(grades),3)
y <- as.data.frame(y)
y[,1]=grades[,2]
y[,2]=grades[,3]
y[,3]=grades[,4]
y_true <- y
r_2 <- rbinom(nrow(y),1,0.3)
y[r_2==1,2] <- NA


####Run MCMC ####
source("../probitBayes.R")
Results <- probitBayesImputation(y, N = 30, Mon = 2000, B = 1000, thin.int = 5, seed = 0)
