---
title: "README.md"
author: "Miguel Conde"
date: "Tuesday, April 14, 2015"
output: html_document
---

This repo contains the code used in DSC final project to:

- Build models
- Predict words

R code is stored in folder ./R

## Building models

- **Modelling.Rmd**: explanation of the process followed and R code used
    + Main R module: ./R/trainModel.R
    + Auxiliary R modules: ./R/makeTTSets.R, ./R/sampleFiles.R, ./R/managePunct.R, ./R/statsNWordGrams.R, ./R/makeTFLs.R

## Predicting words

- **Predicting.Rmd**: explanation of the process followed and R code used (back off and back off + interpolation)
    + Main R module: ./R/predict.R
    + Testing predic: ./R/test_predict.R
    + **Interpolation.Rmd**: how this algorithm works and how weights were optimized minimizing perplexity
        + Main R module: ./R/tuneLambdas.R
        + Auxiliary R modules: ./R/perplex.R
        + Testing perplextity: ./R/test_perplex.R
    + **Accuracy.Rmd**: how prediction accuracy tests were performed and their results
        + Main R module: ./R/makeTests.R
        + Auxiliary R modules: ./R/makeTFLs.R, ./R/testingModels.R, ./R/predict.R


