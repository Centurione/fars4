---
title: "README"
author: "Adam Centurione"
date: "17/01/2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## fars4 Package

## By : Adam Centurione

This R package is the final assignment for the "Building R Packages" on Coursera

Date: January 17, 2020

##Installation

To install this package to run on your system, please first install and load the devtools package. Then you may install and load this package:

``` {r}
install_github('Centurione/fars4',build_vignettes = TRUE)
library(fars4)
```

## Vignette

To get started, read the introduction vignette: vignette('fars4', package = 'fars4').


[![Build Status](https://travis-ci.org/Centurione/fars4.svg?branch=master)](https://travis-ci.org/Centurione/fars4)
