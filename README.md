## What is Rgogo?

Rgogo is a software framework for actuarial modeling.  It is an R package comprising a collection of classes and tools that are designed for actuarial modeling.  These classes and tools are building blocks that allow actuaries to build and run complex actuarial models in R programming environment easily and quickly.

## Modeling Environment

Rgogo is developed in R programming language.  Modelers are required to use R as the modeling language in order to use Rgogo.

Rgogo is cross-platform.  You can install Rgogo in all operating systems that are supported by R including Windows, MacOS or Linux.  This makes actuarial models portable and shareable.  Any model that is built on one platform can also be modified or run on a different platform without code changes.

## Design Objectives

The design of Rgogo adopts object-oriented programming (“OOP”) principles.  OOP principles allow Rgogo to separate actuarial logic and computing algorithm.  Each Rgogo object represents a modeling component that an actuary is familiar with such as a product, a policy, an assumption or a table.  The complex calculation formulae implemented for the modeling component are encapsulated inside the class design and isolated from modelers.  This allows a modeler to focus on actuarial principles and business logic without being stuck in programming details when conducting a modeling exercise.

Rgogo is designed to achieve the following objectives:

* __Ease of Use__.  Rgogo is for modelers of varying levels of programming skill.  With basic knowledge in R, a modeler can use Rgogo to build and run a complex actuarial model easily and quickly.

* __Customizability__.  Rgogo provides flexibility in customizing model components to meet an individual modeler’s special needs when required by innovative ideas.

* __Transparency__.  Rgogo ensures model clarity and auditability by adopting open-source approach and promoting consistent coding styles.   It allows anyone to inspect the codes and validate the calculation.  

* __Scalability__.  Rgogo is designed to accommodate modeling projects of all sizes, ranging from stand-alone projects of solo modelers to large projects requiring team collaboration.  


## Areas of Application

Rgogo is designed for complex actuarial modeling involving life contingency.  The primary areas of application of such models are life insurance and pension.

Common types of project that can be done with Rgogo include:

* valuation

* pricing

* cashflow projection

* scenario testing

## Getting Started

### Setting Up Modeling Environment

Rgogo is cross-platform.  Rgogo runs in R programming environment.  Since R can be installed in Windows, macOS or Linux, so is Rgogo.  

Running Rgogo requires R version 4.0.0 or above.  To install or update R, visit https://cran.r-project.org.

In addition to R environment, I also recommend installing RStudio.  RStudio is a software tool that proveds an integrated development environment for R users.  RStudio desktop edition is an open-source software.  It can be freely downloaded and installed in Windows, macOS or Linux, depending on which operating system you are using.

To download or update to the latest version of RStudio, visit https://posit.co

### Installing Rgogo

_Rgogo_ is an actuarial modeling framework developed in R.  It provides a collection of building blocks and tools for modelers allowing them to build and run complex actuarial models easily and quickly.  

To install _Rgogo_ package, follow the following steps:

1. Launch RStudio

2. Attach ```devtools``` package by entering the following command in R console:

```R
library(devtools)
```

If ```devtools``` package is not installed, you will encounter an error message like below:

```R
Error in library(devtools) : there is no package called 'devtools'
```

In this case, you should install ```devtools``` first, and then attach the package:

```R
install.packages("devtools")
library(devtools)
``` 

3. Install ```Rgogo``` package.  This is done by entering the following command in R console:

```R
install_github(repo = "ActPersp/Rgogo")
```

The ```install_github``` command will download and install ```Rgogo``` package from GitHub.

During the course of Rgogo installation, the process will also automatically install several other packages that Rgogo depends on if they have not been installed in your local R library. 

After the installation process is completed, you can try attaching ```Rgogo``` package to see if it has been installed successfully.

```R
library(Rgogo)
```

If the installation is successful, you should not see any error message after the execution of the above command.

### R Knowledge Requirement

The level of R experience required by ```Rgogo``` depends on how you will use the package.

Simply using ```Rgogo``` to build and run a typical actuarial model does not require a modeler to be a professional R developer.  A moderate level of R experience should be sufficient.

If you intend to extend or customize ```Rgogo``` functions extensively to meet your modeling needs, more advanced knowledge in R will be needed.  In particular, a good understanding of the R object-oriented programming S4 class system will be essential.

