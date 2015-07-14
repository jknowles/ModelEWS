## Wisconsin Dropout Early Warning System

Welcome to the Wisconsin Dropout Early Warning System (DEWS). DEWS is a machine 
learning application built on top of the DPI data warehouse in the R statistical 
computing language. DEWS is designed to be a flexible and semi-automated machine 
learning system which evaluates dozens of possible machine learning algorithms 
for predicting dropout and selects the highest performing model to use to assign 
risk scores to current students. Many of the details behind DEWS have been 
discussed in other places including on the DPI website:
[http://www.dpi.wi.gov/dews](www.dpi.wi.gov/dews).
The technical details of the machine learning approach are discussed in Knowles 2015.

This document is to describe the DEWS program itself. DEWS is, by design, a 
modular application allowing it the flexibility to be adapted as DPI's data 
changes, new measures become available, and new machine learning techniques are 
developed. This modularity consists of four main subroutines (Knowles 2015). 

0. Prepare your environment
1. Data Acquisition
2. Transform Data
3. Train Models
4. Score Cases

Each subroutine has a number of steps within it. In most cases DEWS contains 
a custom R function to apply each of these steps. This document will describe 
the rest of that workflow and highlight the key functions involved. 

## Preparing DEWS

DEWS is designed to work from a clean install of R using the `DEWSbootstrap` 
function. DEWS uses `packrat` from RStudio to freeze the version of all package 
dependencies from the last known working run in order to ensure that results 
can be repeated and that changes in package dependencies will not break the 
performance of DEWS. DEWS depends on a large number of package dependencies (80+) 
due to the use of many packages that provide a single algorithm. The `packrat` 
functionality ensures that changes in these packages do not break the ability 
of DEWS models to run or be used to recreate predictions. Packages can be updated 
as they are demonstrated to not cause a breaking change in the DEWS workflow. 

This also allows DEWS to rely on dependencies that are not avilable on CRAN, such 
as the core DEWS package, `EWStools` which is currently only available on GitHub. 
To start DEWS on a new machine with an installation of R, the user needs only to 
call: 

```
packrat::restore()
library(DEWS)
DEWSbootstrap()
```

## Data Acquisition

This is the least flexible part of the program and the piece most tightly 
integrated with DPI's specific data systems. Changes in the data system or 
underlying data will require modifying the functions in these packages. 

### Access the Databases

Access means authenticating with each of the data systems necessary to obtain 
the data desired for use in the prediction. In the DPI case, this has been 
consolidated to a single data warehouse instance, and thus only one authentication 
token is required. This is handled by a custom internal DPI R package, `dpiR`, 
which helps users authenticate to DPI's myriad of data systems within R. 

### Query

A key to a successful early warning system is consistent data over many years of 
student observations. The data warehouse handles the storing and cleaning of much 
of the data, and thus the project must begin with a query against the data warehouse. 
DEWS uses a nested query system to query each of several data domains separately, 
and then wrap each of those queries around a master query that resolves the records 
into student-year observations and normalizes some inconsistencies. These 
functions are contained in `R\dataExtract.R`. The key function is `assembleCohort`, 
which grabs all of the student level data for a cohort of students. A cohort is 
defined as all students in a given grade/school-year combination. They are then 
followed to graduation, regardless of their retention/early promotion. Within 
these functions are the business rules for assigning a student as a graduate or 
as a non-completer. 

In addition to acquiring the data and enforcing business rules, these functions 
also create aggregate measures of the peers in the cohort. Thus, school-grade-year 
level aggregates of the percentage of students economically disadvantaged, on an 
IEP, suspended, as well as mean attendance and test scores, are attached to the 
student level data. This will prove useful in model training to allow contextual 
factors to influence student probabilities. 

### Impute

Currently DEWS does not contain any imputation routines, but this section will 
be added in the next release.

### Combine

Combining data from all of the various data sources is handled in the `assembleCohort` 
function, which combines the results of the `pullDemog`, `pullWSAS`, `pullDiscipline`, 
and `pullMobility` functions, as well as the function which computes the peer 
measures `peerStats` into a single R dataframe. 

## Transform Data

To make the model training process as efficient as possible, the data needs to be 
transformed into a method most easily consumed by the machine learning routines 
later in the analysis pipeline. 

### Reshape and Recode

The data is shaped into student-wide format with each row representing a student 
and each column representing a measure of the student. In the DEWS case the 
measures are currently all from a single year, but there is no reason that multiple 
years of measures could not be included. At the end of these columns is the outcome 
variable, a binary indicator of the students' on-time graduation status. In most 
cases, where available, multiple cohorts are stacked together.

The function that handles most of this workload is `buildTrainingPool`, which 
calls `assembleCohort` for all grade N cohorts for which graduation data is 
available, combines those cohorts together, and then runs a function to enforce 
data cleaning and business rules on the result -- `cleanTrainingPool`. The 
cleaning function enforces a host of business rules such as collapsing some 
categorical variables into fewer categories, dealing with missing data, and 
changing the labels of some categories. This function 

### Reshape

The cleaned and stacked data is now reshaped into a model matrix. This is done 
by using the `model.matrix` and `formula` methods in R to expand all categorical 
variables into binary variables and apply polynomial transformations to continuous 
variables where appropriate. The `gatherData` function calls `buildTrainingPool` 
which then transforms the training dataset into a model matrix suitable for 
prediction. 

For ease of workflow later on, the data is stored as nested `list` objects in R. 
The list is divided into training, test, and validation data. Each of these lists 
contains a `pred` object which is the model.matrix of all of the predictor variables 
and a `class` object, which is a vector of the dependent variable stored as a 
two-level factor. The conversion to a list is handled by the `EWStools::assembleData`
function, a convenience function drawn from the open-source core library of DEWS 
available at [www.github.com/jknowles/EWStools](www.github.com/jknowles/EWStools)


### Center and Scale (pre-processing)

An optional step before storing the predictor matrices as lists is to preprocess 
the predictors. The most common operations are to center and scale the predictors 
by subtracting the mean and dividing by the standard deviations. DEWS does this 
using the `preProcess.DEWSList` function, which centers and scales the training, 
test, and validation data all relative to the values in the training data. For 
binary variables, the mean is substracted and for continuous variables standard 
centering and scaling is done using the `preProcess` method in the `caret` package. 
The `preProcess` object used to apply the pre-processing is saved for later so the 
variable values can be converted back into their original scale post-prediction.

## Model Training

All of the steps above are in service of creating a clean and well-organized 
dataset that can be systematically analyzed by a number of different predctive 
methods. The functions here form the heart of the early warning system and 
many of them have previously been released in the `EWStools` package. DEWS provides 
an example of a specific implementation of these more general tools that respects 
the peculiarities and limits of the local data. 

The functions in `DEWS` take care of automating the process of parallelizing model 
fit and model construction by specifying the number of CPUs availalbe in the 
`DEWScontrol` function. The parallelization customizations are platform agnostic 
and should work on any Windows, Mac, or Linux environment with multple CPUs and 
sufficient RAM. 

### Search Possible Algorithms

The `caret` package provides dozens and dozens of potential algorithms for 
prediction purposes. Not all of these algorithms are appropriate for use in the 
DEWS context and not all of them are feasible to fit. Limitations on specific 
algorithms include the size of the data (some algorithms are not efficient with 
very large datasets), the environment the models are run (some algorithms require 
access to external libraries such as a compiler, Java, or other outside software), 
the hardware restrictions (depending on data size upwards of 32GB of RAM may be 
necessary), and data structure (some algorithms do not handle highly correlated 
predictors, repeated measures, or sparse predictor data for rare categories). 
Depending on the work done in the prior steps, the available algorithms will be 
reduced. In Wisconsin's case, approximately 30 algorithms are found to be feasible 
to fit and suitable to the data. 

To conduct a model search, we first run the `DEWScontrol` function which defines 
the control parameters for the search. This function allows the user to decide 
which accuracy metric to optimize, how to cross-validate models, how many CPUs to 
use, whether to upsample the data to improve class balance, and other features. 
Currently `DEWScontrol` defines three specific environments for the Wisconsin 
implementation, but it can be modified to meet any number of parameters. 

The workhorse function of this is `DEWS_search` which is a Wisconsin specific 
wrapper around the `EWStools::modelSearch` function. This function automates the 
process of training an algorithm to the training data, assessing its accuracy on 
the test data, and storing the results for a series of user specified methods 
available in the `caret` package. The result is a dataframe which stores the 
names of each method, its accuracy statistics, and a measure of how long the 
model takes to train. This dataframe is used to calculate the most promising 
models. 

### Select Best Models

There is no "best" model. In the Wisconsin system, models are chosen predominantly 
for their job minimizing their distance from a perfect prediction, but careful 
consideration is also given to the length of time the model takes to run. Instead 
of doing this ad hoc, the user defines a function which identifies models based 
on criteria established by the user. The `constructModels` function takes care 
of this, and then creates ensembled versions of these models and caches that 
to the disk. Currently, the function chooses the N most efficient models, where 
efficiency is the ratio of the model AUC to the square root of its running time. 
Additionally, the N most dissimilar models are chosen, to compare an ensemble 
that is chosen specifically for algorithmic diversity. 


### Ensemble Best Models

The `constructModels` also takes care of `ensembling` the models as well using 
the `caretEnsemble` package. For both the dissimilar and most efficient models 
two specific ensembles are created -- a greedy optimization algorithm and a 
`caretStack` which fits a random forest metamodel to the predictions of each 
of the models. 

Modifying the `constructModel` function allows the user to change the way models 
are selected and to change the behavior for how ensembles are created. 

### Store Best Models

Models must be stored for later scoring. This part requires a plan for storing 
and distinguishing the specific model which will be needed for prediction later. 
The `constructModels` function stores the ensembled models as well as a validation 
data set and the preProcessing object necessary to map predictors back to their 
original scales. This is stored in a binary `.rda` file. This file is named 
programatically based on the date of the run and the cohorts included. 

Alternatives exist and can be implemented by modifying this function. 

## Model Scoring

Model scoring is the point of the whole system -- to take what the models have 
'learned' from the historical cohorts and assess the likeliest outcome for 
students currently in those grades. In Wisconsin we score students based on the 
their most recent available grade level of data, and provide this information 
ahead of the next grade. So we score a 6th grader in September using their 
5th grade data (and consequentially, the 5th grade model). 

It is also important to have a defined output file target in mind. In the DPI 
case we are writing out a `.csv` which has a file specification defined by the 
IT department to make their process of ingesting that data back to the data 
warehouse as seamless as possible. Other more sophisticated and streamlined 
approaches are possible including exposing the model as an API in a webservice 
that can be called, scoring the student records in database, or granting a special 
server instance direct write access to the database and directly outputting results 
to a staging table. 

Any of these approaches will require a similar set of steps. 

### Load Stored Models

The storage of the prediction models is critical and the naming scheme for accessing 
the correct model needs to be clear. 

### Predict Current Students

DEWS includes several functions to extract and reshape data on *current* students 
to ensure that the data is compatible with the stored model objects for the 
purposes of making predictions. The test scores must have the same distribution 
and be on the same scale. Attendance, discipline, and demographic categories and 
values must be on the same scale as well. This can be complex in cases where there 
is a long lag time between the cohort used for building the models and the scoring 
cohort; i.e. the lower the grade level being scored. DEWS includes a few intermediary 
functions to assist in this alignment, but depending on the history of measures 
available in a data warehouse, more rigorous equating and rescaling functions 
may be necessary. 

### Test Predictions

Finally predictions have to be tested to make sure they are ready to be 
integrated into the data warehouse. 

### Export

Store the predictions in a format for integration back into the data warehouse. 