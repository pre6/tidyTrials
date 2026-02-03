# tidyTrials
[![R-CMD-check](https://github.com/pre6/tidyTrials/actions/workflows/r.yml/badge.svg)](https://github.com/pre6/tidyTrials/actions/workflows/r.yml)
[![R-CMD-check](https://github.com/pre6/tidyTrials/actions/workflows/r.yml/badge.svg?branch=main)](https://github.com/pre6/tidyTrials/actions/workflows/r.yml)



(please view our individual notebooks at: https://github.com/pre6/tidyTrials/tree/individual_notebooks/_individual_notebooks)

An R wrapper for the ClinicalTrials.gov API that simplifies access to global clinical trial data.


`tidyTrials` is a lightweight R package that provides programmatic access to the ClinicalTrials.gov API, the world’s largest public registry of clinical studies involving human participants.

The package makes it easy to search, retrieve, and analyze global clinical trial data using simple, tidy R functions. The package is designed for students, researchers, and analysts who want to work with real clinical trial data without dealing with deeply nested JSON.

# Why tidyTrials

ClinicalTrials.gov is maintained by the U.S. National Library of Medicine and contains detailed information on hundreds of thousands of clinical studies worldwide, including:

- Trial titles and descriptions

- Medical conditions and interventions

- Study phases and designs

- Recruitment status

- Enrollment numbers

- Locations and sponsors

- Start and completion dates

However, the official API returns complex, nested JSON that is difficult to explore and analyze directly in R.


**tidyTrials solves this by:**

- providing simple R functions to query the API

- exposing trial metadata in tidy tibbles

- grouping related fields into logical modules

- enabling reproducible, analysis-ready workflows

# Installation
```{r}
# install from GitHub
install.packages("remotes")
remotes::install_github("pre6/tidyTrials")

library(tidytrials)
```

# Quick Start
## 1. Search for Clinical trials
```{r}
result <- trials_fetch(
  query = "asthma",
  max_records = 50
)
tabs <- studies_to_tables_by_module(result$studies)

names(tabs)

tabs$identificationModule |> head()
```

## 2. Search for single trial details

```{r}
study <- get_study("NCT04267848")
study
```
## 3. Search trials via a module

```{r}
result <- trials_fetch(
  query = "asthma",
  max_records = 50
)
tabs <- studies_to_tables_by_module(result$studies, modules = c("identificationModule","designModule"))

tabs$identificationModule |> head()
```


# Core Functionality

This package implements a small but complete client for the ClinicalTrials.gov API.

## 1. Study Discovery & Search

Functions to search keyword in the titles of the clinical trials.

`trials_fetch(query, max_records, phase, country, from_date, to_date,date_filter)`

Search for clinical trials using query,max_records , phase or country.

`studies_to_table(studies_json, modules = NULL)`

Creates a clean 'tidy' table from the json output of the search results with an optional parameter for modules. Modules are a group of fields that are similar (e.g. identificationModule, statusModule etc). You can find a list of modules on the [website](https://biomcp.org/backend-services-reference/04-clinicaltrials-gov/#nct-id-filtering-semantics)




## 2. Trial Retrieval

Functions to retrieve detailed information for specific trials.

`get_study_module_table(nct_id, module = NULL)`

Retrieve full metadata for a single clinical trial using its NCT identifier and input a optional module paramter for certain field groups.

Optionally retrieve specific data modules (group of related fields). Default will be the Identification module (e.g. identificationModule, statusModule)

## 3. Metadata & Field Discovery

Functions to explore what data fields are available in the API.

`get_metadata(nct_id)`

Outputs a list of modules for a specific study and a table of fields, their definitions and which modules they belong to.


# Example Analysis

```{r}
res <- trials_fetch(
  query = "asthma",
  max_records = 50
)

tables <- studies_to_tables_by_module(res$studies)

tables$statusModule |>
  count(overall_status, sort = TRUE)
```


This produces a summary of recruitment status across trials — a common first step in trial landscape analysis.

# Intended Use Cases

- Teaching tidy data principles with real-world biomedical data

- Exploratory analysis of clinical trial landscapes

- Meta-analysis and systematic review preparation

- Coursework projects involving APIs and reproducible research
