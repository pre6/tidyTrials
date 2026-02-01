# tidyTrials
An R wrapper for the ClinicalTrials.gov API that simplifies access to global clinical trial data.


`clinicaltrialsR` is a lightweight R package that provides programmatic access to the ClinicalTrials.gov API, the world’s largest public registry of clinical studies involving human participants.

The package makes it easy to search, retrieve, and analyze global clinical trial data using simple, tidy R functions.

# Project Overview

ClinicalTrials.gov is maintained by the U.S. National Library of Medicine and contains detailed information on hundreds of thousands of clinical studies worldwide, including:

- Trial titles and descriptions

- Medical conditions and interventions

- Study phases and designs

- Recruitment status

- Enrollment numbers

- Locations and sponsors

- Start and completion dates

The goal of this project is to build a clean API wrapper that allows researchers, students, and analysts to easily explore and analyze this data directly from R.



# What This Package Will Do

This package implements a small but complete client for the ClinicalTrials.gov API.

## 1. Study Discovery & Search

Functions to search keyword in the titles of the clinical trials.

`trials_run_spec(query, max_records, phase, country, from_date, to_date,date_filter)`

Search for clinical trials using query,max_records , phase or country.

`studies_to_table(studies_json, modules = NULL)`

Creates a clean 'tidy' table from the json output of the search results with an optional parameter for modules. Modules are a group of fields that are similar (e.g. identificationModule, statusModule etc). You can find a list of modules on the [website](https://biomcp.org/backend-services-reference/04-clinicaltrials-gov/#nct-id-filtering-semantics)




## 2. Trial Retrieval

Functions to retrieve detailed information for specific trials.

`ct_get_study(nct_id, module = NULL)`

Retrieve full metadata for a single clinical trial using its NCT identifier and input a optional module paramter for certain field groups.

Optionally retrieve specific data modules (group of related fields). Default will be the Identification module (e.g. identificationModule, statusModule)

## 3. Metadata & Field Discovery

Functions to explore what data fields are available in the API.

`ct_metadata(nct_id)`

Outputs a list of modules for a specific study and a table of fields, their definitions and which modules they belong to.





