Marine Research Example Workbook
================
Marine Science Program, DBCA
2019-05-22 09:05:52

<!-- README.md is generated from README.Rmd. Please edit that file -->

# Marine Research

<!-- badges: start -->

<!-- badges: end -->

## Purpose

The goal of marine\_research is to keep marine research under version
control, make it transparent, and reproducible.

## Audience

Primarily staff of the Marine Science Program of the Department of
Biodiversity, Conservation and Attractions WA
([DBCA](https://github.com/dbca-wa/)). Open to others conducting marine
research (write permissions on request).

## Structure

Every research project lives in one folder. This example workbook
demonstrates best practices. Every folder could contain any of:

  - RMarkdown workbooks as “stories about code”, e.g.
    
      - the technical appendix to a paper explaining a statistical model
        in detail (rationale, assumptions)
      - the technical appendix to a paper explaining how to access the
        data for the paper
      - a stand-alone example of a nice coding technique

  - RMarkdown workbooks as “stories about data”, e.g.
    
      - the QA/QC for the data behind a paper, demonstrating that the
        data is [correct, consistent, and
        comprehensive](https://en.wikipedia.org/wiki/Data_quality).
      - an [ETL
        script](https://en.wikipedia.org/wiki/Extract,_transform,_load)
        extracting the data from its original source, transforming it,
        and loading it to a data repository

  - RMarkdown workbooks as “stories with data”, e.g.
    
      - the paper itself,
      - a technical report producing some insight from the data,
      - a publication for popular science, e.g. for the DBCA newsletter.

  - R scripts waiting to be transformed into any of the above,

  - subfolders (e.g. “/outputs”) for outputs generated from data
    (figures, tables, maps, spreadsheets) which are subsequently
    uploaded to the data catalogue, but exempt from version control.

# Example

## Workbook config

After creating an RMarkdown workbook, you can fine-tune the header, such
as:

``` r
---
title: "Workbook title"
author: "Author names, Affiliation"
date: "`r Sys.time()`"
always_allow_html: yes
output:
  html_document:
    toc: true
    toc_depth: 3
    toc_float: true
    fig_width: 10
    fig_height: 6
    code_folding: show
    theme: lumen
  pdf_document: default
---
```

Read more about RMarkdown at the [official
docs](https://rmarkdown.rstudio.com/lesson-1.html).

## Setup

Authentication and other secrets are written to R environment variables
in each user’s `~/..Rprofile`. We source this file (if existing) to make
sure any such variables are loaded.

The R package `ckanr` is configured with the `CKAN_URL` and
`CKAN_API_KEY`.

Lastly, all required packages are loaded. This helps others to identify
packages requiring installation. Alternatively, all functions can be
prefixed by the package names (e.g. `dplyr::group_by()`).

``` r
if (file.exists("~/.Rprofile")) source("~/.Rprofile")
ckanr::ckanr_setup(url=Sys.getenv("CKAN_URL"), key=Sys.getenv("CKAN_API_KEY"))
library(magrittr)
library(dplyr)
library(readr)
library(geojsonsf)
library(tibble)
library(DT)
```

## Load data

The data should live on the [DBCA Data
Catalogue](https://data.dpaw.wa.gov.au/) as resources (ideally plain
text formats such as CSV and GeoJSON) underneath one or several
datasets.

The standard procedure to load data from CKAN resources is:

  - Find the CKAN resource ID, probably best by browsing the data
    catalogue.
  - Get the resource’s download URL using `ckanr::resource_show()`.
  - Read the file, e.g. via `readr::read_csv`.
  - Now the data can be previewed, e.g. as `DT::datatable`. Take care to
    preview only a subset (`head()`), else the workbook will load the
    entire tibble / dataframe.

<!-- end list -->

``` r
csv_rid <- "2f7d9386-d1b2-4841-803c-4720d90bfc76"
d <- ckanr::resource_show(csv_rid)$url %>% readr::read_csv(.)
#> Parsed with column specification:
#> cols(
#>   .default = col_double(),
#>   site = col_character(),
#>   zone = col_character()
#> )
#> See spec(...) for full column specifications.
d %>% head(n=10) %>% DT::datatable(.)
```

![](README_files/figure-gfm/load_data_csv-1.png)<!-- -->

``` r
geo_rid <- ""
geo <- ckanr::resource_show(geo_rid)$url %>% geojsonsf::geojson_sf(.)
# geo_tibble <- tibble::tibble(code = geo$ID, eoo = geojsonsf::sfc_geojson(geo$geometry))
```

By the end of this section, all data should be in memory (as a well
named environment variable), and parsed into the correct data types
(e.g. numbers, characters, dates).

## Analyse, visualise, present

This section contains the actual analysis and visualisation of the data.
Depending on scope and purpose of the respective workbook, this section
could create a few time series plots, run a model (or several), show
spatial data on a map, or run any other analysis or visualisation.

### Export a figure

To export a figure for re-use in other documents, we need to write this
figure to a file.

### Show live map

TODO: show our GeoJSON on a leaflet map.

### Export map

TODO: create a snapshot of the map using mapview. TODO: create a
timelapse GIF using gganimate.

## Publish outputs

TODO: upload outputs to data catalogue.
