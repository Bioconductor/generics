<!-- README.md is generated from README.Rmd. Please edit that file -->
[![Travis-CI Build Status](https://travis-ci.org/Bioconductor/generics.svg?branch=master)](https://travis-ci.org/Bioconductor/generics) [![Coverage Status](https://img.shields.io/codecov/c/github/Bioconductor/generics/master.svg)](https://codecov.io/github/Bioconductor/generics?branch=master)

Summary of Conflicting Functions
================================

``` r
# Get all conflicting functions from two namespaces
conflicts2 <- function(x, y) {
    Filter(Negate(function(x) grepl("\\.__T__", x)),
           intersect(getNamespaceExports(x), getNamespaceExports(y)))
}

# CRAN packages from https://support.rstudio.com/hc/en-us/articles/201057987-Quick-list-of-useful-R-packages
CRAN <- c(
"base",
"stats",
"graphics",
"RMySQL",
"RSQLite",
"XLConnect",
"xlsx",
"foreign",
"readr",
"haven",
"dplyr",
"tidyr",
"stringr",
"lubridate",
"ggplot2",
"ggvis",
"rgl",
"htmlwidgets",
"leaflet",
"dygraphs",
"DT",
"DiagrammeR",
"networkD3",
"googleVis",
"car",
"mgcv",
"lme4",
"nlme",
"randomForest",
"multcomp",
"vcd",
"glmnet",
"survival",
"caret",
"shiny",
"rmarkdown",
"knitr",
"xtable",
"sp",
"maptools",
"maps",
"ggmap",
"zoo",
"xts",
"quantmod",
"Rcpp",
"data.table",
"parallel",
"XML",
"jsonlite",
"httr",
"rvest",
"devtools",
"testthat",
"roxygen2")

# Bioc Packages from top 25 of https://www.bioconductor.org/packages/stats/
Bioc <- c(
"BiocInstaller",
"BiocGenerics",
"IRanges",
"Biobase",
"AnnotationDbi",
"GenomeInfoDb",
"S4Vectors",
"zlibbioc",
"Biostrings",
"limma",
"XVector",
"GenomicRanges",
"BiocParallel",
"annotate",
"Rsamtools",
"genefilter",
"rtracklayer",
"biomaRt",
"graph",
"GenomicAlignments",
"GenomicFeatures",
"preprocessCore",
"affy",
"BSgenome",
"geneplotter")

suppressPackageStartupMessages(invisible(lapply(c(CRAN, Bioc), library, character.only = TRUE, quietly = TRUE)))
#> Bioconductor version 3.3 (BiocInstaller 1.21.2), ?biocLite for help

all <- expand.grid(CRAN = CRAN, Bioconductor = Bioc, stringsAsFactors = FALSE)

all$conflicts <- Map(conflicts2, all[[1]], all[[2]])

# Filter out all the generics defined in methods

gen_defs <- function(namespace) {
    gens <- getGenerics(where=asNamespace(namespace))

    Map(function(f, package)
        get(f, envir = asNamespace(package), mode = "function"),
        gens@.Data, gens@package)
}

filter_defs <- c(gen_defs("methods"),
                 gen_defs("BiocGenerics"))

in_defs <- function(x, defs) {
    any(as.logical(
        lapply(defs,
            function(xx) {
                identical(xx, x) || .hasSlot(xx, "default") && identical(xx@default@.Data, x)
            })))
}

filter_methods <- function(x, package) {
    if (!length(x)) return(x)
    funs <- lapply(x, get, envir = asNamespace(package), mode = "function")
    in_methods <- vapply(funs, in_defs, logical(1), defs = filter_defs)
    x[!in_methods]
}

all$conflicts <- Map(filter_methods, all$conflicts, all$CRAN)

all$`S3 Generics` <- Map(function(package, funs)
    Filter(function(x) pryr:::is_s3_generic(x, env = asNamespace(package)), funs),
    all$CRAN, all$conflicts)

all$`S4 Generics` <- Map(function(package, funs)
    Filter(function(x) isGeneric(x, where = asNamespace(package)), funs),
    all$CRAN, all$conflicts)

all$normal <- Map(function(conflicts, S3, S4) conflicts[!(conflicts %in% S3 | conflicts %in% S4)],
    all$conflicts, all$`S3 Generics`, all$`S4 Generics`)

res <- all[lengths(all$conflicts) > 0, ]
knitr::kable(res, row.names = FALSE)
```

| CRAN         | Bioconductor      | conflicts                                                                                                                                                                                                                                 | S3 Generics                                                                                    | S4 Generics                         | normal                                                                                                                                                             |
|:-------------|:------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:-----------------------------------------------------------------------------------------------|:------------------------------------|:-------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| base         | BiocGenerics      | setdiff, match, is.unsorted, table, evalq, union, intersect                                                                                                                                                                               |                                                                                                |                                     | setdiff, match, is.unsorted, table, evalq, union, intersect                                                                                                        |
| stats        | BiocGenerics      | mad, xtabs, IQR                                                                                                                                                                                                                           |                                                                                                |                                     | mad, xtabs, IQR                                                                                                                                                    |
| dplyr        | BiocGenerics      | intersect, setdiff, union, combine                                                                                                                                                                                                        | intersect, setdiff, union                                                                      |                                     | combine                                                                                                                                                            |
| lubridate    | BiocGenerics      | intersect, union, setdiff                                                                                                                                                                                                                 |                                                                                                | intersect, union, setdiff           |                                                                                                                                                                    |
| randomForest | BiocGenerics      | combine                                                                                                                                                                                                                                   |                                                                                                |                                     | combine                                                                                                                                                            |
| parallel     | BiocGenerics      | clusterExport, parSapplyLB, clusterApplyLB, parSapply, parApply, clusterMap, clusterApply, clusterEvalQ, clusterCall, parCapply, parRapply, parLapplyLB, parLapply                                                                        |                                                                                                |                                     | clusterExport, parSapplyLB, clusterApplyLB, parSapply, parApply, clusterMap, clusterApply, clusterEvalQ, clusterCall, parCapply, parRapply, parLapplyLB, parLapply |
| base         | IRanges           | t, setdiff, gsub, mean, toupper, merge, chartr, summary, with, match, tolower, diff, by, split\<-, nchar, rev, is.unsorted, sub, table, as.matrix, as.table, which, %in%, within, transform, union, which.min, which.max, drop, intersect | t, mean, merge, summary, with, diff, by, split\<-, rev, as.matrix, as.table, within, transform |                                     | setdiff, gsub, toupper, chartr, match, tolower, nchar, is.unsorted, sub, table, which, %in%, union, which.min, which.max, drop, intersect                          |
| stats        | IRanges           | smoothEnds, mad, runmed, window\<-, cor, cov, quantile, median, update, sd, var, IQR                                                                                                                                                      | window\<-, quantile, median, update                                                            |                                     | smoothEnds, mad, runmed, cor, cov, sd, var, IQR                                                                                                                    |
| RMySQL       | IRanges           | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | IRanges           | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | IRanges           | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | IRanges           | slice, collapse, intersect, setdiff, union, desc                                                                                                                                                                                          | collapse, intersect, setdiff, union                                                            |                                     | slice, desc                                                                                                                                                        |
| tidyr        | IRanges           | expand                                                                                                                                                                                                                                    |                                                                                                |                                     | expand                                                                                                                                                             |
| lubridate    | IRanges           | %within%, intersect, union, setdiff                                                                                                                                                                                                       |                                                                                                | %within%, intersect, union, setdiff |                                                                                                                                                                    |
| nlme         | IRanges           | collapse                                                                                                                                                                                                                                  | collapse                                                                                       |                                     |                                                                                                                                                                    |
| vcd          | IRanges           | tile                                                                                                                                                                                                                                      | tile                                                                                           |                                     |                                                                                                                                                                    |
| sp           | IRanges           | summary, %over%, merge                                                                                                                                                                                                                    |                                                                                                | summary, merge                      | %over%                                                                                                                                                             |
| quantmod     | IRanges           | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| data.table   | IRanges           | shift                                                                                                                                                                                                                                     |                                                                                                |                                     | shift                                                                                                                                                              |
| testthat     | IRanges           | compare                                                                                                                                                                                                                                   | compare                                                                                        |                                     |                                                                                                                                                                    |
| dplyr        | Biobase           | combine                                                                                                                                                                                                                                   |                                                                                                |                                     | combine                                                                                                                                                            |
| randomForest | Biobase           | combine                                                                                                                                                                                                                                   |                                                                                                |                                     | combine                                                                                                                                                            |
| httr         | Biobase           | content                                                                                                                                                                                                                                   |                                                                                                |                                     | content                                                                                                                                                            |
| base         | AnnotationDbi     | summary, ls, sample, eapply, exists                                                                                                                                                                                                       | summary                                                                                        |                                     | ls, sample, eapply, exists                                                                                                                                         |
| RMySQL       | AnnotationDbi     | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | AnnotationDbi     | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | AnnotationDbi     | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | AnnotationDbi     | select                                                                                                                                                                                                                                    |                                                                                                | select                              |                                                                                                                                                                    |
| rmarkdown    | AnnotationDbi     | metadata                                                                                                                                                                                                                                  |                                                                                                |                                     | metadata                                                                                                                                                           |
| sp           | AnnotationDbi     | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| quantmod     | AnnotationDbi     | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| base         | GenomeInfoDb      | merge, summary, intersect                                                                                                                                                                                                                 | merge, summary                                                                                 |                                     | intersect                                                                                                                                                          |
| RMySQL       | GenomeInfoDb      | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | GenomeInfoDb      | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | GenomeInfoDb      | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | GenomeInfoDb      | intersect                                                                                                                                                                                                                                 | intersect                                                                                      |                                     |                                                                                                                                                                    |
| lubridate    | GenomeInfoDb      | intersect                                                                                                                                                                                                                                 |                                                                                                | intersect                           |                                                                                                                                                                    |
| sp           | GenomeInfoDb      | summary, merge                                                                                                                                                                                                                            |                                                                                                | summary, merge                      |                                                                                                                                                                    |
| quantmod     | GenomeInfoDb      | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| base         | S4Vectors         | t, setdiff, merge, substring, substr, split, expand.grid, summary, droplevels, with, match, by, ifelse, as.matrix, levels, as.table, %in%, within, transform, as.factor, union, intersect                                                 | t, merge, split, summary, droplevels, with, by, as.matrix, levels, as.table, within, transform |                                     | setdiff, substring, substr, expand.grid, match, ifelse, %in%, as.factor, union, intersect                                                                          |
| stats        | S4Vectors         | na.exclude, na.omit, complete.cases, window, aggregate, xtabs                                                                                                                                                                             | na.exclude, na.omit, window, aggregate                                                         |                                     | complete.cases, xtabs                                                                                                                                              |
| RMySQL       | S4Vectors         | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | S4Vectors         | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | S4Vectors         | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | S4Vectors         | intersect, setdiff, union, rename                                                                                                                                                                                                         | intersect, setdiff, union                                                                      |                                     | rename                                                                                                                                                             |
| lubridate    | S4Vectors         | intersect, union, setdiff                                                                                                                                                                                                                 |                                                                                                | intersect, union, setdiff           |                                                                                                                                                                    |
| rmarkdown    | S4Vectors         | metadata                                                                                                                                                                                                                                  |                                                                                                |                                     | metadata                                                                                                                                                           |
| sp           | S4Vectors         | summary, split, merge                                                                                                                                                                                                                     |                                                                                                | summary, split, merge               |                                                                                                                                                                    |
| quantmod     | S4Vectors         | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| testthat     | S4Vectors         | compare                                                                                                                                                                                                                                   | compare                                                                                        |                                     |                                                                                                                                                                    |
| base         | Biostrings        | setdiff, chartr, substring, substr, summary, match, nchar, is.unsorted, as.matrix, %in%, toString, union, setequal, intersect                                                                                                             | summary, as.matrix, toString                                                                   |                                     | setdiff, chartr, substring, substr, match, nchar, is.unsorted, %in%, union, setequal, intersect                                                                    |
| RMySQL       | Biostrings        | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | Biostrings        | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | Biostrings        | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | Biostrings        | collapse, intersect, setdiff, setequal, union                                                                                                                                                                                             | collapse, intersect, setdiff, setequal, union                                                  |                                     |                                                                                                                                                                    |
| lubridate    | Biostrings        | intersect, union, setdiff                                                                                                                                                                                                                 |                                                                                                | intersect, union, setdiff           |                                                                                                                                                                    |
| nlme         | Biostrings        | collapse                                                                                                                                                                                                                                  | collapse                                                                                       |                                     |                                                                                                                                                                    |
| sp           | Biostrings        | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| quantmod     | Biostrings        | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| testthat     | Biostrings        | compare                                                                                                                                                                                                                                   | compare                                                                                        |                                     |                                                                                                                                                                    |
| base         | XVector           | match, rev, is.unsorted, toString                                                                                                                                                                                                         | rev, toString                                                                                  |                                     | match, is.unsorted                                                                                                                                                 |
| dplyr        | XVector           | slice                                                                                                                                                                                                                                     |                                                                                                |                                     | slice                                                                                                                                                              |
| testthat     | XVector           | compare                                                                                                                                                                                                                                   | compare                                                                                        |                                     |                                                                                                                                                                    |
| base         | GenomicRanges     | setdiff, merge, split, match, is.unsorted, as.factor, union, intersect                                                                                                                                                                    | merge, split                                                                                   |                                     | setdiff, match, is.unsorted, as.factor, union, intersect                                                                                                           |
| dplyr        | GenomicRanges     | intersect, setdiff, union                                                                                                                                                                                                                 | intersect, setdiff, union                                                                      |                                     |                                                                                                                                                                    |
| lubridate    | GenomicRanges     | intersect, union, setdiff                                                                                                                                                                                                                 |                                                                                                | intersect, union, setdiff           |                                                                                                                                                                    |
| vcd          | GenomicRanges     | tile                                                                                                                                                                                                                                      | tile                                                                                           |                                     |                                                                                                                                                                    |
| sp           | GenomicRanges     | split, merge                                                                                                                                                                                                                              |                                                                                                | split, merge                        |                                                                                                                                                                    |
| data.table   | GenomicRanges     | shift                                                                                                                                                                                                                                     |                                                                                                |                                     | shift                                                                                                                                                              |
| testthat     | GenomicRanges     | compare                                                                                                                                                                                                                                   | compare                                                                                        |                                     |                                                                                                                                                                    |
| base         | Rsamtools         | isOpen, isIncomplete                                                                                                                                                                                                                      |                                                                                                |                                     | isOpen, isIncomplete                                                                                                                                               |
| zoo          | Rsamtools         | index                                                                                                                                                                                                                                     | index                                                                                          |                                     |                                                                                                                                                                    |
| base         | genefilter        | anyNA                                                                                                                                                                                                                                     | anyNA                                                                                          | anyNA                               |                                                                                                                                                                    |
| graphics     | genefilter        | plot                                                                                                                                                                                                                                      | plot                                                                                           |                                     |                                                                                                                                                                    |
| car          | genefilter        | Anova                                                                                                                                                                                                                                     | Anova                                                                                          |                                     |                                                                                                                                                                    |
| sp           | genefilter        | plot                                                                                                                                                                                                                                      |                                                                                                | plot                                |                                                                                                                                                                    |
| base         | rtracklayer       | split, summary, close                                                                                                                                                                                                                     | split, summary, close                                                                          |                                     |                                                                                                                                                                    |
| stats        | rtracklayer       | offset                                                                                                                                                                                                                                    |                                                                                                |                                     | offset                                                                                                                                                             |
| RMySQL       | rtracklayer       | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| RSQLite      | rtracklayer       | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| XLConnect    | rtracklayer       | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| sp           | rtracklayer       | summary, split                                                                                                                                                                                                                            |                                                                                                | summary, split                      |                                                                                                                                                                    |
| quantmod     | rtracklayer       | summary                                                                                                                                                                                                                                   |                                                                                                | summary                             |                                                                                                                                                                    |
| dplyr        | biomaRt           | select                                                                                                                                                                                                                                    |                                                                                                | select                              |                                                                                                                                                                    |
| base         | graph             | union                                                                                                                                                                                                                                     |                                                                                                |                                     | union                                                                                                                                                              |
| graphics     | graph             | plot                                                                                                                                                                                                                                      | plot                                                                                           |                                     |                                                                                                                                                                    |
| dplyr        | graph             | union                                                                                                                                                                                                                                     | union                                                                                          |                                     |                                                                                                                                                                    |
| stringr      | graph             | boundary                                                                                                                                                                                                                                  |                                                                                                |                                     | boundary                                                                                                                                                           |
| lubridate    | graph             | union                                                                                                                                                                                                                                     |                                                                                                | union                               |                                                                                                                                                                    |
| sp           | graph             | plot                                                                                                                                                                                                                                      |                                                                                                | plot                                |                                                                                                                                                                    |
| XML          | graph             | addNode                                                                                                                                                                                                                                   | addNode                                                                                        |                                     |                                                                                                                                                                    |
| dplyr        | GenomicAlignments | first, last                                                                                                                                                                                                                               |                                                                                                |                                     | first, last                                                                                                                                                        |
| xts          | GenomicAlignments | last, first                                                                                                                                                                                                                               | last, first                                                                                    |                                     |                                                                                                                                                                    |
| data.table   | GenomicAlignments | last                                                                                                                                                                                                                                      |                                                                                                |                                     | last                                                                                                                                                               |
| base         | affy              | open, close                                                                                                                                                                                                                               | open, close                                                                                    |                                     |                                                                                                                                                                    |
| graphics     | affy              | hist, barplot                                                                                                                                                                                                                             | hist, barplot                                                                                  |                                     |                                                                                                                                                                    |
| lubridate    | affy              | pm                                                                                                                                                                                                                                        |                                                                                                |                                     | pm                                                                                                                                                                 |
| base         | BSgenome          | nchar                                                                                                                                                                                                                                     |                                                                                                |                                     | nchar                                                                                                                                                              |

Tabulated by function
---------------------

``` r
library(tidyr)
res2 <- unique(unnest(res, conflicts)[c("CRAN", "conflicts")])
res3 <- spread(res2, conflicts, CRAN, fill="")
res4 <- lapply(res3, function(x) x[x != ""])
do.call(`cat`, c(Map(function(nme, pkgs) {
  paste0(" - ", nme, " - ", paste0(pkgs, collapse = ", "))
    }, names(res4), res4, USE.NAMES = FALSE), list(sep = "\n")))
```

-   addNode - XML
-   aggregate - stats
-   Anova - car
-   anyNA - base
-   as.factor - base
-   as.matrix - base
-   as.table - base
-   barplot - graphics
-   boundary - stringr
-   by - base
-   chartr - base
-   close - base
-   clusterApply - parallel
-   clusterApplyLB - parallel
-   clusterCall - parallel
-   clusterEvalQ - parallel
-   clusterExport - parallel
-   clusterMap - parallel
-   collapse - dplyr, nlme
-   combine - dplyr, randomForest
-   compare - testthat
-   complete.cases - stats
-   content - httr
-   cor - stats
-   cov - stats
-   desc - dplyr
-   diff - base
-   drop - base
-   droplevels - base
-   eapply - base
-   evalq - base
-   exists - base
-   expand - tidyr
-   expand.grid - base
-   first - dplyr, xts
-   gsub - base
-   hist - graphics
-   ifelse - base
-   %in% - base
-   index - zoo
-   intersect - base, dplyr, lubridate
-   IQR - stats
-   isIncomplete - base
-   isOpen - base
-   is.unsorted - base
-   last - dplyr, xts, data.table
-   levels - base
-   ls - base
-   mad - stats
-   match - base
-   mean - base
-   median - stats
-   merge - base, sp
-   metadata - rmarkdown
-   na.exclude - stats
-   na.omit - stats
-   nchar - base
-   offset - stats
-   open - base
-   %over% - sp
-   parApply - parallel
-   parCapply - parallel
-   parLapply - parallel
-   parLapplyLB - parallel
-   parRapply - parallel
-   parSapply - parallel
-   parSapplyLB - parallel
-   plot - graphics, sp
-   pm - lubridate
-   quantile - stats
-   rename - dplyr
-   rev - base
-   runmed - stats
-   sample - base
-   sd - stats
-   select - dplyr
-   setdiff - base, dplyr, lubridate
-   setequal - base, dplyr
-   shift - data.table
-   slice - dplyr
-   smoothEnds - stats
-   split - base, sp
-   split\<- - base
-   sub - base
-   substr - base
-   substring - base
-   summary - base, RMySQL, RSQLite, XLConnect, sp, quantmod
-   t - base
-   table - base
-   tile - vcd
-   tolower - base
-   toString - base
-   toupper - base
-   transform - base
-   union - base, dplyr, lubridate
-   update - stats
-   var - stats
-   which - base
-   which.max - base
-   which.min - base
-   window - stats
-   window\<- - stats
-   with - base
-   within - base
-   %within% - lubridate
-   xtabs - stats
