# solesR package

This is our open source R package for the development of Systematic Online Living Evidence Summaries (SOLES). The package includes functions for:
- automating systematic searches
- deduplication
- downloading full texts
- using regular expressions
- pulling in additional metadata from openalex
- measuring the reporting of measures to reduce bias
- assessing code / data sharing
- working with data from SyRF

Read more about SOLES from our [publication in 2023](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10220429/) 
Find out more about the different [SOLES platforms](https://camaradesuk.github.io/soles-projects/)

# Installation

```{r}
library(devtools)
install_github("camaradesuk/solesR")
library(solesR)
```

Refer to Wiki pages for detailed guidance on how to set up a SOLES project

# Packages and libraries used by solesR
We want to aknowledge the software that has made our work here possible, including: 
- ODDPUB: https://github.com/quest-bih/oddpub - [paper](https://datascience.codata.org/articles/10.5334/dsj-2020-042)
- Pre-ROb: https://github.com/camaradesuk/pre-rob - [paper](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9298308/)
- openalexR: https://docs.ropensci.org/openalexR/ - [paper](https://journal.r-project.org/articles/RJ-2023-089/)
- AutoAnnotation: https://github.com/camaradesuk/AutoAnnotation 
- ASySD: https://github.com/camaradesuk/ASySD - [paper](https://bmcbiol.biomedcentral.com/articles/10.1186/s12915-023-01686-z)
- rcrossref: https://github.com/ropensci/rcrossref
- roadoi: https://github.com/ropensci/roadoi
- ScopusAPI: https://github.com/christopherBelter/scopusAPI
- rwos: https://github.com/juba/rwos
- RISMed: https://github.com/kaitlynhair/RISmed
