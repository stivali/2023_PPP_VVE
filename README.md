# Value of a Statistical Life: A Review of Empirical Evidence for Brazil

**Replication materials for:** Stivali (2023), *Valor de uma vida estatística: uma revisão da literatura empírica para o Brasil*, published in *Pesquisa e Planejamento Econômico*, issue 66.

**DOI:** https://doi.org/10.38116/ppp66art7

## Overview

This repository contains the R script and supporting spreadsheet used to produce calculations and figures for a review of empirical estimates of the value of a statistical life (VSL) in Brazil.

The analysis includes a synthesis of estimates reported in the literature and fixed- and random-effects meta-analytic calculations, with forest plots illustrating the results.

The code is provided to document the computational steps associated with the published paper. Comments and figure labels in the R script are primarily in Portuguese.

## Repository structure

```text
2023_PPP_VVE/
├── R/
│   └── analise_vve.R
├── data/
│   └── vve_calculos.xlsx
├── Stivali (2023) - Valor de uma Vida Estatística.pdf
├── 2023_PPP_VVE-main.Rproj
├── .gitignore
└── README.md
```

The script writes figures to `outputs/figures/`. This output directory is excluded from version control.

## Requirements

- R and, optionally, RStudio
- R packages: `tidyverse`, `readxl`, `here`, `metafor`, `PRISMAstatement`, `rsvg`, and `DiagrammeRsvg`
- `DiagrammeR` and `knitr` are also required by functions called in the script.

Install the dependencies, if needed:

```r
install.packages(c(
  "tidyverse", "readxl", "here", "metafor",
  "PRISMAstatement", "rsvg", "DiagrammeRsvg",
  "DiagrammeR", "knitr"
))
```

## Running the analysis

1. Clone or download the repository.
2. Open `2023_PPP_VVE-main.Rproj` in RStudio (or otherwise set the repository as the project root).
3. Create the output directory if it does not already exist:

   ```r
   dir.create(here::here("outputs", "figures"),
              recursive = TRUE, showWarnings = FALSE)
   ```

4. Run the analysis:

   ```r
   source(here::here("R", "analise_vve.R"), encoding = "UTF-8")
   ```

The script reads `data/vve_calculos.xlsx` and generates the indifference-curve illustration, a PRISMA flow diagram, and forest plots for fixed- and random-effects analyses. Figures are saved in formats including EPS, PDF, and SVG.

**Reproducibility note:** The code has not been independently validated in a clean R environment as part of preparing this README. Output-device support (particularly EPS/SVG) and the behavior of package functions may depend on the installed R environment and package versions.

## Methodological note

The random-effects calculation in the published analysis supplies a value of `tau2` based on the sample variance of the VSL estimates in the supporting spreadsheet.

The original script explains that automatic estimation with `metafor` returned a zero between-study variance and that the explicit value was used to align results with earlier spreadsheet calculations.

**This is a study-specific modeling choice, not the standard default estimator of between-study variance in meta-analysis.** The code retains that choice to document the published calculations; users should consult the paper and the script before interpreting or modifying it.

## Citation

Stivali (2023). *Valor de uma vida estatística: uma revisão da literatura empírica para o Brasil*. *Pesquisa e Planejamento Econômico*, issue 66.

https://doi.org/10.38116/ppp66art7

If you use these materials, please cite the published article.

## Questions and reuse

The materials are shared for research transparency and reproducibility. No software license is specified in this repository; publication on GitHub alone does not grant a general license to reuse the code.

For questions about the calculations, please open an issue in this repository.
