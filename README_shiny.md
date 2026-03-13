# 🧬 SDTM Clinical Trial Dashboard — R Shiny

An interactive R Shiny application for exploring CDISC SDTM-formatted clinical trial data,
using a dark clinical theme powered by `bslib` and interactive charts via `plotly`.

---

## Quick Start

### 1. Install R packages

```r
install.packages(c(
  "shiny", "bslib", "plotly", "DT",
  "dplyr", "tidyr", "purrr", "lubridate"
))
```

### 2. Run the app

```r
# From R console (set working directory to the app folder first)
setwd("/path/to/shiny_sdtm")
shiny::runApp("app_final.R")

# Or from terminal
Rscript -e "shiny::runApp('.')"
```

The app will open at **http://127.0.0.1:PORT** in your browser.

---

## Project Structure

```
shiny_sdtm/
├── app_final.R      ← Main app (single file, UI + Server combined)
├── sdtm_data.R      ← SDTM synthetic data generator
└── README.md
```

---

## Features

| Tab | What you get |
|-----|-------------|
| **📊 Trial Overview** | Full module-by-module summaries per trial |
| **👤 Subject Profile** | Per-subject drill-down with lab trends, AE timeline, dose history, visit map |
| **🔀 Compare Trials** | Side-by-side trial comparisons for Demographics, AEs, and Labs |

### Modules

| Module | Visuals |
|--------|---------|
| Demographics | Age histogram · Sex donut · Race bar · ARM donut · Site enrollment |
| Adverse Events | Severity donut · SOC bar · Top terms · Onset histogram · Relatedness · Listing |
| Laboratory | Mean±SD trend with NR lines · Abnormal H/L bar · Box by visit · Listing |
| Exposure | Cumulative dose histogram · Dose count · Mean dose timeline · Listing |
| Schedule of Visits | Attendance heatmap · Completion rate bar · Listing |

---

## Connecting Real SDTM Data

Replace `load_all_data()` in `sdtm_data.R` with your own data loading code.

### Read SAS datasets (`.sas7bdat`)
```r
install.packages("haven")
library(haven)

dm <- read_sas("path/to/sdtm/dm.sas7bdat")
ae <- read_sas("path/to/sdtm/ae.sas7bdat")
lb <- read_sas("path/to/sdtm/lb.sas7bdat")
ex <- read_sas("path/to/sdtm/ex.sas7bdat")
sv <- read_sas("path/to/sdtm/sv.sas7bdat")
```

### Expected SDTM Column Names

| Domain | Key Variables |
|--------|--------------|
| **DM** | STUDYID, USUBJID, AGE, SEX, RACE, ETHNIC, COUNTRY, ARM, ARMCD, SITEID, RFSTDTC, RFENDTC |
| **AE** | STUDYID, USUBJID, AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEREL, AEOUT, AESTDTC, AEENDTC |
| **LB** | STUDYID, USUBJID, LBTEST, LBTESTCD, LBSTRESN, LBSTRESU, LBNRLO, LBNRHI, LBNRIND, VISIT, VISITNUM, LBDTC |
| **EX** | STUDYID, USUBJID, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSTDTC, VISIT |
| **SV** | STUDYID, USUBJID, VISIT, VISITNUM, SVSTDTC, SVENDTC |

---

## Deployment

### shinyapps.io
```r
install.packages("rsconnect")
rsconnect::deployApp("path/to/shiny_sdtm")
```

### Posit Connect / Shiny Server
Copy the folder to the server's Shiny app directory.
