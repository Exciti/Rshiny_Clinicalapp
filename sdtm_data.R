# =============================================================================
# SDTM Synthetic Data Generator
# Generates realistic CDISC SDTM-formatted datasets for demo purposes.
# Replace load_all_data() with real .sas7bdat / CSV readers for production.
# =============================================================================

library(dplyr)
library(tibble)
library(lubridate)

set.seed(42)

TRIALS <- list(
  "TRIAL-001" = list(name = "Phase II - Drug A",       drug = "DrugA 10mg",      n = 40),
  "TRIAL-002" = list(name = "Phase III - Drug B",      drug = "DrugB 25mg",      n = 60),
  "TRIAL-003" = list(name = "Phase II - Drug C (Combo)",drug = "DrugC+Placebo",  n = 35)
)

SITES     <- paste0("Site 00", 1:5)
RACES     <- c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","AMERICAN INDIAN OR ALASKA NATIVE","OTHER")
RACE_W    <- c(0.55, 0.20, 0.15, 0.05, 0.05)
ETHNICITIES <- c("NOT HISPANIC OR LATINO","HISPANIC OR LATINO")
COUNTRIES <- c("USA","GBR","DEU","FRA","CAN")

AE_TERMS <- list(
  list(term="Nausea",      soc="GASTROINTESTINAL DISORDERS",          sevs=c("MILD","MODERATE","SEVERE")),
  list(term="Headache",    soc="NERVOUS SYSTEM DISORDERS",            sevs=c("MILD","MODERATE")),
  list(term="Fatigue",     soc="GENERAL DISORDERS",                   sevs=c("MILD","MODERATE","SEVERE")),
  list(term="Dizziness",   soc="NERVOUS SYSTEM DISORDERS",            sevs=c("MILD","MODERATE")),
  list(term="Vomiting",    soc="GASTROINTESTINAL DISORDERS",          sevs=c("MILD","MODERATE","SEVERE")),
  list(term="Rash",        soc="SKIN AND SUBCUTANEOUS TISSUE DISORDERS", sevs=c("MILD","MODERATE")),
  list(term="Insomnia",    soc="PSYCHIATRIC DISORDERS",               sevs=c("MILD","MODERATE")),
  list(term="Diarrhea",    soc="GASTROINTESTINAL DISORDERS",          sevs=c("MILD","MODERATE","SEVERE")),
  list(term="Back Pain",   soc="MUSCULOSKELETAL DISORDERS",           sevs=c("MILD","MODERATE")),
  list(term="Cough",       soc="RESPIRATORY DISORDERS",               sevs=c("MILD","MODERATE")),
  list(term="Hypertension",soc="CARDIAC DISORDERS",                   sevs=c("MODERATE","SEVERE")),
  list(term="ALT Increased",soc="INVESTIGATIONS",                     sevs=c("MILD","MODERATE","SEVERE"))
)

LAB_PARAMS <- list(
  list(testcd="HGB",   test="Hemoglobin",             unit="g/dL",    lo=12.0, hi=17.5, mu=13.5, sd=1.5),
  list(testcd="WBC",   test="White Blood Cell Count", unit="10^9/L",  lo=4.0,  hi=11.0, mu=7.0,  sd=1.8),
  list(testcd="PLT",   test="Platelet Count",         unit="10^9/L",  lo=150,  hi=400,  mu=250,  sd=50),
  list(testcd="ALT",   test="Alanine Aminotransferase",unit="U/L",    lo=7,    hi=56,   mu=25,   sd=12),
  list(testcd="AST",   test="Aspartate Aminotransferase",unit="U/L",  lo=10,   hi=40,   mu=22,   sd=8),
  list(testcd="BILI",  test="Total Bilirubin",        unit="mg/dL",   lo=0.1,  hi=1.2,  mu=0.6,  sd=0.25),
  list(testcd="CREAT", test="Creatinine",             unit="mg/dL",   lo=0.6,  hi=1.2,  mu=0.9,  sd=0.15),
  list(testcd="GLUC",  test="Glucose",                unit="mg/dL",   lo=70,   hi=100,  mu=85,   sd=12),
  list(testcd="CHOL",  test="Total Cholesterol",      unit="mg/dL",   lo=100,  hi=200,  mu=160,  sd=30),
  list(testcd="SODIUM",test="Sodium",                 unit="mEq/L",   lo=136,  hi=145,  mu=140,  sd=2)
)

VISITS     <- c("SCREENING","DAY 1","WEEK 2","WEEK 4","WEEK 8","WEEK 12","WEEK 16","WEEK 24","END OF TREATMENT","FOLLOW-UP")
VISIT_DAYS <- c(-14, 1, 15, 29, 57, 85, 113, 169, 197, 225)

# ── Demographics ──────────────────────────────────────────────────────────────
generate_dm <- function(studyid, n) {
  base_date <- as.Date("2022-01-15")
  purrr::map_dfr(seq_len(n), function(i) {
    usubjid  <- sprintf("%s-%04d", studyid, i)
    age      <- max(18L, min(85L, as.integer(round(rnorm(1, 52, 12)))))
    sex      <- sample(c("M","F"), 1)
    race     <- sample(RACES, 1, prob=RACE_W)
    ethnic   <- sample(ETHNICITIES, 1, prob=c(0.75, 0.25))
    rfstdtc  <- base_date + sample(0:180, 1)
    arm      <- sample(c("TREATMENT","PLACEBO"), 1, prob=c(0.67, 0.33))
    tibble(
      STUDYID=studyid, DOMAIN="DM", USUBJID=usubjid, SUBJID=sprintf("%04d",i),
      SITEID=sample(SITES,1), AGE=age, AGEU="YEARS", SEX=sex,
      RACE=race, ETHNIC=ethnic, COUNTRY=sample(COUNTRIES,1),
      ARMCD=arm, ARM=ifelse(arm=="TREATMENT","Active Treatment","Placebo"),
      RFSTDTC=format(rfstdtc), RFENDTC=format(rfstdtc+sample(150:225,1)),
      DTHFL=ifelse(runif(1)<0.02,"Y","")
    )
  })
}

# ── Adverse Events ────────────────────────────────────────────────────────────
generate_ae <- function(dm, studyid) {
  purrr::map_dfr(seq_len(nrow(dm)), function(idx) {
    row      <- dm[idx,]
    usubjid  <- row$USUBJID
    rfstdtc  <- as.Date(row$RFSTDTC)
    n_ae     <- rpois(1, 3)
    if (n_ae == 0) return(tibble())
    selected <- sample(AE_TERMS, min(n_ae, length(AE_TERMS)))
    purrr::imap_dfr(selected, function(ae, seq) {
      onset_day <- sample(1:180, 1)
      duration  <- sample(1:30, 1)
      onset_dt  <- rfstdtc + onset_day
      end_dt    <- onset_dt + duration
      severity  <- sample(ae$sevs, 1)
      serious   <- ifelse(severity=="SEVERE" && runif(1)<0.4, "Y", "N")
      tibble(
        STUDYID=studyid, DOMAIN="AE", USUBJID=usubjid, AESEQ=seq,
        AETERM=toupper(ae$term), AEDECOD=toupper(ae$term), AEBODSYS=ae$soc,
        AESEV=severity, AESER=serious,
        AEREL=sample(c("RELATED","NOT RELATED","POSSIBLY RELATED"),1),
        AEOUT=sample(c("RECOVERED/RESOLVED","RECOVERING/RESOLVING","NOT RECOVERED/NOT RESOLVED"),1),
        AESTDTC=format(onset_dt), AEENDTC=format(end_dt),
        AESTDY=onset_day, AEENDY=onset_day+duration,
        AEACN=sample(c("DOSE NOT CHANGED","DOSE REDUCED","DRUG WITHDRAWN","NOT APPLICABLE"),1)
      )
    })
  })
}

# ── Laboratory ────────────────────────────────────────────────────────────────
generate_lb <- function(dm, studyid) {
  purrr::map_dfr(seq_len(nrow(dm)), function(idx) {
    row     <- dm[idx,]
    usubjid <- row$USUBJID
    rfstdtc <- as.Date(row$RFSTDTC)
    purrr::map_dfr(seq_along(VISITS[-length(VISITS)]), function(vi) {
      if (runif(1) > 0.85) return(tibble())
      visit     <- VISITS[vi]; vday <- VISIT_DAYS[vi]
      visit_dt  <- rfstdtc + vday
      purrr::map_dfr(LAB_PARAMS, function(p) {
        drift <- (vi/length(VISITS)) * runif(1, -0.05, 0.15)
        value <- round(max(p$lo*0.5, rnorm(1, p$mu*(1+drift), p$sd)), 2)
        abnorm <- dplyr::case_when(value < p$lo ~ "L", value > p$hi ~ "H", TRUE ~ "")
        tibble(
          STUDYID=studyid, DOMAIN="LB", USUBJID=usubjid,
          LBTEST=p$test, LBTESTCD=p$testcd,
          LBORRES=as.character(value), LBORRESU=p$unit,
          LBSTRESN=value, LBSTRESU=p$unit,
          LBNRLO=p$lo, LBNRHI=p$hi, LBNRIND=abnorm,
          VISIT=visit, VISITNUM=vi, LBDTC=format(visit_dt), LBDY=vday
        )
      })
    })
  })
}

# ── Exposure ──────────────────────────────────────────────────────────────────
generate_ex <- function(dm, studyid) {
  drug_name <- TRIALS[[studyid]]$drug
  purrr::map_dfr(seq_len(nrow(dm)), function(idx) {
    row     <- dm[idx,]
    usubjid <- row$USUBJID
    rfstdtc <- as.Date(row$RFSTDTC)
    arm     <- row$ARMCD
    drug    <- ifelse(arm=="TREATMENT", drug_name, "PLACEBO")
    dose    <- ifelse(arm=="TREATMENT", 100, 0)
    purrr::map_dfr(seq(0, 22, by=2), function(week) {
      if (runif(1) < 0.05) return(tibble())
      actual_dose <- ifelse(runif(1)>0.03, dose, dose*0.5)
      dose_date   <- rfstdtc + week*7 + 1
      tibble(
        STUDYID=studyid, DOMAIN="EX", USUBJID=usubjid,
        EXTRT=drug, EXDOSE=actual_dose, EXDOSU="mg",
        EXDOSFRM="TABLET", EXDOSFRQ="EVERY 2 WEEKS", EXROUTE="ORAL",
        EXSTDTC=format(dose_date), EXENDTC=format(dose_date),
        EXSTDY=week*7+1, VISIT=ifelse(week==0,"DAY 1",paste0("WEEK ",week))
      )
    })
  })
}

# ── Schedule of Visits ────────────────────────────────────────────────────────
generate_sv <- function(dm, studyid) {
  purrr::map_dfr(seq_len(nrow(dm)), function(idx) {
    row     <- dm[idx,]
    usubjid <- row$USUBJID
    rfstdtc <- as.Date(row$RFSTDTC)
    purrr::map_dfr(seq_along(VISITS), function(vi) {
      if (vi > 7 && runif(1) < 0.08) return(tibble())
      visit_dt <- rfstdtc + VISIT_DAYS[vi] + sample(-2:3, 1)
      tibble(
        STUDYID=studyid, DOMAIN="SV", USUBJID=usubjid,
        VISITNUM=vi, VISIT=VISITS[vi],
        SVSTDTC=format(visit_dt), SVENDTC=format(visit_dt),
        SVSTDY=VISIT_DAYS[vi]
      )
    })
  })
}

# ── Load All Data ─────────────────────────────────────────────────────────────
load_all_data <- function() {
  dm_list <- ae_list <- lb_list <- ex_list <- sv_list <- list()
  for (studyid in names(TRIALS)) {
    dm <- generate_dm(studyid, TRIALS[[studyid]]$n)
    dm_list[[studyid]] <- dm
    ae_list[[studyid]] <- generate_ae(dm, studyid)
    lb_list[[studyid]] <- generate_lb(dm, studyid)
    ex_list[[studyid]] <- generate_ex(dm, studyid)
    sv_list[[studyid]] <- generate_sv(dm, studyid)
  }
  list(
    DM = bind_rows(dm_list),
    AE = bind_rows(ae_list),
    LB = bind_rows(lb_list),
    EX = bind_rows(ex_list),
    SV = bind_rows(sv_list)
  )
}
