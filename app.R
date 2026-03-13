# =============================================================================
# SDTM Clinical Trial Dashboard — app.R  (single-file entry point)
# =============================================================================
# Run:  Rscript -e "shiny::runApp('.')"   or   shiny::runApp()
# Install packages first:
#   install.packages(c("shiny","bslib","plotly","DT","dplyr","tidyr","purrr","lubridate"))
# =============================================================================

library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

source("sdtm_data.R")

# ─────────────────────────────────────────────────────────────────────────────
# GLOBALS
# ─────────────────────────────────────────────────────────────────────────────
TRIAL_COLORS <- c("TRIAL-001"="#58a6ff","TRIAL-002"="#3fb950","TRIAL-003"="#e3b341")
SEV_COLORS   <- c(MILD="#3fb950", MODERATE="#e3b341", SEVERE="#f85149")
ARM_COLORS   <- c("Active Treatment"="#58a6ff", Placebo="#e3b341")

LAB_META <- list(
  "Hemoglobin"                = list(lo=12.0, hi=17.5, unit="g/dL"),
  "White Blood Cell Count"    = list(lo=4.0,  hi=11.0, unit="10^9/L"),
  "Platelet Count"            = list(lo=150,  hi=400,  unit="10^9/L"),
  "Alanine Aminotransferase"  = list(lo=7,    hi=56,   unit="U/L"),
  "Aspartate Aminotransferase"= list(lo=10,   hi=40,   unit="U/L"),
  "Total Bilirubin"           = list(lo=0.1,  hi=1.2,  unit="mg/dL"),
  "Creatinine"                = list(lo=0.6,  hi=1.2,  unit="mg/dL"),
  "Glucose"                   = list(lo=70,   hi=100,  unit="mg/dL"),
  "Total Cholesterol"         = list(lo=100,  hi=200,  unit="mg/dL"),
  "Sodium"                    = list(lo=136,  hi=145,  unit="mEq/L")
)

LAB_NAMES  <- names(LAB_META)
VISIT_ORDER <- c("SCREENING","DAY 1","WEEK 2","WEEK 4","WEEK 8",
                 "WEEK 12","WEEK 16","WEEK 24","END OF TREATMENT","FOLLOW-UP")

# Plotly dark theme helper
pt <- function(p, title=NULL, xlab=NULL, ylab=NULL) {
  p %>%
    layout(
      title         = list(text=title, font=list(family="IBM Plex Mono",size=14,color="#e6edf3")),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "#0d1117",
      font          = list(family="IBM Plex Sans", color="#e6edf3", size=12),
      xaxis = list(gridcolor="#21262d", linecolor="#30363d",
                   title=xlab, titlefont=list(color="#8b949e"), tickfont=list(color="#8b949e")),
      yaxis = list(gridcolor="#21262d", linecolor="#30363d",
                   title=ylab, titlefont=list(color="#8b949e"), tickfont=list(color="#8b949e")),
      legend = list(bgcolor="rgba(22,27,34,0.85)", bordercolor="#21262d",
                    borderwidth=1, font=list(color="#e6edf3")),
      margin = list(l=50, r=20, t=50, b=50)
    ) %>%
    config(displayModeBar=FALSE)
}

dtopts <- function(len=12) list(
  pageLength=len, scrollX=TRUE, dom="lftip",
  initComplete=JS("function(s,j){$(this.api().table().header()).css({'background-color':'#1c2128','color':'#8b949e'});}")
)

metric_box <- function(value, label, cls="") {
  div(class=paste("metric-box", cls),
    div(class="metric-value", value),
    div(class="metric-label", label)
  )
}

# Load data once
message("⏳ Generating SDTM synthetic data…")
SDTM <- load_all_data()
DM <- SDTM$DM; AE <- SDTM$AE; LB <- SDTM$LB; EX <- SDTM$EX; SV <- SDTM$SV
message(sprintf("✅ %d subjects | %d AEs | %d labs | %d exposures | %d visits",
  nrow(DM), nrow(AE), nrow(LB), nrow(EX), nrow(SV)))

# ─────────────────────────────────────────────────────────────────────────────
# THEME + CSS
# ─────────────────────────────────────────────────────────────────────────────
sdtm_theme <- bs_theme(
  version=5, bg="#0d1117", fg="#e6edf3",
  primary="#58a6ff", secondary="#8b949e",
  success="#3fb950", warning="#e3b341", danger="#f85149", info="#39c5cf",
  base_font=font_google("IBM Plex Sans"),
  code_font=font_google("IBM Plex Mono"),
  heading_font=font_google("IBM Plex Mono"),
  `border-radius`="8px", `border-color`="#21262d"
)

css <- tags$style(HTML("
  body,.stApp{background-color:#0d1117!important;}
  .navbar,.navbar-brand{background:linear-gradient(135deg,#0d2137 0%,#0a3d62 60%,#1a1a2e 100%)!important;border-bottom:1px solid #1f6feb!important;}
  .navbar-brand{color:#58a6ff!important;font-size:1.1rem!important;font-weight:700!important;font-family:'IBM Plex Mono',monospace!important;}
  .sidebar{background:linear-gradient(180deg,#161b22 0%,#0d1117 100%)!important;border-right:1px solid #21262d!important;}
  .card{background:#161b22!important;border:1px solid #21262d!important;border-radius:10px!important;}
  .card-header{background:#1c2128!important;border-bottom:1px solid #21262d!important;font-family:'IBM Plex Mono',monospace;font-size:.78rem;color:#8b949e;text-transform:uppercase;letter-spacing:1.5px;}
  .metric-box{background:#161b22;border:1px solid #21262d;border-radius:10px;padding:16px 20px;text-align:center;transition:border-color .2s;margin-bottom:12px;}
  .metric-box:hover{border-color:#1f6feb;}
  .metric-value{font-family:'IBM Plex Mono',monospace;font-size:2rem;font-weight:700;color:#58a6ff;line-height:1;}
  .metric-label{font-size:.7rem;color:#8b949e;text-transform:uppercase;letter-spacing:1px;margin-top:6px;}
  .metric-box.danger .metric-value{color:#f85149;}
  .metric-box.success .metric-value{color:#3fb950;}
  .metric-box.warning .metric-value{color:#e3b341;}
  .metric-box.purple  .metric-value{color:#bc8cff;}
  .subj-header{background:#161b22;border:1px solid #21262d;border-radius:10px;padding:18px 24px;margin-bottom:16px;}
  .subj-id{font-family:'IBM Plex Mono',monospace;font-size:1.4rem;color:#58a6ff;font-weight:700;}
  .compare-banner{background:linear-gradient(90deg,#1a3a1a,#0d1117);border:1px solid #3fb950;border-radius:8px;padding:10px 18px;font-family:'IBM Plex Mono',monospace;font-size:.8rem;color:#3fb950;margin-bottom:14px;}
  .nav-tabs .nav-link{color:#8b949e!important;font-size:.85rem;border:1px solid transparent!important;}
  .nav-tabs .nav-link.active{background:#161b22!important;color:#58a6ff!important;border-color:#21262d #21262d #161b22!important;}
  .selectize-input,.form-select,.form-control{background-color:#161b22!important;border-color:#30363d!important;color:#e6edf3!important;}
  .selectize-dropdown{background-color:#161b22!important;border-color:#30363d!important;}
  .selectize-dropdown-content .option{color:#e6edf3!important;}
  .selectize-dropdown-content .option:hover,.selectize-dropdown-content .active{background:#21262d!important;}
  .dataTables_wrapper{color:#e6edf3!important;}
  table.dataTable thead{background:#1c2128!important;}
  table.dataTable thead th{color:#8b949e!important;border-bottom:1px solid #21262d!important;font-size:.78rem;text-transform:uppercase;}
  table.dataTable tbody tr{background:#0d1117!important;}
  table.dataTable tbody tr:hover{background:#161b22!important;}
  table.dataTable tbody td{border-top:1px solid #21262d!important;color:#e6edf3!important;font-size:.82rem;}
  .dataTables_filter input,.dataTables_length select{background:#161b22!important;border-color:#30363d!important;color:#e6edf3!important;}
  .dataTables_info,.dataTables_paginate{color:#8b949e!important;}
  .paginate_button.current{background:#1f6feb!important;color:#fff!important;border-radius:4px!important;}
  ::-webkit-scrollbar{width:6px;height:6px;}
  ::-webkit-scrollbar-track{background:#0d1117;}
  ::-webkit-scrollbar-thumb{background:#30363d;border-radius:3px;}
  ::-webkit-scrollbar-thumb:hover{background:#58a6ff;}
"))

# ─────────────────────────────────────────────────────────────────────────────
# UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title="🧬 SDTM Clinical Dashboard",
  theme=sdtm_theme, header=css, fillable=FALSE,
  bg="#0d2137", window_title="SDTM Dashboard",

  # ── TAB 1: TRIAL OVERVIEW ────────────────────────────────────────────────
  nav_panel("📊 Trial Overview",
    layout_sidebar(fillable=FALSE,
      sidebar=sidebar(width=260, bg="#161b22",
        tags$div(style="color:#8b949e;font-size:.72rem;text-transform:uppercase;letter-spacing:2px;border-bottom:1px solid #21262d;padding-bottom:6px;margin-bottom:12px;","Trial Settings"),
        selectInput("ov_trial","Select Trial",
          choices=setNames(names(TRIALS), map_chr(names(TRIALS), ~paste(.x,"—",TRIALS[[.x]]$name)))),
        selectInput("ov_module","Module",
          choices=c("Demographics","Adverse Events","Laboratory","Exposure","Schedule of Visits")),
        conditionalPanel("input.ov_module=='Laboratory'",
          selectInput("ov_lab","Lab Parameter", choices=LAB_NAMES)),
        conditionalPanel("input.ov_module=='Adverse Events'",
          checkboxGroupInput("ov_ae_sev","Severity Filter",
            choices=c("MILD","MODERATE","SEVERE"), selected=c("MILD","MODERATE","SEVERE")))
      ),
      uiOutput("ov_header"),
      uiOutput("ov_metrics"),
      hr(style="border-color:#21262d;"),
      uiOutput("ov_body")
    )
  ),

  # ── TAB 2: SUBJECT PROFILE ───────────────────────────────────────────────
  nav_panel("👤 Subject Profile",
    layout_sidebar(fillable=FALSE,
      sidebar=sidebar(width=260, bg="#161b22",
        tags$div(style="color:#8b949e;font-size:.72rem;text-transform:uppercase;letter-spacing:2px;border-bottom:1px solid #21262d;padding-bottom:6px;margin-bottom:12px;","Subject Selection"),
        selectInput("sp_trial","Trial",
          choices=setNames(names(TRIALS), map_chr(names(TRIALS), ~paste(.x,"—",TRIALS[[.x]]$name)))),
        selectInput("sp_subject","Subject", choices=NULL),
        selectInput("sp_lab","Lab Parameter", choices=LAB_NAMES)
      ),
      uiOutput("sp_header"),
      uiOutput("sp_metrics"),
      hr(style="border-color:#21262d;"),
      navset_tab(
        nav_panel("🧪 Lab Trends",
          card(card_header("Lab Trend with Normal Range"), plotlyOutput("sp_lab_plot", height="380px")),
          card(card_header("Lab Listing"), DTOutput("sp_lab_tbl"))
        ),
        nav_panel("⚠️ Adverse Events",
          card(card_header("AE Timeline"), plotlyOutput("sp_ae_tl", height="320px")),
          card(card_header("AE Listing"), DTOutput("sp_ae_tbl"))
        ),
        nav_panel("💊 Exposure",
          card(card_header("Dose Administrations"), plotlyOutput("sp_ex_plt", height="320px")),
          card(card_header("Exposure Listing"), DTOutput("sp_ex_tbl"))
        ),
        nav_panel("📅 Visits",
          card(card_header("Visit Completion"), plotlyOutput("sp_sv_plt", height="300px")),
          card(card_header("Visit Listing"), DTOutput("sp_sv_tbl"))
        )
      )
    )
  ),

  # ── TAB 3: CROSS-TRIAL COMPARISON ────────────────────────────────────────
  nav_panel("🔀 Compare Trials",
    layout_sidebar(fillable=FALSE,
      sidebar=sidebar(width=260, bg="#161b22",
        tags$div(style="color:#8b949e;font-size:.72rem;text-transform:uppercase;letter-spacing:2px;border-bottom:1px solid #21262d;padding-bottom:6px;margin-bottom:12px;","Comparison"),
        checkboxGroupInput("cmp_trials","Trials",
          choices=names(TRIALS), selected=names(TRIALS)),
        selectInput("cmp_module","Module",
          choices=c("Demographics","Adverse Events","Laboratory")),
        conditionalPanel("input.cmp_module=='Laboratory'",
          selectInput("cmp_lab","Lab Parameter", choices=LAB_NAMES))
      ),
      uiOutput("cmp_banner"),
      card(card_header("Summary Comparison"), DTOutput("cmp_summary")),
      hr(style="border-color:#21262d;"),
      uiOutput("cmp_plots")
    )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
# SERVER
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ──────────────────────────────────────────────────────────────────────────
  # TRIAL OVERVIEW
  # ──────────────────────────────────────────────────────────────────────────
  ov_dm <- reactive({ DM %>% filter(STUDYID==input$ov_trial) })
  ov_ae <- reactive({ AE %>% filter(STUDYID==input$ov_trial) })
  ov_lb <- reactive({ LB %>% filter(STUDYID==input$ov_trial) %>% mutate(LBSTRESN=as.numeric(LBSTRESN),VISITNUM=as.numeric(VISITNUM)) })
  ov_ex <- reactive({ EX %>% filter(STUDYID==input$ov_trial) })
  ov_sv <- reactive({ SV %>% filter(STUDYID==input$ov_trial) })

  output$ov_header <- renderUI({
    info <- TRIALS[[input$ov_trial]]
    div(class="card", style="margin-bottom:16px;padding:18px 24px;",
      div(style="display:flex;align-items:center;gap:16px;",
        div(style="font-size:2rem;","🧬"),
        div(
          div(style="font-family:'IBM Plex Mono';font-size:1.3rem;color:#58a6ff;font-weight:700;",
            paste(input$ov_trial,"·",info$name)),
          div(style="color:#8b949e;font-size:.82rem;margin-top:4px;",
            paste("Drug:",info$drug,"· N =",info$n))
        )
      )
    )
  })

  output$ov_metrics <- renderUI({
    dm <- ov_dm(); ae <- ov_ae()
    fluidRow(
      column(2, metric_box(nrow(dm),                               "Subjects")),
      column(2, metric_box(nrow(ae),                               "Total AEs")),
      column(2, metric_box(sum(ae$AESER=="Y"),                     "Serious AEs", "danger")),
      column(2, metric_box(round(nrow(ae)/max(nrow(dm),1),1),      "AEs / Subject")),
      column(2, metric_box(round(mean(dm$AGE,na.rm=TRUE),1),       "Mean Age")),
      column(2, metric_box(paste0(round(mean(dm$SEX=="F")*100,1),"%"), "% Female","purple"))
    )
  })

  output$ov_body <- renderUI({
    switch(input$ov_module,
      "Demographics"       = uiOutput("ov_dm_ui"),
      "Adverse Events"     = uiOutput("ov_ae_ui"),
      "Laboratory"         = uiOutput("ov_lb_ui"),
      "Exposure"           = uiOutput("ov_ex_ui"),
      "Schedule of Visits" = uiOutput("ov_sv_ui")
    )
  })

  # Demographics
  output$ov_dm_ui <- renderUI({ tagList(
    fluidRow(
      column(6, card(card_header("Age Distribution"),   plotlyOutput("dm_age",  height="300px"))),
      column(6, card(card_header("Sex Distribution"),   plotlyOutput("dm_sex",  height="300px")))
    ),
    fluidRow(
      column(6, card(card_header("Race Distribution"),  plotlyOutput("dm_race", height="300px"))),
      column(6, card(card_header("Treatment Arms"),     plotlyOutput("dm_arm",  height="300px")))
    ),
    card(card_header("Enrollment by Site"),              plotlyOutput("dm_site", height="270px")),
    card(card_header("Subject Listing"),                 DTOutput("dm_tbl"))
  )})

  output$dm_age  <- renderPlotly({
    dm <- ov_dm()
    p <- plot_ly(dm,x=~AGE,type="histogram",nbinsx=20,
      marker=list(color="#58a6ff",line=list(color="#0d1117",width=1))) %>%
      add_lines(x=c(mean(dm$AGE),mean(dm$AGE)),y=c(0,15),
        line=list(color="#e3b341",dash="dash",width=2),
        name=sprintf("Mean: %.1f",mean(dm$AGE)),showlegend=TRUE)
    pt(p,"Age Distribution","Age (Years)","Count")
  })
  output$dm_sex  <- renderPlotly({
    d <- ov_dm() %>% count(SEX)
    pt(plot_ly(d,labels=~SEX,values=~n,type="pie",hole=.45,
      marker=list(colors=c("#58a6ff","#bc8cff"),line=list(color="#0d1117",width=2)),
      textinfo="percent+label",textfont=list(size=13,color="#e6edf3")),"Sex Distribution")
  })
  output$dm_race <- renderPlotly({
    d <- ov_dm() %>% count(RACE) %>% arrange(n)
    pt(plot_ly(d,x=~n,y=~RACE,type="bar",orientation="h",
      marker=list(color="#39c5cf",line=list(color="#0d1117",width=1))),"Race Distribution","Count","")
  })
  output$dm_arm  <- renderPlotly({
    d <- ov_dm() %>% count(ARM)
    pt(plot_ly(d,labels=~ARM,values=~n,type="pie",hole=.45,
      marker=list(colors=unname(ARM_COLORS[d$ARM]),line=list(color="#0d1117",width=2)),
      textinfo="percent+label",textfont=list(size=13,color="#e6edf3")),"Treatment Arms")
  })
  output$dm_site <- renderPlotly({
    d <- ov_dm() %>% count(SITEID) %>% arrange(desc(n))
    pt(plot_ly(d,x=~SITEID,y=~n,type="bar",
      marker=list(color="#ff9e64",line=list(color="#0d1117",width=1))),"Enrollment by Site","Site","Subjects")
  })
  output$dm_tbl  <- renderDT({
    ov_dm() %>% select(USUBJID,AGE,SEX,RACE,ETHNIC,COUNTRY,ARM,SITEID,RFSTDTC) %>%
      arrange(USUBJID) %>% datatable(options=dtopts(),rownames=FALSE,class="display nowrap")
  })

  # Adverse Events
  output$ov_ae_ui <- renderUI({ tagList(
    fluidRow(
      column(6, card(card_header("AE Severity"),          plotlyOutput("ae_sev",   height="300px"))),
      column(6, card(card_header("System Organ Class"),   plotlyOutput("ae_soc",   height="300px")))
    ),
    card(card_header("Top AE Terms"),                     plotlyOutput("ae_terms", height="280px")),
    card(card_header("Onset Day by Severity"),            plotlyOutput("ae_onset", height="280px")),
    card(card_header("Drug Relatedness"),                 plotlyOutput("ae_rel",   height="260px")),
    card(card_header("AE Listing"),                       DTOutput("ae_tbl"))
  )})

  output$ae_sev   <- renderPlotly({
    d <- ov_ae() %>% count(AESEV)
    pt(plot_ly(d,labels=~AESEV,values=~n,type="pie",hole=.4,
      marker=list(colors=unname(SEV_COLORS[d$AESEV]),line=list(color="#0d1117",width=2)),
      textinfo="percent+label",textfont=list(size=13,color="#e6edf3")),"AE Severity")
  })
  output$ae_soc   <- renderPlotly({
    d <- ov_ae() %>% count(AEBODSYS) %>% arrange(n) %>% tail(10)
    pt(plot_ly(d,x=~n,y=~AEBODSYS,type="bar",orientation="h",
      marker=list(color="#58a6ff",line=list(color="#0d1117",width=1))),"SOC (Top 10)","Count","")
  })
  output$ae_terms <- renderPlotly({
    d <- ov_ae() %>% count(AEDECOD) %>% arrange(desc(n)) %>% head(12)
    pt(plot_ly(d,x=~AEDECOD,y=~n,type="bar",
      marker=list(color="#39c5cf",line=list(color="#0d1117",width=1))),"Top AE Terms","Term","Count") %>%
      layout(xaxis=list(tickangle=-35))
  })
  output$ae_onset <- renderPlotly({
    d <- ov_ae() %>% filter(!is.na(AESTDY)) %>% mutate(AESTDY=as.numeric(AESTDY))
    pt(plot_ly(d,x=~AESTDY,color=~AESEV,type="histogram",nbinsx=30,
      colors=SEV_COLORS,alpha=.85),"AE Onset Distribution","Study Day","Count")
  })
  output$ae_rel   <- renderPlotly({
    d <- ov_ae() %>% count(AEREL)
    pt(plot_ly(d,x=~AEREL,y=~n,type="bar",
      marker=list(color="#e3b341",line=list(color="#0d1117",width=1))),"Drug Relatedness","","Count")
  })
  output$ae_tbl   <- renderDT({
    ae_f <- ov_ae() %>% filter(AESEV %in% input$ov_ae_sev)
    ae_f %>% select(USUBJID,AETERM,AEBODSYS,AESEV,AESER,AEREL,AEOUT,AESTDTC,AEENDTC) %>%
      arrange(USUBJID,AESTDTC) %>% datatable(options=dtopts(),rownames=FALSE,class="display nowrap")
  })

  # Laboratory
  output$ov_lb_ui <- renderUI({ tagList(
    card(card_header("Mean ± SD Over Visits"),   plotlyOutput("lb_trend", height="350px")),
    card(card_header("Abnormal Values (H/L)"),   plotlyOutput("lb_abn",   height="280px")),
    card(card_header("Distribution by Visit"),   plotlyOutput("lb_box",   height="300px")),
    card(card_header("Lab Listing"),              DTOutput("lb_tbl"))
  )})

  ov_plb <- reactive({
    ov_lb() %>% filter(LBTEST==input$ov_lab)
  })
  output$lb_trend <- renderPlotly({
    d <- ov_plb() %>%
      group_by(VISITNUM,VISIT) %>%
      summarise(mu=mean(LBSTRESN,na.rm=TRUE),sd=sd(LBSTRESN,na.rm=TRUE),.groups="drop") %>%
      arrange(VISITNUM)
    d$VISIT <- factor(d$VISIT,levels=VISIT_ORDER[VISIT_ORDER %in% d$VISIT])
    meta <- LAB_META[[input$ov_lab]]
    vr   <- as.character(range(d$VISIT))
    p <- plot_ly(d) %>%
      add_ribbons(x=~VISIT,ymin=~(mu-sd),ymax=~(mu+sd),
        fillcolor="rgba(88,166,255,0.15)",line=list(color="transparent"),name="±1 SD") %>%
      add_lines(x=~VISIT,y=~mu,line=list(color="#58a6ff",width=2.5),name="Mean") %>%
      add_markers(x=~VISIT,y=~mu,marker=list(color="#58a6ff",size=8),showlegend=FALSE) %>%
      add_lines(x=vr,y=c(meta$lo,meta$lo),line=list(color="#f85149",dash="dot",width=1.5),name=paste("LLN:",meta$lo)) %>%
      add_lines(x=vr,y=c(meta$hi,meta$hi),line=list(color="#f85149",dash="dot",width=1.5),name=paste("ULN:",meta$hi))
    pt(p,paste(input$ov_lab,"— Mean ± SD"),"Visit",meta$unit) %>%
      layout(xaxis=list(tickangle=-25))
  })
  output$lb_abn   <- renderPlotly({
    d <- ov_lb() %>% filter(LBNRIND %in% c("H","L")) %>% count(LBTEST,LBNRIND)
    if(nrow(d)==0) return(plotly_empty())
    pt(plot_ly(d,x=~LBTEST,y=~n,color=~LBNRIND,type="bar",
      colors=c(L="#39c5cf",H="#f85149"),barmode="group"),"Abnormal Lab Values","Parameter","Count") %>%
      layout(xaxis=list(tickangle=-30))
  })
  output$lb_box   <- renderPlotly({
    d <- ov_plb()
    d$VISIT <- factor(d$VISIT,levels=VISIT_ORDER[VISIT_ORDER %in% d$VISIT])
    meta <- LAB_META[[input$ov_lab]]
    pt(plot_ly(d,x=~VISIT,y=~LBSTRESN,type="box",
      fillcolor="rgba(188,140,255,0.3)",line=list(color="#bc8cff"),
      marker=list(color="#bc8cff",size=4)) %>% layout(showlegend=FALSE),
      paste(input$ov_lab,"by Visit"),"Visit",meta$unit) %>%
      layout(xaxis=list(tickangle=-25))
  })
  output$lb_tbl   <- renderDT({
    ov_plb() %>%
      select(USUBJID,LBTEST,LBSTRESN,LBSTRESU,LBNRLO,LBNRHI,LBNRIND,VISIT,LBDTC) %>%
      arrange(USUBJID,VISITNUM) %>% datatable(options=dtopts(),rownames=FALSE,class="display nowrap")
  })

  # Exposure
  output$ov_ex_ui <- renderUI({ tagList(
    fluidRow(
      column(6, card(card_header("Cumulative Dose by Arm"), plotlyOutput("ex_cum",  height="300px"))),
      column(6, card(card_header("Dose Count / Subject"),  plotlyOutput("ex_cnt",  height="300px")))
    ),
    card(card_header("Mean Dose Over Study Time"),          plotlyOutput("ex_time", height="280px")),
    card(card_header("Exposure Listing"),                   DTOutput("ex_tbl"))
  )})

  output$ex_cum  <- renderPlotly({
    d <- ov_ex() %>% group_by(USUBJID) %>% summarise(CumDose=sum(EXDOSE,na.rm=TRUE),.groups="drop") %>%
      left_join(ov_dm() %>% select(USUBJID,ARM),by="USUBJID")
    pt(plot_ly(d,x=~CumDose,color=~ARM,type="histogram",colors=ARM_COLORS,nbinsx=20,alpha=.8),
      "Cumulative Dose","Total Dose (mg)","Subjects")
  })
  output$ex_cnt  <- renderPlotly({
    d <- ov_ex() %>% group_by(USUBJID) %>% summarise(n=n(),.groups="drop")
    pt(plot_ly(d,x=~n,type="histogram",nbinsx=15,
      marker=list(color="#39c5cf",line=list(color="#0d1117",width=1))),
      "Dose Count / Subject","Administrations","Subjects")
  })
  output$ex_time <- renderPlotly({
    d <- ov_ex() %>% mutate(EXSTDY=as.numeric(EXSTDY)) %>%
      group_by(EXSTDY) %>% summarise(mu=mean(EXDOSE,na.rm=TRUE),.groups="drop") %>% arrange(EXSTDY)
    pt(plot_ly(d,x=~EXSTDY,y=~mu,type="scatter",mode="lines+markers",
      line=list(color="#3fb950",width=2),marker=list(color="#3fb950",size=6)),
      "Mean Dose Over Time","Study Day","Dose (mg)")
  })
  output$ex_tbl  <- renderDT({
    ov_ex() %>% select(USUBJID,EXTRT,EXDOSE,EXDOSU,EXDOSFRM,EXROUTE,EXSTDTC,VISIT) %>%
      arrange(USUBJID,EXSTDTC) %>% datatable(options=dtopts(),rownames=FALSE,class="display nowrap")
  })

  # Schedule of Visits
  output$ov_sv_ui <- renderUI({ tagList(
    card(card_header("Attendance Heatmap"),  plotlyOutput("sv_heat", height="500px")),
    card(card_header("Completion Rate (%)"), plotlyOutput("sv_comp", height="280px")),
    card(card_header("Visit Listing"),       DTOutput("sv_tbl"))
  )})

  output$sv_heat <- renderPlotly({
    sv <- ov_sv()
    pivot <- sv %>%
      mutate(attended=1L, VISIT=factor(VISIT,levels=VISIT_ORDER)) %>%
      select(USUBJID,VISIT,attended) %>%
      complete(USUBJID,VISIT,fill=list(attended=0L)) %>%
      pivot_wider(names_from=VISIT,values_from=attended,values_fill=0L)
    mat <- as.matrix(pivot[,-1]); rownames(mat) <- pivot$USUBJID
    pt(plot_ly(z=mat,x=colnames(mat),y=rownames(mat),type="heatmap",
      colorscale=list(c(0,"#0d1117"),c(1,"#3fb950")),showscale=FALSE,
      hovertemplate="Subject: %{y}<br>Visit: %{x}<br>Attended: %{z}<extra></extra>"),
      "Subject Visit Attendance")
  })
  output$sv_comp <- renderPlotly({
    total <- nrow(ov_dm())
    d <- ov_sv() %>% group_by(VISIT) %>% summarise(n=n_distinct(USUBJID),.groups="drop") %>%
      mutate(pct=round(n/total*100,1),
             VISIT=factor(VISIT,levels=VISIT_ORDER),
             col=case_when(pct>=85~"#3fb950",pct>=60~"#e3b341",TRUE~"#f85149")) %>%
      arrange(VISIT)
    pt(plot_ly(d,x=~VISIT,y=~pct,type="bar",
      marker=list(color=~col,line=list(color="#0d1117",width=1))),
      "Visit Completion Rate (%)","Visit","% Completed") %>%
      layout(yaxis=list(range=c(0,105)),xaxis=list(tickangle=-20))
  })
  output$sv_tbl  <- renderDT({
    ov_sv() %>% arrange(USUBJID,VISITNUM) %>%
      datatable(options=dtopts(),rownames=FALSE,class="display nowrap")
  })

  # ──────────────────────────────────────────────────────────────────────────
  # SUBJECT PROFILE
  # ──────────────────────────────────────────────────────────────────────────
  observe({
    subjs <- DM %>% filter(STUDYID==input$sp_trial) %>% pull(USUBJID) %>% sort()
    updateSelectInput(session,"sp_subject",choices=subjs,selected=subjs[1])
  })

  sp_dm  <- reactive({ DM %>% filter(USUBJID==input$sp_subject) })
  sp_ae  <- reactive({ AE %>% filter(USUBJID==input$sp_subject) })
  sp_lb  <- reactive({ LB %>% filter(USUBJID==input$sp_subject) %>% mutate(LBSTRESN=as.numeric(LBSTRESN),VISITNUM=as.numeric(VISITNUM)) })
  sp_ex  <- reactive({ EX %>% filter(USUBJID==input$sp_subject) })
  sp_sv  <- reactive({ SV %>% filter(USUBJID==input$sp_subject) })

  output$sp_header <- renderUI({
    req(nrow(sp_dm())>0)
    dm <- sp_dm()[1,]
    ac <- ifelse(dm$ARMCD=="TREATMENT","#3fb950","#e3b341")
    div(class="subj-header",
      div(style="display:flex;gap:32px;flex-wrap:wrap;align-items:center;",
        div(div(class="subj-id",dm$USUBJID),
            div(style="color:#8b949e;font-size:.8rem;margin-top:4px;",paste("Trial:",dm$STUDYID))),
        div(style="display:flex;gap:24px;flex-wrap:wrap;",
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","AGE"),  div(style="color:#e6edf3;font-weight:600;",dm$AGE)),
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","SEX"),  div(style="color:#e6edf3;font-weight:600;",dm$SEX)),
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","RACE"), div(style="color:#e6edf3;font-weight:600;",dm$RACE)),
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","SITE"), div(style="color:#e6edf3;font-weight:600;",dm$SITEID)),
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","ARM"),  div(style=paste0("color:",ac,";font-weight:600;"),dm$ARM)),
          div(div(style="color:#8b949e;font-size:.75rem;text-transform:uppercase;","START"),div(style="color:#e6edf3;font-weight:600;",dm$RFSTDTC))
        )
      )
    )
  })

  output$sp_metrics <- renderUI({
    ae <- sp_ae(); ex <- sp_ex(); sv <- sp_sv(); lb <- sp_lb()
    fluidRow(
      column(2, metric_box(nrow(ae),"Adverse Events")),
      column(2, metric_box(sum(ae$AESER=="Y"),"Serious AEs","danger")),
      column(2, metric_box(nrow(sv),"Visits Done")),
      column(2, metric_box(sum(as.numeric(ex$EXDOSE),na.rm=TRUE),"Total Dose (mg)","success")),
      column(2, metric_box(n_distinct(lb$LBTEST),"Lab Params")),
      column(2, metric_box(sum(lb$LBNRIND %in% c("H","L")),"Abnormal Labs","warning"))
    )
  })

  output$sp_lab_plot <- renderPlotly({
    d <- sp_lb() %>% filter(LBTEST==input$sp_lab) %>% arrange(VISITNUM)
    req(nrow(d)>0)
    meta <- LAB_META[[input$sp_lab]]
    mc   <- ifelse(d$LBNRIND %in% c("H","L"),"#f85149","#58a6ff")
    vr   <- as.character(range(d$VISIT))
    p <- plot_ly(d,x=~VISIT,y=~LBSTRESN) %>%
      add_lines(line=list(color="#58a6ff",width=2),name=input$sp_lab,showlegend=FALSE) %>%
      add_markers(marker=list(color=mc,size=10),
        text=~paste(VISIT,"<br>Value:",round(LBSTRESN,2),LBSTRESU,"<br>Flag:",ifelse(LBNRIND=="","Normal",LBNRIND)),
        hoverinfo="text",showlegend=FALSE) %>%
      add_lines(x=vr,y=c(meta$lo,meta$lo),line=list(color="#f85149",dash="dot",width=1.5),name=paste("LLN:",meta$lo)) %>%
      add_lines(x=vr,y=c(meta$hi,meta$hi),line=list(color="#f85149",dash="dot",width=1.5),name=paste("ULN:",meta$hi))
    pt(p,paste(input$sp_lab,"—",meta$unit),"Visit",meta$unit)
  })
  output$sp_lab_tbl <- renderDT({
    sp_lb() %>% filter(LBTEST==input$sp_lab) %>%
      select(LBTEST,LBSTRESN,LBSTRESU,LBNRLO,LBNRHI,LBNRIND,VISIT,LBDTC) %>%
      arrange(VISITNUM) %>% datatable(options=dtopts(8),rownames=FALSE,class="display nowrap")
  })

  output$sp_ae_tl <- renderPlotly({
    ae <- sp_ae()
    if(nrow(ae)==0) return(pt(plot_ly() %>%
      add_annotations(text="No adverse events",showarrow=FALSE,font=list(color="#8b949e",size=14)),
      "AE Timeline"))
    ae <- ae %>% mutate(start=as.Date(AESTDTC),end=as.Date(AEENDTC),col=SEV_COLORS[AESEV])
    p <- plot_ly()
    for(i in seq_len(nrow(ae))) {
      r <- ae[i,]
      p <- p %>% add_segments(x=~r$start,xend=~r$end,y=~r$AETERM,yend=~r$AETERM,
        line=list(color=r$col,width=14),
        hovertext=paste0("<b>",r$AETERM,"</b><br>Sev: ",r$AESEV,"<br>SAE: ",r$AESER,"<br>",r$AESTDTC,"→",r$AEENDTC),
        hoverinfo="text",showlegend=FALSE)
    }
    pt(p,"AE Timeline","Date","")
  })
  output$sp_ae_tbl <- renderDT({
    sp_ae() %>% select(AETERM,AEBODSYS,AESEV,AESER,AEREL,AEOUT,AESTDTC,AEENDTC) %>%
      datatable(options=dtopts(8),rownames=FALSE,class="display nowrap")
  })

  output$sp_ex_plt <- renderPlotly({
    ex <- sp_ex(); req(nrow(ex)>0)
    pt(plot_ly(ex,x=~EXSTDTC,y=~EXDOSE,type="bar",
      marker=list(color="#3fb950",line=list(color="#0d1117",width=1)),
      hovertext=~paste(VISIT,"<br>Dose:",EXDOSE,EXDOSU),hoverinfo="text"),
      "Dose Administrations","Date","Dose (mg)") %>%
      layout(xaxis=list(tickangle=-30))
  })
  output$sp_ex_tbl <- renderDT({
    sp_ex() %>% select(EXTRT,EXDOSE,EXDOSU,EXDOSFRM,EXROUTE,EXSTDTC,VISIT) %>%
      datatable(options=dtopts(8),rownames=FALSE,class="display nowrap")
  })

  output$sp_sv_plt <- renderPlotly({
    sv <- sp_sv()
    all_v <- data.frame(VISIT=VISIT_ORDER,VO=seq_along(VISIT_ORDER),stringsAsFactors=FALSE)
    mg <- all_v %>% left_join(sv %>% select(VISIT,SVSTDTC),by="VISIT") %>%
      mutate(Status=ifelse(!is.na(SVSTDTC),"Completed","Missing"),
             Col=ifelse(!is.na(SVSTDTC),"#3fb950","#f85149"))
    p <- plot_ly(mg,x=~VO,y=rep(1,nrow(mg)),type="scatter",mode="markers+text",
      marker=list(size=22,color=~Col,line=list(color="#0d1117",width=2)),
      text=~paste0("<br>",VISIT),textposition="bottom center",
      hovertext=~paste0(VISIT,"<br>",ifelse(is.na(SVSTDTC),"—",SVSTDTC),"<br>",Status),
      hoverinfo="text",showlegend=FALSE)
    pt(p,"Visit Completion") %>%
      layout(yaxis=list(showticklabels=FALSE,range=c(0.5,1.9)),
             xaxis=list(showticklabels=FALSE),height=300)
  })
  output$sp_sv_tbl <- renderDT({
    all_v <- data.frame(VISIT=VISIT_ORDER,VO=seq_along(VISIT_ORDER))
    sp_sv() %>% right_join(all_v,by="VISIT") %>%
      mutate(Status=ifelse(!is.na(SVSTDTC),"✅ Completed","❌ Missing")) %>%
      select(VO,VISIT,SVSTDTC,SVENDTC,Status) %>% arrange(VO) %>%
      datatable(options=dtopts(10),rownames=FALSE,class="display nowrap")
  })

  # ──────────────────────────────────────────────────────────────────────────
  # CROSS-TRIAL COMPARISON
  # ──────────────────────────────────────────────────────────────────────────
  cmp_dm  <- reactive({ DM %>% filter(STUDYID %in% input$cmp_trials) })
  cmp_ae  <- reactive({ AE %>% filter(STUDYID %in% input$cmp_trials) })
  cmp_lb  <- reactive({ LB %>% filter(STUDYID %in% input$cmp_trials) %>% mutate(LBSTRESN=as.numeric(LBSTRESN),VISITNUM=as.numeric(VISITNUM)) })
  cmp_plb <- reactive({ cmp_lb() %>% filter(LBTEST==input$cmp_lab) })

  output$cmp_banner <- renderUI({
    req(length(input$cmp_trials)>0)
    div(class="compare-banner", paste("⚡ COMPARING:", paste(input$cmp_trials,collapse=" · ")))
  })

  output$cmp_summary <- renderDT({
    req(length(input$cmp_trials)>0)
    map_dfr(input$cmp_trials, function(tid) {
      tdm <- DM %>% filter(STUDYID==tid); tae <- AE %>% filter(STUDYID==tid)
      tibble(Trial=tid, Name=TRIALS[[tid]]$name, Subjects=nrow(tdm),
        `Mean Age`=round(mean(tdm$AGE),1), `% Female`=round(mean(tdm$SEX=="F")*100,1),
        `Total AEs`=nrow(tae), `AEs/Subject`=round(nrow(tae)/max(nrow(tdm),1),2),
        `Serious AEs`=sum(tae$AESER=="Y"))
    }) %>% datatable(options=list(dom="t",pageLength=10),rownames=FALSE,class="display")
  })

  output$cmp_plots <- renderUI({
    req(length(input$cmp_trials)>0)
    switch(input$cmp_module,
      "Demographics" = tagList(
        fluidRow(
          column(6, card(card_header("Age by Trial"),         plotlyOutput("cmp_age",  height="300px"))),
          column(6, card(card_header("Sex by Trial"),         plotlyOutput("cmp_sex",  height="300px")))
        ),
        card(card_header("Race Comparison (%)"),              plotlyOutput("cmp_race", height="300px")),
        card(card_header("Treatment Arms"),                   plotlyOutput("cmp_arm",  height="280px"))
      ),
      "Adverse Events" = tagList(
        card(card_header("AE Rate by Severity"),              plotlyOutput("cmp_ae_rate", height="320px")),
        card(card_header("Top SOC Comparison"),               plotlyOutput("cmp_ae_soc",  height="300px")),
        card(card_header("Serious AEs by Trial"),             plotlyOutput("cmp_ae_sae",  height="280px"))
      ),
      "Laboratory" = tagList(
        card(card_header("Distribution by Trial"),            plotlyOutput("cmp_lb_box",   height="320px")),
        card(card_header("Mean Trend by Trial"),              plotlyOutput("cmp_lb_trend", height="320px")),
        card(card_header("Abnormal Rate (%)"),                plotlyOutput("cmp_lb_abn",   height="280px"))
      )
    )
  })

  # Demographics
  output$cmp_age  <- renderPlotly({ pt(plot_ly(cmp_dm(),x=~AGE,color=~STUDYID,type="box",colors=TRIAL_COLORS,points="outliers"),"Age by Trial","Trial","Age") })
  output$cmp_sex  <- renderPlotly({
    d <- cmp_dm() %>% count(STUDYID,SEX)
    pt(plot_ly(d,x=~STUDYID,y=~n,color=~SEX,type="bar",colors=c(M="#58a6ff",`F`="#bc8cff"),barmode="group"),"Sex by Trial","Trial","Count")
  })
  output$cmp_race <- renderPlotly({
    tots <- cmp_dm() %>% count(STUDYID,name="Total")
    d <- cmp_dm() %>% count(STUDYID,RACE) %>% left_join(tots,by="STUDYID") %>% mutate(Pct=round(n/Total*100,1))
    pt(plot_ly(d,x=~RACE,y=~Pct,color=~STUDYID,type="bar",colors=TRIAL_COLORS,barmode="group"),"Race (%)","Race","%") %>%
      layout(xaxis=list(tickangle=-25))
  })
  output$cmp_arm  <- renderPlotly({
    d <- cmp_dm() %>% count(STUDYID,ARM)
    pt(plot_ly(d,x=~STUDYID,y=~n,color=~ARM,type="bar",colors=ARM_COLORS,barmode="group"),"Treatment Arms","Trial","Subjects")
  })

  # AE
  output$cmp_ae_rate <- renderPlotly({
    subj_n <- cmp_dm() %>% count(STUDYID,name="N")
    d <- cmp_ae() %>% count(STUDYID,AESEV) %>% left_join(subj_n,by="STUDYID") %>% mutate(Rate=round(n/N,3))
    pt(plot_ly(d,x=~STUDYID,y=~Rate,color=~AESEV,type="bar",colors=SEV_COLORS,barmode="stack"),"AE Rate by Severity","Trial","AEs/Subject")
  })
  output$cmp_ae_soc <- renderPlotly({
    top7 <- AE %>% count(AEBODSYS) %>% arrange(desc(n)) %>% head(7) %>% pull(AEBODSYS)
    d <- cmp_ae() %>% filter(AEBODSYS %in% top7) %>% count(STUDYID,AEBODSYS)
    pt(plot_ly(d,x=~AEBODSYS,y=~n,color=~STUDYID,type="bar",colors=TRIAL_COLORS,barmode="group"),"Top SOC","SOC","Count") %>%
      layout(xaxis=list(tickangle=-25))
  })
  output$cmp_ae_sae <- renderPlotly({
    d <- cmp_ae() %>% filter(AESER=="Y") %>% count(STUDYID)
    pt(plot_ly(d,x=~STUDYID,y=~n,color=~STUDYID,type="bar",colors=TRIAL_COLORS,showlegend=FALSE),"Serious AEs","Trial","Count")
  })

  # Lab
  output$cmp_lb_box <- renderPlotly({
    d <- cmp_plb(); meta <- LAB_META[[input$cmp_lab]]
    p <- plot_ly(d,x=~STUDYID,y=~LBSTRESN,color=~STUDYID,type="box",colors=TRIAL_COLORS,points="outliers")
    pt(p,paste(input$cmp_lab,"by Trial"),"Trial",meta$unit)
  })
  output$cmp_lb_trend <- renderPlotly({
    d <- cmp_plb() %>%
      group_by(STUDYID,VISITNUM,VISIT) %>% summarise(mu=mean(LBSTRESN,na.rm=TRUE),.groups="drop") %>%
      arrange(STUDYID,VISITNUM)
    d$VISIT <- factor(d$VISIT,levels=VISIT_ORDER[VISIT_ORDER %in% d$VISIT])
    meta <- LAB_META[[input$cmp_lab]]
    pt(plot_ly(d,x=~VISIT,y=~mu,color=~STUDYID,type="scatter",mode="lines+markers",
      colors=TRIAL_COLORS,marker=list(size=7)),paste(input$cmp_lab,"Mean Trend"),"Visit",meta$unit) %>%
      layout(xaxis=list(tickangle=-20))
  })
  output$cmp_lb_abn <- renderPlotly({
    d <- map_dfr(input$cmp_trials, function(tid) {
      t <- cmp_plb() %>% filter(STUDYID==tid)
      tibble(Trial=tid,AbnRate=round(sum(t$LBNRIND %in% c("H","L"))/max(nrow(t),1)*100,1))
    })
    pt(plot_ly(d,x=~Trial,y=~AbnRate,color=~Trial,type="bar",colors=TRIAL_COLORS,showlegend=FALSE),
      paste(input$cmp_lab,"Abnormal Rate"),"Trial","% Abnormal")
  })

} # end server

# ─────────────────────────────────────────────────────────────────────────────
shinyApp(ui=ui, server=server)
