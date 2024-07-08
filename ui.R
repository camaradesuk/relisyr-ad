shinyUI(fluidPage(
  tags$head(
    tags$style(
      "
      .inlineText * {
      display: inline;
      }
      .buff * {
      padding:50px 5px 30px 5px}
      "
    )
  ),
  theme = shinytheme("flatly"),
  navbarPage(
    "ReLiSyR-AD",
    tabPanel(
      "Home"
      ,
      column(
        3,
        selectInput(inputId = "candidateCategory", "Select type of candidate drugs", c("All Candidates in included publications" = "all", 
                                                                                       "Candidates meeting ReLiSyR-AD logic" = "logicOnly"
                                                                                        , 
                                                                                        "Longlisted candidates only" = "longlist"
        )),
        
        br(),hr(),
        downloadButton("DownloadFilteredPublications", label = "Clinical publications for filtered drugs", class = "btn-info"),
        br(),br(),
        downloadButton("DownloadSelectedPublications", label = "Clinical publications for selected drugs", class = "btn-primary"),
        hr(),
        # downloadButton("DownloadFilteredPublicationsinvivo", label = "in vivo publications for filtered drugs", class = "btn-info"), 
        # br(),br(),
        # downloadButton("DownloadSelectedPublicationsinvivo", label = "in vivo publications for selected drugs", class = "btn-primary"),  
        br(),br(),
        tags$p(textOutput("lastupdatetime"))
      )
      ,
      column(
        9
        ,
        tabsetPanel(type = "tabs",
                    tabPanel("Drug Disease Cross Table",   DT::dataTableOutput("frequencyCrossTable") %>% withSpinner(type = 5, color = "#2c3e50")),
                    tabPanel("Study List",   DT::dataTableOutput("studyTable") %>% withSpinner(type = 5, color = "#2c3e50"))
        )
      )),
    tabPanel(
      "About",
      h3("The project"),
      p("Repurposing Living Systematic Review (ReLiSyR) is a machine learning assisted crowdsourced living systematic review of neurodegenerative diseases with the aim of informing identification and prioritisation of repurposing candidates to be evaluated in clinical trials. In this app, we summarise drugs identified from clinical studies of Alzheimer's disease, Frontotemporal dementia, Huntington's disease, motor neuron disease, multiple sclerosis, and Parkinson's disease. We deem drugs to meet the AD logic if they have been described in at least 1 clinical study in Alzheimer's disease, or described in clinical studies in at least two other diseases of interest. We have previously developed ReLiSyR-MND to inform drug selection for MND-SMART, a multi-arm multi-stage trial for motor neuron disease. We have adapted this here for Alzheimer's disease. "), 
      
      h4("ReLiSyR-MND Methodology"),
      p("Our methodology is detailed in our",
        tags$a(href="https://mfr.de-1.osf.io/render?url=https://osf.io/5jp43/?direct%26mode=render%26action=download%26mode=render", "protocol."),
        "We adopted a systematic approach of evaluating drug candidates which we have previously used to guide drug selection for the Multple Sclerosis Secondary Progressive Multi-Arm Randomisation Trial (MS-SMART) a  multi-arm phase IIb randomised controlled trial comparing the efficacy of three neuroprotective drugs in secondary progressive multiple sclerosis. These principles of drug selection were published by",
        tags$a(href="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0117705","Vesterinen et al."),
        "in 2015."),
      p("This approach, which adopts a structured, systematic method combined with independent expert(s) evaluation, was designed to identify candidate drugs for evaluation in clinical trials for people with neurodegenerative diseases, including MND, on account of the compelling evidence for shared dysregulated pathways and processes across neurodegenerative disorders. 
                     Critically, the structured evaluation takes into account not only biological plausibility and efficacy but also safety and quality of previous studies. This includes adopting benchmark practice such as Delphi and PICOS framework."),
      p("1.", strong("Living Search"), 
        ": We use the",
        tags$a(href="https://syrf.org.uk","Systematic Review Facility (SyRF) platform"),
        ", taking as its starting point automatic updating of the PubMed search."),
      p("2.", strong("Citation Screening"),
        ": Using a machine learning algorithm which has been trained and validated using human decisions, publications are screened for inclusion based on title and abstract."), 
      p("3.", strong("Filtering drugs by inclusion logic"),
        ": Text mining approaches (Regular Expressions deployed in R and taking as source material title and abstract) are used to identify disease and drug studied. A second algorithm is used to identify drugs which have been tested in at least one clinical study in MND; or have been tested clinically in two of the other specified conditions."),
      p("4.", strong("Longlisting by trial investigators"),
        ": Trial investigators reviewed the drugs filtered, excluding drugs which met the following critera: (i) previously considered unsuitable by expert panel due to lack of biological plausibility, drugs with unfavourable safety profiles in MND patients and drugs tested more than 3 times in MND population; (ii) drugs available over-the-counter as these may affect trial integrity; (iii) compounds which are not feasible for the next arms due to supply issues, such as compopunds not listed in the current version of the British National Formulary; (iv) drugs without oral preparations; and (v) drugs that are deemed by investigators to be unsafe/inappropriate for clinical trial in the current setting."),
      p("5.", strong("Data extraction"), 
        ": Our team of reviewers extract data specified in our protocol on the",
        tags$a(href="https://syrf.org.uk", "SyRF platform"),
        "from all included publications for longlisted drugs. Each publication will be annotated by at least two reviewers, with any differences reconciled by a third reviewer."),
      p("6.", strong("Data Analysis"),
        ": We will analyse the results as follows:",
        tags$ul(
          tags$li("Clinical review:","For each publication, we calculated a",
                  # includeHTML("Drug-Scoring-Method.html"),
                  tags$a(href = "https://mfr.de-1.osf.io/render?url=https://osf.io/8k4h2/?direct%26mode=render%26action=download%26mode=render", "distance score"),
                  "based on Euclidean distance of efficacy and safety scores weighted by quality and study size. For each drug, we calculate a drug score using the number of publications describing the drug (n) and median publication distance score for all publications describing data for that drug:", withMathJax("$$\\text{drug score}\\ = log10{(n+1)} \\times {(\\text{median distance score})}$$"),
                  
                  "Separately, we will calculate median subscores for efficacy, safety, quality and study size across all publications for each drug."),
          tags$li("Animal invivo review and in vitro review: An individual meta‐analysis will be carried out for each intervention identified. We will summarise the effects of interventions where there are 3 or more publications in which that intervention has been tested reporting findings from at least 5 experiments. Depending on the nature of the outcomes reported we will use either standardised mean difference (SMD) or normalised mean difference (NMD) random effects meta-analysis with REML estimates of tau. Specifically, if fewer than 70% of outcomes are suitable for NMD analysis we will use SMD. Differences between groups of studies will be identified using meta-regression."
          )
        )),
    
    h3("About us"),
    p("The", tags$a(href="https://www.ed.ac.uk/clinical-brain-sciences/research/camarades/", "CAMARADES"), "(Collaborative Approach to Meta-Analysis and Review of 
                     Animal Data from Experimental Studies) group specialise in performing", strong("systematic review and meta-analysis"), "of data
                     from experimental studies. Our interests range from identifying potential sources of bias in in vivo and in vitro studies; 
                     developing automation tools for evidence synthesis; developing recommendations for improvements in the design and
                     reporting; through to developing meta-analysis methodology to better apply to in basic research studies."),
    p("Follow us on twitter", tags$a(href="https://twitter.com/camarades_?", "@CAMARADES_")))
    
    
    
  )))
