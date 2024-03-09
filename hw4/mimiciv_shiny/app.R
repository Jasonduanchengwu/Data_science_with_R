library(shiny)
library(tidyverse)
library(ggplot2)
library(bigrquery)
library(stringr)

# path to the service account token
satoken <- "../biostat-203b-2024-winter-313290ce47a6.json"

# BigQuery authentication using service account
bq_auth(path = satoken)

# connect to the BigQuery database `biostat-203b-2024-winter.mimic4_v2_2`
con_bq <- dbConnect(
  bigrquery::bigquery(),
  project = "biostat-203b-2024-winter",
  dataset = "mimic4_v2_2",
  billing = "biostat-203b-2024-winter"
)

# read in dataset from RDS
ds = readRDS("~/biostat-203b-hw/hw4/mimiciv_shiny/mimic_icu_cohort.rds")

# Define different groups of variables based on types
cat_vars <- c('first_careunit', 'last_careunit', 'admission_type',
              'admission_location', 'discharge_location', 'insurance',
              'language', 'marital_status', 'race', 'hospital_expire_flag',
              'gender', 'dod')

cont_vars <- c('temperature_fahrenheit', 'non_invasive_blood_pressure_diastolic',
               'respiratory_rate', 'non_invasive_blood_pressure_systolic',
               'heart_rate')

lab_vars <- c('sodium', 'chloride', 'creatinine', 'potassium', 'glucose',
              'hematocrit', 'wbc', 'bicarbonate')

non_lab_choices = c(cat_vars, cont_vars)

# UI definition
ui <- fluidPage(
  titlePanel("Dynamic MIMIC ICU Cohort and ICU Stay Plotting"),
  
  # Use tabsetPanel to create tabs
  tabsetPanel(
    # First tab with (patient variables and lab events)
    tabPanel("Patient characteristics", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("dataset", 
                             "Variable of interest:", 
                             choices = c(non_lab_choices, "Lab Events")),
                 checkboxInput("outlier", 
                               label = "Remove outliers in IQR method for measurements?", 
                               value = FALSE)
               ),
               mainPanel(
                 verbatimTextOutput("summary"),
                 plotOutput("tab1plot")  # Placeholder for the plot
               )
             )
    ),
    
    # Second tab (ADT and ICU stay information)
    tabPanel("Patient's ADT and ICU stay information", 
             sidebarLayout(
               sidebarPanel(
                 # Your existing inputs
                 textInput("id", label = h3("Patient ID"), value = "10001217")
               ),
               mainPanel(
                 plotOutput("adtplot"),
                 plotOutput("icuplot") # Placeholder for the plot
               )
             )
    )
  )
)

# pivot lab events to plot
labevents <- ds %>% 
  select(lab_vars) %>%
  pivot_longer(cols = everything(), names_to = "labs", values_to = "value")

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  output$tab1plot <- renderPlot({
    var_name <- input$dataset
    
    # plots of categorical variables
    if (var_name %in% cat_vars){
      ggplot(data = ds, aes_string(x = var_name, fill = var_name)) +
        geom_bar() +
        labs(title = paste("Barplot of", var_name)) +
        theme(legend.position = "none") +
        coord_flip()
    
    # plots of continuous variables
    } else if (var_name %in% cont_vars){
      ggplot(data = ds, aes_string(x = var_name)) +
        geom_boxplot(outlier.shape = ifelse(input$outlier, NA, 19)) +
        labs(title = paste("Boxplot of", var_name)) +
        theme(legend.position = "none") 
    
    # plots of lab events
    } else if (var_name == "Lab Events"){
      ggplot(data = labevents, aes(y = value, color = labs, x = labs)) +
        geom_boxplot(outlier.shape = ifelse(input$outlier, NA, 19)) +
        labs(title = paste("Boxplot of Lab Events")) +
        theme(legend.position = "none") +
        coord_flip()
      
    }
  })
  
  # numeric summary of the selected variable
  output$summary <- renderPrint({
    var_name <- input$dataset
    if(var_name != "Lab Events") {
      summary(ds[[var_name]])}
    else{
      summary(ds %>% select(lab_vars))}
  })
  
  # ADT plot in second tab
  output$adtplot <- renderPlot({
    pid = str_c("Patient ", input$id)
    id <- as.numeric(input$id)
    
    procedures = tbl(con_bq, "procedures_icd") %>%
      filter(subject_id == id) %>%
      left_join(tbl(con_bq, "d_icd_procedures")) %>%
      mutate(chartdate = as.POSIXct(chartdate)) %>%
      select(chartdate, long_title)
    
    labe = tbl(con_bq, "labevents") %>%
      filter(subject_id == id) %>%
      select(charttime)
    
    adt = tbl(con_bq, "transfers") %>%
      filter(subject_id == id & eventtype != "discharge") %>%
      select(intime, outtime, careunit)
    
    patients = tbl(con_bq, "patients")%>%
      filter(subject_id == id) %>%
      select(gender, anchor_age)
    
    tdiagnoses = tbl(con_bq, "diagnoses_icd") %>%
      filter(subject_id == id) %>%
      head(3) %>%
      left_join(tbl(con_bq, "d_icd_diagnoses")) %>%
      select(long_title)
    
    admissions = tbl(con_bq, "admissions") %>%
      filter(subject_id == id) %>%
      select(race)
    
    page = str_c(patients %>% pull(anchor_age), " years old")
    prace = tolower(admissions %>% pull(race))
    
    ggplot() +
      geom_point(data = procedures,
                 aes(x = chartdate,
                     y = "Procedure",
                     shape = long_title),
                 size = 3,
                 position = position_jitter(width = 0, height = 0.3)) +
      scale_shape_manual(values = c(1:50))+
      geom_point(data = labe,
                 aes(x = charttime,
                     y = "Lab"), 
                 shape = 3,
                 show.legend = F,
                 position = position_jitter(width = 0,height = 0.2),
                 alpha = 0.5) +
      geom_segment(data = adt,
                   aes(x = intime,
                       xend = outtime,
                       y = "ADT",
                       yend = "ADT",
                       color = careunit,
                       linewidth = str_detect(careunit, "(ICU|CCU)"))) +
      labs(y = "", x = "Calendar Time",
           title = str_c(pid, patients %>% pull(gender), page, prace, sep = ", "),
           subtitle = str_c(tdiagnoses %>% pull(long_title), collapse = "\n")) +
      theme_bw() + 
      scale_y_discrete(limits = c("Procedure", "Lab","ADT"))+
      theme(legend.position = "bottom", legend.box = "vertical", 
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.2, "cm"),
            legend.title = element_text(size=6)) +
      guides(linewidth = "none",
             shape = guide_legend(order = 1, title = "Procedure", nrow = 5),
             color = guide_legend(order = 2, title = "Care Unit"))
    })
  
  # ICU stay plot in second tab
  output$icuplot <- renderPlot({
    id <- as.numeric(input$id)
    
    d_items = tbl(con_bq, "d_items") %>%
      filter(itemid %in% c(220045, 220179, 220180, 220210, 223761)) %>%
      select(itemid, abbreviation)
    
    chartevents = tbl(con_bq, "chartevents") %>% 
      filter(subject_id %in% id, 
             itemid %in% c(220045, 220179, 220180, 220210, 223761))
    
    icu_data = left_join(chartevents, d_items)
    
    ggplot(icu_data, aes(x = charttime, y = valuenum, color = abbreviation, group = abbreviation)) +
      geom_point() +
      geom_line() +
      facet_grid(abbreviation ~ stay_id, scales = "free") +
      scale_x_datetime(guide = guide_axis(n.dodge = 2)) +
      labs(title = str_c("Patient ", id, " ICU stays - Vitals"),
           x = "",
           y = "") +
      theme_light() +
      theme(legend.position = "none")
  })
  if (exists("con_bq")) {
    dbDisconnect(con_bq)
  }
}

# Create Shiny app ----
shinyApp(ui, server)

