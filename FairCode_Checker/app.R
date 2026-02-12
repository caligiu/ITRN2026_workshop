# app.R
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyAce)
library(httr)  

# Function to validate DOI
validate_doi <- function(doi) {
  # Extract DOI if it's a full URL
  doi_pattern <- "10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+"
  doi_match <- regmatches(doi, regexpr(doi_pattern, doi))
  
  if(length(doi_match) == 0) {
    return(list(valid = FALSE, message = "Invalid DOI format"))
  }
  
  doi_clean <- doi_match[1]
  
  # Try to resolve DOI using dx.doi.org
  tryCatch({
    response <- httr::HEAD(
      paste0("https://doi.org/", doi_clean),
      httr::timeout(5)
    )
    
    if(response$status_code == 200) {
      return(list(
        valid = TRUE, 
        message = paste0("✅ Valid DOI: ", doi_clean),
        doi = doi_clean
      ))
    } else if(response$status_code == 404) {
      return(list(
        valid = FALSE, 
        message = paste0("❌ DOI not found: ", doi_clean)
      ))
    } else {
      return(list(
        valid = FALSE, 
        message = paste0("⚠️ Cannot verify DOI (status: ", response$status_code, ")")
      ))
    }
  }, error = function(e) {
    return(list(
      valid = FALSE, 
      message = paste0("⚠️ Error checking DOI: ", e$message)
    ))
  })
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ITalianReproducibilityNetwork - FAIR Principles Learning App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Learn FAIR", tabName = "learn", icon = icon("graduation-cap")),
      menuItem("Practice", tabName = "practice", icon = icon("code")),
      menuItem("External Resources", tabName = "resources", icon = icon("link"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .fair-box { background-color: #d4edda; padding: 10px; border-radius: 5px; margin: 10px 0; }
        .unfair-box { background-color: #f8d7da; padding: 10px; border-radius: 5px; margin: 10px 0; }
        .feedback-success { color: #28a745; font-weight: bold; }
        .feedback-error { color: #dc3545; font-weight: bold; }
        .feedback-warning { color: #ffc107; font-weight: bold; }
        .principle-tag { display: inline-block; padding: 3px 8px; margin: 2px; 
                        border-radius: 3px; font-size: 12px; }
        .findable { background-color: #007bff; color: white; }
        .accessible { background-color: #28a745; color: white; }
        .interoperable { background-color: #ffc107; color: black; }
        .reusable { background-color: #17a2b8; color: white; }
        .doi-validation { padding: 10px; margin: 10px 0; border-radius: 5px; 
                         border-left: 4px solid; }
        .doi-valid { background-color: #d4edda; border-color: #28a745; }
        .doi-invalid { background-color: #f8d7da; border-color: #dc3545; }
        .doi-warning { background-color: #fff3cd; border-color: #ffc107; }
      "))
    ),
    
    tabItems(
      # Learn Tab
      tabItem(
        tabName = "learn",
        fluidRow(
          box(
            title = "What are FAIR Principles?",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            HTML("
              <p>FAIR principles are guidelines to make data <strong>Findable, Accessible, Interoperable, and Reusable</strong>.</p>
              <ul>
                <li><strong>Findable:</strong> Data should be easy to find for both humans and computers</li>
                <li><strong>Accessible:</strong> Data should be retrievable by standard protocols</li>
                <li><strong>Interoperable:</strong> Data should work with other data and applications</li>
                <li><strong>Reusable:</strong> Data should be well-described to be replicated and combined</li>
              </ul>
            ")
          )
        ),
        
        fluidRow(
          column(
            width = 6,
            box(
              title = "❌ Non-FAIR Code Example",
              width = NULL,
              status = "danger",
              solidHeader = TRUE,
              aceEditor(
                "unfair_code",
                value = "# Analysis script
d <- read.csv('data.csv')
result <- mean(d$x)
print(result)",
                mode = "r",
                theme = "github",
                readOnly = TRUE,
                height = "200px"
              ),
              actionButton("show_unfair_issues", "Show Issues", 
                           class = "btn-danger", icon = icon("exclamation-triangle"))
            ),
            
            hidden(
              div(
                id = "unfair_issues",
                box(
                  title = "Issues with this code:",
                  width = NULL,
                  status = "warning",
                  HTML("
                    <div class='unfair-box'>
                      <span class='principle-tag findable'>F</span> 
                      <strong>No metadata:</strong> No description of what this analysis does<br>
                      <span class='principle-tag findable'>F</span> 
                      <strong>No unique identifier:</strong> No DOI or persistent identifier<br>
                      <span class='principle-tag accessible'>A</span> 
                      <strong>Hardcoded path:</strong> 'data.csv' location unclear<br>
                      <span class='principle-tag interoperable'>I</span> 
                      <strong>No standard format:</strong> No information about data structure<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>No license:</strong> Unclear if code can be reused<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>No documentation:</strong> No comments or README<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>No dependencies:</strong> Required packages not specified
                    </div>
                  ")
                )
              )
            )
          ),
          
          column(
            width = 6,
            box(
              title = "✅ FAIR Code Example",
              width = NULL,
              status = "success",
              solidHeader = TRUE,
              aceEditor(
                "fair_code",
                value = "# Title: Temperature Analysis Script
# Author: Jane Doe (ORCID: 0000-0001-2345-6789)
# Date: 2024-01-15
# License: MIT
# DOI: 10.5281/zenodo.1234567
# Description: Calculates mean temperature from weather data
# Dependencies: R >= 4.0.0, readr, dplyr

# Load required packages
library(readr)
library(dplyr)

# Read data from persistent repository
data_url <- 'https://doi.org/10.5281/zenodo.1234567/data.csv'
weather_data <- read_csv(
  data_url,
  col_types = cols(
    date = col_date(),
    temperature = col_double(),
    location = col_character()
  )
)

# Calculate mean temperature
mean_temp <- weather_data %>%
  summarise(mean_temperature = mean(temperature, na.rm = TRUE))

# Output results with metadata
cat('Mean Temperature Analysis\\n')
cat('Data source', data_url, '\\n')
cat('Result', mean_temp$mean_temperature, '°C\\n')",
              mode = "r",
              theme = "github",
              readOnly = TRUE,
              height = "400px"
              ),
            actionButton("show_fair_features", "Show FAIR Features", 
                         class = "btn-success", icon = icon("check-circle"))
            ),
          
          hidden(
            div(
              id = "fair_features",
              box(
                title = "FAIR Features",
                width = NULL,
                status = "success",
                HTML("
                    <div class='fair-box'>
                      <span class='principle-tag findable'>F</span> 
                      <strong>Rich metadata:</strong> Title, author, date, description<br>
                      <span class='principle-tag findable'>F</span> 
                      <strong>Unique identifiers:</strong> DOI and ORCID provided<br>
                      <span class='principle-tag accessible'>A</span> 
                      <strong>Persistent URL:</strong> Data accessible via DOI<br>
                      <span class='principle-tag interoperable'>I</span> 
                      <strong>Standard format:</strong> CSV with explicit column types<br>
                      <span class='principle-tag interoperable'>I</span> 
                      <strong>Structured data:</strong> Clear data schema<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>License specified:</strong> MIT license for reuse<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>Well documented:</strong> Comments explain each step<br>
                      <span class='principle-tag reusable'>R</span> 
                      <strong>Dependencies listed:</strong> Required packages and versions
                    </div>
                  ")
              )
            )
          )
          )
        )
      ),
    
    # Practice Tab
    tabItem(
      tabName = "practice",
      fluidRow(
        box(
          title = "Practice Making Code FAIR",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          p("Edit the code below to make it FAIR-compliant. Click 'Check My Code' to get feedback!")
        )
      ),
      
      fluidRow(
        column(
          width = 6,
          box(
            title = "Your Code",
            width = NULL,
            aceEditor(
              "user_code",
              value = "# Fix this code to be FAIR!
data <- read.csv('mydata.csv')
x <- mean(data$value)
print(x)",
              mode = "r",
              theme = "monokai",
              height = "400px"
            ),
            actionButton("check_code", "Check My Code", 
                         class = "btn-primary", icon = icon("check")),
            actionButton("reset_code", "Reset", 
                         class = "btn-warning", icon = icon("undo"))
          )
        ),
        
        column(
          width = 6,
          box(
            title = "Feedback",
            width = NULL,
            status = "primary",
            uiOutput("feedback_output")
          ),
          
          box(
            title = "FAIR Checklist",
            width = NULL,
            status = "info",
            HTML("
                <h5>Make sure your code includes:</h5>
                <ul>
                  <li>📋 Title and description</li>
                  <li>👤 Author with ORCID</li>
                  <li>📅 Date</li>
                  <li>⚖️ License (e.g., MIT, GPL, CC-BY)</li>
                  <li>🔗 DOI or persistent identifier (validated)</li>
                  <li>📦 Required packages listed</li>
                  <li>💬 Meaningful comments</li>
                  <li>🌐 Accessible data source (URL/DOI)</li>
                  <li>📊 Data structure documentation</li>
                </ul>
              ")
          )
        )
      )
    ),
    
    # Resources Tab
    tabItem(
      tabName = "resources",
      fluidRow(
        box(
          title = "External Resources for FAIR Principles",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          
          h4("Official FAIR Resources"),
          tags$ul(
            tags$li(tags$a(href = "https://www.go-fair.org/fair-principles/", 
                           target = "_blank", "GO FAIR - Official FAIR Principles")),
            tags$li(tags$a(href = "https://www.nature.com/articles/sdata201618", 
                           target = "_blank", "Original FAIR Paper (Nature)")),
            tags$li(tags$a(href = "https://fairsharing.org/", 
                           target = "_blank", "FAIRsharing - Standards & Databases"))
          ),
          
          h4("FAIR Assessment Tools"),
          tags$ul(
            tags$li(tags$a(href = "https://fair-checker.france-bioinformatique.fr/", 
                           target = "_blank", "FAIR-Checker - Evaluate Dataset FAIRness")),
            tags$li(tags$a(href = "https://www.fairsfair.eu/f-uji-automated-fair-data-assessment-tool", 
                           target = "_blank", "F-UJI - Automated FAIR Assessment")),
            tags$li(tags$a(href = "https://ardc.edu.au/resources/working-with-data/fair-data/fair-self-assessment-tool/", 
                           target = "_blank", "ARDC FAIR Data Self Assessment Tool"))
          ),
          
          h4("R-Specific FAIR Resources"),
          tags$ul(
            tags$li(tags$a(href = "https://cran.r-project.org/web/packages/FAIRmaterials/index.html", 
                           target = "_blank", "FAIRmaterials R Package")),
            tags$li(tags$a(href = "https://www.r-bloggers.com/2021/07/fair-principles-for-data-science/", 
                           target = "_blank", "FAIR Principles for Data Science in R")),
            tags$li(tags$a(href = "https://github.com/topics/fair-data", 
                           target = "_blank", "GitHub - FAIR Data Projects"))
          ),
          
          h4("Data Repositories Supporting FAIR"),
          tags$ul(
            tags$li(tags$a(href = "https://zenodo.org/", 
                           target = "_blank", "Zenodo - General Purpose Repository")),
            tags$li(tags$a(href = "https://figshare.com/", 
                           target = "_blank", "Figshare - Research Data Repository")),
            tags$li(tags$a(href = "https://datadryad.org/", 
                           target = "_blank", "Dryad - Scientific Data Repository")),
            tags$li(tags$a(href = "https://osf.io/", 
                           target = "_blank", "Open Science Framework"))
          ),
          
          h4("Learning Materials"),
          tags$ul(
            tags$li(tags$a(href = "https://www.openaire.eu/how-to-make-your-data-fair", 
                           target = "_blank", "OpenAIRE - How to Make Your Data FAIR")),
            tags$li(tags$a(href = "https://www.fosteropenscience.eu/learning/", 
                           target = "_blank", "FOSTER Open Science Training")),
            tags$li(tags$a(href = "https://psicostat.github.io/psicostat-teaching/", 
                           target = "_blank", "Psicostat - Teachings & Workshops"))
          )
        )
      ),
      
      fluidRow(
        box(
          title = "Evaluate Others' Work",
          width = 12,
          status = "info",
          solidHeader = TRUE,
          
          textInput("eval_url", "Enter a GitHub repository or DOI to evaluate:", 
                    placeholder = "e.g., https://github.com/user/repo or 10.5281/zenodo.1234567"),
          actionButton("evaluate_external", "Evaluate", class = "btn-info"),
          
          br(), br(),
          
          uiOutput("evaluation_result")
        )
      )
    )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Learn Tab - Show/Hide Issues
  observeEvent(input$show_unfair_issues, {
    toggle("unfair_issues")
  })
  
  observeEvent(input$show_fair_features, {
    toggle("fair_features")
  })
  
  # Practice Tab - Check Code
  observeEvent(input$check_code, {
    code <- input$user_code
    
    # Extract DOI if present
    doi_pattern <- "10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+"
    doi_matches <- regmatches(code, gregexpr(doi_pattern, code))
    has_doi <- length(doi_matches[[1]]) > 0
    
    # Validate DOI if present
    doi_validation <- NULL
    if(has_doi) {
      doi_validation <- validate_doi(doi_matches[[1]][1])
    }
    
    # Check for FAIR elements
    checks <- list(
      title = grepl("(?i)title", code),
      author = grepl("(?i)author", code),
      orcid = grepl("(?i)orcid", code) || grepl("\\d{4}-\\d{4}-\\d{4}-\\d{3}[0-9X]", code),
      date = grepl("(?i)date", code),
      license = grepl("(?i)license", code),
      doi = has_doi,
      doi_valid = if(has_doi) doi_validation$valid else FALSE,
      dependencies = grepl("(?i)(dependencies|library\\(|require\\()", code),
      comments = grepl("#.*[a-zA-Z]{10,}", code),
      url = grepl("(https?://|doi\\.org)", code),
      description = grepl("(?i)description:", code)
    )
    
    # Calculate score (DOI validity is bonus, not required for base score)
    base_checks <- checks[!names(checks) %in% c("doi_valid")]
    passed <- sum(unlist(base_checks))
    total <- length(base_checks)
    score <- round((passed / total) * 100)
    
    # Bonus points for valid DOI
    if(checks$doi && checks$doi_valid) {
      score <- min(100, score + 5)
    }
    
    output$feedback_output <- renderUI({
      feedback_html <- paste0(
        "<h4>Score: ", score, "% (", passed, "/", total, " checks passed)</h4>",
        "<div style='margin: 20px 0;'>",
        "<div style='background-color: #e9ecef; height: 30px; border-radius: 5px; overflow: hidden;'>",
        "<div style='background-color: ", 
        if(score >= 80) "#28a745" else if(score >= 50) "#ffc107" else "#dc3545",
        "; height: 100%; width: ", score, "%; transition: width 0.5s;'></div>",
        "</div></div>"
      )
      
      feedback_html <- paste0(feedback_html, "<h5>Detailed Feedback:</h5><ul>")
      
      if(checks$title) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Title found</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add a title (# Title: ...)</li>")
      }
      
      if(checks$author) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Author specified</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add author information (# Author: ...)</li>")
      }
      
      if(checks$orcid) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ ORCID found</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add ORCID (# ORCID: 0000-0001-2345-6789)</li>")
      }
      
      if(checks$date) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Date included</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add date (# Date: YYYY-MM-DD)</li>")
      }
      
      if(checks$license) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ License specified</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add license (# License: MIT)</li>")
      }
      
      # DOI validation feedback
      if(checks$doi) {
        if(checks$doi_valid) {
          feedback_html <- paste0(feedback_html, 
                                  "<li class='feedback-success'>✅ DOI found and validated: ", 
                                  doi_validation$doi, " 🎉</li>")
        } else {
          feedback_html <- paste0(feedback_html, 
                                  "<li class='feedback-warning'>⚠️ DOI found but ", 
                                  doi_validation$message, "</li>")
        }
      } else {
        feedback_html <- paste0(feedback_html, 
                                "<li class='feedback-error'>❌ Add DOI (# DOI: 10.5281/zenodo.1234567)</li>")
      }
      
      if(checks$dependencies) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Dependencies mentioned</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ List required packages (# Dependencies: ...)</li>")
      }
      
      if(checks$comments) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Code has meaningful comments</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add more descriptive comments</li>")
      }
      
      if(checks$url) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Accessible data source (URL/DOI)</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Use persistent URL for data (https://doi.org/...)</li>")
      }
      
      if(checks$description) {
        feedback_html <- paste0(feedback_html, "<li class='feedback-success'>✅ Description provided</li>")
      } else {
        feedback_html <- paste0(feedback_html, "<li class='feedback-error'>❌ Add description (# Description: ...)</li>")
      }
      
      feedback_html <- paste0(feedback_html, "</ul>")
      
      # Add DOI validation details if present
      if(has_doi && !is.null(doi_validation)) {
        doi_class <- if(doi_validation$valid) "doi-valid" else if(grepl("Cannot verify", doi_validation$message)) "doi-warning" else "doi-invalid"
        feedback_html <- paste0(
          feedback_html,
          "<div class='doi-validation ", doi_class, "'>",
          "<strong>DOI Validation:</strong> ", doi_validation$message,
          "</div>"
        )
      }
      
      if(score == 100) {
        feedback_html <- paste0(
          feedback_html,
          "<div class='alert alert-success' style='margin-top: 20px;'>",
          "<strong>🎉 Excellent!</strong> Your code follows all FAIR principles!",
          "</div>"
        )
      } else if(score >= 70) {
        feedback_html <- paste0(
          feedback_html,
          "<div class='alert alert-info' style='margin-top: 20px;'>",
          "<strong>👍 Good job!</strong> Just a few more improvements needed.",
          "</div>"
        )
      } else {
        feedback_html <- paste0(
          feedback_html,
          "<div class='alert alert-warning' style='margin-top: 20px;'>",
          "<strong>📚 Keep learning!</strong> Review the FAIR checklist and try again.",
          "</div>"
        )
      }
      
      HTML(feedback_html)
    })
  })
  
  # Reset code
  observeEvent(input$reset_code, {
    updateAceEditor(session, "user_code", 
                    value = "# Fix this code to be FAIR!\ndata <- read.csv('mydata.csv')\nx <- mean(data$value)\nprint(x)")
    output$feedback_output <- renderUI({
      HTML("<p>Code reset. Start editing to make it FAIR-compliant!</p>")
    })
  })
  
  # Evaluate external work
  observeEvent(input$evaluate_external, {
    url <- input$eval_url
    
    output$evaluation_result <- renderUI({
      if(url == "") {
        return(HTML("<p class='text-danger'>Please enter a valid URL or DOI.</p>"))
      }
      
      # Check if it's a DOI
      doi_validation <- validate_doi(url)
      
      eval_html <- paste0(
        "<div class='alert alert-info'>",
        "<h5>Evaluation for: <code>", url, "</code></h5>"
      )
      
      # Add DOI validation if applicable
      if(grepl("10\\.\\d{4,}/", url)) {
        doi_class <- if(doi_validation$valid) "doi-valid" else if(grepl("Cannot verify", doi_validation$message)) "doi-warning" else "doi-invalid"
        eval_html <- paste0(
          eval_html,
          "<div class='doi-validation ", doi_class, "' style='margin: 15px 0;'>",
          "<strong>DOI Validation:</strong> ", doi_validation$message,
          "</div>"
        )
      }
      
      eval_html <- paste0(
        eval_html,
        "<p>When evaluating this resource for FAIRness, consider:</p>",
        "<h6><span class='principle-tag findable'>Findable</span></h6>",
        "<ul>",
        "<li>Does it have a unique identifier (DOI, Handle)?</li>",
        "<li>Is there rich metadata describing the content?</li>",
        "<li>Is it registered in a searchable resource?</li>",
        "</ul>",
        "<h6><span class='principle-tag accessible'>Accessible</span></h6>",
        "<ul>",
        "<li>Can you retrieve the data/code using standard protocols?</li>",
        "<li>Is the access procedure clearly described?</li>",
        "<li>Will the metadata remain even if data is removed?</li>",
        "</ul>",
        "<h6><span class='principle-tag interoperable'>Interoperable</span></h6>",
        "<ul>",
        "<li>Does it use standard formats (CSV, JSON, etc.)?</li>",
        "<li>Does it reference other data/resources with identifiers?</li>",
        "<li>Are vocabularies and ontologies used?</li>",
        "</ul>",
        "<h6><span class='principle-tag reusable'>Reusable</span></h6>",
        "<ul>",
        "<li>Is there a clear license for reuse?</li>",
        "<li>Is the provenance well-documented?</li>",
        "<li>Does it meet community standards?</li>",
        "</ul>",
        "<p><strong>Tip:</strong> Use the FAIR assessment tools in the links above for detailed automated evaluation!</p>",
        "</div>"
      )
      
      HTML(eval_html)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
