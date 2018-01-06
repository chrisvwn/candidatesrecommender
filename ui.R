library(shinydashboard)
#source("recommender3.R")

shinydashboard::dashboardPage(
  
  # Application title
  shinydashboard::dashboardHeader(title="Candidate Recommender"),
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      
      shinydashboard::menuItem("candidates", tabName = "candidates"),
      
      shinydashboard::menuItem("jobs", tabName = "jobs"),
      
      shinydashboard::menuItem("newCandidates", tabName = "newCandidates"),
      
      shinydashboard::menuItem("newJobs", tabName = "newJobs")
    )
  ),
  
  # body
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "candidates",
                              shiny::uiOutput(outputId = "candidate"),

                              DT::dataTableOutput(outputId = "candidateDetails"),
                              
                              DT::dataTableOutput(outputId = "candidateTrainApps"),
                              
                              DT::dataTableOutput(outputId = "candidateTestApps"),
                              
                              DT::dataTableOutput(outputId = "candidateRecommApps")
                              
      ),
      shinydashboard::tabItem(tabName = "jobs",
                              shiny::uiOutput(outputId = "selectJob"),

                              DT::dataTableOutput(outputId = "jobDetails"),
                              
                              DT::dataTableOutput(outputId = "jobCandidateApps"),
                              
                              DT::dataTableOutput(outputId = "jobRecommCandidates")
                              
      ),
      shinydashboard::tabItem(tabName = "newCandidates",
                              shiny::textInput(inputId = "newCandCity", label = "City"),
                              shiny::textInput(inputId = "newCandState", label = "State"),
                              shiny::textInput(inputId = "newCandCountry", label = "Country"),
                              shiny::textInput(inputId = "newCandZipCode", label = "ZipCode"),
                              shiny::textInput(inputId = "newCandDegreeType", label = "DegreeType"),
                              shiny::textInput(inputId = "newCandMajor", label = "Major"),
                              shiny::textInput(inputId = "newCandGraduationDate", label = "GraduationDate"),
                              shiny::textInput(inputId = "newCandWorkHistoryCount", label = "WorkHistoryCount"),
                              shiny::textInput(inputId = "newCandTotalYearsExperience", label = "TotalYearsExperience"),
                              shiny::textInput(inputId = "newCandCurrentlyEmployed", label = "CurrentlyEmployed"),
                              shiny::textInput(inputId = "newCandManagedOthers", label = "ManagedOthers"),
                              shiny::textInput(inputId = "newCandManagedHowMany", label = "ManagedHowMany"),

                              DT::dataTableOutput(outputId = "newCandidateRecommJobs")
                              
      ),
      shinydashboard::tabItem(tabName = "newJobs",
                              shiny::textAreaInput(inputId = "newJob", label = "New Job Description"),
                              
                              DT::dataTableOutput(outputId = "newJobRecommCandidates")
                              
      )
    )
  )
)
