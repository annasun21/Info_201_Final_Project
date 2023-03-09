#Group 5: Kaitlyn B, Anna S, Nawal D, and Lydia S
#TA: Runhan Chen
#Progam: INFO 201 Final Project

#load needed libraries
library(shiny)
library(tidyverse)

#load dataset and update column type
staff <- read_delim("../City_of_Seattle_Staff_Demographics.csv")
transform(staff, "Hourly Rate" = as.numeric("Hourly Rate"))


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Seattle Staff Demographics"),
  
  tabsetPanel(
    # Overview UI
    tabPanel("Overview", 
             sidebarLayout(
               #brief description of our data and what we will be analyzing
               sidebarPanel(
                 h4("Our dataset is on City of Seattle Staff Demographics. 
                    The data was created February 25, 2019 and is provided by the Seattle 
                    Department of Human Resources. It is updated on a monthly basis, with its most
                    recent update being January 31, 2023."),
                 
                 h4("This dataset contains ", 
                    nrow(data), 
                    " observations and ",
                    ncol(data),
                    " variables."
                 ),
                 
                 h4("An analysis of this data will be most helpful for Seattle HR employees. 
                 By analyzing employees in various departments by gender, race, and age,  hiring
                 practices can be improved for fostering diversity and inclusion, an important 
                 aspect of hiring that companies should strive for. Here are some questions that
                 will help guide our analysis: 
                        
                        “What is the percentage of White vs. Non-white employees per City of Seattle department?”
                      “How many male vs. female employees make up each City of Seattle department?
                        “How do the wages of City of Seattle employees compare by race?”"
                    
                 )
               ),
               mainPanel(
                 imageOutput("image")
               )
             )
    ),
    
    # Dep & Race UI
    tabPanel("Department & Race",
             sidebarLayout(
               #widget allows user to choose what Departments they want to look at
               sidebarPanel(
                 uiOutput("checkboxDepTable")
               ),
               #displays the table and a description of what the table is showing
               mainPanel(
                 tableOutput("tableRace"),
                 textOutput("entriesTableRace")
               )
             )
    ),

    # Dep & Sex UI
    tabPanel("Department & Sex", 
         sidebarLayout(
           #allows user to choose what Departments to look at and how the data should be presented
           sidebarPanel(
             radioButtons("position",
                          "What position would you like to see the data presented in: ",
                          choices = c("stack", "dodge"),
                          selected = "stack"),
             checkboxGroupInput("plotDepartments",
                                "What departments would you like to see: ",
                                choices = unique(staff$Department),
                                selected = unique(staff$Department))
           ),
           #displays a bar chart and includes a description of what is being shown
           mainPanel(
             plotOutput("distPlot"),
             textOutput("plotObservation")
           )
         )
    ), 
    # Wage & Race UI
    tabPanel("Wage & Race",
         sidebarLayout(
           #allows user to choose which Race/Ethnicity they want to compare and what color the points should be shown in
           sidebarPanel(
             checkboxGroupInput("Race/Ethnicity",
                                "Select a race/ethnicity",
                                choices = unique(staff$`Race/Ethnicity`),
                                selected = unique(staff$`Race/Ethnicity`)),
             radioButtons("rb", label = "Choose point color",
                          choices = c("red",
                                           "blue",
                                           "green",
                                           "purple"),
                                           selected = "blue")
             
           ),
           #displays a point graph and a description of the graph
           mainPanel(
             plotOutput('distPlotRace'), 
             textOutput("plotNumbers")
           )
         )
    ),
    # Conclusion UI
    tabPanel("Conclusion", 
         sidebarLayout(
           #our conclusion drawn from our data analysis
           sidebarPanel(
             h4("Through analyzing this dataset, we have noticed several patterns regarding diversity and inclusion. Age remains quite diverse, with the youngest employed being 16 years old and the oldest being 91 years old. However, race and gender makeup of the departments could be improved to be more inclusive. All 6 major departments (departments with over 700 employees) were male-dominated. As for race, all 6 major departments are white-dominated. All of them have over 44% white people. Since this is specifically for white vs. non-white (all races except white) individuals we can conclude that these spaces employ significantly more white individuals. Furthermore, when looking at race and wage, white individuals tend to be paid higher wages than other races."),
             h4(strong("Broader Implications:"), "Having poor diversity and inclusion hiring practices is discriminatory. Knowing what departments have poor diversity and inclusion can be helpful to make improvements in hiring practices in the future."), 
             
             h4(strong("Future Ideas:"), "Tracking the dataset over time to compare change. Presenting data analysis to City of Seattle HR employees to showcase findings and advocate for better hiring practices."), 
             h4(strong("Data Quality:"), "Data is of good quality and is updated frequently. Data seems accurate and unbiased and therefore, not harmful towards certain groups.")
           ),
           #tables to represent the pattern we found
           mainPanel(
             tableOutput("conclusionTable1"),
             tableOutput("conclusionTable2")
           )
         )
     )

  )
)



server <- function(input, output) {

  #Overview page
  #image used for overview page
  output$image <- renderImage({
    list(src = "../worker-image.jpeg",
         width = "100%")
  }, deleteFile = FALSE)
  
  
  # Dep & Race page
  #organizes data table based off user input
  staffCleanTable <- reactive({
    staff %>%
      group_by(Department) %>%
      filter(n() > 700) %>% 
      filter(`Department` %in% input$departmentTable)
  })
  
  #widget that allows user to choose departments
  output$checkboxDepTable <- renderUI({
    staff_over_700 <- staff %>% 
      group_by(Department) %>%
      filter(n() > 700) 
    checkboxGroupInput("departmentTable",
                       "Department: ",
                       choices = unique(staff_over_700$`Department`),
                       selected = unique(staff_over_700$`Department`))
  })
  
  #table that shows percentage of white to non-whites
  output$tableRace <- renderTable({
    staffCleanTable() %>% 
      summarize(`Percentage of White Employees`=mean(`Race/Ethnicity` == "White")*100, 
                `Percentage of Non-white Employees`=mean(`Race/Ethnicity` != "White")*100) %>% 
      arrange(desc(`Percentage of White Employees`))
    
  })
  
  #overview of what this table is used for
  output$entriesTableRace <- renderText({
    paste("This table displays the percentage of white vs. non-white employees 
            per City of Seattle Department. It presents data for", 
          length(staffCleanTable()$Department), "employees at the top ten most employed departments. 
          The checkbox allows you to view specific data of interest, allowing for a better, more clear
          comparison between departments.")
  })  
  
  
  # Dep & Sex page
  #bar chart of department and their gender make-up
  output$distPlot <- renderPlot({
    staff %>% 
      filter(Department %in% input$plotDepartments) %>%
      ggplot(aes(Department, fill=Sex)) +
      geom_bar(position=input$position) + 
      theme(axis.text.x = element_text(angle = 90)) +
      ggtitle('Department by Counts')
  })
  
  #overview and info on our chart
  output$plotObservation <- renderPrint({
    num <- staff %>% 
      filter(Department %in% input$plotDepartments) %>%
      nrow()
    cat("This graph provides a visual depiction of the male to female ratio of Seattle employees.
    It presents up-to-date information regarding the sex makeup of each City of Seattle department,
    and can be viewed in stacked or dodged form to be more easily interpreted. The plot uses ",
        num, " observations to create this depiction, and can be filtered with the checkboxes to
        show a different number of departments.")
  })
  
  
  # Wage & Race page
  #plot of hourly wage for each race
  output$distPlotRace <- renderPlot({
    staff %>% 
      filter(`Race/Ethnicity`%in% input$`Race/Ethnicity`) %>% 
      ggplot(aes(`Race/Ethnicity`, `Hourly Rate`)) +
      geom_point(col = input$rb) +
      scale_fill_manual(
        values = colors
      ) +
      labs(x = "Race/Ethnicity",
           y = 'Hourly Rate') +
      theme(text = element_text(size=15), axis.text.x = element_text(angle=90)) +
      ggtitle('City of Seattle Wages categorized by race')
  })
  
  #description of plot
  output$plotNumbers <- renderPrint({
    rows <- staff %>% 
      filter(`Race/Ethnicity`%in% input$`Race/Ethnicity`) %>% 
      nrow()
    cat("The selected subset contains ", rows, "observations which compare the wages of Seattle 
    employees based on their self-identified Race/Ethnicity. It plots the various hourly rates of 
    individuals and organizes them by their racial identity. With this plot, one can view any race
    based inequalities that can be assessed and later addressed to ensure inclusivity within the 
    workplace.")
  })
  
  
  # Conclusion page
  #table that shows age, percentage of white, and percentage of male for each major department
  output$conclusionTable1 <- renderTable ({
    staff %>%
      group_by(Department) %>%
      filter(n() > 700) %>%
      summarize("Average Age" = mean(Age), 
                `Percentage of White Employees`=mean(`Race/Ethnicity` == "White")*100, 
                "Percent Male" = mean(Sex == "M")*100)
  })
  
  #table that compares average wage for each race/ethnicity
  output$conclusionTable2 <- renderTable({
    staff %>% 
      select(`Hourly Rate`, `Race/Ethnicity`) %>% 
      group_by(`Race/Ethnicity`) %>% 
      summarize(
        "Average Hourly Rate" = mean(`Hourly Rate`)
      )
  })

}

# Run the application 
shinyApp(ui, server)
