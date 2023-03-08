library(shiny)
library(tidyverse)
staff <- read_delim("../City_of_Seattle_Staff_Demographics.csv")
transform(staff, "Hourly Rate" = as.numeric("Hourly Rate"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Seattle Staff Demographics"),
  
  # Red UI
  tabsetPanel(
    tabPanel("Overview", 
             sidebarLayout(
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
    # Orange UI
    tabPanel("Department & Race",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("checkboxDepTable")
               ),
               mainPanel(
                 tableOutput("tableRace"),
                 textOutput("entriesTableRace")
               )
             )
    ),

# Green UI
    tabPanel("Department & Sex", 
         sidebarLayout(
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
           mainPanel(
             plotOutput("distPlot"),
             textOutput("plotObservation")
           )
         )
    ), 
# Pink-ish UI
    tabPanel("Wage & Race",
         sidebarLayout(
           sidebarPanel(
             p("This plot displays the hourly rate of employees categorized by their race. This plot aims to understand the disparities in wages for each race."),
             #Check race
             checkboxGroupInput("Race/Ethnicity",
                                "Select a race/ethnicity",
                                choices = unique(staff$`Race/Ethnicity`),
                                selected = unique(staff$`Race/Ethnicity`)),
             
             #Select color for points
             radioButtons("rb", label = "Choose point color",
                          choices = c("red",
                                           "blue",
                                           "green",
                                           "purple"),
                                           selected = "blue")
             
           ),
           mainPanel(
             plotOutput('distPlotRace'), 
             textOutput("plotNumbers")
           )
         )
    ),
# Blue UI
tabPanel("Conclusion", 
         sidebarLayout(
           sidebarPanel(
             h4("Through analyzing this dataset, we have noticed several patterns regarding diversity and inclusion. Age remains quite diverse, with the youngest employed being 16 years old and the oldest being 91 years old. However, race and gender makeup of the departments could be improved to be more inclusive. All 6 major departments (departments with over 700 employees) were male-dominated. As for race, all 6 major departments are white-dominated. All of them have over 44% white people. Since this is specifically for white vs. non-white (all races except white) individuals we can conclude that these spaces employ significantly more white individuals. Furthermore, when looking at race and wage, white individuals tend to be paid higher wages than other races."),
             h4(strong("Broader Implications:"), "Having poor diversity and inclusion hiring practices is discriminatory. Knowing what departments have poor diversity and inclusion can be helpful to make improvements in hiring practices in the future."), 
             
             h4(strong("Future Ideas:"), "Tracking the dataset over time to compare change. Presenting data analysis to City of Seattle HR employees to showcase findings and advocate for better hiring practices."), 
             h4(strong("Data Quality:"), "Data is of good quality and is updated frequently. Data seems accurate and unbiased and therefore, not harmful towards certain groups.")
           ),
           mainPanel(
             tableOutput("conclusionTable1"),
             tableOutput("conclusionTable2")
           )
         )
)


  )
)



server <- function(input, output) {

# Red Server
  
output$image <- renderImage({
  list(src = "../worker-image.jpeg",
       width = "100%")
}, deleteFile = FALSE)

# Orange Server
staffCleanTable <- reactive({
  staff %>%
    group_by(Department) %>%
    filter(n() > 500) %>% 
    filter(`Department` %in% input$departmentTable)
})

output$checkboxDepTable <- renderUI({
  staff_over_400 <- staff %>% 
    group_by(Department) %>%
    filter(n() > 400) 
  checkboxGroupInput("departmentTable",
                     "Department: ",
                     choices = unique(staff_over_400$`Department`),
                     selected = unique(staff_over_400$`Department`))
})

output$tableRace <- renderTable({
  staffCleanTable() %>% 
    summarize(`Percentage of White Employees`=mean(`Race/Ethnicity` == "White")*100, 
              `Percentage of Non-white Employees`=mean(`Race/Ethnicity` != "White")*100) %>% 
    arrange(desc(`Percentage of White Employees`))
  
})

output$entriesTableRace <- renderText({
  paste("This table displays the percentage of white vs. non-white employees 
          per City of Seattle Department. Presenting data for", 
        length(staffCleanTable()$Department), "employees.")
})  

# Green Server
output$distPlot <- renderPlot({
  staff %>% 
    filter(Department %in% input$plotDepartments) %>%
    ggplot(aes(Department, fill=Sex)) +
    geom_bar(position=input$position) + 
    theme(axis.text.x = element_text(angle = 90)) +
    ggtitle('Department by Counts')
})

output$plotObservation <- renderPrint({
  num <- staff %>% 
    filter(Department %in% input$plotDepartments) %>%
    nrow()
  cat("This graph provides a visual depiction of the male to female ratio of Seattle employees.
  The plot uses ", num, " observations to create this depiction.")
})

# Pink-ish Server
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

output$plotNumbers <- renderPrint({
  rows <- staff %>% 
    filter(`Race/Ethnicity`%in% input$`Race/Ethnicity`) %>% 
    nrow()
  cat("The selected subset contains ", rows, "observations which compare the wages of Seattle 
  employees based on their self-identified Race/Ethnicity.")
  
})

# Blinding Blue 
output$conclusionTable1 <- renderTable ({
  staff %>%
    group_by(Department) %>%
    filter(n() > 700) %>%
    summarize("Average Age" = mean(Age), `Percentage of White Employees`=mean(`Race/Ethnicity` == "White")*100)
})

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
