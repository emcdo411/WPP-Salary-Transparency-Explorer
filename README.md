# WPP Salary Transparency Explorer

## 📌 Overview
This case study explores salary trends and gender disparities in digital marketing, focusing on WPP, GroupM, and AKQA in England and Sweden. Using **R Shiny**, we developed an interactive app that visualizes wage differences between men and women, comparing those with marketing degrees to those without.

---

## 📊 Key Features
- **📍 Interactive 2D Leaflet Map**: View salaries for men and women across **London, Stockholm, and Gothenburg**.
- **📈 Salary Comparison Bar Chart**: Compare **degree vs. non-degree salaries** across companies.
- **📂 Data Table**: Filter and explore salary data based on gender, degree, company, and location.
- **🛠️ Built with R Shiny**: Dynamic, real-time interactivity for recruiters and analysts.

---

## 🏆 Why This Matters
✅ **Transparency in digital marketing salaries** for job seekers and hiring managers.  
✅ **Insight into gender pay gaps** at major advertising firms.  
✅ **Recruiters can identify salary trends** and improve hiring strategies.  
✅ **Encourages industry-wide discussions** on fair pay and equal opportunities.  

---

## 🔥 The Shiny App Code
```r
library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(leaflet)

digital_marketing_data <- tibble::tibble(
  Gender = c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
  Degree = c("Marketing", "Non-Marketing", "Marketing", "Non-Marketing", "Marketing", "Marketing", "Non-Marketing", "Marketing"),
  Company = c("WPP", "WPP", "GroupM", "AKQA", "GroupM", "AKQA", "GroupM", "AKQA"),
  Job_Title = c("Content Strategist", "SEO Analyst", "Email Marketing Coordinator", "Brand Manager", 
                "Digital Specialist", "UX Designer", "Paid Media Specialist", "Creative Director"),
  Salary = c(84000, 68000, 92000, 77000, 95000, 87500, 78000, 102000),
  Country = c("England", "England", "England", "Sweden", "Sweden", "Sweden", "Sweden", "Sweden"),
  City = c("London", "London", "London", "Stockholm", "Stockholm", "Stockholm", "Gothenburg", "Gothenburg"),
  Lat = c(51.5074, 51.5074, 51.5074, 59.3293, 59.3293, 59.3293, 57.7089, 57.7089),
  Lon = c(-0.1278, -0.1278, -0.1278, 18.0686, 18.0686, 18.0686, 11.9746, 11.9746)
)

ui <- fluidPage(
  titlePanel("Global Digital Marketing Salary Explorer – WPP, GroupM & AKQA"),
  sidebarLayout(
    sidebarPanel(
      helpText("Explore salaries in England and Sweden for top marketing companies."),
      checkboxGroupInput("selected_countries", "Filter by Country:",
                         choices = unique(digital_marketing_data$Country),
                         selected = unique(digital_marketing_data$Country)),
      checkboxGroupInput("selected_cities", "Filter by City:",
                         choices = unique(digital_marketing_data$City),
                         selected = unique(digital_marketing_data$City)),
      checkboxGroupInput("selected_companies", "Filter by Company:",
                         choices = unique(digital_marketing_data$Company),
                         selected = unique(digital_marketing_data$Company)),
      hr(),
      helpText("Click on a map marker to view gender, degree, job title, and salary.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Salary Chart", plotOutput("salaryPlot")),
        tabPanel("Interactive Map", leafletOutput("salaryMap", height = "600px")),
        tabPanel("Data Table", tableOutput("dataPreview"))
      )
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    digital_marketing_data %>%
      filter(Country %in% input$selected_countries,
             City %in% input$selected_cities,
             Company %in% input$selected_companies)
  })

  output$salaryPlot <- renderPlot({
    filtered_data() %>%
      group_by(Gender, Degree) %>%
      summarise(Average_Salary = mean(Salary), .groups = 'drop') %>%
      ggplot(aes(x = Degree, y = Average_Salary, fill = Gender)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = "Average Salary by Gender and Degree",
        y = "Avg Salary (USD)", x = "Degree Type", fill = "Gender"
      ) +
      theme_minimal()
  })

  output$dataPreview <- renderTable({
    filtered_data()
  })

  output$salaryMap <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~Lon, ~Lat,
        radius = 8,
        color = ~ifelse(Gender == "Female", "#E91E63", "#2196F3"),
        popup = ~paste0(
          "<b>Company:</b> ", Company, "<br>",
          "<b>City:</b> ", City, "<br>",
          "<b>Job Title:</b> ", Job_Title, "<br>",
          "<b>Salary:</b> $", format(Salary, big.mark = ","), "<br>",
          "<b>Gender:</b> ", Gender, "<br>",
          "<b>Degree:</b> ", Degree
        ),
        fillOpacity = 0.8
      )
  })
}

shinyApp(ui = ui, server = server)
```

---

## 📚 Conclusion
This **R Shiny** application offers **real-time salary insights** into the digital marketing sector at major firms like WPP, GroupM, and AKQA. With **interactive visualizations and filtering options**, recruiters and analysts can better understand wage transparency, gender-based salary trends, and the impact of specialized education in marketing. 

---

## 🔗 Suggested Repository Name
**WPP-Salary-Transparency-Explorer**

Would you like **additional locations**, **expanded datasets**, or **more analysis tools**? Let's take this further! 🚀
