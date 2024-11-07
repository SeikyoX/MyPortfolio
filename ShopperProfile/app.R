# Load required library
library(RMySQL)
library(tidyverse)
library(ggrepel)
library(shiny) # To quick view dashboard in R Console
library(shinydashboard) # To create a dashboard
library(shinyWidgets) # To use HTML codes in Dashboard.
theme_set(theme_bw()) # To Set the general theme.

# Set the working directory to a folder called "data" within your project
setwd("ShopperProfiles") 

# Now, any file operations within your script will be relative to this directory
data.frame <- read.csv("shopper_profile.csv")

# To review the class types for each variables.
str(data.frame)

# To review the records for verification.
head(data.frame)
dim(data.frame)

# To check for any NULL values
colSums(!is.na(data.frame))

# To create age group based on the following
#  Silent Generation (SilentGen) : Ages above 77
#  Baby Boomers: Ages 58-76 (born 1946-1964)
#  Generation X (GenX): Ages 43-57 (born 1965-1980)
#  Millennials: Ages 27-42 (born 1981-1996)
#  Generation Z (GenZ): Ages 11-26 (born 1997-2012)
data.frame.bin <-
  data.frame %>%
  mutate(age_bin =
           cut(
             age,
             breaks = c(10, 26, 42, 57, 76, Inf),
             labels = c("GenZ", "Millennials", "GenX",
                        "BabyBoomers", "SilentGen"),
             right = FALSE
           )
  )

# Summarize Shopper age group and percentage in malls.
data.frame.age <- data.frame.bin %>%
  group_by(mall, age_bin) %>%
  summarize(N = n()) %>%
  mutate(total_agebin = sum(N),
         percent_agebin = round((N / total_agebin) * 100, 1))

# Assign colour palettes on each age grouping.
colours <- c("GenZ" = "#00BFFF",
             "Millennials" = "#FF7F50",
             "GenX" = "#808000",
             "BabyBoomers" = "#FFD700",
             "SilentGen" = "#D3D3D3"
)

# Assign colour palettes on each gender grouping.
gender_colours <- c("Male" = "blue", "Female" = "red")

## Dashboard Creation With Shiny packages
ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  # Plot charts by rows using fluidRow.
  dashboardBody(fluidRow(
    box(plotOutput("plot1", height = 500)),
    box(plotOutput("plot2", height = 500)),
    box(
      title = strong("Age Group \n& Malls Selection"),
      # Dropdown Box to select the age group variable to plot
      # Millennials is selected as default as it is the main
      # shopper in malls.
      selectInput(
        "AgeGroup",
        "Select Age Group:",
        choices =
          c(
            "Silent Generation (Ages above 77)" = "SilentGen",
            "Baby Boomers (Ages 58-76)" = "BabyBoomers",
            "Generation X (Ages 43-57)" = "GenX",
            "Millennials (Ages 27-42)" = "Millennials",
            "Generation Z (Ages 11-26)" = "GenZ"
          ),
        selected = "Millennials"
      ),
      br(),
      # 1st CheckBox row of malls for selection. The selected malls 
      # will be plotting together in the charts.
      strong("Please Check the Required Malls:"),
      fluidRow(
        column(
          4, checkboxInput("mall_centrepoint", "Centrepoint", 
                           value = TRUE)
        ),
        column(4, checkboxInput("mall_hougang", "Hougang Mall", 
                                value = TRUE)
        ),
        column(4, checkboxInput("mall_jurong", "Jurong Point", 
                                value = TRUE))
      ),
      # 2nd checkBox row of malls for selection.
      fluidRow(
        column(4, checkboxInput("mall_raffles", "Raffles City", 
                                value = TRUE)
        ),
        column(4, checkboxInput("mall_vivo", "Vivocity", 
                                value = TRUE)
        ),
        column(4, checkboxInput("mall_313", "313 Somerset", 
                                value = TRUE))
      ),
      br(),
      actionButton("filter_button", "Filter Mall"),
      br(),
      br(),
      tags$div(style = "font-size:20px;", strong(p("Observations")), 
      ),
      # Observation section that allow HTML code to be used.
      htmlOutput("myObservation")
    ),
    # Create easy to view races breakdown in this section.
    # It will change upon above selection.
    box(
      br(),
      tags$div(style = "font-size:20px;", strong(
        p("Races Breakdown Based on Age Group & Selected Malls")
      ), ),
      br(),
      br(),
      fluidRow(
        infoBoxOutput("Chinese01"),
        infoBoxOutput("Malay01"),
        infoBoxOutput("Indian01"),
        infoBoxOutput("Eurasian01"),
        infoBoxOutput("Others01")
      ),
      br(),
      br(),
      br(),
      # Create easy to view races breakdown in this section.
      # It will change upon above selection.
      tags$div(style = "font-size:20px;", strong(
        p("Citizenship Breakdown Based on Age Group & Selected Malls")
      ), ),
      br(),
      br(),
      fluidRow(
        infoBoxOutput("SGC"),
        infoBoxOutput("SGPR"),
        infoBoxOutput("StudentPass")
      ),
      br(),
    )
  ))
)

# Define server/dashboard logic
server <- function(input, output) {
  # To filter the malls on dashboard on CheckBox response.
  selected_malls <- reactiveVal(
    c(
      "Centrepoint",
      "Hougang Mall",
      "Jurong Point",
      "Raffles City",
      "Vivocity",
      "313Somerset"
    )
  )
  
  # Update selected malls based on checkbox input
  observeEvent(input$filter_button, {
    selected <- c()
    
    if (input$mall_centrepoint)
      selected <- c(selected, "Centrepoint")
    if (input$mall_hougang)
      selected <- c(selected, "Hougang Mall")
    if (input$mall_jurong)
      selected <- c(selected, "Jurong Point")
    if (input$mall_raffles)
      selected <- c(selected, "Raffles City")
    if (input$mall_vivo)
      selected <- c(selected, "Vivocity")
    if (input$mall_313)
      selected <- c(selected, "313Somerset")
    
    # Update the reactive value with selected malls
    selected_malls(selected)
  })
  
  # Generate 1st plot
  # To understand the demographics (age) visiting each malls.
  output$plot1 <- renderPlot({
    data.frame.age %>%
      filter(mall %in% selected_malls(), 
             age_bin == input$AgeGroup) %>%
      ggplot(aes(
        x = reorder(mall, N),
        y = N,
        group = mall,
        fill = age_bin
      )) +
      geom_col() +
      coord_flip() +
      geom_text(
        data = data.frame.age %>%
          filter(mall %in% selected_malls(), 
                 age_bin == input$AgeGroup),
        aes(label = paste0(
          age_bin, "\n", N, "\n(", percent_agebin, "%)"
        )),
        position = "stack",
        hjust = 1.2,
        color = "black",
        size = 5
      ) +
      scale_fill_manual(values = colours) +
      labs(
        title = "Visitors Count by Age Group & Selected Malls",
        x = "Malls",
        y = "No of Visitors",
        fill = "Age Group"
      ) +
      theme(
        legend.position = "none",
        title = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 14, face = "plain"),
        axis.text.x = element_text(
          angle = 45,
          size = 14,
          hjust = 1
        ),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
      )
  })
  
  # Generate 2nd plot
  # To understand the demographics (age/gender) visiting 
  # each stores.
  output$plot2 <- renderPlot({
    # Summarize Shopper age and gender group by store level 
    # based on selection.
    data.frame.binrev <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, store, gender) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin, store) %>%
      mutate(total_agebin = sum(N),
             percent_gender = round((N / total_agebin) * 100, 1)) %>%
      mutate(gender = factor(gender, 
                             levels = c("Male", "Female"))) %>%
      filter(age_bin == input$AgeGroup)
    
    # To plot the chart with malls filtered.
    ggplot(data = data.frame.binrev, aes(
      x = reorder(store, -N, sum),
      y = N,
      fill = age_bin,
      colour = gender
    )) +
      geom_col(alpha = 0.5,
               position = "stack",
               size = 1) +
      geom_text(
        aes(
          label = paste0(gender, "\n", percent_gender, "%"),
          colour = gender
        ),
        position = position_stack(vjust = 0.5),
        hjust = 0.5,
        vjust = 0.8,
        color = "black",
        size = 4
      ) +
      scale_fill_manual(values = colours) +
      scale_colour_manual(values = gender_colours) +
      labs(
        title ="Selected Malls' Store's Demographics by Age Group & Gender",
        x = "Stores",
        y = "No of Visitors",
        fill = "Age Group"
      ) +
      theme(
        legend.position = "none",
        title = element_text(size = 14, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(
          angle = 45,
          size = 14,
          hjust = 1
        ),
        axis.title.y = element_text(size = 14, face = "plain"),
        axis.text.y = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()
      )
  })
  
  # Generate InfoBox
  # To easily understand the demographics (races) 
  # visiting each stores by using InfoBox.
  output$Chinese01 <- renderInfoBox({
    # Summarize Shopper race (Chinese)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, race) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_race = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, race == "Chinese")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      chinese_percent <- summarized_data$percent_race[1]
      
      infoBox(
        title = "Chinese",
        value = tags$div(paste0(chinese_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("person"),
        color = "red",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Chinese",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("person"),
        color = "red",
        fill = FALSE
      )
    }
  })
  
  output$Malay01 <- renderInfoBox({
    # Summarize Shopper race (Malay)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, race) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_race = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, race == "Malay")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      malay_percent <- summarized_data$percent_race[1]
      
      infoBox(
        title = "Malay",
        value = tags$div(paste0(malay_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("person"),
        color = "green",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Malay",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("person"),
        color = "green",
        fill = FALSE
      )
    }
  })
  
  output$Indian01 <- renderInfoBox({
    # Summarize Shopper race (Indian)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, race) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_race = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, race == "Indian")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      indian_percent <- summarized_data$percent_race[1]
      
      infoBox(
        title = "Indian",
        value = tags$div(paste0(indian_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("person"),
        color = "purple",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Indian",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("person"),
        color = "purple",
        fill = FALSE
      )
    }
  })
  
  output$Eurasian01 <- renderInfoBox({
    # Summarize Shopper race (Eurasian)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, race) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_race = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, race == "Eurasian")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      eurasian_percent <- summarized_data$percent_race[1]
      
      infoBox(
        title = "Eurasian",
        value = tags$div(paste0(eurasian_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("person"),
        color = "yellow",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Eurasian",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("person"),
        color = "yellow",
        fill = FALSE
      )
    }
  })
  output$Others01 <- renderInfoBox({
    # Summarize Shopper race (Others)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, race) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_race = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, race == "Others")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      others_percent <- summarized_data$percent_race[1]
      
      infoBox(
        title = "Others",
        value = tags$div(paste0(others_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("person"),
        color = "teal",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Others",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("person"),
        color = "teal",
        fill = FALSE
      )
    }
    
  })
  
  output$SGC <- renderInfoBox({
    # Summarize Shopper citizenship (Singapore Citizen)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, citizenship) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_citizen = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, citizenship == "SG Citizen")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      sgc_percent <- summarized_data$percent_citizen[1]
      
      infoBox(
        title = "SG Citizen",
        value = tags$div(paste0(sgc_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("house"),
        color = "red",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "SG Citizen",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("house"),
        color = "red",
        fill = FALSE
      )
    }
  })
  
  output$SGPR <- renderInfoBox({
    # Summarize Shopper itizenship (Singapore PR)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, citizenship) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_citizen = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, citizenship == "SG PR")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      sgpr_percent <- summarized_data$percent_citizen[1]
      
      infoBox(
        title = "SG PR",
        value = tags$div(paste0(sgpr_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("house"),
        color = "light-blue",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "SG PR",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("house"),
        color = "light-blue",
        fill = FALSE
      )
    }
  })
  
  output$StudentPass <- renderInfoBox({
    # Summarize Shopper citizenship (Student Pass)
    summarized_data <- data.frame.bin %>%
      filter(mall %in% selected_malls()) %>%
      group_by(age_bin, citizenship) %>%
      summarize(N = n(), .groups = "drop") %>%
      group_by(age_bin) %>%
      mutate(total_agebin = sum(N),
             percent_citizen = round((N / total_agebin) * 100, 1)) %>%
      filter(age_bin == input$AgeGroup, citizenship == "Student Pass")
    
    # Provide the display of the InfoBox
    if (nrow(summarized_data) > 0) {
      student_percent <- summarized_data$percent_citizen[1]
      
      infoBox(
        title = "Student Pass",
        value = tags$div(paste0(student_percent, "%"), 
                         style = "font-size: 40px;"),
        icon = icon("house"),
        color = "lime",
        fill = FALSE
      )
    } else {
      infoBox(
        title = "Student Pass",
        value = tags$div("0%", style = "font-size: 40px;"),
        icon = icon("house"),
        color = "lime",
        fill = FALSE
      )
    }
  })
  
  # Display the observation summary using HTML coding.
  output$myObservation <- renderText({
    HTML(
      "
    <div style='font-size: 18px;'>
      1) Majority Shoppers : <B>Millennials</B><BR>
      2) Baby Boomers & Silent Generation like shopping in <B>Hougang Mall</B>.<BR>
      3) Most of the Age Group except Baby Boomers & Silent Generation like shopping in <B>Jurong Point.</B><BR>
      4) Major Purchased Items by Age Group :<BR>
      <div style='margin-left: 20px;'>
         a) <B>Silent Generation</B>: Health Supplement<br>
         b) <B>Baby Boomers</B>: Health Supplement<br>
         c) <B>Generation X</B>: Health Products, Jewellery<br>
         d) <B>Millennials</B>: Jewellery, Health Products<br>
         e) <B>Generation Z</B>: Health Supplement, Fashion<br>
      </div>
      </div>
         "
    )
  })
}

# Run the dashboard result using shinyApp 
shinyApp(ui, server)

sessionInfo()

# End
