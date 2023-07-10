library(epiR)
library(dplyr)
library(ggplot2)
library(shiny)

respiratory_by_state <- read.delim("data/CDC/respiratory.txt") # assumes you are in root of the project
respiratory_by_state <- respiratory_by_state[, 2:8]
respiratory_by_state$Year <- substr(respiratory_by_state$Month.Code, 1, 4)


population_state_year <- read.delim("data/CDC/population_state_year.txt")
population_state_year <- population_state_year[,c("Population", "State", "Year")]
population_state_year <- na.omit(population_state_year)

respiratory_by_state <- merge(respiratory_by_state, population_state_year, by = c("State", "Year"))

respiratory_by_state$crude_rate <- respiratory_by_state$Deaths/respiratory_by_state$Population * 100000

CI <- apply(respiratory_by_state[, c("Deaths", "Population")], 1, function(x) {
  poisson.test(x[1], T = x[2] / 100000)$conf.int
})

respiratory_by_state$ci_lower <- CI[1,]
respiratory_by_state$ci_upper <- CI[2,]


names(respiratory_by_state) <- c("state", "year", "state_code", "month_verbal", "month",
                                 "death_cause", "icd_10", "deaths", "population",
                                 "rate", "ci_lower", "ci_upper")


respiratory_by_state$month <- paste(respiratory_by_state$month, "/28", sep = "")

respiratory_by_state$death_cause <- gsub("#", "", respiratory_by_state$death_cause)

# Remove specified rows
respiratory_by_state <- subset(respiratory_by_state, 
                               !(death_cause %in% c("Other acute lower respiratory infections (J20-J22,U04)", 
                                                    "Pneumoconioses and chemical effects (J60-J66,J68,U07.0)")))



respiratory_by_state$month <- as.Date(respiratory_by_state$month, format = "%Y/%m/%d")

#STATIC

filtered_data <- subset(respiratory_by_state, state == "Alabama" & death_cause == "Influenza and pneumonia (J09-J18)")


years <- unique(format(filtered_data$month, "%Y"))

# Corrected dates for breaks
breaks <- as.Date(c("2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31",
                    "2019-03-31", "2019-06-30", "2019-09-30", "2019-12-31", 
                    "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
                    "2021-03-31", "2021-06-30", "2021-09-30", "2021-12-31"), 
                  format = "%Y-%m-%d")

# Corresponding labels
labels <- c("3/18", "6/18", "9/18", "12/18",
            "3/19", "6/19", "9/19", "12/19",
            "3/20", "6/20", "9/20", "12/20",
            "3/21", "6/21", "9/21", "12/21")

# Plot with ggplot2
ggplot(filtered_data, aes(x = month, y = rate)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
  geom_smooth(method = "loess", se = TRUE, color = "red", fill = "pink", alpha = 0.6) +
  scale_x_date(breaks = breaks, labels = labels, date_minor_breaks = "1 month") +
  labs(x = "Month", y = "Death Rate", 
       title = "Death rate from Influenza and pneumonia and 95% confidence interval over time in Alabama",
       fill = "95% Confidence Interval") +
  theme_minimal()


#SHINY

ui <- fluidPage(
  titlePanel("Death rate by cause and state over time"),
  fluidRow(
    column(4,
           selectInput("state", "Choose a state:", 
                       choices = unique(respiratory_by_state$state))
    ),
    column(4, offset = 1,
           selectInput("cause", "Choose a cause:", 
                       choices = unique(respiratory_by_state$death_cause))
    )
  ),
  mainPanel(
    plotOutput("deathPlot", height = "600px", width = "1200px")
  )
)


server <- function(input, output) {
  output$deathPlot <- renderPlot({
    filtered_data <- subset(respiratory_by_state, 
                            state == input$state & death_cause == input$cause)
    
    
    breaks <- as.Date(c("2018-03-31", "2018-06-30", "2018-09-30", "2018-12-31",
                        "2019-03-31", "2019-06-30", "2019-09-30", "2019-12-31", 
                        "2020-03-31", "2020-06-30", "2020-09-30", "2020-12-31",
                        "2021-03-31", "2021-06-30", "2021-09-30", "2021-12-31"), 
                      format = "%Y-%m-%d")
    
    labels <- c("3/18", "6/18", "9/18", "12/18",
                "3/19", "6/19", "9/19", "12/19",
                "3/20", "6/20", "9/20", "12/20",
                "3/21", "6/21", "9/21", "12/21")
    
    
    ggplot(filtered_data, aes(x = month, y = rate)) +
      geom_line() +
      geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2) +
      geom_smooth(method = "loess", se = TRUE, color = "red3", fill = "pink", alpha = 0.6) +
      scale_x_date(breaks = breaks, labels = labels, date_minor_breaks = "1 month") +
      labs(x = "Month", y = "Death Rate", 
           title = paste("Death rate from", input$cause, 
                         "and 95% confidence interval over time in", input$state),
           fill = "95% Confidence Interval") +
      theme_minimal() +
      theme(text = element_text(size = 18), 
            axis.text = element_text(size = 16), 
            axis.title = element_text(size = 18)
      )
  })
}

shinyApp(ui = ui, server = server)


