# libraries that we need
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}
download("injuries.tsv.gz") #gzip file. `vroom` will read it. 
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")



# code to get the products for filtering
prod_codes <- setNames(products$prod_code, products$title)

# useful factor lumping function
count_top <- function(df, var, n=5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

#<< ui
ui <- fluidPage(
  
  fluidRow(
    # user input select product
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    # Add an input control that lets the user decide how many rows to show in the summary tables
    column(2,numericInput("rows", "Number of Rows", min = 1, max = 10, value = 5)),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    # output from the server as tables
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    # output from server as a plot
    column(12, plotOutput("age_sex"))
  ),
  fluidRow(
    column(2, actionButton("prev_story", "Previous story")),
    column(2, actionButton("next_story", "Next story")),
    column(8, textOutput("narrative"))
  )
)
#>>

#<< server
server <- function(input, output, session) {
  # make a reactive to select a product code
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  # Find the maximum possible of rows.
  max_no_rows <- reactive(
    max(length(unique(selected()$diag)),
        length(unique(selected()$body_part)),
        length(unique(selected()$location)))
  )
  
  # Update the maximum value for the numericInput based on max_no_rows().
  observeEvent(input$code, {
    updateNumericInput(session, "rows", max = max_no_rows())
  })
  
  table_rows <- reactive(input$rows - 1)
  
  # output table for diag
  output$diag <- renderTable(count_top(selected(), diag, n = table_rows()), width = "100%")
  
  # output table for body_part
  output$body_part <- renderTable(count_top(selected(), body_part, n = table_rows()), width = "100%")
  
  # output table for location
  output$location <- renderTable(count_top(selected(), location, n = table_rows()), width = "100%")
  
  # Reactive for getting rate of injury per 10k and raw number
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  # output a plot
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(y = "Estimated number of injuries")
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(y = "Injuries per 10,000 people")
    }
  }, res = 96)
  
  # Store the maximum possible number of stories.
  max_no_stories <- reactive(length(selected()$narrative))
  
  # Reactive used to save the current position in the narrative list.
  story <- reactiveVal(1)
  
  # Reset the story counter if the user changes the product code. 
  observeEvent(input$code, {
    story(1)
  })
  
  # When the user clicks "Next story", increase the current position in the
  # narrative but never go beyond the interval [1, length of the narrative].
  # Note that the mod function (%%) is keeping `current`` within this interval.
  observeEvent(input$next_story, {
    story((story() %% max_no_stories()) + 1)
  })
  
  # When the user clicks "Previous story" decrease the current position in the
  # narrative. Note that we also take advantage of the mod function.
  observeEvent(input$prev_story, {
    story(((story() - 2) %% max_no_stories()) + 1)
  })
  
  output$narrative <- renderText({
    selected()$narrative[story()]
  })
}

shinyApp(ui, server)
