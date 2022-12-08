#
# Author: Mitchi Kamigaki-Baron
# This is a Shiny web application. You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#

#if (interactive()) { #adding this as some apps for interactive colors added this, surrounded whole code in this bracket

#relevant libraries used
library(shiny)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)
library(ggpubr)
library(ggplot2)
library(colourpicker)
library(emojifont)
library(rsconnect)
#note to self: you can go to https://shiny.rstudio.com/articles/tag-glossary.html for more help

#importing dataset
bcl <- read_csv("bcl-data.csv")

#beginning the ui
ui <- fluidPage( #positioned as top to bottom as it appears on website
  titlePanel("BC Liquor Store Data"), #title page
  h5("Welcome to Mitchi Kamigaki-Baron's shiny app"), #header
  br(), #line break
  sidebarPanel( #side panel positioning
    img(width = "50%", src='bcl.jpg', align = "center"), # add an image (must also add image into a folder called www)
    sliderInput("priceInput", "Price", 0, 100, value = c(25, 40), pre = "$"), #price slider
    radioButtons("typeInput", "Type", choices = c("BEER", "REFRESHMENTS", "SPIRITS", "WINE")), #radio buttons, alcohol type
    pickerInput(inputId= "countryInput", label= "Country", choices = c("CANADA", "UNITED KINGDOM", "FRANCE", "ITALY", "JAPAN", "UNITED STATES OF AMERICA", "AUSTRALIA", "IRELAND"), selected = c("CANADA", "UNITED KINGDOM", "FRANCE", "ITALY", "JAPAN", "UNITED STATES OF AMERICA", "AUSTRALIA", "IRELAND"), options = list(`actions-box` = TRUE, `select-all-text` = "All Countries (Select)"), multiple = TRUE)
  ) #country picker, to use functionality of multiple selections To use this functionality must use pickerInput() instead of selectInput()
  ,
  mainPanel( #main panel position
    
    plotOutput("alcohol_hist") #placeholder wont do anything until you place in logic in function bit.
  ),
  a(href="https://github.com/daattali/shiny-server/blob/master/bcl/data/bcl-data.csv", "Link to the original dataset 🍷 ") #hehe I added an emoji! 😸
  #adding link to original data set
  
)

server <- function(input, output) { #requires input & output elements (sometimes additional linking is needed) for this to work
  #observe(cat(input$priceInput)) #useful for debugging
  observe({
    print(input$countryInput)
  })
  
  filtered_data <-
    reactive({
      bcl %>% filter(Price > input$priceInput[1] & #filters with labeled input titles, this one is for price limits
                       Price < input$priceInput[2] &
                       Type == input$typeInput & #type input
                       Country == input$countryInput) #country input
      
    })
  
  output$alcohol_hist <- #output element for the histogram plot portion of this shiny app
    renderPlot({
      filtered_data() %>%ggplot(aes(Alcohol_Content)) + geom_histogram(color="grey60", fill= "cornsilk") + #ggplot histogram of data, and adding color
        geom_density(color=NA, fill="blue", alpha=0.2)+ geom_line(stat='density', color="red") # adding an extra layer to this ggplot
      
    })
  
  
  
  output$data_table <- #output placeholder for data table
    renderTable({
      filtered_data()
    })
  
}
#} #enclosing bracket for interactive bit

shinyApp(ui = ui, server = server)


#help(sidebarLayout)
#sidebarLayout(sidebarPanel(), mainPanel()) #gives you HTML to see what is going on behind the hood

# https://stat545.stat.ubc.ca/notes/notes-b04/ use interactive code
