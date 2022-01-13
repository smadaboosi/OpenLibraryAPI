#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(httr)
library(jsonlite)
library(shinyWidgets)
library(stringr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(DT)
library(shinyjs)
source("functions.R")

url <- "https://openlibrary.org/"
covers <- "http://covers.openlibrary.org/b/id/"
# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Shantanu Madaboosi"),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(
                                         id = 'links',
                                         menuItem(text = "OpenLibrary API", tabName = "openLibrary",icon=icon("book")),
                                         menuItem(text = "Statistical Computing",icon=icon("signal"),
                                                  href="https://smadaboo.shinyapps.io/StatisticalComputing-Presentation/"),
                                         menuItem(text = "LinkedIn Profile",icon = icon("linkedin"),
                                                  href="https://www.linkedin.com/in/smboosi/")
                                         
                                     )),
                    dashboardBody(
                        tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                            tags$script(src="functions.js")
                        ),
                        # Title
                        HTML('<span><h3>
                                <i class="fas fa-atlas"></i>
                                OpenLibrary Search</h3> <p style="margin-left:2vw"><i>Developed by</i><b> Shantanu Madaboosi</b></p></span>'),
                        # Search bar for user to input search terms
                        fluidRow(
                            column(4,
                                   textInput(
                                       inputId = "search",
                                       label = "",
                                       placeholder = "Enter the title of a book",
                                       width = "600px"
                                   )),
                            conditionalPanel(condition="input.search != ''",
                            column(1,
                                   # Search button
                                   actionButton("searchButton","Search", icon = icon("search"))
                                   ))),
                        conditionalPanel(condition="input.search != ''",
                                         htmlOutput("message"),
                                         # UI Plot for output
                                         plotlyOutput(outputId = "histogram"),
                                         # Table output of data
                                         dataTableOutput("info")
                        )
                        
                    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    data <- reactiveValues(e=NULL, 
                           res=NULL, 
                           docs=NULL)
    # Get click events
    observe({
        data$e <- event_data("plotly_click")
    })
    
    observeEvent(input$search, {
        data$e <- NULL
        data$res <- NULL
        data$docs <- NULL
        output$icon <- renderUI({
        })
        output$message <- renderUI({
            message <- HTML('<p>
                                <i class="fas fa-info-circle"></i>
                                <i>Press the <b>search</b> button to fetch results</i>
                            </p>')
        })
    })

    # Event Triggered: Search Button
    observeEvent(input$searchButton, {
        data$res <- search.API(input$search)
        data$docs <- data$res$docs
        data$e <- NULL
        
        output$message <- renderUI({
            message <- HTML('<p>
                                <i class="fas fa-info-circle"></i>
                                <i>Select a bar on the plot to see results from that year</i>
                            </p>')
            
        })
        
        output$histogram <- renderPlotly({

            e <- data$e
            
            if(is.null(e)){
                sel.yr <- min(data$docs$first_publish_date)
            }else{
                sel.yr <- event_data("plotly_click")$x
            }
            
            `Published Year` <- data$docs$first_publish_year
            
            Selected <-factor(ifelse(`Published Year` == sel.yr, 'Yes','No'))
            
            p <- ggplot() + 
                geom_bar(aes(x=`Published Year`, 
                             fill=Selected)) +
                scale_fill_manual(name = "Selected", values=c("steelblue","red")) +
                geom_vline(xintercept=sel.yr,
                           colour="red",
                           linetype = "dashed") +
                xlab("Year") + 
                ylab("#") + 
                theme_bw() +
                theme(plot.background = element_rect(fill="#ecf0f5"),
                      panel.background = element_rect(fill="#ecf0f5"),
                      panel.border = element_blank(),
                      panel.grid.major = element_line(color = "#d3d6db"),
                      legend.position = "none")
                ggplotly(p)
            
        })
        
        output$info <- renderDataTable({
            e <- data$e
            
            if(is.null(e)){
                filtered.data <- data.frame("first_publish_year"=data$docs$first_publish_year, 
                                            "title"=data$docs$title, 
                                            "key"=data$docs$key, 
                                            "cover_i"=data$docs$cover_i)
                
                filtered.data$cover_image <- paste0('<a href="',url,filtered.data$key,'" target="_blank">',
                                                    '<img src="',covers,
                                                    filtered.data$cover_i,
                                                    '.jpg" width="80"></img></a>')
                
                filtered.data$link <- paste0('<a href="',url,filtered.data$key,'" target="_blank">',
                                             '<i class="fas fa-info-circle"></i></a>')
                
                table.out <- data.frame("Book"=filtered.data$cover_image,
                                        "Info"=filtered.data$link,
                                        "Title"=filtered.data$title,
                                        "Year Published"=filtered.data$first_publish_year)
                
                datatable(table.out, escape = FALSE)
                
            }else{
                output$message <- renderUI({
                    message <- HTML(paste0('<p>
                                                <i class="fas fa-info-circle"></i>
                                                <i>Showing Results published in</i> <b style="color:red">',
                                                event_data("plotly_click")$x,'</b>
                                           </p>'))
                    
                })
                
                filtered.data <- data.frame("first_publish_year"=data$docs$first_publish_year, 
                                            "title"=data$docs$title, 
                                            "key"=data$docs$key, 
                                            "cover_i"=data$docs$cover_i)
                
                filtered.data$cover_image <- paste0('<a href="',url,filtered.data$key,'" target="_blank">',
                                                    '<img src="',covers,
                                                    filtered.data$cover_i,
                                                    '.jpg" width="80"></img></a>')
                
                filtered.data$link <- paste0('<a href="',url,filtered.data$key,'" target="_blank">',
                                             '<i class="fas fa-info-circle"></i></a>')
                
                filtered.years <- filter(filtered.data, 
                                         first_publish_year == event_data("plotly_click")$x)
                
                table.out <- data.frame("Book"=filtered.years$cover_image,
                                        "Info"=filtered.years$link,
                                        "Title"=filtered.years$title,
                                        "Year Published"=filtered.years$first_publish_year)
                
                datatable(table.out, escape = FALSE)
            }
            
        })
    })
    

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)






















