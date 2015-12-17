library(shiny)
library(shinyjs)
library(plyr)
library(DT)
library(rCharts)
Products<-levels(FoodDatabase$Product)

shinyUI(fluidPage(
                        title = "Precision Ketogenic Therapy Meal Making Database",
                        shinyjs::useShinyjs(),
                        tags$head(includeCSS(file.path("www", "app.css"))),
                        div(
                          id = "titlePanel",
                          "Precision Ketogenic Therapy Meal Making Database"
                            ),
  
  # Select storage type and show a description about it
 fluidRow(
    column(4, 
     wellPanel(id = "leftPanel",
      div(
        id = "storageTypePanel",
        strong(p("Precision Ketogenic Therapy", align = "center")),
        hr(),
        p("Meal Making Database", align = "center")),
   
                       
      div(
        selectInput(inputId = "show_vars",label="Product Info",choices = c("Choose" = "",names(FoodDatabase)), multiple = TRUE, selectize=TRUE,
                    selected = c(
                    "NDID",
                    "Product",
                    "Pro (%)", 
                    "Fat (%)",
                    "CHO (%)",
                    "Cat. 1"
                    )
                    ),
        helpText('Click to add more, backspace to delete')),

        
        
        hr(),
      div(
        id = "appDesc",
        strong(p('Download Options')),
          p(class = 'text-center', downloadButton('wholeDB', 'Download All')),
        h6(class = 'text-center',"Or"),
          selectInput(inputId = "col",label="",choices = c("Choose Some (Click here)" = "",levels(FoodDatabase$`Cat. 1`)), multiple = TRUE, selectize=TRUE),
          p(class = 'text-center', downloadButton('x6', 'Download')),
          helpText('Contact prb@ufl.edu for more information.')
      )
    )
    
    ),
    
    column(8, 
      wellPanel(
      tabsetPanel(
        id = "mainTabs", type = "tabs", value = "1",
        
                  tabPanel(
                    title = "About", id ="descTab", value = "descTab",
                    
                    br(),
                    div(
                      id = "storageDesc",
                      h3("Welcome to the PKT Meal Making Database!", class = 'text-center'),
                      fluidRow(
                        column(12,  
                               div(showOutput("Composition", "highcharts"), class = "span6"))
                        ),
                      fluidRow(
                        column(4,
                        strong(p("The PKT Database")),
                        HTML("<ul><li>Fat, carbohydate, and protein content of foods for over 1,600 products commonly used in PKT</li><li>Updated yearly to account for changes in the marketplace</li><li>Contains a recent price for each product</li><li> Downloadable as an excel sheet</li></ul>")
                        ),
                        column(4,
                        strong(p("Favorites")),
                        HTML("<ul><li>Select foods from the database</li><li>Download favorite foods as a list</li><li>Build a PKT recipe with favorite foods</li><li> Downloadable as an excel sheet</li><li>Select foods to build a PKT recipe</li></ul>")
                        ),
                        column(4,
                        strong(p("Recipe Builder")),
                        HTML("<ul><li>Enter a PKT diet prescription</li><li>Create a PKT meal using foods from your favorites list</li><li>Save the meal for use or send</li></ul>")              
                        )))
          
                    ),

          tabPanel(
            title = "Search", id = "FoodDatabase01", value = "2",
            br(),
              div(style = 'overflow-x: scroll',DT::dataTableOutput('maintable')),
              actionButton('clear1', 'Clear Selected')
          ),
          
          tabPanel(
            title = "Favorites", id = "codeTab2", value = "3",
              div(id = "form",
              br(),
              div(style = 'overflow-x: scroll',DT::dataTableOutput('favs'))),
              p(class = 'text-center', downloadButton('favoritestable', 'Download All'))
          ),
     
         
        
        # Build the form
        tabPanel(
          title = "Recipe Builder", id = "playTab", value = "4",
          br(),
          div(id = "form",
              fluidRow(
                column(4,
                       strong(p('Before you begin')),
                       p("Diet prescription information is required. Your favorites tab should have a list of only the ingredients you want to make a meal with. If it has more this won't work right now. If you edit proportions of ingredients in the table and then go back to the ingredient selector to add a new product it will reset your progress. The colors in the table are supposed to help you see where the levels of the macronutrients are coming from."),
                       radioButtons('menutype', 'Menu Type', c("Meal","Snack"), selected = "Meal", inline = FALSE)
                       ),
                
                column(4,
                     strong(p('Enter Diet Prescription')),
                     numericInput(
                       inputId = "ratio",
                       label = strong("Ketogenic Ratio"),
                       value = 4.0
                     ),
                     numericInput(
                       inputId = "calories",
                       label = strong("Daily Calories"),
                       value = 1500
                     ),
                     numericInput(
                       inputId = "protein",
                       label = strong("Daily Protein"),
                       value = 20
                     ),
                     numericInput(
                       inputId = "snackcalories",
                       label = strong("Snack Calories"),
                       value = 200
                     ),
                     numericInput(
                       inputId = "mealnumber",
                       label = strong("Meal Number"),
                       value = 3
                     ),
                     numericInput(
                       inputId = "snacknumber ",
                       label = strong("Snack Number"),
                       value = 2)
              ),
              
              column(4,
                     strong(p('Diet Prescription Parameters')),
                     uiOutput("result1"),
                     uiOutput("result2"),
                     uiOutput("result3"),
                     uiOutput("result4"),
                     uiOutput("result5"),
                     uiOutput("result6"),
                     uiOutput("result7"),
                     uiOutput("result8"),
                     uiOutput("result9"),
                     uiOutput("result10")
              )),
              br(),
              strong(p('(2) Adjust Ingredients')),
              div(style = 'overflow-x: scroll',d3tfOutput('meals2')),
              strong(p('(3) Review Proportions')),
             # tableOutput("weighedfoods"),
            #  uiOutput("net_sums"),

              div(style = 'overflow-x: scroll',d3tfOutput('weighedfoods2')),
              strong(p('(4) Review Goals')),
              uiOutput("CHOresult"),
              uiOutput("PROresult"),
              uiOutput("FATresult"),
              uiOutput("CALresult"),
              uiOutput("RATIOresult"),
              br(),
              uiOutput("FINAL")
          )
        ),
        # Build the form
        tabPanel(
          title = "Suggest a Food/Meal", id = "descTab", value = "10",
          div(id = "foodform",
              textInput("name", "Name:", value = "Your name"),
              numericInput("foodform", label = "Number of Foods", value="1"),
              uiOutput("foods"))
        )     ,
        # Build the form
        tabPanel(
          title = "TEST RECIPE BUILDER", id = "descTab", value = "6",
          div(id = "foodform",
              tags$head(
                tags$style(type="text/css", "input { font-size:10px; width:40px; display:inline-block; }"),
                tags$style(type="text/css", "#lml, #lmml, #lmh, #lmconf { font-size:10px; width:100px; display:inline-block; }"),
                tags$style(type="text/css", "label { font-size:10px; display:inline-block; }")
              ),
              
              
              p("Meal Name"),
              textInput("name", "Name:", value = "Meal name"),
              selectizeInput(
                'ingred1', 'Ingredient 1', choices = Products),
              numericInput("num", label = "Adjust by", value="0"),
              submitButton(text = "Apply Changes", icon = NULL))          ))
          
        )     
        
        
       ))         
     )
    )
