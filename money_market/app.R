library(shinyjs)
library(shiny)
library(readr)
library(plotly)
source("../src/ui_functions.r")
source("../src/LM_functions.r", local = T)

ui <- fluidPage(
    includeCSS("../www/style.css"),
    useShinyjs(),
    withMathJax(),
    
    tags$script(HTML('MathJax.Hub.Config({
                   jax: ["input/TeX","output/HTML-CSS"],
                   displayAlign: "left"
                   });')),
    
    tags$head(
        tags$link(rel="stylesheet", 
                  href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", 
                  crossorigin="anonymous"),
        tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", 
                    crossorigin="anonymous")
    ),
    
    titlePanel("La courbe LM"),
    
    div(id = "page-wrapper",
        fluidRow(
            column(3,
                   div(id = "settings",
                       ui_LM_params1,
                       ui_LM_params2,
                       fluidRow(
                           column(4,
                                  numericInput("ystar","\\(y*\\)",0,10000,value = 2000, step=10)
                           )
                       ),
                       fluidRow(
                           column(6,
                                  sliderInput("ymax","y max", 100,10000,4000,step = 100)          
                           ),
                           column(6,
                                  sliderInput("Mmax","M max", 100,4000,2000,step = 100)       
                           )
                       ),
                       fluidRow(
                           column(4,
                                  actionButton("shock", "Ajouter un choc", `data-toggle`="collapse", `data-target`="#shock_set", style = "margin-bottom: 15px;")
                           )
                       ),
                       div(id = "shock_set", class = "collapse",
                           fluidRow(
                               column(5,
                                      LM_shock
                               ),
                               column(4,
                                    numericInput("new_value_LM","Nouvelle Valeur", value = NULL)
                               )
                           )
                       )
                   ),
                   div(id = "model",
                       h4("Équation de demande:"),
                       helpText(class = "math",
                                "$$\\bar{M} = p(L_y \\cdot y + L_r \\cdot r)$$"
                       ),
                       h4("Équation de LM:"),
                       helpText(class = "math",
                                "$$r = \\frac{1}{L_r}(\\frac{\\bar{M}}{p} - L_y y)$$"
                       )
                   )
            ),
            column(9,
                   plotlyOutput("RM_LM_plot", height = 500)
            )
        )
    )
)

server <- function(session, input, output) {
    values <- reactiveValues(
        shock = F,
        params = c(),
        shocked_params = c(),
        eq = list()
    )
    
    observeEvent({
        input$p
        input$ly
        input$lr
        input$ystar
        input$Ms
    },{
        values$params = c("p" = input$p,
                          "ly" = input$ly,
                          "lr" = input$lr,
                          "ystar" = input$ystar,
                          "Ms" = input$Ms)
        
        values$eq$r = unname(1/input$lr*(input$Ms/input$p-input$ly*input$ystar))
    })
    
    observeEvent({
        input$shock
    },{
        values$shock = !values$shock
        if(values$shock){
            updateActionButton(session,"shock","Retirer le choc")
        }
        else{
            updateActionButton(session,"shock","Ajouter un choc")
        }
    })
    
    observeEvent({
        input$shocked_var_LM
        input$new_value_LM
    },{
        if(values$shock){
            values$shocked_params = values$params
            if(!is.na(input$new_value_LM)){
                values$shocked_params[input$shocked_var_LM] = input$new_value_LM
                values$new_eq$r = unname(1/values$shocked_params["lr"]*(values$shocked_params["Ms"]/values$shocked_params["p"]-values$shocked_params["ly"]*values$shocked_params["ystar"]))
            }
        }
    })
    
    output$RM_LM_plot <- rm_lm_plot(session,input,output,values)
    
}

shinyApp(ui = ui, server = server)

