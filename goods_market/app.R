library(shinyjs)
library(shiny)
library(readr)
library(plotly)
source("../src/ui_functions.r")
source("../src/IS_functions.r", local = T)

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
  
  titlePanel("La courbe IS"),
  
  div(id = "page-wrapper",
      fluidRow(
        column(3,
               div(id = "settings",
                   fluidRow(
                     column(4,
                        numericInput("alpha","\\(\\alpha\\)",0,1,value = 0.6,step=0.05)
                     ),
                     column(4,
                      numericInput("iy","\\(I_y\\)",0,1,value = 0.05,step=0.05)
                     ),
                     column(4,
                      numericInput("ir","\\(I_r\\)",-1000,1000,value = -100,step=10)
                     )
                   ),
                   fluidRow(
                     column(4,
                      numericInput("cpi","\\(C_{\\pi}\\)",0,1000,value = 200,step=10)
                     ),
                     column(4,
                      numericInput("bari","\\(\\bar{I}\\)",0,1000,value = 350,step=10)
                     ),
                     column(4,
                      numericInput("g","\\(g\\)",0,1000,value = 350,step=10)
                     )
                   ),
                   fluidRow(
                     column(6,
                      numericInput("r0","\\(r_0\\)",0,10,value = 5, step=1)
                     ),
                     column(6,
                      sliderInput("ymax","y max", 100,10000,4000,step = 100)       
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
                              IS_shock
                         ),
                         column(4,
                                numericInput("new_value_IS","Nouvelle Valeur", value = NULL)
                         )
                       ),
                       fluidRow(
                         column(10,
                              checkboxInput("show_path", "Montrer le chemin")  
                          )
                       )
                   )
               ),
               div(id = "model",
                   h4("Équation de demande:"),
                   helpText(class = "math",
                            "$$Y^d = (\\alpha + I_y) \\cdot y + I_r \\cdot r_0 + C_{\\pi} + \\bar{I} + g$$"
                   ),
                   h4("Niveau d'équilibre du revenu:"),
                   helpText(class = "math",
                              "$$y* = \\frac{I_r \\cdot r_0 + C_{\\pi} + \\bar{I} + g}{1-\\alpha - I_y}$$"
                   ),
                   h4("Équation de IS:"),
                   helpText(class = "math",
                            "$$r = \\frac{(1-\\alpha - I_y) \\cdot y - C_{\\pi}-\\bar{I} -g}{I_r}$$"
                   )
               )
        ),
        column(9,
            tabsetPanel(
               tabPanel("45°, épargne et investissement",
                  plotlyOutput("fortyfivedegrees_IeqS", height = 500)
               ),
               tabPanel("45° et IS",
                  plotlyOutput("fortyfivedegrees_IS", height = 500)
               )
            )
          )
        )
    )
)

server <- function(session, input, output) {
  values <- reactiveValues(
    shock = F,
    params = c(),
    shocked_params = c()
  )
  
  observeEvent({
    input$alpha
    input$iy
    input$ir
    input$cpi
    input$bari
    input$g
  },{
    values$params = c("alpha" = input$alpha,
                      "iy" = input$iy,
                      "ir" = input$ir,
                      "r" = input$r0,
                      "cpi" = input$cpi,
                      "bari" = input$bari,
                      "g" = input$g)
    
    values$eq$y = (input$ir*input$r0+input$cpi+input$bari+input$g)/(1-input$alpha-input$iy)
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
    input$shocked_var_IS
    input$new_value_IS
  },{
    # browser()
    if(values$shock){
      values$shocked_params = values$params
      if(!is.na(input$new_value_IS)){
        values$shocked_params[input$shocked_var_IS] = input$new_value_IS
        values$new_eq$y = unname((values$shocked_params["ir"]*values$shocked_params["r"]+values$shocked_params["cpi"]+values$shocked_params["bari"]+values$shocked_params["g"])/(1-values$shocked_params["alpha"]-values$shocked_params["iy"]))
      }
    }
  })
  
  output$fortyfivedegrees_IS <- fortyFiveISPlot(session,input,output,values)
  output$fortyfivedegrees_IeqS <- fortyFiveIeqSPlot(session,input,output,values)
}

shinyApp(ui = ui, server = server)

