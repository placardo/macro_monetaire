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
                   h4("Paramètres globaux"),
                   glob_params,
                   h4("Paramètres de IS"),
                   ui_IS_params1,
                   ui_IS_params2,
                   fluidRow(
                     column(6,
                      numericInput("r0","\\(r_0\\)",0,10,value = 4.676471, step=1)
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
        ),
      br(),
      div(id = "model",
        fluidRow(
          column(5,
            h4("Équation de demande:"),
            helpText(class = "math",
                     "$$Y^d = (\\alpha + I_y) \\cdot y + I_{D/p} \\cdot \\frac{D}{p} + I_r \\cdot r_0 + C_{\\pi} + \\bar{I} + g$$"
            )
          ),
          column(4,
            h4("Niveau d'équilibre du revenu:"),
            helpText(class = "math",
                     "$$y* = \\frac{I_r \\cdot r_0 + C_{\\pi} + \\bar{I} + g}{1-\\alpha - I_y}$$"
            )
          ),
          column(3,
            h4("Équation de IS:"),
            helpText(class = "math",
                     "$$r = \\frac{(1-\\alpha - I_y) \\cdot y - C_{\\pi}-\\bar{I} -g}{I_r}$$"
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
    shocked_params_IS = c()
  )
  
  observeEvent({
    input$alpha
    input$iy
    input$ir
    input$id
    input$cpi
    input$bari
    input$g
    input$r0
    input$p
  },{
    values$p = input$p/100
  
    values$params_IS = c("alpha" = input$alpha,
                         "iy" = input$iy,
                         "ir" = input$ir,
                         "id" = input$id,
                         "cpi" = input$cpi,
                         "bari" = input$bari,
                         "g" = input$g,
                         "t" = input$t)
    
    values$params_LM = c("p" = values$p,
                         "ly" = input$ly,
                         "lr" = input$lr,
                         "Ms" = input$Ms)
    
    values$shocked_params_IS = values$params_IS
    values$shocked_params_LM = values$params_LM
    
    values$eq$y = (input$ir*input$r0+input$cpi+input$id*input$D/values$p+input$bari+input$g)/(1-input$alpha-input$iy)
    values$eq$r = input$r0
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
      values$shocked_params_IS = values$params
      if(!is.na(input$new_value_IS)){
        values$shocked_params_IS[input$shocked_var_IS] = input$new_value_IS
        values$new_eq$y = unname((values$shocked_params_IS["ir"]*values$shocked_params_IS["r"]+values$shocked_params_IS["cpi"]+values$shocked_params_IS["bari"]+values$shocked_params_IS["g"])/(1-values$shocked_params_IS["alpha"]-values$shocked_params_IS["iy"]))
      }
    }
  })
  
  output$fortyfivedegrees_IS <- renderPlotly({
    fig1 = fortyFivePlot(input,output,values)
    fig2 = ISPlot(input,output,values)
    fig = subplot(fig1,fig2,shareX = TRUE,titleY = TRUE)
    fig
  })
  
  output$fortyfivedegrees_IeqS <- renderPlotly({
    fig1 = fortyFivePlot(input,output,values)
    fig2 = IeqSPlot(input,output,values)
    fig = subplot(fig1,fig2,shareX = TRUE,titleY = TRUE)
    fig
  })
}

shinyApp(ui = ui, server = server)

