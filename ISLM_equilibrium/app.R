library(shinyjs)
library(shiny)
library(readr)
library(plotly)
source("../src/ui_functions.r")
source("../src/LM_functions.r")
source("../src/ISLM_functions.r")

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
    
    titlePanel("L'équilibre IS-LM"),
    
    div(id = "page-wrapper",
        fluidRow(
            column(3,
                   div(id = "settings",
                       h4("Paramètres de IS"),
                       ui_IS_params1,
                       ui_IS_params2,
                       h4("Paramètres de LM"),
                       ui_LM_params1,
                       ui_LM_params2,
                       h4("Paramètres des graphs"),
                       fluidRow(
                           column(6,
                                  sliderInput("ymax","y max", 100,10000,2500,step = 100)          
                           )#,
                           # column(6,
                           #        sliderInput("Mmax","M max", 100,4000,2000,step = 100)       
                           # )
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
                               column(5,
                                      LM_shock
                               )
                           ),
                           fluidRow(
                               column(5,
                                      numericInput("new_value_IS","Nouvelle Valeur", value = NULL)
                               ),
                               column(5,
                                      numericInput("new_value_LM","Nouvelle Valeur", value = NULL)
                               )
                           )
                       )
                   ),
                   div(id = "model",
                       h4("Équation de demande de biens:"),
                       helpText(class = "math",
                                "$$Y^d = (\\alpha + I_y) \\cdot y + I_r \\cdot r + C_{\\pi} + \\bar{I} + g$$"
                       ),
                       h4("Équation de demande de monnaie:"),
                       helpText(class = "math",
                                "$$\\bar{M} = p(L_y \\cdot y + L_r \\cdot r)$$"
                       ),
                       h4("Système à deux équations et deux inconnues"),
                       helpText(class = "math",
                                "$$(1-\\alpha-I_y) \\cdot y + I_r \\cdot r = C_{\\pi} + \\bar{I} + g$$"     
                       ),
                       helpText(class = "math",
                                "$$L_y \\cdot y + L_r \\cdot r = \\frac{\\bar{M}}{p}$$"     
                       ),
                       h4("Solutions du système"),
                       helpText(class = "math",
                                "$$y* = \\frac{(C_{\\pi} + \\bar{I} + g)L_r+I_r \\frac{\\bar{M}}{p}}{L_r(1-\\alpha-I_y) + I_r L_y}$$"     
                       ),
                       helpText(class = "math",
                                "$$r* = \\frac{\\frac{\\bar{M}}{p}(1-\\alpha-I_y) - L_y(C_{\\pi} + \\bar{I} + g)}{L_r(1-\\alpha-I_y) + I_r L_y}$$"     
                       )
                   )
            ),
            column(9,
                   plotlyOutput("diagrammes", height =600)
            )
        )
    )
)

server <- function(session, input, output) {
    values <- reactiveValues(
        shock = F,
        params_IS = c(),
        shocked_params_IS = c(),
        params_LM = c(),
        shocked_params_LM = c()
    )
    
    compute_equilibrium <- function(alpha,iy,ir,cpi,bari,g,t,p,ly,lr,Ms){
        denom = lr*(1-alpha-iy) + ir*ly
        eq_y = ((cpi+bari-alpha*t+g)*lr+ir*Ms/p)/denom
        eq_r = (Ms/p*(1-alpha-iy)-ly*(cpi+bari-alpha*t+g))/denom
        return(list(y = unname(eq_y), r = unname(eq_r)))
    }
    
    observeEvent({
        input$alpha
        input$iy
        input$ir
        input$cpi
        input$bari
        input$g
        input$t
        input$p
        input$ly
        input$lr
        input$Ms
    },{
        values$params_IS = c("alpha" = input$alpha,
                          "iy" = input$iy,
                          "ir" = input$ir,
                          "cpi" = input$cpi,
                          "bari" = input$bari,
                          "g" = input$g,
                          "t" = input$t)
        
        values$params_LM = c("p" = input$p,
                          "ly" = input$ly,
                          "lr" = input$lr,
                          "Ms" = input$Ms)
        
        values$shocked_params_IS = values$params_IS
        values$shocked_params_LM = values$params_LM
        
        values$eq = compute_equilibrium(input$alpha,input$iy,input$ir,input$cpi,input$bari,input$g,input$t,input$p,input$ly,input$lr,input$Ms)
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
            values$shocked_params_IS = values$params_IS
            if(!is.na(input$new_value_IS)){
                values$shocked_params_IS[input$shocked_var_IS] = input$new_value_IS
                values$new_eq = compute_equilibrium(values$shocked_params_IS["alpha"],values$shocked_params_IS["iy"],values$shocked_params_IS["ir"],values$shocked_params_IS["cpi"],values$shocked_params_IS["bari"],values$shocked_params_IS["g"],values$shocked_params_IS["t"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"])
            }
        }
    })
    
    observeEvent({
        input$shocked_var_LM
        input$new_value_LM
    },{
        # browser()
        if(values$shock){
            values$shocked_params_LM = values$params_LM
            if(!is.na(input$new_value_LM)){
                values$shocked_params_LM[input$shocked_var_LM] = input$new_value_LM
                values$new_eq = compute_equilibrium(values$shocked_params_IS["alpha"],values$shocked_params_IS["iy"],values$shocked_params_IS["ir"],values$shocked_params_IS["cpi"],values$shocked_params_IS["bari"],values$shocked_params_IS["g"],values$shocked_params_IS["t"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"])
            }
        }
    })
    
    output$diagrammes <- renderPlotly({
        fig = ISLMPlot(input,output,values)
        fig
    })
}

shinyApp(ui = ui, server = server)

