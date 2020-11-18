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
                       h4("Paramètres globaux"),
                       glob_params,
                       h4("Paramètres de IS"),
                       ui_IS_params1,
                       ui_IS_params2,
                       h4("Paramètres de LM"),
                       ui_LM_params1,
                       # h4("Paramètres des graphs"),
                       # fluidRow(
                       #     column(6,
                       #            sliderInput("ymax","y max", 100,10000,2500,step = 100)          
                       #     )#,
                       #     # column(6,
                       #     #        sliderInput("Mmax","M max", 100,4000,2000,step = 100)       
                       #     # )
                       # ),
                       fluidRow(
                           column(4,
                                  actionButton("shock", "Ajouter des chocs", `data-toggle`="collapse", `data-target`="#shock_set", style = "margin-bottom: 15px;")
                           )
                       ),
                       div(id = "shock_set", class = "collapse",
                           shock_params(c(choicesGlob, choicesIS, choicesLM),1),
                           shock_params(c(choicesGlob, choicesIS, choicesLM),2)
                       )
                   )
            ),
            column(9,
                   plotlyOutput("islm", height = 600)
            )
        ),
        div(id = "model",
            fluidRow(
                column(4,
                       h4("Équation de demande de biens:"),
                       helpText(class = "math",
                                "$$Y^d = \\alpha \\cdot (y-t) + C_{\\pi} + I_y \\cdot y + I_{D} \\cdot \\frac{D}{p} + I_r \\cdot r + \\bar{I} + g$$"
                       ),
                       h4("Équation de demande de monnaie:"),
                       helpText(class = "math",
                                "$$\\bar{M} = p(L_y \\cdot y + L_r \\cdot r)$$"
                       )
                ),
                column(4,
                       h4("Système à deux équations et deux inconnues"),
                       helpText(class = "math",
                                "$$\\begin{align}
                                (1-\\alpha-I_y) \\cdot y - I_r \\cdot r &= -\\alpha t + C_{\\pi} + I_{D} \\cdot \\frac{D}{p} + \\bar{I} + g\\\\
                                L_y \\cdot y + L_r \\cdot r &= \\frac{\\bar{M}}{p}
                                \\end{align}$$"     
                       )
                ),
                column(4,
                       h4("Solutions du système"),
                       helpText(class = "math",
                                "$$y* = \\frac{(-\\alpha t + C_{\\pi} + I_{D} \\cdot \\frac{D}{p} + \\bar{I} + g)L_r+I_r \\frac{\\bar{M}}{p}}{L_r(1-\\alpha-I_y) + I_r L_y}$$"     
                       ),
                       helpText(class = "math",
                                "$$r* = \\frac{\\frac{\\bar{M}}{p}(1-\\alpha-I_y) - L_y(-\\alpha t + C_{\\pi} + I_{D} \\cdot \\frac{D}{p} + \\bar{I} + g)}{L_r(1-\\alpha-I_y) + I_r L_y}$$"     
                       )
                )
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
    
    compute_equilibrium <- function(alpha,iy,ir,id,cpi,bari,g,t,D,p,ly,lr,Ms){
        denom = lr*(1-alpha-iy) + ir*ly
        eq_y = ((-alpha*t+cpi+id*D/p+bari+g)*lr+ir*Ms/p)/denom
        eq_r = (Ms/p*(1-alpha-iy)-ly*(-alpha*t+cpi+id*D/p+bari+g))/denom
        return(list(y = unname(eq_y), r = unname(eq_r)))
    }
        
    observeEvent({
        input$alpha
        input$iy
        input$ir
        input$id
        input$cpi
        input$bari
        input$g
        input$t
        input$p
        input$ly
        input$lr
        input$Ms
    },{
        values$p = input$p/100

        values$params = list("p" = values$p,
                         "alpha" = input$alpha,
                          "iy" = input$iy,
                          "ir" = input$ir,
                         "id" = input$id,
                          "cpi" = input$cpi,
                          "bari" = input$bari,
                          "g" = input$g,
                          "t" = input$t,
                          "ly" = input$ly,
                          "lr" = input$lr,
                          "Ms" = input$Ms)
        
        values$shocked_params_1 = values$params
        values$shocked_params_2 = values$params
        
        values$eq = compute_equilibrium(input$alpha,input$iy,input$ir,input$id,input$cpi,input$bari,input$g,input$t,input$D,values$p,input$ly,input$lr,input$Ms)
    })
    
    observeEvent({
        input$shock
    },{
        values$shock = !values$shock
        if(values$shock){
            updateActionButton(session,"shock","Retirer les chocs")
        }
        else{
            updateActionButton(session,"shock","Ajouter des chocs")
        }
    })
    
    observeEvent({
        input$shocked_var_1
        input$new_value_1
    },{
        # browser()
        if(values$shock){
            if(!is.na(input$new_value_1)){
                if(input$shocked_var_1 == "p"){
                    values$shocked_params_1$p = input$new_value_1/100
                } else{
                    values$shocked_params_1[input$shocked_var_1] = input$new_value_1
                }
                values$new_eq_1 = with(values$shocked_params_1, compute_equilibrium(alpha,iy,ir,id,cpi,bari,g,t,input$D,p,ly,lr,Ms))
            }
        }
    })

    observeEvent({
        input$shocked_var_2
        input$new_value_2
    },{
        # browser()
        if(values$shock){
            if(!is.na(input$new_value_2)){
                values$shocked_params_2 = values$shocked_params_1
                if(input$shocked_var_2 == "p"){
                    values$shocked_params_2$p = input$new_value_2/100
                } else{
                    values$shocked_params_2[input$shocked_var_2] = input$new_value_2
                }
                values$new_eq_2 = with(values$shocked_params_2, compute_equilibrium(alpha,iy,ir,id,cpi,bari,g,t,input$D,p,ly,lr,Ms))
            }
        }
    })
    
    output$islm <- renderPlotly({
        fig = ISLMPlot(input,output,values)
        fig
    })
    
}

shinyApp(ui = ui, server = server)

