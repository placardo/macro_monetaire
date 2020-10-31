library(shinyjs)
library(shiny)
library(readr)
library(plotly)

choicesIS = c("alpha","iy","ir","cpi","bari","g")
choicesNamesIS = c("\\\\\\alpha",
                 "\\\\I_y",
                 "\\\\I_r",
                 "\\\\C_{\\pi}",
                 "\\\\\\bar{I}",
                 "\\\\g")
choicesIS <- setNames(choicesIS, choicesNamesIS)

choicesLM = c("p","ly","lr","Ms")
choicesNamesLM = c("\\\\p",
                 "\\\\L_y",
                 "\\\\L_r",
                 "\\\\M^s")
choicesLM <- setNames(choicesLM, choicesNamesLM)

ui <- fluidPage(
    includeCSS("www/style.css"),
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
                       h4("Paramètres de LM"),
                       fluidRow(
                           column(4,
                                  numericInput("p","\\(p\\)",0,10,value = 2,step=1)
                           ),
                           column(4,
                                  numericInput("ly","\\(L_y\\)",0,1,value = 0.9,step=0.05)
                           ),
                           column(4,
                                  numericInput("lr","\\(L_r\\)",-1000,0,value = -150,step=10)
                           )
                       ),
                       fluidRow(
                           column(6,
                                  numericInput("Ms","\\(M^s\\)", 0,4000,value=1000,step = 100)       
                           )
                       ),
                       h4("Paramètres des graphs"),
                       fluidRow(
                           column(6,
                                  sliderInput("ymax","y max", 100,10000,3000,step = 100)          
                           )#,
                           # column(6,
                           #        sliderInput("Mmax","M max", 100,4000,2000,step = 100)       
                           # )
                       ),
                       div(
                           fluidRow(
                               column(4,
                                      actionButton("shock", "Ajouter un choc", `data-toggle`="collapse", `data-target`="#shock_set", style = "margin-bottom: 15px;")
                               )
                           )
                       ),
                       div(id = "shock_set", class = "collapse",
                           fluidRow(
                               column(5,
                                      selectizeInput("shocked_var_IS","Variable choquée (IS):",
                                                     choicesIS,
                                                     options = list(render = I("
                              {
                                item: function(item, escape) {
                                        var html = katex.renderToString(item.label);
                                        return '<div>' + html + '</div>';
                                      },
                                option: function(item, escape) {
                                          var html = katex.renderToString(item.label);
                                          return '<div>' + html + '</div>';
                                        }
                              }"))
                                      )
                               ),
                               column(5,
                                      selectizeInput("shocked_var_LM","Variable choquée (LM):",
                                                     choicesLM,
                                                     options = list(render = I("
                              {
                                item: function(item, escape) {
                                        var html = katex.renderToString(item.label);
                                        return '<div>' + html + '</div>';
                                      },
                                option: function(item, escape) {
                                          var html = katex.renderToString(item.label);
                                          return '<div>' + html + '</div>';
                                        }
                              }"))
                                      )
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
                                "$$y = (\\alpha + I_y) \\cdot y + I_r \\cdot r + C_{\\pi} + \\bar{I} + g$$"
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
    
    compute_equilibrium <- function(alpha,iy,ir,cpi,bari,g,p,ly,lr,Ms){
        denom = lr*(1-alpha-iy) + ir*ly
        eq_y = ((cpi+bari+g)*lr+ir*Ms/p)/denom
        eq_r = (Ms/p*(1-alpha-iy)-ly*(cpi+bari+g))/denom
        return(list(y = unname(eq_y), r = unname(eq_r)))
    }
    
    observeEvent({
        input$alpha
        input$iy
        input$ir
        input$cpi
        input$bari
        input$g
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
                          "g" = input$g)
        
        values$params_LM = c("p" = input$p,
                          "ly" = input$ly,
                          "lr" = input$lr,
                          "Ms" = input$Ms)
        
        values$shocked_params_IS = values$params_IS
        values$shocked_params_LM = values$params_LM
        
        values$eq = compute_equilibrium(input$alpha,input$iy,input$ir,input$cpi,input$bari,input$g,input$p,input$ly,input$lr,input$Ms)
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
                values$new_eq = compute_equilibrium(values$shocked_params_IS["alpha"],values$shocked_params_IS["iy"],values$shocked_params_IS["ir"],values$shocked_params_IS["cpi"],values$shocked_params_IS["bari"],values$shocked_params_IS["g"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"])
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
                values$new_eq = compute_equilibrium(values$shocked_params_IS["alpha"],values$shocked_params_IS["iy"],values$shocked_params_IS["ir"],values$shocked_params_IS["cpi"],values$shocked_params_IS["bari"],values$shocked_params_IS["g"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"])
            }
        }
    })
    
    output$diagrammes <- renderPlotly({
        prod = seq(0,input$ymax,length.out = 1000)
        to_plot = data.frame(revenu = prod)
        fig = plot_ly(to_plot, x = ~revenu)
        
        fig = plot_ly(to_plot, x = ~revenu)
        fig = fig %>% add_trace(y = ((1-input$alpha-input$iy)*prod-input$cpi-input$bari-input$g)/input$ir, type = "scatter", mode = "lines", name = "$$IS_1$$", line = list(color = "#ff0000"))
        fig = fig %>% add_trace(y = 1/input$lr*(input$Ms/input$p-input$ly*prod), type = "scatter", mode = "lines", name = "$$LM_1$$", line = list(color = "#007bff"))
        
        fig = fig %>% add_segments(0,values$eq$r,values$eq$y,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE) 
        fig = fig %>% add_segments(values$eq$y,0,values$eq$y,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
        
        if(values$shock & (!is.na(input$new_value_LM) || !is.na(input$new_value_IS))){
            if(!is.na(input$new_value_IS)){
                fig = fig %>% add_trace(y = ((1-values$shocked_params_IS["alpha"]-values$shocked_params_IS["iy"])*prod-values$shocked_params_IS["cpi"]-values$shocked_params_IS["bari"]-values$shocked_params_IS["g"])/values$shocked_params_IS["ir"], type = "scatter", mode = "lines", name = "$$IS_2$$", line = list(color = "#aa0000"))
            }
            if(!is.na(input$new_value_LM)){
                fig = fig %>% add_trace(y = 1/values$shocked_params_LM["lr"]*(values$shocked_params_LM["Ms"]/values$shocked_params_LM["p"]-values$shocked_params_LM["ly"]*prod), type = "scatter", mode = "lines", name = "$$LM_2$$", line = list(color = "#02346b"))
            }
            fig = fig %>% add_segments(0,values$new_eq$r,values$new_eq$y,values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE) 
            fig = fig %>% add_segments(values$new_eq$y,0,values$new_eq$y,values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
            
            islm_new_eq = list(
                x = values$new_eq$y,
                y = values$new_eq$r,
                text = paste0("y*=",round(values$new_eq$y), ", r*=",round(values$new_eq$r,1))
            )
            fig = fig %>% layout(annotations = islm_new_eq)
        } else{
            islm_eq = list(
                x = values$eq$y,
                y = values$eq$r,
                text = paste0("y*=",round(values$eq$y), ", r*=",round(values$eq$r,1))
            )
            fig = fig %>% layout(annotations = islm_eq)
        }
        
        f <- list(
            family = "Courier New, monospace",
            size = 18,
            color = "#7f7f7f"
        )
        revenu <- list(
            title = "Revenu, y",
            titlefont = f
        )
        interest <- list(
            title = "Taux d'intérêt, r",
            titlefont = f
        )
        fig = fig %>% layout(xaxis = revenu, yaxis = interest)
        fig
    })
    
    output$functions <- renderPlotly({
        
    })
}

shinyApp(ui = ui, server = server)

