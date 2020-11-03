library(shinyjs)
library(shiny)
library(readr)
library(plotly)
source("../src/ui_functions.r")
source("../src/LM_functions.r")
source("../src/IS_functions.r")

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
    
    titlePanel("IS-LM et le marché des titres dans un modèle à prix fixes"),
    
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
                       div(id = "endo_var", class = "collapse",
                           h4("Variables endogènes"),
                           fluidRow(
                                    column(6,
                                           numericInput("ystar","\\(y*\\)",0,10000,value = 1235.294, step=10)
                                    ),
                                    column(6,
                                           numericInput("r0","\\(r_0\\)",0,5,value = 4.676471, step = 0.5)
                                    )
                           )
                       ),
                       div(class = "collapse",
                           h4("Paramètres des graphs"),
                           fluidRow(
                               column(6,
                                      sliderInput("ymax","y max", 100,10000,3000,step = 100)          
                               ),
                               column(6,
                                      sliderInput("Mmax","M max", 100,4000,1200,step = 100)
                               )
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
                           ),
                           fluidRow(
                               column(6, class = "collapse",
                                      checkboxInput("show_path", "Montrer le chemin")
                               )
                           )
                       )
                   ),
                   div(id = "model",
                       h4("Demande de biens:"),
                       helpText(class = "math",
                                "$$Y^d = (\\alpha + I_y) \\cdot y + I_r \\cdot r + C_{\\pi} + \\bar{I} + g$$"
                       ),
                       h4("Demande de monnaie:"),
                       helpText(class = "math",
                                "$$M^d = p(L_y \\cdot y + L_r \\cdot r)$$"
                       ),
                       h4("Demande de titres:"),
                       helpText(class = "math",
                                "$$B^d = p_B(B_y \\cdot y + B_r \\cdot r)$$"
                       )
                   )
            ),
            column(9,
                   tabsetPanel(
                       tabPanel("Marché des biens",
                                plotlyOutput("FFD_IS_plot", height = 600)
                       ),
                       tabPanel("Loi de Walras",
                                tags$table(id = "dep_rev_table",
                                    tags$tr(
                                        tags$th(
                                            ""
                                        ),
                                        tags$th(
                                            "Dépenses"
                                        ),
                                        tags$th(
                                            "Revenus"
                                        )
                                    ),
                                    tags$tr(
                                        tags$th(
                                            "Ménages"
                                        ),
                                        tags$td(
                                            "\\(PC+M^d_M+PT_M\\)"
                                        ),
                                        tags$td(
                                            "\\(WN^s+rP_B B_M\\)"
                                        )
                                    ),
                                    tags$tr(
                                        tags$th(
                                            "Entreprises"
                                        ),
                                        tags$td(
                                            "\\(WN^d+ r P_B B_E+P I + PT_E + M^d_E\\)"
                                        ),
                                        tags$td(
                                            "\\(PY+P_B B_E\\)"
                                        )
                                    ),
                                    tags$tr(
                                        tags$th(
                                            "Gouvernement (et BC)"
                                        ),
                                        tags$td(
                                            "\\(P G + r P_B B_G\\)"
                                        ),
                                        tags$td(
                                            "\\(M^s+ PT\\)"
                                        )
                                    ),
                                ),
                                tags$div(id = "markets",
                                    tags$p(tags$b(tags$ins("Marché des biens:"))," \\(P(Y-T) = P(C+I+G - T_M - T_E) \\Rightarrow PY^s = PY^d\\),"), 
                                    tags$p(tags$b(tags$ins("Marché du travail:")), " \\(WN^s = WN^d\\),"), 
                                    tags$p(tags$b(tags$ins("Marché des titres:"))," \\(rP_B B_M = rP_B (B_E + B_G)\\)"),
                                    tags$p(tags$b(tags$ins("'Marché' de la monnaie:")), " \\(M^s = M^d_E+M^d_M\\),")
                                ),
                                tags$div(id = "walras_law",
                                    tags$p(tags$b(tags$ins("Loi de Walras"))),
                                    tags$p("\\( PY^d + WN^d + r P_B B^d + M^d = PY^s + WN^s + r P_B B^s + M^s \\)"),
                                    tags$p("\\( P(Y^d - Y^s) + W(N^d - N^s) + r P_B (B^d - B^s) + M^d - M^s = 0 \\)")
                                ),
                                plotlyOutput("real_econ", height = 300)
                       ),
                       tabPanel("'Marché' de la monnaie",
                                plotlyOutput("RM_LM_plot", height = 600)  
                       ),
                       tabPanel("Marché des titres",
                                plotlyOutput("Bonds_plot", height = 600)  
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
                             "r" = input$r0,
                             "cpi" = input$cpi,
                             "bari" = input$bari,
                             "g" = input$g)
        
        values$params_LM = c("p" = input$p,
                             "ly" = input$ly,
                             "lr" = input$lr,
                             "ystar" = input$ystar,
                             "Ms" = input$Ms)
        
        values$eq$y = (input$ir*input$r0+input$cpi+input$bari+input$g)/(1-input$alpha-input$iy)
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
        input$shocked_var_IS
        input$new_value_IS
    },{
        # browser()
        if(values$shock){
            values$shocked_params_IS = values$params_IS
            if(!is.na(input$new_value_IS)){
                values$shocked_params_IS[input$shocked_var_IS] = input$new_value_IS
                values$new_eq$y = unname((values$shocked_params_IS["ir"]*values$shocked_params_IS["r"]+values$shocked_params_IS["cpi"]+values$shocked_params_IS["bari"]+values$shocked_params_IS["g"])/(1-values$shocked_params_IS["alpha"]-values$shocked_params_IS["iy"]))
            }
        }
    })
    
    observeEvent({
        input$shocked_var_LM
        input$new_value_LM
    },{
        if(values$shock){
            values$shocked_params_LM = values$params_LM
            if(!is.na(input$new_value_LM)){
                values$shocked_params_LM[input$shocked_var_LM] = input$new_value_LM
                values$new_eq$r = unname(1/values$shocked_params_LM["lr"]*(values$shocked_params_LM["Ms"]/values$shocked_params_LM["p"]-values$shocked_params_LM["ly"]*values$shocked_params_LM["ystar"]))
            }
        }
    })
    
    output$FFD_IS_plot <- fortyFiveISPlot(session,input,output,values)
    output$RM_LM_plot <- rm_lm_plot(session,input,output,values)
    
    output$Bonds_plot <- renderPlotly({
        
    })
}

shinyApp(ui = ui, server = server)

