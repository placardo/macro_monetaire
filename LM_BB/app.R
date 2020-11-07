library(shinyjs)
library(shiny)
library(readr)
library(plotly)
source("../src/ui_functions.r")
source("../src/LM_functions.r")
source("../src/IS_functions.r")
source("../src/LaborMarket_functions.r")
source("../src/BB_functions.r")

# Revenu potentiel. On suppose dans ce modèle que la masse salariale et la productivité sont constantes
POTY = 2912

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
                       h4("Paramètres du marché du travail"),
                       ui_LabM_params,
                       h4("Paramètres de LM"),
                       ui_LM_params1,
                       ui_LM_params2,
                       h4("Paramètres de BB"),
                       ui_BB_params,
                       div(id = "endo_var", class = "collapse",
                           h4("Variables endogènes"),
                           fluidRow(
                                    column(6,
                                           numericInput("ystar","\\(y*\\)",0,10000,value = 1411.765, step=10)
                                    ),
                                    column(6,
                                           numericInput("r0","\\(r_0\\)",0,5,value = 4.058824, step = 0.5)
                                    )
                           )
                       ),
                       # div(class = "collapse",
                           h4("Paramètres des graphs"),
                           fluidRow(
                               column(6,
                                      sliderInput("ymax","y max", 100,10000,3000,step = 100)          
                               ),
                               column(6,
                                      sliderInput("Mmax","M max", 100,4000,800,step = 100)
                               )
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
                       # tabPanel("Marché des biens",
                       #          plotlyOutput("FFD_IS_plot", height = 600)
                       # ),
                       tabPanel("Comptes nationaux",
                                uiOutput("comptes_nat")
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
                                    )
                                ),
                                tags$div(id = "markets",
                                    tags$p(tags$b(tags$ins("Marché des biens:"))," \\(P(Y-T) = P(C+I+G - T_M - T_E) \\Rightarrow PY^s = PY^d\\),"), 
                                    tags$p(tags$b(tags$ins("Marché du travail:")), " \\(WN^s = WN^d\\),"), 
                                    tags$p(tags$b(tags$ins("Marché des titres:"))," \\(rP_B B_M = rP_B (B_E + B_G)\\)"),
                                    tags$p(tags$b(tags$ins("'Marché' de la monnaie:")), " \\(M^s = M^d_E+M^d_M\\),")
                                ),
                                tags$div(id = "walras_law",
                                    tags$p(tags$b(tags$ins("Loi de Walras (D = O)"))),
                                    tags$p("\\( PY^d + WN^d + r P_B B^d + M^d = PY^s + WN^s + r P_B B^s + M^s \\)"),
                                    tags$p("\\( P(Y^d - Y^s) + W(N^d - N^s) + r P_B (B^d - B^s) + M^d - M^s = 0 \\)")
                                ),
                                plotlyOutput("real_econ", height = 700)
                       ),
                       tabPanel("Marché du travail",
                                plotlyOutput("lab_mark_plot", height = 600)
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
        shocked_params_LM = c(),
        first = T,
        POTY = POTY
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
        input$W
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
                             "Ms" = input$Ms)
    
        values$p = input$p/100
    
        if(values$first){
            values$shocked_params_IS = values$params_IS
            values$shocked_params_LM = values$params_LM
            values$first = F
        }
        
        # Les marchés de la monnaie et des titres sont directement liés dans notre modèle
        # Une hausse de l'offre de monnaie s'accompagne nécessairement d'une hausse de la demande de titre
        # C'est le même paramètre qui détermine les deux: la préférence pour la liquidité, c'est-à-dire la manière dont on choisit de répartir le revenu non consommé

        values$eq = compute_equilibrium(input$alpha,input$iy,input$ir,input$cpi,input$bari,input$g,values$p,input$ly,input$lr,input$Ms)
        values$eq$B = (100-0.5*input$by*values$eq$y)/(0.3072734+0.2)
        values$eq$Nstar = 57
        values$eq$N = -2*(input$W/values$p-57)
        values$eq$subN = values$eq$N - 0.01*(POTY-values$eq$y)
        values$eq$subN = input$alpha*values$p*values$eq$y/input$W
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
                # values$new_eq = compute_equilibrium(values$shocked_params_IS["alpha"],values$shocked_params_IS["iy"],values$shocked_params_IS["ir"],values$shocked_params_IS["cpi"],values$shocked_params_IS["bari"],values$shocked_params_IS["g"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"])
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
    
    output$FFD_IS_plot <- renderPlotly({
        fig1 = fortyFivePlot(input,output,values)
        fig2 = ISPlot(input,output,values)
        fig = subplot(fig1,fig2,shareX = TRUE,titleY = TRUE)
        fig
    })
    
    output$RM_LM_plot <- renderPlotly({
        fig1 = RMPlot(input,output,values)
        fig2 = LMPlot(input,output,values)
        fig = subplot(fig1,fig2,shareX = TRUE,titleY = TRUE)
        fig
    })

    output$real_econ <- renderPlotly({
        fig1 = fortyFivePlot(input,output,values)
        fig2 = laborMarketPlot(input,output,values)
        fig3 = RMPlot(input,output,values)
        fig4 = BBPlot(input,output,values)
        fig = subplot(fig1,fig2,fig3,fig4, nrows = 2, titleX = F, titleY = F)
        fig = fig %>% layout(annotations = list(
            list(x = 0.12, y = 1, text = "Marché des biens et services", showarrow = F, xref = "paper", yref = "paper"),
            list(x = 0.82, y = 1, text = "Marché du travail", showarrow = F, xref = "paper", yref = "paper"),
            list(x = 0.15, y = 0.43, text = "'Marché' de la monnaie", showarrow = F, xref = "paper", yref = "paper"),
            list(x = 0.82, y = 0.43, text = "Marché des titres", showarrow = F, xref = "paper", yref = "paper")
        ), showlegend = F)
    })

    output$Bonds_plot <- renderPlotly({
        fig1 = BBPlot(input,output,values)
        fig2 = RPBPlot(input,output,values)
        fig = subplot(fig1,fig2, nrows = 1, shareY = T, titleX = T)
    })
    
    output$lab_mark_plot <- renderPlotly({
        fig1 = laborMarketPlot(input,output,values)
        fig2 = firmEqPlot(input,output,values)
        fig = subplot(fig1,fig2, nrows = 1, shareX = T, titleY = T)
    })
    
    output$comptes_nat <- renderUI({
        tags$table(id = "comptes_nat_table",
                   tags$tr(
                       tags$th(
                           ""
                       ),
                       tags$th(
                           "Marché des biens"
                       ),
                       tags$th(
                           "Marché du travail"
                       ),
                       tags$th(
                           "'Marché' de la monnaie"
                       ),
                       tags$th(
                           "Marché des titres"
                       ),
                       tags$th(
                           "Total"
                       )
                   ),
                   tags$tr(
                       tags$th(
                           "Ménages"
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DD", "\\(C =\\)",format(round(input$alpha*values$eq$y+input$cpi)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "OR", "\\(WN^s=\\)", format(round(input$W/values$p*values$eq$subN)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DD", "\\(M^d=\\)", format(round(values$p*(input$ly*values$eq$y + input$lr*values$eq$r))))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DR", "\\(r P_B B=\\)",format(round(values$eq$r*values$eq$B)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "Dep", "Dép =", format(round(input$alpha*values$eq$y+input$cpi + values$p*(input$ly*values$eq$y + input$lr*values$eq$r)))),
                               tags$p(class = "R", "R = ", format(round(input$W/values$p*values$eq$subN+input$max_price*values$eq$r/100*values$eq$B)))
                       )
                   ),
                   tags$tr(
                       tags$th(
                           "Entreprises"
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DD", "\\(I =\\)",format(round(input$iy*values$eq$y+input$ir*values$eq$r + input$bari))),
                               tags$p(class = "OR", "\\(PY =\\)",format(round(values$p*values$eq$y)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DD", "\\(WN^d=\\)", format(round(input$W/values$p*values$eq$subN)))
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "Dep", "Dép =", format(round(input$iy*values$eq$y+input$ir*values$eq$r + input$bari+input$W/values$p*values$eq$subN))),
                               tags$p(class = "R", "R = ", format(round(values$p*values$eq$y)))
                       )
                   ),
                   tags$tr(
                       tags$th(
                           "Gouvernement"
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DD", "\\(g =\\)",format(round(input$g)))
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(
                           ""
                       )
                   ),
                   tags$tr(
                       tags$th(
                           "Banque Centrale"
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(
                           ""
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "OD", "\\(M^s =\\)", format(round(input$Ms)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "DR", "\\(B^d =\\)", format(round((1+values$eq$r/100)*input$Ms/input$max_price,2)))
                       ),
                       tags$td(
                           ""
                       )
                   ),
                   tags$tr(
                       tags$th(
                           "Total"
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "D", "\\(D =\\)", format(round(input$alpha*values$eq$y+input$cpi+input$iy*values$eq$y+input$ir*values$eq$r + input$bari+input$g))),
                               tags$p(class = "O", "\\(O =\\)", format(round(values$p*values$eq$y)))
                       ),
                       tags$td(class = "container-comptes",
                               tags$p(class = "D", "\\(D =\\)", format(round(input$W/values$p*values$eq$subN))),
                               tags$p(class = "O", "\\(O =\\)", format(round(input$W/values$p*values$eq$subN)))
                       ),
                       tags$td(
                           "\\(\\)"
                       ),
                       tags$td(
                           "\\(\\)"
                       ),
                       tags$td(
                           "\\(\\)"
                       )
                   )
            )
    })
}

shinyApp(ui = ui, server = server)

