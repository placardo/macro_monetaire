library(shinyjs)
library(shiny)
library(readr)
library(plotly)

choices = c("p","ly","lr","ystar","Ms")
choicesNames = c("\\\\p",
                 "\\\\L_y",
                 "\\\\L_r",
                 "\\\\y*",
                 "\\\\M^s")
choices <- setNames(choices, choicesNames)

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
    
    titlePanel("La courbe LM"),
    
    div(id = "page-wrapper",
        fluidRow(
            column(3,
                   div(id = "settings",
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
                           ),
                           column(6,
                                  numericInput("ystar","\\(y*\\)",0,10000,value = 1600, step=10)
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
                       div(
                           fluidRow(
                               column(4,
                                      actionButton("shock", "Ajouter un choc", `data-toggle`="collapse", `data-target`="#shock_set", style = "margin-bottom: 15px;")
                               )
                           )
                       ),
                       div(id = "shock_set", class = "collapse",
                           fluidRow(
                               column(6,
                                      selectizeInput("shocked_var","Variable choquée:",
                                                     choices,
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
                               column(6,
                                    numericInput("new_value","Nouvelle Valeur", value = NULL)
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
                   plotlyOutput("diagrammes", height = 500)
                   # plotlyOutput("functions")
            )
        )
    )
)

server <- function(session, input, output) {
    values <- reactiveValues(
        shock = F,
        params = c(),
        shocked_params = c(),
        eq = 0
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

        values$eq = unname(1/input$lr*(input$Ms/input$p-input$ly*input$ystar))
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
        input$shocked_var
        input$new_value
    },{
        # browser()
        if(values$shock){
            values$shocked_params = values$params
            if(!is.na(input$new_value)){
                values$shocked_params[input$shocked_var] = input$new_value
                values$new_eq = unname(1/values$shocked_params["lr"]*(values$shocked_params["Ms"]/values$shocked_params["p"]-values$shocked_params["ly"]*values$shocked_params["ystar"]))
            }
        }
    })
    
    output$diagrammes <- renderPlotly({
        masse = seq(0,input$Mmax,length.out = 1000)
        prod = seq(0,input$ymax,length.out = 1000)
        to_plot = data.frame(money = masse, revenu = prod)
        fig1 = plot_ly(to_plot, x = ~money)
        
        fig1 = fig1 %>% add_segments(x = input$Ms, y = 0, xend = input$Ms, yend = 20, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = I("blue"))
        fig1 = fig1 %>% add_trace(y = 1/input$lr*(masse/input$p - input$ly*input$ystar), type = "scatter", mode = "lines", name = "$$M^d$$", color = I("red"))
        fig1 = fig1 %>% add_segments(x = 0, y = values$eq, xend = input$Mmax, yend = values$eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)

        fig2 = plot_ly(to_plot, x = ~revenu)
        fig2 = fig2 %>% add_trace(y = 1/input$lr*(input$Ms/input$p-input$ly*prod), type = "scatter", mode = "lines", name = "$$LM_1$$", color = I("red"))
        fig2 = fig2 %>% add_segments(0,values$eq,input$ystar,values$eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
        fig2 = fig2 %>% add_segments(input$ystar,0,input$ystar,values$eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
        
        if(values$shock & !is.na(input$new_value)){
            if(input$shocked_var == "Ms"){
                fig1 = fig1 %>% add_segments(x = values$shocked_params["Ms"], y = 0, xend = values$shocked_params["Ms"], yend = 20, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = "rgb(0,20,200)")
                fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq, xend = input$Mmax, yend = values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
            } else{
                fig1 = fig1 %>% add_trace(y = 1/values$shocked_params["lr"]*(masse/values$shocked_params["p"] - values$shocked_params["ly"]*values$shocked_params["ystar"]), type = "scatter", mode = "lines", name = "$$M^d$$", color = "rgb(0,200,20)")
                fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq, xend = input$Mmax, yend = values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
            }
            new_eq = list(
                x = unname(values$shocked_params["Ms"]),
                y = values$new_eq,
                text = paste0("r*=",round(values$new_eq,1))
            )
            fig1 = fig1 %>% layout(annotations = new_eq)
            
            fig2 = fig2 %>% add_trace(y = 1/values$shocked_params["lr"]*(values$shocked_params["Ms"]/values$shocked_params["p"]-values$shocked_params["ly"]*prod), type = "scatter", mode = "lines", name = "$$LM_2$$", color = "rgb(0,200,20)")
            fig2 = fig2 %>% add_segments(0,values$new_eq,values$shocked_params["ystar"],values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
            fig2 = fig2 %>% add_segments(values$shocked_params["ystar"],0,values$shocked_params["ystar"],values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
            lm_eq = list(
                x = unname(values$shocked_params["ystar"]),
                y = values$new_eq,
                text = paste0("r*=",round(values$new_eq,1))
            )
            fig2 = fig2 %>% layout(annotations = lm_eq)
        } else{
            eq = list(
                x = input$Ms,
                y = values$eq,
                text = paste0("r*=",round(values$eq,1))
            )
            fig1 = fig1 %>% layout(annotations = eq)
            is_eq = list(
                x = input$ystar,
                y = values$eq,
                text = paste0("r*=",round(values$eq,1))
            )
            fig2 = fig2 %>% layout(annotations = is_eq)
        }
        
        f <- list(
            family = "Courier New, monospace",
            size = 18,
            color = "#7f7f7f"
        )
        money <- list(
            title = "Masse monétaire, M",
            titlefont = f
        )
        revenu <- list(
            title = "Revenu, y",
            titlefont = f
        )
        interest <- list(
            title = "Taux d'intérêt, r",
            titlefont = f
        )
        fig1 = fig1 %>% layout(xaxis = money, yaxis = interest)
        fig2 = fig2 %>% layout(xaxis = revenu, yaxis = interest)
        fig = subplot(fig1,fig2,shareY = TRUE,titleX = TRUE)
        fig
    })
    
    output$functions <- renderPlotly({
        
    })
}

shinyApp(ui = ui, server = server)

