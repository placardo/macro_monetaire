library(shinyjs)
library(shiny)
library(readr)
library(plotly)

choices = c("alpha","iy","ir","r","cpi","bari","g")
choicesNames = c("\\\\\\alpha",
                 "\\\\I_y",
                 "\\\\I_r",
                 "\\\\r",
                 "\\\\C_{\\pi}",
                 "\\\\\\bar{I}",
                 "\\\\g")
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
                  plotlyOutput("fortyfivedegrees", height = 500)
                  # plotlyOutput("functions")
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
    
    values$eq = (input$ir*input$r0+input$cpi+input$bari+input$g)/(1-input$alpha-input$iy)
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
        values$new_eq = unname((values$shocked_params["ir"]*values$shocked_params["r"]+values$shocked_params["cpi"]+values$shocked_params["bari"]+values$shocked_params["g"])/(1-values$shocked_params["alpha"]-values$shocked_params["iy"]))
      }
    }
  })
  
  output$fortyfivedegrees <- renderPlotly({
    prod = seq(0,input$ymax,length.out = 1000)
    to_plot = data.frame(revenu = prod)
    fig1 = plot_ly(to_plot, x = ~revenu)
    
    fig1 = fig1 %>% add_trace(y = prod, type = "scatter", mode = "lines", name = "$$Y^d=y$$", color = I("blue"))
    fig1 = fig1 %>% add_trace(y = (input$alpha + input$iy) * prod + input$ir*input$r0 + input$cpi + input$bari + input$g, type = "scatter", mode = "lines", name = "$$Y_1^d$$", color = I("red"))
    fig1 = fig1 %>% add_segments(0,values$eq,values$eq,values$eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
    fig1 = fig1 %>% add_segments(values$eq,0,values$eq,values$eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)

    fig2 = plot_ly(to_plot, x = ~revenu)
    fig2 = fig2 %>% add_trace(y = ((1-input$alpha-input$iy)*prod-input$cpi-input$bari-input$g)/input$ir, type = "scatter", mode = "lines", name = "$$IS_1$$", color = I("red"))
    fig2 = fig2 %>% add_segments(0,input$r0,values$eq,input$r0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE) 
    fig2 = fig2 %>% add_segments(values$eq,input$r0,values$eq,0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
    
    if(values$shock & !is.na(input$new_value)){
      fig1 = fig1 %>% add_trace(y = (values$shocked_params["alpha"] + values$shocked_params["iy"]) * prod + values$shocked_params["ir"]*values$shocked_params["r"] + values$shocked_params["cpi"] + values$shocked_params["bari"] + values$shocked_params["g"], type = "scatter", mode = "lines", name = "$$Y_2^d$$", color = "rgb(0,200,20)")    

      fig1 = fig1 %>% add_segments(0,values$new_eq,values$new_eq,values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend=F)
      fig1 = fig1 %>% add_segments(values$new_eq,0,values$new_eq,values$new_eq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend=F)
      new_eq = list(
        x = values$new_eq,
        y = values$new_eq,
        text = paste0("y*=",round(values$new_eq))
      )
      fig1 = fig1 %>% layout(annotations = new_eq)
      
      fig2 = fig2 %>% add_trace(y = ((1-values$shocked_params["alpha"]-values$shocked_params["iy"])*prod-values$shocked_params["cpi"]-values$shocked_params["bari"]-values$shocked_params["g"])/values$shocked_params["ir"], type = "scatter", mode = "lines", name = "$$IS_2$$", color = "rgb(0,150,20)")
      fig2 = fig2 %>% add_segments(0,values$shocked_params["r"],values$new_eq,values$shocked_params["r"], line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE) 
      fig2 = fig2 %>% add_segments(values$new_eq,values$shocked_params["r"],values$new_eq,0, line = list(color = 'rgb(150, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
      is_eq = list(
        x = values$new_eq,
        y = values$shocked_params["r"],
        text = paste0("y*=",round(values$new_eq))
      )
      fig2 = fig2 %>% layout(annotations = is_eq)
    } else{
      eq = list(
        x = values$eq,
        y = values$eq,
        text = paste0("y*=",round(values$eq))
      )
      fig1 = fig1 %>% layout(annotations = eq)
      is_eq = list(
        x = values$eq,
        y = input$r0,
        text = paste0("y*=",round(values$eq))
      )
      fig2 = fig2 %>% layout(annotations = is_eq)
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
    demand <- list(
      title = "Demande, Yd",
      titlefont = f
    )
    interest <- list(
      title = "Taux d'intérêt, r",
      titlefont = f
    )
    fig1 = fig1 %>% layout(xaxis = revenu, yaxis = demand)
    fig2 = fig2 %>% layout(xaxis = revenu, yaxis = interest)
    fig = subplot(fig1,fig2,shareX = TRUE,titleY = TRUE)
    fig
  })
  
  output$functions <- renderPlotly({
    
  })
}

shinyApp(ui = ui, server = server)

