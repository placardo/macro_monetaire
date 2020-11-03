
lm_curve = function(lr,Ms,p,ly,prod,r0){
  r = 1/lr*(Ms/p - ly*prod)
  r[r < r0] = r0
  return(r)
}

rm_lm_plot <- function(session,input,output,values){
  renderPlotly({
    masse = seq(0,input$Mmax,length.out = 1000)
    prod = seq(0,input$ymax,length.out = 1000)
    to_plot = data.frame(money = masse, revenu = prod)
    fig1 = plot_ly(to_plot, x = ~money)
    
    fig1 = fig1 %>% add_segments(x = input$Ms, y = 0, xend = input$Ms, yend = 15, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = I("blue"))
    fig1 = fig1 %>% add_trace(y = 1/input$lr*(masse/input$p - input$ly*input$ystar), type = "scatter", mode = "lines", name = "$$M^d$$", color = I("red"))
    fig1 = fig1 %>% add_segments(x = 0, y = values$eq$r, xend = input$Mmax, yend = values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
    
    fig2 = plot_ly(to_plot, x = ~revenu)
    fig2 = fig2 %>% add_trace(y = lm_curve(input$lr, input$Ms, input$p, input$ly, prod, input$r0), type = "scatter", mode = "lines", name = "$$LM_1$$", color = I("red"))
    fig2 = fig2 %>% add_segments(0,values$eq$r,input$ystar,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
    fig2 = fig2 %>% add_segments(input$ystar,0,input$ystar,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
    
    if(values$shock & !is.na(input$new_value_LM)){
      if(input$shocked_var_LM == "Ms"){
        fig1 = fig1 %>% add_segments(x = values$shocked_params["Ms"], y = 0, xend = values$shocked_params["Ms"], yend = 20, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = "rgb(0,20,200)")
        fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq$r, xend = input$Mmax, yend = values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
      } else{
        fig1 = fig1 %>% add_trace(y = 1/values$shocked_params["lr"]*(masse/values$shocked_params["p"] - values$shocked_params["ly"]*values$shocked_params["ystar"]), type = "scatter", mode = "lines", name = "$$M^d$$", color = "rgb(0,200,20)")
        fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq$r, xend = input$Mmax, yend = values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
      }
      new_eq = list(
        x = unname(values$shocked_params["Ms"]),
        y = values$new_eq$r,
        text = paste0("r*=",round(values$new_eq$r,1))
      )
      fig1 = fig1 %>% layout(annotations = new_eq)
      
      if(input$shocked_var_LM != "ystar"){
        fig2 = fig2 %>% add_trace(y = lm_curve(values$shocked_params["lr"],values$shocked_params["Ms"],values$shocked_params["p"],values$shocked_params["ly"],prod,input$r0), type = "scatter", mode = "lines", name = "$$LM_2$$", color = "rgb(0,200,20)")
      }
      fig2 = fig2 %>% add_segments(0,values$new_eq$r,values$shocked_params["ystar"],values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
      fig2 = fig2 %>% add_segments(values$shocked_params["ystar"],0,values$shocked_params["ystar"],values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
      lm_eq = list(
        x = unname(values$shocked_params["ystar"]),
        y = values$new_eq$r,
        text = paste0("r*=",round(values$new_eq$r,1))
      )
      fig2 = fig2 %>% layout(annotations = lm_eq)
    } else{
      eq = list(
        x = input$Ms,
        y = values$eq$r,
        text = paste0("r*=",round(values$eq$r,1))
      )
      fig1 = fig1 %>% layout(annotations = eq)
      is_eq = list(
        x = input$ystar,
        y = values$eq$r,
        text = paste0("r*=",round(values$eq$r,1))
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
}