
lm_curve = function(lr,Ms,p,ly,prod,rmin){
  r = 1/lr*(Ms/p - ly*prod)
  r[r < rmin] = rmin
  return(r)
}

rm_lm_plot <- function(session,input,output,values){
  renderPlotly({
    masse = seq(0,input$Mmax,length.out = 100)
    prod = seq(0,input$ymax,length.out = 100)
    to_plot = data.frame(money = masse, revenu = prod)
    fig1 = plot_ly(to_plot, x = ~money)
    
    fig1 = fig1 %>% add_segments(x = input$Ms, y = 0, xend = input$Ms, yend = 15, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = I("blue"),
                                 hovertemplate = paste("Ms=%{x:.0f}","<extra></extra>"))
    fig1 = fig1 %>% add_trace(y = lm_curve(input$lr, masse,input$p,input$ly,input$ystar, input$rmin), type = "scatter", mode = "lines", name = "$$M^d$$", color = I("red"),
                              hovertemplate = paste("Ms=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    fig1 = fig1 %>% add_segments(x = 0, y = values$eq$r, xend = input$Mmax, yend = values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                 hovertemplate = paste("Ms=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    
    fig2 = plot_ly(to_plot, x = ~revenu)
    fig2 = fig2 %>% add_trace(y = lm_curve(input$lr, input$Ms, input$p, input$ly, prod, input$rmin), type = "scatter", mode = "lines", name = "$$LM_1$$", color = I("red"),
                              hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    fig2 = fig2 %>% add_segments(0,values$eq$r,input$ystar,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                 hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    fig2 = fig2 %>% add_segments(input$ystar,0,input$ystar,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                 hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    
    if(values$shock & !is.na(input$new_value_LM)){
      if(input$shocked_var_LM == "Ms"){
        fig1 = fig1 %>% add_segments(x = values$shocked_params_LM["Ms"], y = 0, xend = values$shocked_params_LM["Ms"], yend = 20, type = "scatter", mode = "lines", name = "$$M = M^s$$", color = "rgb(0,20,200)",
                                     hovertemplate = paste("Ms=%{x:.0f}","<extra></extra>"))
        fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq$r, xend = input$Mmax, yend = values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                     hovertemplate = paste("Ms=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      } else{
        fig1 = fig1 %>% add_trace(y = lm_curve(values$shocked_params_LM["lr"],masse,values$shocked_params_LM["p"],values$shocked_params_LM["ly"],values$shocked_params_LM["ystar"],input$rmin), type = "scatter", mode = "lines", name = "$$M^d$$", color = "rgb(0,200,20)",
                                  hovertemplate = paste("Ms=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
        fig1 = fig1 %>% add_segments(x = 0, y = values$new_eq$r, xend = input$Mmax, yend = values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                     hovertemplate = paste("Ms=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      }
      new_eq = list(
        x = unname(values$shocked_params_LM["Ms"]),
        y = values$new_eq$r,
        text = paste0("r*=",round(values$new_eq$r,1))
      )
      fig1 = fig1 %>% layout(annotations = new_eq)
      
      if(input$shocked_var_LM != "ystar"){
        fig2 = fig2 %>% add_trace(y = lm_curve(values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"],values$shocked_params_LM["p"],values$shocked_params_LM["ly"],prod,input$rmin), type = "scatter", mode = "lines", name = "$$LM_2$$", color = "rgb(0,200,20)",
                                  hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      }
      fig2 = fig2 %>% add_segments(0,values$new_eq$r,values$shocked_params_LM["ystar"],values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      fig2 = fig2 %>% add_segments(values$shocked_params_LM["ystar"],0,values$shocked_params_LM["ystar"],values$new_eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      lm_eq = list(
        x = unname(values$shocked_params_LM["ystar"]),
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