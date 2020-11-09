ISLMPlot <- function(input,output,values){
  prod = seq(0,input$ymax,length.out = 1000)
  to_plot = data.frame(revenu = prod)
  fig = plot_ly(to_plot, x = ~revenu)
  
  fig = plot_ly(to_plot, x = ~revenu)
  fig = fig %>% add_trace(y = ((1-input$alpha-input$iy)*prod-input$cpi-input$bari-input$g)/input$ir, type = "scatter", mode = "lines", name = "$$IS_1$$", line = list(color = "#ff0000"))
  fig = fig %>% add_trace(y = lm_curve(input$lr, input$Ms, values$p, input$ly, prod, input$rmin), type = "scatter", mode = "lines", name = "$$LM_1$$", line = list(color = "#007bff"))
  
  fig = fig %>% add_segments(0,values$eq$r,values$eq$y,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE) 
  fig = fig %>% add_segments(values$eq$y,0,values$eq$y,values$eq$r, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE)
  
  if(values$shock & (!is.na(input$new_value_LM) || !is.na(input$new_value_IS))){
    if(!is.na(input$new_value_IS)){
      fig = fig %>% add_trace(y = ((1-values$shocked_params_IS["alpha"]-values$shocked_params_IS["iy"])*prod-values$shocked_params_IS["cpi"]-values$shocked_params_IS["bari"]-values$shocked_params_IS["g"])/values$shocked_params_IS["ir"], type = "scatter", mode = "lines", name = "$$IS_2$$", line = list(color = "#aa0000"))
    }
    if(!is.na(input$new_value_LM)){
      fig = fig %>% add_trace(y = lm_curve(values$shocked_params_LM["lr"],values$shocked_params_LM["Ms"],values$shocked_params_LM["p"]/100,values$shocked_params_LM["ly"],prod,input$rmin), type = "scatter", mode = "lines", name = "$$LM_2$$", line = list(color = "#02346b"))
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
  return(fig)
}