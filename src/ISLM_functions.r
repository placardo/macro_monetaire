ISLMPlot <- function(input,output,values){
  prod = seq(0,input$ymax,length.out = 1000)
  to_plot = data.frame(revenu = prod)
  fig = plot_ly(to_plot, x = ~revenu)
  
  fig = fig %>% add_trace(y = ((1-input$alpha-input$iy)*prod+input$alpha*input$t-input$cpi-input$id*input$D/values$p-input$bari-input$g)/input$ir, 
                          type = "scatter", mode = "lines", name = "$$IS_1$$", line = list(color = "#ff0000"),
                          hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
  fig = fig %>% add_trace(y = lm_curve(input$lr, input$Ms, values$p, input$ly, prod, input$rmin), 
                          type = "scatter", mode = "lines", name = "$$LM_1$$", line = list(color = "#007bff"),
                          hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
  
  fig = fig %>% add_segments(0,values$eq$r,values$eq$y,values$eq$r,
                             line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")) 
  fig = fig %>% add_segments(values$eq$y,0,values$eq$y,values$eq$r, 
                             line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
  
  if(values$shock){
    for(i in 1:2){
      if(!is.na(input[[paste0("new_value_",i)]])){
        if(input[[paste0("shocked_var_",i)]] %in% choicesIS){
          fig = with(values[[paste0("shocked_params_",i)]], 
                     add_trace(fig, y = ((1-alpha-iy)*prod+alpha*t-cpi-id*input$D/p-bari-g)/ir,
                               type = "scatter", mode = "lines",
                               name = paste0("$$IS_2^",i,"$$"), line = list(color = paste0("#",i+7,"a0000")),
                               hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")))
        } else if(input[[paste0("shocked_var_",i)]] %in% choicesLM){
          fig = with(values[[paste0("shocked_params_",i)]], 
                     add_trace(fig, y = lm_curve(lr,Ms,p,ly,prod,input$rmin), 
                               type = "scatter", mode = "lines", 
                               name = paste0("$$LM_2^",i,"$$"), line = list(color = paste0("#0234",3+i^2,"b")),
                               hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")))
        } else if(input[[paste0("shocked_var_",i)]] == "p"){
          fig = with(values[[paste0("shocked_params_",i)]], 
                     add_trace(fig, y = ((1-alpha-iy)*prod+alpha*t-cpi-id*input$D/p-bari-g)/ir,
                               type = "scatter", mode = "lines",
                               name = paste0("$$IS_2^",i,"$$"), line = list(color = paste0("#",i+7,"a0000")),
                               hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")))
          fig = with(values[[paste0("shocked_params_",i)]], 
                     add_trace(fig, y = lm_curve(lr,Ms,p,ly,prod,input$rmin), 
                               type = "scatter", mode = "lines", 
                               name = paste0("$$LM_2^",i,"$$"), line = list(color = paste0("#0234",3+i^2,"b")),
                               hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")))
        }
        
        fig = fig %>% add_segments(0,values[[paste0("new_eq_",i)]]$r,values[[paste0("new_eq_",i)]]$y,values[[paste0("new_eq_",i)]]$r, 
                                   line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")) 
        fig = fig %>% add_segments(values[[paste0("new_eq_",i)]]$y,0,values[[paste0("new_eq_",i)]]$y,values[[paste0("new_eq_",i)]]$r, 
                                   line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
        
        islm_new_eq = list(
          x = values[[paste0("new_eq_",i)]]$y,
          y = values[[paste0("new_eq_",i)]]$r,
          text = paste0("y*=",round(values[[paste0("new_eq_",i)]]$y), ", r*=",round(values[[paste0("new_eq_",i)]]$r,4))
        )
        fig = fig %>% layout(annotations = islm_new_eq)
      }
    }
  } else{
    islm_eq = list(
      x = values$eq$y,
      y = values$eq$r,
      text = paste0("y*=",round(values$eq$y), ", r*=",round(values$eq$r,4))
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
    titlefont = f,
    range = c(0,10)
  )
  fig = fig %>% layout(xaxis = revenu, yaxis = interest)
  return(fig)
}