labMarketOD <- function(N,sal_res,ny,y,OD){
  if(OD == "o"){
    func = 0.5*N
    func[func<sal_res] = sal_res
  } else{
    func = 50-0.5*N+0.5*ny*y
  }
  return(func)
}

laborMarketPlot <- function(input,output,values){
  emp_level = seq(0,100,length.out = 100)
  to_plot = data.frame(emp_level = emp_level)
  
  eqNo = input$W/(0.5*input$p)

  fig = plot_ly(to_plot, x = ~emp_level)
  fig = fig %>% add_trace(y = labMarketOD(emp_level,input$sal_res,input$ny,values$eq$y,"o"), type = "scatter", mode = "lines", name = "$$N^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("Ns=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(y = labMarketOD(emp_level,input$sal_res,input$ny,values$eq$y,"d"), type = "scatter", mode = "lines", name = "$$N^d(N)$$", line = list(color = "red"),
                          hovertemplate = paste("Nd=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,input$W,eqNo,input$W, line = list(color = 'blue', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N*=%{x:.0f}","<br>W*=%{y:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(values$eq$N,0,values$eq$N,input$W, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N*=%{x:.0f}","<br>W*=%{y:.0f}","<extra></extra>"))
  
  eq_label = list(
    x = values$eq$N,
    y = input$W,
    text = paste0("N*=",round(values$eq$N),", (W/p)*=",round(input$W))
  )
  fig = fig %>% layout(annotations = eq_label)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  empLevel <- list(
    title = "Niveau d'emploi, N",
    titlefont = f
  )
  salReels <- list(
    title = "Salaires réels, W/p",
    titlefont = f
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = salReels)
}
