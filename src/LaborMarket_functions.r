labMarketOD <- function(N,sal_min, OD){
  if(OD == "o"){
    func = 0.5*N
    func[func<sal_min] = sal_min
  } else{
    func = 50-0.5*N
  }
  return(func)
}

laborMarketPlot <- function(input,output,values){
  emp_level = seq(0,100,length.out = 100)
  to_plot = data.frame(emp_level = emp_level)
  
  sal_min = 10
  Nd = 30
  eq = list(N = Nd, W = labMarketOD(Nd,sal_min,"d"))
  fig = plot_ly(to_plot, x = ~emp_level)
  fig = fig %>% add_trace(y = labMarketOD(emp_level,sal_min,"o"), type = "scatter", mode = "lines", name = "$$N^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("Ns=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(y = labMarketOD(emp_level,sal_min,"d"), type = "scatter", mode = "lines", name = "$$N^d(N)$$", line = list(color = "red"),
                          hovertemplate = paste("Nd=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eq$W,eq$N,eq$W, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N*=%{x:.0f}","<br>W*=%{y:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(eq$N,0,eq$N,eq$W, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N*=%{x:.0f}","<br>W*=%{y:.0f}","<extra></extra>"))
  
  eq_label = list(
    x = eq$N,
    y = eq$W,
    text = paste0("N*=",round(eq$N),", W/p*=",round(eq$W))
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
