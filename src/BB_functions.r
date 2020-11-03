BBOD <- function(B,By,y,max_price, OD){
  if(OD == "o"){
    func = 0.5*(B-By*y)
  } else{
    func = max_price-0.5*(B-By*y)
    func[func>max_price] = max_price
  }
  return(func)
}

BBPlot <- function(input,output,values){
  bonds = seq(0,200,length.out = 100)
  to_plot = data.frame(bonds = bonds)
  
  max_price = 100
  By = 0.01
  eq = list(B = 100+1*By*input$ystar)
  eq$Pb = BBOD(eq$B,By,input$ystar,max_price, "o")
  fig = plot_ly(to_plot, x = ~bonds)
  fig = fig %>% add_trace(y = BBOD(bonds,By,input$ystar,max_price,"o"), type = "scatter", mode = "lines", name = "$$B^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("Bs=%{x:.0f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(y = BBOD(bonds,By,input$ystar,max_price,"d"), type = "scatter", mode = "lines", name = "$$B^d(N)$$", line = list(color = "red"),
                          hovertemplate = paste("Bd=%{x:.0f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eq$Pb,eq$B,eq$Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(eq$B,0,eq$B,eq$Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>"))
  
  eq_label = list(
    x = eq$B,
    y = eq$Pb,
    text = paste0("B*=",round(eq$B),", Pb*=",round(eq$Pb))
  )
  fig = fig %>% layout(annotations = eq_label)
  
  f <- list(
    family = "Colibri",
    size = 14,
    color = "#000000"
  )
  empLevel <- list(
    title = "Nombre de titres en circulation, B",
    titlefont = f
  )
  salReels <- list(
    title = "Prix des titres, Pb",
    titlefont = f
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = salReels)
}

PBRplot <- function(input,output,values){
  r = seq(0,15,length.out = 100)
  to_plot = data.frame(interest = r)

  eq = list(B = 100+1*By*input$ystar)
  fig = plot_ly(to_plot, x = ~interest)
  fig = fig %>% add_trace(y = 100/(r+1), type = "scatter", mode = "lines", name = "$$B^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("r=%{x:.2f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eq$Pb,eq$B,eq$Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(eq$B,0,eq$B,eq$Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>"))
  
  eq_label = list(
    x = eq$B,
    y = eq$Pb,
    text = paste0("B*=",round(eq$B),", Pb*=",round(eq$Pb))
  )
  fig = fig %>% layout(annotations = eq_label)
  
  f <- list(
    family = "Colibri",
    size = 14,
    color = "#000000"
  )
  empLevel <- list(
    title = "Nombre de titres en circulation, B",
    titlefont = f
  )
  salReels <- list(
    title = "Prix des titres, Pb",
    titlefont = f
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = salReels)
}
