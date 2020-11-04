BBOD <- function(B,By,y,max_price, OD){
  if(OD == "o"){
    func = 0.2*B+By*y
  } else{
    func = max_price-0.3072734*B+0.5*By*y
    func[func>max_price] = max_price
  }
  return(func)
}

BBPlot <- function(input,output,values){
  bonds = seq(0,200,length.out = 100)
  to_plot = data.frame(bonds = bonds)

  eq_Pb = BBOD(values$eq$B,input$by,values$eq$y,input$max_price, "o")
  fig = plot_ly(to_plot, x = ~bonds)
  fig = fig %>% add_trace(y = BBOD(bonds,input$by,values$eq$y,input$max_price,"o"), type = "scatter", mode = "lines", name = "$$B^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("Bs=%{x:.0f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(y = BBOD(bonds,input$by,values$eq$y,input$max_price,"d"), type = "scatter", mode = "lines", name = "$$B^d(N)$$", line = list(color = "red"),
                          hovertemplate = paste("Bd=%{x:.0f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eq_Pb,max(bonds),eq_Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(values$eq$B,0,values$eq$B,eq_Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>"))
  
  eq_label = list(
    x = values$eq$B,
    y = eq_Pb,
    text = paste0("B*=",round(values$eq$B),", Pb*=",round(eq_Pb))
  )
  fig = fig %>% layout(annotations = eq_label)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  empLevel <- list(
    title = "Nombre de titres en circulation, B",
    titlefont = f
  )
  salReels <- list(
    title = "Prix des titres, Pb",
    titlefont = f,
    range = c(70,110)
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = salReels)
}

RPBPlot <- function(input,output,values){
  r = seq(0,20,length.out = 1000)
  to_plot = data.frame(interest = r)

  eq_Pb = BBOD(values$eq$B,input$by,values$eq$y,input$max_price, "o")
  eq_R = values$eq$r
  fig = plot_ly(to_plot, x = ~interest)
  fig = fig %>% add_trace(y = 100/(r/100+1), type = "scatter", mode = "lines", name = "$$\\frac{100}{r+1}$$", line = list(color = "gold"),
                          hovertemplate = paste("r=%{x:.1f}","<br>Pb=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eq_Pb,eq_R,eq_Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(eq_R,0,eq_R,eq_Pb, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("B*=%{x:.0f}","<br>Pb*=%{y:.0f}","<extra></extra>"))

  eq_label = list(
    x = eq_R,
    y = eq_Pb,
    text = paste0("r*=",round(eq_R,1),", Pb*=",round(eq_Pb))
  )
  fig = fig %>% layout(annotations = eq_label)

  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  empLevel <- list(
    title = "Taux d'intérêt, r",
    titlefont = f
  )
  salReels <- list(
    title = "Prix des titres, Pb",
    titlefont = f
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = salReels)
}
