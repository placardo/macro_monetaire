labMarketOD <- function(N,sal_res,OD){
  if(OD == "o"){
    func = 0.5*N
    func[func<sal_res] = sal_res
  } else{
    func = 57-0.5*N
  }
  return(func)
}

laborMarketPlot <- function(input,output,values){
  emp_level = seq(0,100,length.out = 100)
  to_plot = data.frame(emp_level = emp_level)
  
  eqNo = (input$W)/(0.5*values$p)
  eqNstar = labMarketOD(values$eq$Nstar,input$sal_res,"o")

  fig = plot_ly(to_plot, x = ~emp_level)
  fig = fig %>% add_trace(y = labMarketOD(emp_level,input$sal_res,"o"), type = "scatter", mode = "lines", name = "$$N^s(N)$$", line = list(color = "blue"),
                          hovertemplate = paste("Ns=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(y = labMarketOD(emp_level,input$sal_res,"d"), type = "scatter", mode = "lines", name = "$$N^d(N)$$", line = list(color = "red"),
                          hovertemplate = paste("Nd=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,input$W/values$p,values$eq$subN,input$W/values$p, line = list(color = 'blue', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$subN,input$W/values$p,values$eq$N,input$W/values$p, line = list(color = 'green', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$N,input$W/values$p,eqNo,input$W/values$p, line = list(color = 'grey', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$subN,0,values$eq$subN,input$W/values$p, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("N=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_trace(x = values$eq$subN, y = input$W/values$p, mode = "markers", marker = list(color = "blue"), showlegend = F,
                          hovertemplate = paste("N=%{x:.0f}","<br>W/p=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$Nstar,0,values$eq$Nstar,eqNstar, line = list(color = 'rgb(200,0,0)', width = 1, dash = 'dash'), showlegend = F,
                             hovertemplate = paste("N*=%{x:.0f}","<br>(W/p)*=%{y:.0f}","<extra></extra>"))

  if(abs(values$eq$Nstar - values$eq$N) >= 5 & abs(values$eq$subN - values$eq$N) >= 5){
    x_label = c(values$eq$Nstar,values$eq$N,values$eq$subN)
    y_label = c(eqNstar,input$W/values$p,input$W/values$p)
    text_label = c(paste0("N*=",round(values$eq$Nstar),",(W/p)*=",round(eqNstar,1)),paste0("N=",round(values$eq$N)),paste0("N=",round(values$eq$subN)))
  } else if(abs(values$eq$Nstar - values$eq$N) >= 5 & abs(values$eq$subN - values$eq$N) <= 5){
    x_label = c(values$eq$Nstar,values$eq$N)
    y_label = c(eqNstar,input$W/values$p)
    text_label = c(paste0("N*=",round(values$eq$Nstar),",(W/p)*=",round(eqNstar,1)),paste0("N=",round(values$eq$N)))
  } else if(abs(values$eq$Nstar - values$eq$N) <= 5 & abs(values$eq$subN - values$eq$N) >= 5){
    x_label = c(values$eq$Nstar,values$eq$subN)
    y_label = c(eqNstar,input$W/values$p)
    text_label = c(paste0("N*=",round(values$eq$Nstar),",(W/p)*=",round(eqNstar,1)),paste0("N=",round(values$eq$subN)))
  } else{
    x_label = c(values$eq$Nstar)
    y_label = c(eqNstar)
    text_label = paste0("N*=",round(values$eq$Nstar),",(W/p)*=",round(eqNstar,1))
  }
  eq_label = list(
    x = x_label,
    y = y_label,
    text = text_label
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

margCost = function(N,w){
  return(w/(-N^0.6+13)+5)
}

firmEqPlot <- function(input,output,values){
  N = seq(0,70,length.out = 1000)
  to_plot = data.frame(emp_level = N)

  eqRev = margCost(values$eq$N,input$W/values$p)#*values$p
  subEqRev = margCost(values$eq$subN,input$W/values$p)
  fig = plot_ly(to_plot, x = ~emp_level)
  fig = fig %>% add_trace(y = margCost(N,input$W/values$p), type = "scatter", mode = "lines", name = "$$N^s(N)$$", line = list(color = "red"),
                          hovertemplate = paste("N=%{x:.0f}","<br>R=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(0,eqRev,98,eqRev, line = list(color = 'blue', width = 1), showlegend = FALSE,
                             hovertemplate = paste("N=%{x:.0f}","<br>R=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$subN,0,values$eq$subN,subEqRev, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("Nd=%{x:.0f}","<extra></extra>")) 
  fig = fig %>% add_segments(values$eq$subN, subEqRev, values$eq$subN, eqRev,  line = list(color = "green", width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("Nd=%{x:.0f}","<br>R=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$subN, subEqRev, values$eq$N, subEqRev,  line = list(color = "green", width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("Nd=%{x:.0f}","<br>R=%{y:.0f}","<extra></extra>"))
  fig = fig %>% add_segments(values$eq$N, 0, values$eq$N, eqRev,  line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                             hovertemplate = paste("Nd=%{x:.0f}","<br>R=%{y:.0f}","<extra></extra>"))
    
  if(abs(values$eq$subN - values$eq$N) > 5){
    x_label = c(values$eq$N,values$eq$subN)
    y_label = c(eqRev,subEqRev)
    text_label = c(paste0("N=",round(values$eq$N)),paste0("N=",round(values$eq$subN)))
  } else{
    x_label = c(values$eq$N)
    y_label = c(eqRev)
    text_label = paste0("N=",round(values$eq$N))
  }
  eq_label = list(
    x = x_label,
    y = y_label,
    text = text_label
  )
  fig = fig %>% layout(annotations = eq_label)
  
  f <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#7f7f7f"
  )
  empLevel <- list(
    title = "Niveau d'emploi, N",
    titlefont = f,
    range = c(0,70)
  )
  revCosts <- list(
    title = "Revenus et Couts",
    titlefont = f,
    range = c(0,40)
  )
  fig = fig %>% layout(xaxis = empLevel, yaxis = revCosts)
  
}
