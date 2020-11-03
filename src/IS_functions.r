fortyFivePlot <- function(input, output,values){
    prod = seq(0,input$ymax,length.out = 100)
    to_plot = data.frame(revenu = prod)
    fig = plot_ly(to_plot, x = ~revenu)
    
    fig = fig %>% add_trace(y = prod, type = "scatter", mode = "lines", name = "$$Y^d=y$$", color = I("blue"),
      hovertemplate = "y=yd=%{y:.0f}<extra></extra>")
    fig = fig %>% add_trace(y = (input$alpha + input$iy) * prod + input$ir*input$r0 + input$cpi + input$bari + input$g, type = "scatter", mode = "lines", name = "$$Y_1^d$$", color = I("red"),
      hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
    if(values$eq$y > 0){
      fig = fig %>% add_segments(0,values$eq$y,values$eq$y,values$eq$y, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
        hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
      fig = fig %>% add_segments(values$eq$y,0,values$eq$y,values$eq$y, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
        hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
    }
    
    if(values$shock & !is.na(input$new_value_IS)){
      fig = fig %>% add_trace(y = (values$shocked_params_IS["alpha"] + values$shocked_params_IS["iy"]) * prod + values$shocked_params_IS["ir"]*values$shocked_params_IS["r"] + values$shocked_params_IS["cpi"] + values$shocked_params_IS["bari"] + values$shocked_params_IS["g"], type = "scatter", mode = "lines", name = "$$Y_2^d$$", color = "rgb(0,200,20)",
        hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))    
      
      if(values$new_eq$y > 0){
        fig = fig %>% add_segments(0,values$new_eq$y,values$new_eq$y,values$new_eq$y, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend=F,
          hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
        fig = fig %>% add_segments(values$new_eq$y,0,values$new_eq$y,values$new_eq$y, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend=F,
          hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
        
        if(input$show_path){
          eq_tmp = values$eq$y
          for(i in 1:10){
            new_eq_tmp = unname((values$shocked_params_IS["alpha"]+values$shocked_params_IS["iy"])*eq_tmp + values$shocked_params_IS["ir"]*values$shocked_params_IS["r"]+values$shocked_params_IS["cpi"]+values$shocked_params_IS["bari"]+values$shocked_params_IS["g"])
            fig = fig %>% add_segments(eq_tmp,eq_tmp,eq_tmp,new_eq_tmp, line = list(color = 'rgb(150,150,150', width = 1, dash = "dash"), showlegend=F,
              hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
            fig = fig %>% add_segments(eq_tmp,new_eq_tmp,new_eq_tmp,new_eq_tmp, line = list(color = 'rgb(150,150,150', width = 1, dash = "dash"), showlegend=F,
              hovertemplate = paste("y=%{x:.0f}","<br>yd=%{y:.0f}","<extra></extra>"))
            eq_tmp = new_eq_tmp
          }
        }
        
        new_eq = list(
          x = values$new_eq$y,
          y = values$new_eq$y,
          text = paste0("y*=",round(values$new_eq$y))
        )
        fig = fig %>% layout(annotations = new_eq)
      }
    } else if(values$eq$y > 0){
      eq = list(
        x = values$eq$y,
        y = values$eq$y,
        text = paste0("y*=",round(values$eq$y))
      )
      fig = fig %>% layout(annotations = eq)
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
    fig = fig %>% layout(xaxis = revenu, yaxis = demand)
}

ISPlot <- function(input,output,values){
  prod = seq(0,input$ymax,length.out = 100)
  to_plot = data.frame(revenu = prod)
  
  fig = plot_ly(to_plot, x = ~revenu)
  fig = fig %>% add_trace(y = ((1-input$alpha-input$iy)*prod-input$cpi-input$bari-input$g)/input$ir, type = "scatter", mode = "lines", name = "$$IS_1$$", color = I("red"),
                            hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
  if(values$eq$y > 0){
    fig = fig %>% add_segments(0,input$r0,values$eq$y,input$r0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                 hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")) 
    fig = fig %>% add_segments(values$eq$y,input$r0,values$eq$y,0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                 hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
  }
  if(values$shock & !is.na(input$new_value_IS)){      
    fig = fig %>% add_trace(y = ((1-values$shocked_params_IS["alpha"]-values$shocked_params_IS["iy"])*prod-values$shocked_params_IS["cpi"]-values$shocked_params_IS["bari"]-values$shocked_params_IS["g"])/values$shocked_params_IS["ir"], type = "scatter", mode = "lines", name = "$$IS_2$$", color = "rgb(0,150,20)",
                              hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
    if(values$new_eq$y > 0){
      fig = fig %>% add_segments(0,values$shocked_params_IS["r"],values$new_eq$y,values$shocked_params_IS["r"], line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>")) 
      fig = fig %>% add_segments(values$new_eq$y,values$shocked_params_IS["r"],values$new_eq$y,0, line = list(color = 'rgb(150, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>r=%{y:.2f}","<extra></extra>"))
      is_eq = list(
        x = values$new_eq$y,
        y = values$shocked_params_IS["r"],
        text = paste0("y*=",round(values$new_eq$y))
      )
      fig = fig %>% layout(annotations = is_eq)
    }
  } else if(values$eq$y > 0){
    is_eq = list(
      x = values$eq$y,
      y = input$r0,
      text = paste0("y*=",round(values$eq$y))
    )
    fig = fig %>% layout(annotations = is_eq)
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
  
}

IeqSPlot <- function(input,output,values){
    prod = seq(0,input$ymax,length.out = 100)
    to_plot = data.frame(revenu = prod)

    invEpEq = (1-input$alpha)*values$eq$y-input$cpi
    fig = plot_ly(to_plot, x = ~revenu)
    fig = fig %>% add_trace(y = (1-input$alpha)*prod-input$cpi, type = "scatter", mode = "lines", name = "$$S_1(y)$$", line = list(color = "#00b70c"),
                              hovertemplate = paste("y=%{x:.0f}","<br>S=%{y:.0f}","<extra></extra>"))
    fig = fig %>% add_trace(y = input$iy*prod+input$ir*input$r0+input$bari+input$g, type = "scatter", mode = "lines", name = "$$I_1(y,r_0)$$", line = list(color = "#ab3bf2"),
                              hovertemplate = paste("y=%{x:.0f}","<br>I=%{y:.0f}","<extra></extra>"))
    if(invEpEq > 0){
      fig = fig %>% add_segments(0,invEpEq,values$eq$y,invEpEq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>I=S=%{y:.0f}","<extra></extra>")) 
      fig = fig %>% add_segments(values$eq$y,invEpEq,values$eq$y,0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                   hovertemplate = paste("y=%{x:.0f}","<br>I=S=%{y:.0f}","<extra></extra>"))
    }
    
    if(values$shock & !is.na(input$new_value_IS)){
      new_invEpEq = (1-values$shocked_params_IS["alpha"])*values$new_eq$y-values$shocked_params_IS["cpi"]
      if(input$shocked_var_IS == "cpi" | input$shocked_var_IS == "alpha"){
        fig = fig %>% add_trace(y = (1-values$shocked_params_IS["alpha"])*prod-values$shocked_params_IS["cpi"], type = "scatter", mode = "lines", name = "$$S_2(y)$$", line = list(color = "#006806"),
                                  hovertemplate = paste("y=%{x:.0f}","<br>S=%{y:.0f}","<extra></extra>"))
      } else{
        fig = fig %>% add_trace(y = values$shocked_params_IS["iy"]*prod+values$shocked_params_IS["ir"]*values$shocked_params_IS["r"]+values$shocked_params_IS["bari"]+values$shocked_params_IS["g"], type = "scatter", mode = "lines", name = "$$I_2(y,r_0)$$", line = list(color = "#581e7f"),
                                  hovertemplate = paste("y=%{x:.0f}","<br>I=%{y:.0f}","<extra></extra>"))
      }
      if(new_invEpEq > 0){
        fig = fig %>% add_segments(0,new_invEpEq,values$new_eq$y,new_invEpEq, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                     hovertemplate = paste("y=%{x:.0f}","<br>I=S=%{y:.0f}","<extra></extra>")) 
        fig = fig %>% add_segments(values$new_eq$y,new_invEpEq,values$new_eq$y,0, line = list(color = 'rgb(200, 0, 0)', width = 1, dash = 'dash'), showlegend = FALSE,
                                     hovertemplate = paste("y=%{x:.0f}","<br>I=S=%{y:.0f}","<extra></extra>"))
        
        is_eq = list(
          x = values$new_eq$y,
          y = new_invEpEq,
          text = paste0("y*=",round(values$new_eq$y), ", I=S=",round(new_invEpEq))
        )
        fig = fig %>% layout(annotations = is_eq)
      }
    } else{
      invEp_eq = list(
        x = values$eq$y,
        y = invEpEq,
        text = paste0("y*=",round(values$eq$y),", I=S=",round(invEpEq))
      )
      fig = fig %>% layout(annotations = invEp_eq)
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
    invEp <- list(
      title = "Investissement, Épargne, I,S",
      titlefont = f
    )
    fig = fig %>% layout(xaxis = revenu, yaxis = invEp)
}
