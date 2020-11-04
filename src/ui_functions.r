choicesIS = c("alpha","iy","ir","cpi","bari","g")
choicesNamesIS = c("\\\\\\alpha",
                   "\\\\I_y",
                   "\\\\I_r",
                   "\\\\C_{\\pi}",
                   "\\\\\\bar{I}",
                   "\\\\g")
choicesIS <- setNames(choicesIS, choicesNamesIS)

choicesLM = c("p","ly","lr","ystar","Ms")
choicesNamesLM = c("\\\\p",
                   "\\\\L_y",
                   "\\\\L_r",
                   "\\\\y*",
                   "\\\\M^s")
choicesLM <- setNames(choicesLM, choicesNamesLM)


IS_shock <- selectizeInput("shocked_var_IS","Variable choquée (IS):",
                           choicesIS,
                           options = list(render = I("
                                          {
                                            item: function(item, escape) {
                                                    var html = katex.renderToString(item.label);
                                                    return '<div>' + html + '</div>';
                                                  },
                                            option: function(item, escape) {
                                                      var html = katex.renderToString(item.label);
                                                      return '<div>' + html + '</div>';
                                                    }
                                          }"))
                          )

LM_shock <- selectizeInput("shocked_var_LM","Variable choquée (LM):",
                           choicesLM,
                           options = list(render = I("
                                          {
                                            item: function(item, escape) {
                                                    var html = katex.renderToString(item.label);
                                                    return '<div>' + html + '</div>';
                                                  },
                                            option: function(item, escape) {
                                                      var html = katex.renderToString(item.label);
                                                      return '<div>' + html + '</div>';
                                                    }
                                          }"))
)

ui_LM_params1 <- fluidRow(
  column(4,
         numericInput("p","\\(p\\)",0,10,value = 1,step=1)
  ),
  column(4,
         numericInput("ly","\\(L_y\\)",0,1,value = 0.5,step=0.05)
  ),
  column(4,
         numericInput("lr","\\(L_r\\)",-1000,0,value = -100,step=10)
  )
)

ui_LM_params2 <- fluidRow(
  column(4,
         numericInput("Ms","\\(M^s\\)", 0,4000,value=300,step = 100)       
  ),
  column(4,
         numericInput("rmin", "\\(r_{min}\\)", 0,5,value=1,step=0.5)
  )
)

ui_IS_params1 <- fluidRow(
  column(4,
         numericInput("alpha","\\(\\alpha\\)",0,1,value = 0.6,step=0.05)
  ),
  column(4,
         numericInput("iy","\\(I_y\\)",0,1,value = 0.05,step=0.05)
  ),
  column(4,
         numericInput("ir","\\(I_r\\)",-1000,1000,value = -100,step=10)
  )
) 

ui_IS_params2 <- fluidRow(
  column(4,
         numericInput("cpi","\\(C_{\\pi}\\)",0,1000,value = 200,step=10)
  ),
  column(4,
         numericInput("bari","\\(\\bar{I}\\)",0,1000,value = 350,step=10)
  ),
  column(4,
         numericInput("g","\\(g\\)",0,1000,value = 350,step=10)
  )
)

ui_BB_params <- fluidRow(
  column(4,
         numericInput("by","\\(B_y\\)",0,1,value = 0.05,step = 0.05)
  ),
  column(4,
         numericInput("br","\\(B_r\\)",0,10,value = 1)
  ),
  column(4,
         numericInput("max_price","Val. Faciale",0,1,value = 100)
  )
)

ui_LabM_params <- fluidRow(
  column(4,
        numericInput("W","\\(\\bar{W}\\)",0,100,value = 40)
  ),
  column(4,
        numericInput("ny","\\(N_y\\)",0,1,value = 0.01,step = 0.01)
  ),
  column(4,
         numericInput("sal_res","Sal. réserve",0,100,value = 10)
  )
)