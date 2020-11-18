choicesGlob = c("p")
choicesNamesGlob = c("\\\\p")
choicesGlob <- setNames(choicesGlob, choicesNamesGlob)

choicesIS = c("alpha","iy","ir","id","cpi","bari","g","t")
choicesNamesIS = c("\\\\\\alpha",
                   "\\\\I_y",
                   "\\\\I_r",
                   "\\\\I_d",
                   "\\\\C_{\\pi}",
                   "\\\\\\bar{I}",
                   "\\\\g",
                   "\\\\t")
choicesIS <- setNames(choicesIS, choicesNamesIS)

choicesLM = c("ly","lr","ystar","Ms")
choicesNamesLM = c("\\\\L_y",
                   "\\\\L_r",
                   "\\\\y*",
                   "\\\\M^s")
choicesLM <- setNames(choicesLM, choicesNamesLM)

shock_params <- function(vars,i){
  shock <- fluidRow(
              column(5,
                     selectizeInput(paste0("shocked_var_",i),"Var. choquée:",
                          vars,
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
                  ),
                  column(5,
                         numericInput(paste0("new_value_",i),"Nouvelle Valeur", value = NULL)
                  )
                )
  
  return(shock)
}

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

glob_params <- fluidRow(
  column(3,
         numericInput("p","\\(p\\)",0,1000,value = 100,step=1)
  ),
  column(3),
  column(6,
          sliderInput("ymax","y max", 100,10000,2500,step = 100)          
  )
)

ui_LM_params1 <- fluidRow(
  column(3,
         numericInput("ly","\\(L_y\\)",0,1,value = 0.5,step=0.05)
  ),
  column(3,
         numericInput("lr","\\(L_r\\)",-1000,0,value = -100,step=10)
  ),
  column(3,
         numericInput("Ms","\\(M^s\\)", 0,4000,value=300,step = 100)       
  ),
  column(3,
         numericInput("rmin", "\\(r_{min}\\)", 0,5,value=1,step=0.5)
  )
)

ui_IS_params1 <- fluidRow(
  column(3,
         numericInput("alpha","\\(\\alpha\\)",0,1,value = 0.6,step=0.05)
  ),
  column(3,
         numericInput("iy","\\(I_y\\)",0,1,value = 0.05,step=0.05)
  ),
  column(3,
         numericInput("ir","\\(I_r\\)",-1000,1000,value = -100,step=10)
  ),
  column(3,
         numericInput("id","\\(I_D\\)",-1000,1000,value = -0.1,step=0.01)
  )
) 

ui_IS_params2 <- fluidRow(
  column(3,
         numericInput("cpi","\\(C_{\\pi}\\)",0,1000,value = 500,step=10)
  ),
  column(3,
         numericInput("bari","\\(\\bar{I}\\)",0,1000,value = 400,step=10)
  ),
  column(3,
         numericInput("g","\\(g\\)",0,1000,value = 350,step=10)
  ),
  column(3,
         numericInput("t","\\(t\\)",0,1000,value = 0,step=10)
  ),
  column(3, class = "collapse",
         numericInput("D","\\(D\\)",0,1000,value = 2000,step=10)
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
        numericInput("W","\\(\\bar{W}\\)",0,100,value = 40,step=0.5)
  ),
  column(4,
         numericInput("sal_res","Sal. réserve",0,100,value = 6)
  )
)