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