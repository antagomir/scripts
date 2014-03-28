


require(brew)


include.fig = function (fname, captiontext, width = "5cm", height="5cm", rotate = NULL) {    

  
    cat("\\begin{figure}[h!]\\begin{center}\n")

    if (length(rotate)>0) {cat("\\rotatebox{",rotate,"}{")}
    cat("\\includegraphics")
    cat("[width=",width,", height=",height,"]")
    cat("{")
    cat(fname)
    cat("}")
    if (length(rotate)>0) {cat("}")}
    cat("\n")

    cat("\\caption*{")
    cat(captiontext)
    cat("}")

    cat("\n\\end{center}\\end{figure}\n\n\n")
    
}

include.figs = function (fnames, captiontext, width = "5cm", height="5cm", rotate = NULL) {    

  
    cat("\\begin{figure}[h!]\\begin{center}\n")


    for (i in 1:length(fnames)) {

      fname = fnames[[i]]
      
      if (length(rotate[[i]])>0) {cat("\\rotatebox{",rotate[[i]],"}{")}
      cat("\\includegraphics")
      cat("[width=",width,", height=",height,"]")
      cat("{")
      cat(fname)
      cat("}")
      if (length(rotate)>0) {cat("}")}
      cat("\n")
            
    }
    
    cat("\\caption*{")
    cat(captiontext)
    cat("}")

    cat("\n\\end{center}\\end{figure}\n\n\n")
    
}



################

include_graph <- function(width = 1, filename) {
    paste("\\includegraphics[width=", width, "\\linewidth]{",filename, "}", sep = "")
}

include_tbl <- function(width = 1, filename) {
     print(xtable(filename), table.placement = "",
         latex.environments = "", include.rownames = FALSE,
         floating = FALSE)
}

subfloat_graph <- function(width, filename, caption = "") {
     paste("\\subfloat[", caption, "]{", "\\begin{minipage}[h]{",
         width, "\\linewidth}\\centering", include_graph(width = 1,
             filename), "\\end{minipage}}", sep = "")
}

subfloat_tbl <- function(width, filename, caption) {
     paste("\\subfloat[", caption, "]{", "\\begin{minipage}[h]{",
         width, "\\linewidth}\\centering", print(xtable(filename),
             file = stderr(), table.placement = "",
             latex.environments = "", include.rownames = FALSE,
             floating = FALSE), "\\end{minipage}}",
         sep = "")
}

