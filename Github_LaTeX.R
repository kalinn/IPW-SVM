rm(list=ls())
library(knitr)
library(Hmisc)
library(stringr)
library(animation)
ani.options(autobrowse = FALSE)
ani.options(interval = 0)
converter = function(pdfname, extra.opts = "-density 300"){
  pngname = gsub("[.]pdf$", ".png", pdfname)
  im.convert(pdfname, output = pngname, extra.opts = extra.opts)
  pngname
}

crop_latex = function(rmd){
  all_latexs = parse_latex(rmd)
  filenames = sapply(seq_along(all_latexs), function(eq_no){
    latex = all_latexs[eq_no]
    outfile = get_latex(latex)
    filename = sprintf("eq_no%00d.png", eq_no)
    file.copy(outfile, filename)
    return(filename)
  })
}

get_latex = function(latex){
  # latex = latexTranslate(latex)
  mystr = c("\\documentclass{article}",
            "\\usepackage{amsmath}",
            "\\usepackage{amsfonts}",
            "\\usepackage{amssymb}",
            "\\begin{document}", 
            "\\thispagestyle{empty}",
            latex, "\\end{document}")
  stub = tempfile(fileext = "")
  infile = paste0(stub, ".tex")
  writeLines(mystr, con = infile)
  # system(sprintf("open %s", infile))
  outfile = paste0(stub, ".pdf")
  cwd = getwd()
  setwd(tempdir())
  system(sprintf("pdflatex %s", basename(infile)))
  setwd(cwd)
  plot_crop(outfile)
  converter(outfile)
  outfile = paste0(stub, ".png")
  return(outfile)
}

# insert_string = '![latex equation](%s%s)'
parse_latex = function(rmd, new_md, img_prefix = "", 
                       text_height = 20,
                       insert_string = 
                         paste0('<img src="%s%s" alt="Equation not rendered" height="', 
                                text_height, '">')){
  outdir = dirname(rmd)
  stopifnot(file.exists(rmd))
  ext = strsplit(rmd, "[.]")[[1]]
  ext = toupper(ext[length(ext)])
  tfile = tempfile(fileext = ".md")
  stopifnot(ext %in% c("MD", "RMD"))
  if (ext == "RMD"){
    knit(input = rmd, output = tfile)
  }
  if (ext == "MD"){
    file.copy(from = rmd, to = tfile)
  }  
  xmd = md = readLines(tfile)
  
  bad_string = "ZZZZZZZZZZZZZZZ"
  
  md = paste(md, collapse = "\n")
  double_latex = gsub("\\$\\$(.+?)\\$\\$", 
                      paste0(bad_string, "$$\\1$$", bad_string), 
                      md)
  double_latex = strsplit(double_latex, bad_string)[[1]]
  double_latex = double_latex[grepl("$$", double_latex, fixed=TRUE)]
  
  eq_no = 0
  if (length(double_latex) > 0){
    outfiles = sapply(double_latex, get_latex)
    filenames = sprintf("eq_no_%02.0f.png", 
                        seq(eq_no+1, eq_no + length(outfiles)))
    eq_no = eq_no + length(outfiles)
    filenames = file.path(outdir, filenames)
    mapply(function(x, y){
      file.copy(x, y, overwrite = TRUE)
    }, outfiles, filenames)
    
    new_str = sprintf(insert_string, img_prefix, 
                      basename(filenames))
    for (istr in seq(length(outfiles))){
      md = sub("\\$\\$(.+?)\\$\\$", 
               new_str[istr], 
               md)
    }
    
    writeLines(md, con = tfile)
    xmd = readLines(tfile)
  }
  
  md = xmd
  start_ticks = grep("^```", md)
  rm.ind = NULL
  if (length(start_ticks) > 0){
    start_ticks = matrix(start_ticks, ncol = 2, byrow = TRUE)
    rm.ind = unlist(apply(start_ticks, 1, function(x){
      seq(x[1], x[2])
    }))
  }
  
  rm_vals = md[rm.ind]
  md = md[-rm.ind]
  double_latex = gsub("\\$(.+?)\\$", paste0(bad_string, "$\\1$", bad_string), 
                      md)
  double_latex = strsplit(double_latex, bad_string)
  double_latex = unlist(sapply(double_latex, function(x){
    x = str_trim(x[grepl("$", x, fixed = TRUE)])
  }))
  #### turn all into equations
  #   double_latex = sapply(double_latex, function(x){
  #     x = paste0("$", x, "$")
  #   })
  #   
  if (length(double_latex) > 0){
    outfiles = sapply(double_latex, get_latex)
    filenames = sprintf("eq_no_%02.0f.png", 
                        seq(eq_no+1, eq_no + length(outfiles)))
    eq_no = eq_no + length(outfiles)
    filenames = file.path(outdir, filenames)
    mapply(function(x, y){
      file.copy(x, y, overwrite = TRUE)
    }, outfiles, filenames)
    
    md = xmd
    md = paste(md, collapse = "\n")
    new_str = sprintf(insert_string, img_prefix, 
                      basename(filenames))
    for (istr in seq(length(outfiles))){
      md = sub("\\$(.+?)\\$", 
               new_str[istr], 
               md)
    }
    
    writeLines(md, con = tfile)
    xmd = readLines(tfile)
  }
  
  file.copy(tfile, new_md, overwrite = TRUE)
  #  
}



rmd = "~/Dropbox/Packages/Github_Markdown_LaTeX/README_unparse.Rmd"
new_md = "~//Dropbox/Packages/Github_Markdown_LaTeX/README.md"
img_prefix = "https://rawgit.com/muschellij2/Github_Markdown_LaTeX/master/"

parse_latex(rmd, 
            new_md, 
            img_prefix)
