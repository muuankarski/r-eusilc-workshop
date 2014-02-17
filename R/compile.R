#
pandoc_md <- function(name="index") {
  input <- paste(name,".md", sep="")
  output <- paste(name,".html", sep="")
  system(paste("pandoc -s -S --toc --number-section -H /home/aurelius/workspace/web/css/rmarkdown.css",input,"-o",output))
}

library(knitr)
knit("index.Rmd")
system("cp index.md README.md")
pandoc_md('index')
knit("data_longi.Rmd")
pandoc_md('data_longi')


# render('index.md')
# render('plan2010.md')
# render('plan2012.md')
# render('plan2013.md')
# render('plan2014.md')
# render('summary/summary.md')
# # render('pending/pending.md')
# 
# 
# library(markdown)
# library(rmarkdown)
# 
# markdownToHTML('index.md','index.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('plan2010.md','plan2010.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('plan2012.md','plan2012.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('plan2013.md','plan2013.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('plan2014.md','plan2014.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('summary/summary.md','summary/summary.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# markdownToHTML('pending/pending.md','pending/pending.html',options='toc',stylesheet='~/workspace/web/css/rmarkdown.css')
# 
# 
