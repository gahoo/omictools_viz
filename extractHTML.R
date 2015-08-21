library(XML)
library(plyr)

extractTable<-function(html){
  readHTMLTable(html, stringsAsFactors=F)[[1]] %>%
    mutate(V1=gsub(' ', '_', V1)) %>%
    dlply(.variables='V1', .fun=function(x){x$V2})
}

extractHtmlInfo<-function(html_file, xpaths, ...){
  extractXpath<-function(doc, xpath){
    if(is.null(xpath$attr)){
      xpathSApply(doc, xpath$xpath, xmlValue)
    }else{
      xpathSApply(doc, xpath$xpath, xmlGetAttr, name=xpath$attr)
    }
  }
  
  html<-readLines(html_file)
  doc<-htmlParse(html, asText=TRUE, ...)
  
  info<-lapply(xpaths, extractXpath, doc=doc)
  c(info, extractTable(html))
}

html_files<-dir('omictools.com/', pattern="*.html")

s_idx<-grep("s[0-9]+\\.html",html_files)
c_idx<-grep("c[0-9]+-",html_files)
html_files[setdiff(1:10720,c(s_idx,c_idx))]

software_xpaths<-list(
  Description=list(
       xpath="id('main')/article/div/div/div[1]"),
  Link=list(
       xpath="id('main')/article/div/div/div/a",
       attr='href'),
  Related=list(
       xpath="id('main')/div[@class='main-item box']/ul/li/a",
       attr='title')
  )

test_file<-'omictools.com/-13-c-based-metabolic-flux-analysis-s7233.html'
test<-extractHtmlInfo(test_file, software_xpaths)

kk<-html_files[s_idx[1:500]] %>%
  sprintf(fmt="omictools.com/%s") %>%
  lapply(extractHtmlInfo, xpaths=software_xpaths)

names(kk)<-html_files[s_idx[1:5]]