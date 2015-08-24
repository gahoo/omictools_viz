library(XML)
library(plyr)
library(dplyr)

extractTable<-function(html){
  readHTMLTable(html, stringsAsFactors=F)[[1]] %>%
    mutate(V1=gsub(' ', '_', V1)) %>%
    dlply(.variables='V1', .fun=function(x){x$V2})
}

extractHtmlInfo<-function(html, xpaths, ...){
  extractXpath<-function(doc, xpath){
    if(is.null(xpath$attr)){
      xpathSApply(doc, xpath$xpath, xmlValue)
    }else{
      xpathSApply(doc, xpath$xpath, xmlGetAttr, name=xpath$attr)
    }
  }
  
  doc<-htmlParse(html, asText=TRUE, ...)
  lapply(xpaths, extractXpath, doc=doc)
}

extractSoftHtmlInfo<-function(html_file, xpaths, ...){
  html<-readLines(html_file)
  c(extractHtmlInfo(html, xpaths, ...), extractTable(html))
}

extractCatalogHtmlInfo<-function(html_file, ...){
  html<-readLines(html_file)
  if(length(grep("categories-nav", html))!=0){
    xpaths=catalog_folder_xpaths
  }else{
    xpaths=catalog_software_xpaths
  }
  
  extractHtmlInfo(html, xpaths, ...)
}


html_files<-dir('omictools.com/', pattern="*.html")

s_idx<-grep("-s[0-9]+\\.html",html_files)
c_idx<-grep("-c[0-9]+-",html_files)
html_files[setdiff(1:length(html_files), c(s_idx,c_idx))]

software_xpaths<-list(
  Description=list(
       xpath="id('main')/article/div/div/div[1]"),
  Link=list(
       xpath="id('main')/article/div/div/div/a",
       attr='href'),
  Related=list(
       xpath="id('main')/div[@class='main-item box']/ul/li/a",
       attr='title'),
  Catalog=list(
       xpath="id('main')/div[@class='breadcrumb']/a[position()>1]")
  )

catalog_folder_xpaths<-list(
  parent_alias=list(
    xpath="id('main')/div[@class='header1 navbar']/h1"),
  parent=list(
       xpath="id('main')/div[@class='breadcrumb']/a[last()]"),
  name=list(
       xpath="//nav[@class='categories-nav']/ul/li/a",
       attr='title'),
  href=list(
       xpath="//nav[@class='categories-nav']/ul/li/a",
       attr='href'),
  img=list(
       xpath="//nav[@class='categories-nav']/ul/li/a/img",
       attr='src'),
  number=list(
      xpath="//nav[@class='categories-nav']/ul/li/span"
      )
  )

catalog_software_xpaths<-list(
  #parent_desc=list(
  #  xpath="//div[@class='main-item box'][1]"),
  name=list(
    xpath="//div[@class='category-site-details']//a"),
  href=list(
    xpath="//div[@class='category-site-details']//a",
    attr='href'),
  type=list(
    xpath="//div[@class='category-site-details']/header[not(abbr)]/h3/a|//div[@class='category-site-details']/header/abbr",
    attr='title')
  )

checklink_xpaths<-list(
  href=list(
    xpath='//a',
    attr='href'),
  map=list(
    xpath='//area',
    attr='href')
  )

test_file<-'omictools.com/-13-c-based-metabolic-flux-analysis-s7233.html'
test<-extractSoftHtmlInfo(test_file, software_xpaths)

broken<-html_files[c_idx] %>%
  sprintf(fmt="omictools.com/%s") %>%
  lapply(extractCatalogHtmlInfo, xpaths=checklink_xpaths) %>%
  unlist

undownload_links_idx<-grep('omictools.com', broken)
unique(broken[undownload_links_idx])

software<-html_files[s_idx[1:5]] %>%
  sprintf(fmt="omictools.com/%s") %>%
  lapply(extractSoftHtmlInfo, xpaths=software_xpaths)

names(software)<-html_files[s_idx[1:5]]

catalog<-html_files[c_idx[1:30]] %>%
  sprintf(fmt="omictools.com/%s") %>%
  lapply(extractCatalogHtmlInfo)

names(catalog)<-html_files[c_idx[1:30]]

catalog_df<-catalog %>%
  lapply(as.data.frame) %>%
  ldply(.id='parent_href')

catalog_df_c<-catalog_df %>%
  filter(!is.na(number)) %>%
  filter(parent_href %in% html_files[c_idx[1:30]])
