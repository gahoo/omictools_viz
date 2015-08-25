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
      values<-xpathSApply(doc, xpath$xpath, xmlValue)
    }else{
      values<-xpathSApply(doc, xpath$xpath, xmlGetAttr, name=xpath$attr)
    }
    
    if(length(values)==0){
      NA
    }else{
      values
    }
  }
  
  doc<-htmlParse(html, asText=TRUE, ...)
  lapply(xpaths, extractXpath, doc=doc)
}

extractSoftHtmlInfo<-function(html_file, xpaths, ...){
  html<-readLines(html_file, warn = F)
  c(extractHtmlInfo(html, xpaths, ...), extractTable(html))
}

extractCatalogHtmlInfo<-function(html_file, xpaths=NULL, ...){
  html<-readLines(html_file, warn = F)
  if(is.null(xpaths)){
    if(length(grep("categories-nav", html))!=0){
      xpaths=catalog_folder_xpaths
    }else{
      xpaths=catalog_software_xpaths
    }
  }
  
  extractHtmlInfo(html, xpaths, ...)
}

software_xpaths<-list(
  description=list(
    xpath="id('main')/article/div/div/div[1]"),
  link=list(
    xpath="id('main')/article/div/div/div/a",
    attr='href'),
  related=list(
    xpath="id('main')/div[@class='main-item box']/ul/li/a",
    attr='title'),
  catalog=list(
    xpath="id('main')/div[@class='breadcrumb']/a[position()>1]"),
  img=list(
    xpath="//div[@class='site-preview']/a/img",
    attr='src')
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
  count=list(
      xpath="//nav[@class='categories-nav']/ul/li/span"
      )
  )

catalog_software_xpaths<-list(
  parent_desc=list(
    xpath="//div[@class='main-item box'][not(ul)]"),
  parent_alias=list(
    xpath="id('main')/div[@class='header1 navbar']/h1"),
  parent=list(
    xpath="id('main')/div[@class='breadcrumb']/a[last()]"),
  name=list(
    xpath="//div[@class='category-site-details']//a"),
  href=list(
    xpath="//div[@class='category-site-details']//a",
    attr='href'),
  type=list(
    xpath="//div[@class='category-site-details']/header[not(abbr)]/h3/a|//div[@class='category-site-details']/header/abbr",
    attr='title'),
  img=list(
    xpath="//aside[@class='category-site-thumbnail']/a/img",
    attr='src')
  )

checklink_xpaths<-list(
  href=list(
    xpath='//a',
    attr='href'),
  map=list(
    xpath='//area',
    attr='href')
  )


html_files<-dir('omictools.com/', pattern="*.html")
s_idx<-grep("-s[0-9]+\\.html",html_files)
c_idx<-grep("-c[0-9]+-",html_files)
software_html_files<-html_files[s_idx]
catalog_html_files<-html_files[c_idx]
other_files<-setdiff(html_files, c(software_html_files, catalog_html_files))
print(other_files)

#test_file<-'omictools.com/-13-c-based-metabolic-flux-analysis-s7233.html'
#test<-extractSoftHtmlInfo(test_file, software_xpaths)

extractInfo<-function(htmlfiles, fun, xpaths=NULL){
  info<-htmlfiles %>%
    sprintf(fmt="omictools.com/%s") %>%
    llply(fun, xpaths=xpaths,
          .progress=progress_text(char='.'))
  
  names(info)<-htmlfiles
  info
}

checkCatalogUnDownloadLink<-function(htmlfiles, checklink_xpaths){  
  un_down<-extractInfo(htmlfiles, extractCatalogHtmlInfo, checklink_xpaths) %>%
    unlist
  
  idx<-grep('omictools.com', un_down)
  unique(un_down[idx])
}

un_down<-checkCatalogUnDownloadLink(catalog_html_files[1:10], checklink_xpaths)
print(un_down)

software<-extractInfo(software_html_files[1:10], extractSoftHtmlInfo, software_xpaths)
catalog<-extractInfo(catalog_html_files[1:10], extractCatalogHtmlInfo)

software_df<-software %>%
  lapply(function(x){lapply(x, paste0, collapse = ';')}) %>%
  lapply(as.data.frame) %>%
  ldply(.id='omictools_link')

folder_idx<-catalog %>%
  lapply(function(x){is.null(x$count)}) %>%
  unlist

catalog_folder<-catalog[folder_idx]
catalog_software<-catalog[!folder_idx]

catalog_df<-catalog %>%
  lapply(as.data.frame) %>%
  ldply(.id='parent_href')

catalog_folder_df<-catalog_df %>%
  filter(!is.na(count)) %>%
  filter(parent_href %in% catalog_html_files) %>%
  select(-parent_desc, -type) %>%
  mutate(count = as.numeric(gsub('[()]', '', as.character(count))) )

catalog_software_df<-catalog_df %>%
  filter(is.na(count)) %>%
  filter(parent_href %in% catalog_html_files) %>%
  select(-count)

save(software, software_df, catalog, catalog_software, catalog_folder, catalog_software_df, catalog_folder_df, file='omictools.RData')
