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
    }else{ #category-site-details
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
    xpath="//div[@class='main-item box'][not(ul)][last()]"),
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

applyHtml<-function(htmlfiles, fun, ...){
  htmls<-htmlfiles %>%
    sprintf(fmt="omictools.com/%s") %>%
    llply(fun, ...,
          .progress=progress_text(char='.'))
  
  names(htmls)<-htmlfiles
  htmls
}

checkCatalogUnDownloadLink<-function(htmlfiles, checklink_xpaths){  
  un_down<-applyHtml(htmlfiles, extractCatalogHtmlInfo, checklink_xpaths) %>%
    unlist
  
  idx<-grep('omictools.com', un_down)
  unique(un_down[idx])
}

un_down<-checkCatalogUnDownloadLink(catalog_html_files, checklink_xpaths)
print(un_down)

software<-applyHtml(software_html_files[1:1000], extractSoftHtmlInfo, software_xpaths)
catalog<-applyHtml(catalog_html_files, extractCatalogHtmlInfo)

software_df<-software %>%
  lapply(function(x){lapply(x, paste0, collapse = ';')}) %>%
  lapply(as.data.frame) %>%
  ldply(.id='omictools_link')

software_idx<-catalog %>%
  lapply(function(x){is.null(x$count)}) %>%
  unlist

catalog_folder<-catalog[!software_idx]
catalog_software<-catalog[software_idx]

catalog_df<-catalog %>%
  lapply(as.data.frame) %>%
  ldply(.id='parent_href') %>%
  mutate(parent = gsub(' $', '', gsub('^ ', '', parent)) )

catalog_folder_df<-catalog_df %>%
  filter(!is.na(count)) %>%
  #  filter(parent_href %in% catalog_html_files) %>%
  select(-parent_desc, -type) %>%
  mutate(count = as.numeric(gsub('[()]', '', as.character(count))) )

catalog_software_df<-catalog_df %>%
  filter(is.na(count)) %>%
  #  filter(parent_href %in% catalog_html_files) %>%
  mutate(type = gsub("^Details :.*$", "", type)) %>%
  select(-count)

save(software, software_df, catalog, catalog_software, catalog_folder, catalog_software_df, catalog_folder_df, file='omictools.RData')
############### patch
load('omictools.RData')
getCatalogType<-function(html_file){
  html<-readLines(html_file, warn = F)
  types<-c()
  
  if(length(grep("categories-nav", html))!=0){
    types<-c(types, "categories-nav")
  }
  if(length(grep("category-site-details", html))!=0){
    types<-c(types, "category-site-details")
  }
  data.frame(type=types)
}

missing_catalog_software<-catalog_html_files %>%
  applyHtml(getCatalogType) %>%
  ldply(.id='htmlfile')%>%
  filter(duplicated(htmlfile)) %>%
  select(htmlfile) %>%
  mutate(htmlfile = as.character(htmlfile)) %>%
  unlist %>%
  applyHtml(extractCatalogHtmlInfo,
            xpaths = catalog_software_xpaths)

missing_catalog_software_df<-missing_catalog_software %>%
  lapply(as.data.frame) %>%
  ldply(.id='parent_href') %>%
  mutate(parent = gsub(' $', '', gsub('^ ', '', parent)) )

####### do the right thing from beginning
contentFilter<-function(filename, pattern){
  content<-readLines(filename, warn = F)
  if(length(grep(pattern, content))!=0){
    TRUE
  }else{
    FALSE
  }
}

catalog_folder_idx<-catalog_html_files %>%
  applyHtml(contentFilter, pattern="categories-nav") %>%
  unlist
catalog_software_idx<-catalog_html_files %>%
  applyHtml(contentFilter, pattern="category-site-details") %>%
  unlist

catalog_folder<-catalog_html_files[catalog_folder_idx] %>%
  applyHtml(extractCatalogHtmlInfo, xpaths = catalog_folder_xpaths)
catalog_software<-catalog_html_files[catalog_software_idx] %>%
  applyHtml(extractCatalogHtmlInfo, xpaths = catalog_software_xpaths)

list2df<-function(list2trans){
  list2trans %>%
    lapply(as.data.frame) %>%
    ldply(.id='parent_href') %>%
    mutate(parent = gsub(' $', '', gsub('^ ', '', parent)) )  
}

lsapply<-function(x){sapply(x,length)}

catalog_folder_df<-catalog_folder %>%
  list2df %>%
  mutate(count = as.numeric(gsub('[()]', '', as.character(count))) )

catalog_software_df<-catalog_software %>%
  list2df %>%
  mutate(type = gsub("^Details :.*$", "", type))
