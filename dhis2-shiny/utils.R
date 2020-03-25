
require(glue)
require(dplyr)
require(tibble)
require(jsonlite)
require(httr)
require(tidyr)
require(stringr)
require(DT)
require(rpivotTable)

options(baseurl="https://play.dhis2.org/2.33.2/")

DHISLogin <- function(baseurl, username, password) {
  httr::set_config(httr::config(http_version = 0))
  url <- URLencode(URL = paste0(getOption("baseurl"), "api/me"))
  #Logging in here will give us a cookie to reuse
  r <- httr::GET(url,
                 httr::authenticate(username, password),
                 httr::timeout(60))
  if (r$status != 200L) {
    return(FALSE)
  } else {
    me <- jsonlite::fromJSON(httr::content(r, as = "text"))
    options("organisationUnit" = me$organisationUnits$id)
    return(TRUE)
  }
}

d2_analyticsResponse <- function(url,remapCols=TRUE) {
  d <- jsonlite::fromJSON(content(GET(url), "text"))
  if ( NROW(d$rows) > 0 ) {
    metadata <- do.call(bind_rows,
                        lapply(d$metaData$items,
                               data.frame, stringsAsFactors = FALSE))
    remapMeta <-
      function(x) {
        plyr::mapvalues(x, metadata$uid,metadata$name, warn_missing = FALSE)
      }
    
    d<-tibble::as_tibble(d$rows) %>% `names<-`(., d$headers$column)
    if(remapCols == TRUE) {
      d<-plyr::colwise(remapMeta)(d)
    }
    return(d) } else {
      return(NULL)
    }
}

getDemoData <- function() {
  
  base_url<-getOption("baseurl")
  
  url<-glue::glue("{base_url}api/29/analytics?dimension=dx:FnYCr2EAzWS;EdN7qlmI5FS;d9thHOJMROr;
                  eTDtyyaSA7f;FbKK4ofIv5R;n5nS0SmkUpq;YlTWksXEhEO;JoEzWYGdX7s;
                  tUIlpyeeX9N&dimension=ou:O6uvpzGd5pu;OU_GROUP-EYbopBOJWsW&filter=pe:THIS_YEAR&displayProperty=SHORTNAME&
                  skipData=false&includeMetadataDetails=true") %>% 
    stringr::str_replace_all( "[\r\n]" , "") %>% 
    URLencode(.) 
  
  
 
  df <- d2_analyticsResponse(url)
   
  
}

demoPivot<-function(d) {
  
  rpivotTable(data =   d   ,  rows = c( "Organisation unit"),cols = "Data",
              vals = "Value", aggregatorName = "Integer Sum", rendererName = "Table"
              , width="70%", height="700px")
  
}

demoPlot<-function(d) {
  foo<-d %>%  
    dplyr::filter( stringr::str_detect(Data, 'OPV') ) %>% 
    dplyr::mutate(Value = as.numeric(Value))  %>% 
    tidyr::pivot_wider(names_from=Data,values_from=Value) %>%  
    dplyr::mutate(rank = dense_rank(`OPV 3 Coverage <1y`)) %>% 
    dplyr::arrange(rank) %>% 
    dplyr::filter(rank <= 5) %>% 
    dplyr::select(-rank) %>% 
    tidyr::pivot_longer(-`Organisation unit`,names_to = 'Data') %>% 
    dplyr::mutate(Data = as.factor(Data),
                  `Organisation unit` = as.factor(`Organisation unit`)) %>% 
    dplyr::mutate(value = ifelse(is.na(value),"0",value))
  
  ggplot(foo,aes(fill=Data,y=value,x=`Organisation unit`)) + 
    geom_bar(position = 'dodge', stat = 'identity')
}
