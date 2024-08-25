### 財務数値を取得
library(magrittr)
library(stringr)
library(countrycode)
library(xml2)

#doc_id = "S100QMW3"
#path_zip = "data\\2024-05-20_dl2023-04-01_to_2023-06-30\\zip"
#edinetCode = "E02293"

make_xpath_expr = function(elem, type, format = "jpcrp"){
  xpath_expr =  paste0("//",
                       format,
                       "_cor:",
                       elem,
                       "[ @contextRef = 'CurrentYear",　　# 単体のデータの指定
                       type,
                       "' or @contextRef = 'CurrentYear",　# 連結も含めたデータの指定
                       type,
                       "_NonConsolidatedMember' ]"
  )
  return(xpath_expr)
}

getDatas = function(doc_id, path_zip, edinetCode){
  
  path_zipdata = paste(path_zip, paste0(doc_id, ".zip"), sep = "/")
  unzip(zipfile = path_zipdata, exdir = path_zip)
  # browser()
  path_xbrl = 
    dir(str_c(path_zip, "/XBRL/Publicdoc"), pattern = "\\.xbrl", full.names = T) %>% 
    str_subset("ifrs", negate = TRUE) %>%
    str_subset(edinetCode)
  #browser()
  list_elem = list(
    PL = list(
      netsals       = list(type_elem = "Duration", elem = "NetSalesSummaryOfBusinessResults", format = "jpcrp"), #売上高
      operatinginc = list(type_elem = "Duration", elem = "OperatingIncome", format = "jppfs"), # 営業利益
      operatingrev  = list(type_elem = "Duration", elem = "OperatingRevenueSummaryOfBusinessResults", format = "jpcrp"), # 営業利益
      
      ordnryinc     = list(type_elem = "Duration", elem = "OrdinaryIncomeLossSummaryOfBusinessResults", format = "jpcrp"),# 経常収支
      extordnryinc  = list(type_elem = "Duration",elem = "ExtraordinaryIncome", format = "jppfs"), # 特別利益
      extordnryloss = list(type_elem = "Duration", elem = "ExtraordinaryLoss", format = "jppfs"), # 特別損失
      netinc = list(type_elem = "Duration", elem = "NetIncomeLossSummaryOfBusinessResults", format = "jpcrp") # 当期純利益
    ),
    BS = list(
      netassets   = list(type_elem = "Instant", elem = "NetAssetsSummaryOfBusinessResults", format = "jpcrp"), # 純資産
      totalassets = list(type_elem = "Instant", elem = "TotalAssetsSummaryOfBusinessResults", format = "jpcrp"),  # 総資産 
      liability   = list(type_elem = "Instant", elem = "Liabilities", format = "jppfs") # 負債
    ),
    CF = list(
      oprtcf  = list(type_elem = "Duration", elem = "NetCashProvidedByUsedInOperatingActivitiesSummaryOfBusinessResults", format = "jpcrp"), # operating cf
      invstcf = list(type_elem = "Duration", elem = "NetCashProvidedByUsedInInvestingActivitiesSummaryOfBusinessResults", format = "jpcrp"),  # investing cf 
      fincf   = list(type_elem = "Duration", elem = "NetCashProvidedByUsedInFinancingActivitiesSummaryOfBusinessResults", format = "jpcrp") # financing cf
    ),
    otherindex = list(
      bps       = list(type_elem = "Instant", elem = "NetAssetsPerShareSummaryOfBusinessResults", format = "jpcrp"), # 一株当たり純資産 instant 
      eps       = list(type_elem = "Duration", elem = "BasicEarningsLossPerShareSummaryOfBusinessResults", format = "jpcrp"), # 一株あたり純利益 duration 
      per       = list(type_elem = "Duration", elem = "PriceEarningsRatioSummaryOfBusinessResults", format = "jpcrp"), # 株価収益率 duration
      num_share = list(type_elem = "Instant", elem = "TotalNumberOfIssuedSharesSummaryOfBusinessResults", format = "jpcrp"), # 発行株式数(普通株式) instant
      segment   = list(type_elem = "Duration", elem = "NotesSegmentInformationEtcConsolidatedFinancialStatementsTextBlock", format = "jpcrp") # 国・事業セグメント duration
    )
  )
  
  if(length(path_xbrl) == 0){
    print(paste("no document is found:", doc_id))
    for (c in names(list_elem)) {
      list_elem_by_category = list_elem[[c]]
      for (idx in names(list_elem_by_category)) {
        list_elem[[c]][[idx]]$value = NA_character_
      }
    }
  } 
  else {
    temp = read_xml(path_xbrl) 
    ##browser()
    
    for (c in names(list_elem)) {
      list_elem_by_category = list_elem[[c]]
      for (idx in names(list_elem_by_category)) {
        list_elem_by_idx = list_elem_by_category[[idx]]
        
          xpath_expr =  make_xpath_expr(
            elem = list_elem_by_idx$elem, 
            type = list_elem_by_idx$type_elem,
            format = list_elem_by_idx$format) 
        
        if(idx == "segment"){
            elem2 = stringr::str_replace(list_elem_by_idx$elem, "Consolidated", "")
            xpath_expr = paste0(xpath_expr,
                                "|",
                                make_xpath_expr(elem = elem2, type = list_elem_by_idx$type_elem)
            )
        }
        
        value = xml_find_all(temp, xpath = xpath_expr) %>% xml_text() %>% .[. != ""] # 連結も含める場合
        if(length(value) > 1){ # 連結と単体ど知らもある場合は連結のデータを採用
          value = value[1]
        }
        list_elem[[c]][[idx]]$value = as.character(value)
        }
      }
    }
  return(list_elem)
}

count_segment = function(textdata){
  if(is.na(textdata)){
    res = list(NA_character_, NA_character_)
  }else{
    cntry_ja = countrycode::codelist$cldr.variant.ja
    count_business = stringr::str_count(textdata, pattern = "事業") # 事業セグメントの代理変数
    count_country = sum( # 国セグメントの代理変数
      unlist(
        purrr::map(cntry_ja, ~ stringr::str_count(textdata, pattern = .x)
        )
      ),
      na.rm = TRUE)
    res = list(as.character(count_business), as.character(count_country)) 
  }
  return(res)
}

debug = function(){
  ## define function for cleaning text data with html format
  clean_text = function(text_data){
    if(!is.na(text_data)){
      str = rvest::html_text(rvest::read_html(text_data))
      str_out = gsub("\\n.", " ", str) 
    } else {
      str_out = NA_character_
    }
    return(str_out)
  }
  
  t = clean_text(value)
  frequency_jigyo = stringr::str_count(t, pattern = "事業")
  
  elemlist = list(
    # PL duration
    elem = "NetSalesSummaryOfBusinessResults", #売上高,
    elem = "OperatingRevenueSummaryOfBusinessResults", # 営業利益,
    elem = "OrdinaryIncomeLossSummaryOfBusinessResults", # 経常収支,
    elem = "ExtraordinaryIncome", # 特別利益,
    elem = "ExtraordinaryLoss", # 特別損失,
    # BS instant
    elem = "NetAssetsSummaryOfBusinessResults", # 純資産
    elem = "TotalAssetsSummaryOfBusinessResults", # 総資産
    elem = "CurrentLiabilities", # 負債
    
    #CF duration
    elem = "NetCashProvidedByUsedInOperatingActivitiesSummaryOfBusinessResults", # operating cf
    elem = "NetCashProvidedByUsedInInvestingActivitiesSummaryOfBusinessResults", # investing cf
    elem = "NetCashProvidedByUsedInFinancingActivitiesSummaryOfBusinessResults", # financing cf
    
    # index 
    elem = "NetAssetsPerShareSummaryOfBusinessResults", # 一株当たり純資産 instant 
    elem = "BasicEarningsLossPerShareSummaryOfBusinessResults", # 一株あたり純利益 duration
    elem = "PriceEarningsRatioSummaryOfBusinessResults", # 株価収益率 duration
    
    # 株式 instant
    elem = "TotalNumberOfIssuedSharesSummaryOfBusinessResults", # 発行株式数(普通株式) instant
    
    elem = "NotesSegmentInformationEtcConsolidatedFinancialStatementsTextBlock" # 国・事業セグメント duration
    #"事業"の単語でヒットする件数を事業数の近似値とする
    
    #contextRef = FilingDateInstant
  )
  list_xpath_expr = list(
    duration = paste0(
      "//jpcrp_cor:", 
      elem,
      "[ @contextRef = 'CurrentYearDuration' or @contextRef = 'CurrentYearDuration_NonConsolidatedMember' ]" # 連結も含める場合
    ),
    instant = paste0(
      "//jpcrp_cor:", 
      elem,
      "[ @contextRef = 'CurrentYearInstant' or @contextRef = 'CurrentYearInstant_NonConsolidatedMember' ]" # 連結も含める場合
    )
  )
  
  
  v =list_elem$otherindex$segment$value
  count_business = stringr::str_count(v, pattern = "事業")
  count_country = sum(
    unlist(
      purrr::map(cntry_ja, ~ stringr::str_count(v, pattern = .x)
      )
    ),
    na.rm = TRUE)
  cntry_ja = countrycode::codelist$cldr.variant.ja
  
  getDatas(doc_id, path_zip, edinetCode)
  
}