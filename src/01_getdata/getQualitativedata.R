### 
#'以下のエクセルファイルのシート[9]のI列「要素名」を参照
#'https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fwww.fsa.go.jp%2Fsearch%2F20211109%2F1e_ElementList.xlsx&wdOrigin=BROWSELINK
#'M&Dの記述は、「1【経営方針、経営環境及び対処すべき課題等】」と「3【経営者による財政状態、経営成績及びキャッシュ・フローの状況の分析】」の2つの章。
#'これに対応するI列要素は以下。
#'BusinessPolicyBusinessEnvironmentIssuesToAddressEtcTextBlock
#'ManagementAnalysisOfFinancialPositionOperatingResultsAndCashFlowsTextBlock

getTextdata = function(doc_id, path_zip, edinetCode, year){
  
  path_zipdata = paste(path_zip, paste0(doc_id, ".zip"), sep = "/")
  unzip(zipfile = path_zipdata, exdir = path_zip)
  # browser()
  path_xbrl = 
    dir(str_c(path_zip, "/XBRL/Publicdoc"), pattern = "\\.xbrl", full.names = T) %>% 
    str_subset("ifrs", negate = TRUE) %>%
    str_subset(edinetCode)
  #browser()
  
  if(length(path_xbrl) == 0){
    print(paste("no document is found:", doc_id))
    list_text = list(
      "info_ManagementAnalysis" = NA_character_,
      "info_BusinessPolicy" = NA_character_
    )
  } else {
    temp = read_xml(path_xbrl) 
    #browser()
    if(year == "2018" || year == "2017" ||  year == "2016" ){ ### 年によってタクソノミが変更されている
      elem_ManagementAnalysis = "AnalysisOfFinancialPositionOperatingResultsAndCashFlowsTextBlock"
    }else{
      elem_ManagementAnalysis = "ManagementAnalysisOfFinancialPositionOperatingResultsAndCashFlowsTextBlock"
    }
    if(year == "2016"){
      elem_BusinessPolicy = "IssuesToAddressTextBlock"
    }else{
      elem_BusinessPolicy = "BusinessPolicyBusinessEnvironmentIssuesToAddressEtcTextBlock"
    }
    
    if(year == "2019" || year == "2020"){
      info_ManagementAnalysis = xml_find_all(temp, xpath = paste0("//jpcrp030000-asr_", edinetCode, "-000:", elem_ManagementAnalysis)) %>% xml_text()
    }else{
      info_ManagementAnalysis = xml_find_all(temp, xpath = paste0("//jpcrp_cor:", elem_ManagementAnalysis)) %>% xml_text()
    }
    info_BusinessPolicy = xml_find_all(temp, xpath = paste0("//jpcrp_cor:", elem_BusinessPolicy)) %>% xml_text()
    
    
    list_text = list(
      "info_ManagementAnalysis" = info_ManagementAnalysis,
      "info_BusinessPolicy" = info_BusinessPolicy
    )
  }
  return(list_text)
}

debug = function(){
  zipfile = "./data/2024-01-14_dl/2023-11-21_filing/zip/S100SC5K.zip"
  path_zip = "./data/2024-01-14_dl/2023-11-21_filing/zip"
  
  unzip(zipfile = zipfile, exdir = path_zip)
  path_xbrl <- 
    dir(str_c(path_zip, "/XBRL/Publicdoc"), pattern = "\\.xbrl", full.names = T) %>% 
    str_subset("ifrs", negate = TRUE)
  
  temp = read_xml(path_xbrl)
  xml_find_all(temp, xpath = '//jppfs_cor:CallLoansCAFND[@contextRef = "CurrentYearInstant_NonConsolidatedMember"]') %>% xml_text()
  xml_find_all(temp, xpath = '//jpcrp_cor:ManagementAnalysisOfFinancialPositionOperatingResultsAndCashFlowsTextBlock') %>% xml_text()
  xml_find_all(temp, xpath = '//jpcrp_cor:BusinessPolicyBusinessEnvironmentIssuesToAddressEtcTextBlock') %>% xml_text()
}
