#' @title Coinbase Pro Account Detail
#' @description  A function for accessing account information via the Coinbase Pro API.
#'
#' @param key Your Coinbase Pro API key
#' @param secret_key Your Coinbase Pro secret API key
#' @param passphrase Your Coinbase Pro API passphrase
#' @param account The Coinbase Pro account ID you wish to retreive history for
#' @return A list of all Coinbase Pro accounts with details of their holdings.
#' @export

coinbase_pro_account_history <- function(key, secret_key, passphrase, account){
  
  base_url <- "https://api.pro.coinbase.com"
  req_url <-  paste0("/accounts/",account,"/ledger")
  url <- paste0(base_url,req_url)
  
  timestamp <- format(as.numeric(Sys.time()), digits = 13) # create nonce
  encoded_key <- RCurl::base64Decode(secret_key, mode = "raw") # encode api secret
  
  sign <- RCurl::base64Encode(digest::hmac(key = encoded_key,
                                           object = paste0(timestamp,"GET",req_url),
                                           algo = "sha256", 
                                           raw = TRUE)) # hash
  
  
  account_detail <- tibble::tibble(account_detail = httr::GET(url,
                                          httr::add_headers(
                                            .headers =
                                              c('CB-ACCESS-KEY' = key,
                                                'CB-ACCESS-SIGN' = sign[1],
                                                'CB-ACCESS-TIMESTAMP' = timestamp,
                                                'CB-ACCESS-PASSPHRASE' = passphrase))) %>%
                       httr::content()) %>%
    tidyr::unnest_wider(col = account_detail) 
  #%>% tidyr::unnest_wider(col = details)
  
  
  
  return(account_detail)
  
}


