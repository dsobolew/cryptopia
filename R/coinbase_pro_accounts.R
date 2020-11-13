#' @title Coinbase Pro Account Listing Function
#' @description  A function for accessing account information via the Coinbase Pro API.
#'
#' @param key Your Coinbase Pro API key
#' @param secret_key Your Coinbase Pro secret API key
#' @param passphrase Your Coinbase Pro API passphrase
#' @return A list of all Coinbase Pro accounts with details of their holdings.
#' 
#' @import magrittr
#' @export


coinbase_pro_accounts <- function(key, secret_key, passphrase){
  
  base_url <- "https://api.pro.coinbase.com"
  req_url <-  "/accounts/"
  url <- paste0(base_url,req_url)
  
  timestamp <- format(as.numeric(Sys.time()), digits = 13) # create nonce
  encoded_key <- RCurl::base64Decode(secret_key, mode = "raw") # encode api secret
  
  sign <- RCurl::base64Encode(digest::hmac(key = encoded_key,
                                           object = paste0(timestamp,"GET",req_url),
                                           algo = "sha256", 
                                           raw = TRUE)) # hash
  
  
  accounts <- tibble::tibble(accounts = httr::GET(url,
                                          httr::add_headers(
                                            .headers =
                                              c('CB-ACCESS-KEY' = key,
                                                'CB-ACCESS-SIGN' = sign[1],
                                                'CB-ACCESS-TIMESTAMP' = timestamp,
                                                'CB-ACCESS-PASSPHRASE' = passphrase))) %>%
                       httr::content()) %>%
    tidyr::unnest_wider(col = accounts) 
  
  
  
  return(accounts)
  
}



