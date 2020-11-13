#' @title Coinbase Account Info Function
#' @description  A function for accessing account information via the Coinbase API.
#'
#' @param key Your coinbase API key
#' @param secret_key Your coinbase secret API key
#' @return A list of all Coinbase accounts with details of their holdings.
#' @export

coinbase_accounts <- function(key, secret_key){
  
  coinbase_url <- "https://api.coinbase.com"
  coinbase_reqPath <- "/v2/accounts"
  coinbase_fullPath <- paste(coinbase_url, coinbase_reqPath,sep = "")
  
  cb_timestamp <- format(as.numeric(Sys.time()), digits=10)
  coinbase_message <- paste0(cb_timestamp,"GET", coinbase_reqPath)
  coinbase_sig <- digest::hmac(key = secret_key, object = coinbase_message, algo = "sha256", raw = F)
  
  accounts <- httr::content(httr::GET(coinbase_fullPath,
                                      httr::add_headers(
                                        "CB-ACCESS-KEY" = key,
                                        "CB-ACCESS-SIGN" = coinbase_sig,
                                        "CB-ACCESS-TIMESTAMP" = cb_timestamp,
                                        "Content-Type"="application/json")))
  
  
  return(accounts)
  
}