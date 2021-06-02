#' Make a GET call to Bridge
#'
#' @param endpoint Server endpoint.
#' @param debug Whether to print the request string and body. FALSE by default.
#' @param ... Either the body of the request or named arguments to
#' pass to \code{httr::modify_url}.
bridgeGET <- function(
    endpoint,
    debug = FALSE,
    ...) {
  results <- bridge(endpoint = endpoint,
                    method = httr::GET,
                    debug = debug,
                    ...)
  return(results)
}

#' Make a POST call to Bridge
#'
#' @param endpoint Server endpoint.
#' @param debug Whether to print the request string and body. FALSE by default.
#' @param ... Either the body of the request or named arguments to
#' pass to \code{httr::modify_url}.
bridgePOST <- function(
    endpoint,
    debug = FALSE,
    ...) {
  results <- bridge(endpoint = endpoint,
                    method = httr::POST,
                    debug = debug,
                    ...)
  return(results)
}

#' Make a DELETE call to Bridge
#'
#' @param endpoint Server endpoint.
#' @param debug Whether to print the request string and body. FALSE by default.
#' @param ... Either the body of the request or named arguments to
#' pass to \code{httr::modify_url}.
bridgeDELETE <- function(
    endpoint,
    debug = FALSE,
    ...) {
  results <- bridge(endpoint = endpoint,
                    method = httr::DELETE,
                    debug = debug,
                    ...)
  return(results)
}

#' Log in to Bridge
#'
#' Log in by inputting your username and password or cache your credentials in
#' a YAML file with the name \code{.bridgeCredentials} in your home (~) directory.
#' This file should have keys \code{email} and \code{password}.
#'
#' @param study The study identifier. Ask Alx Dark if you do not know this.
#' For mPower 2, study = 'sage-mpower-2'. For MindKind, study = 'wellcome'.
#' @param email Your email
#' @param password Your Bridge password
#' @export
bridge_login <- function(
    study,
    email = NULL,
    password = NULL) {
  if (is.null(email) || is.null(password)) {
    credentials <- get_credentials()
  } else {
    credentials <- list(email = email, password = password)
  }
  content <- bridgePOST(
      "/v4/auth/signIn",
      body = list(study = study,
                  email = credentials$email,
                  password = credentials$password))
  session_token <- content$sessionToken
  Sys.setenv(BRIDGE_SESSION_TOKEN = session_token)
}

#' Make a REST call to Bridge
#'
#' @param endpoint The API endpoint.
#' @param method An HTTP verb function.
#' @param debug Whether to print the request string and body. FALSE by default.
#' @param ... named arguments to pass to \code{httr::modify_url}.
bridge <- function(
    endpoint,
    method,
    debug = FALSE,
    ...) {
  base_url <- "https://webservices.sagebridge.org"
  args <- list(...)
  url <- httr::modify_url(base_url, path = endpoint, scheme = args$scheme,
                          hostname = args$hostname, port = args$port,
                          query = args$query, params = args$params,
                          fragment = args$fragment, username = args$username,
                          password = args$password)
  if (debug) {
    debug_str <- glue::glue(
        "URL:", url,
        "args:", args,
        .sep = "\n")
    message(debug_str)
  }
  session_token <- get_session_token()
  response <- method(url,
                     httr::user_agent("https://github.com/philerooski/bridgeclient"),
                     httr::add_headers("Bridge-Session" = session_token),
                     body = args$body,
                     encode = "json")
  content <- httr::content(response)
  if (httr::http_error(response)) {
    stop(glue::glue(content$status, ": ", content$message))
  }
  return(content)
}

#' Fetch credentials from Bridge credentials file
#'
#' @return A list with elements \code{email} and \code{password}
get_credentials <- function(path = "~/.bridgeCredentials") {
  credentials <- yaml::read_yaml(path)
  return(credentials)
}

get_session_token <- function(env_var = "BRIDGE_SESSION_TOKEN") {
  session_token <- Sys.getenv(env_var)
  if (nchar(session_token) == 0) {
    return(NULL)
  }
  return(session_token)
}
