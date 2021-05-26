#' Get a study participant (user) record using the external ID of the account
#'
#' All of the parameters are optional, but at least one participant
#' identifier must be supplied. If multiple identifiers are supplied,
#' only the first identifier (as determined by the function definition)
#' will be used. If a phone number or email address is supplied, there
#' can only be one matching user, else an error is thrown.
#'
#' @param external_id The external ID of the participant
#' @param health_code The healthcode of the account to delete.
#' @param user_id The user ID of the account to delete.
#' @param study_id The study ID. Must be set with user_id
#' to have an effect. Will use the v5 APIs (rather than v3).
#' @param phone The participant's phone number.
#' @param email The participant's email address.
#' @export
get_participant <- function(external_id=NULL, health_code=NULL, study_id=NULL,
                            user_id=NULL, phone=NULL, email=NULL) {
  if (is.null(external_id) && is.null(health_code) && is.null(user_id)
      && is.null(phone) && is.null(email)) {
    stop(glue::glue("One of `external_id`, `health_code`, `user_id`, `phone`,",
                    " or `email` must be provided."))
  }
  tryCatch({
    if (!is.null(user_id)) {
      if (is.null(study_id)) {
        response <- bridgeGET(glue::glue(
            "/v3/participants/{user_id}"))
      } else {
        response <- bridgeGET(glue::glue(
            "/v5/studies/{study_id}/participants/{user_id}"))
      }
    } else if (!is.null(external_id)) {
      if (is.null(study_id)) {
        response <- bridgeGET(glue::glue(
            "/v3/participants/externalId:{external_id}"))
      } else {
        search_results
        response <- bridgeGET(glue::glue(
            "/v5/studies/{study_id}/participants/{user_id}"))
      }
    } else if (!is.null(health_code)) {
      response <- bridgeGET(glue::glue(
          "/v3/participants/healthCode:{health_code}"))
    } else if (!is.null(phone) || !is.null(email)) {
      search_results <- search_participants(
          phone = phone,
          email = email,
          study_id = study_id)
      if (search_results$total == 1) {
        return(get_participant(
            user_id = search_results[[1]][[1]]$id),
            study_id = study_id)
      } else {
        stop(glue::glue("Either more than one or zero participants have that ",
                        "phone number or email address."))
      }
    }
    participant <- httr::content(response)
    return(participant)
  }, error = function(e) {
      stop(message(e))
  })
}

#' Get all participants in the study
#'
#' This users the deprecated GET /v3/participants API and may stop working
#' at any time.
#' @return A list of participant accounts
#' @export
get_all_participants <- function() {
  offset_by <- 0
  page_size <- 100
  total_participants <- Inf
  all_participants <- list()
  while (offset_by <= total_participants) {
    response <- bridgeGET(
        endpoint = "/v3/participants",
        query = list(offsetBy = offset_by,
                     pageSize = page_size))
    these_participants <- httr::content(response)
    all_participants <- c(all_participants, these_participants[[1]])
    total_participants <- these_participants$total
    offset_by <- offset_by + page_size
  }
  return(all_participants)
}

#' Search for a study participant
#'
#' Returns the first 100 results.
#'
#' @param email The email address to filter upon.
#' @param phone The phone number to filter upon.
#' @param study_id The study ID. Will use the v5 APIs (rather than v3).
#' @return The first 100 search results.
#' @export
search_participants <- function(email=NULL, phone=NULL, study_id=NULL) {
  if (is.null(email) && is.null(phone)) {
    stop("One of `email` or `phone` must be provided.")
  } else if (is.null(study_id)) {
    response <- bridgePOST(
        endpoint = glue::glue("/v3/participants/search"),
        body = list(phoneFilter = phone,
                    emailFilter = email,
                    pageSize = 100))
  } else {
    response <- bridgePOST(
        endpoint = glue::glue("/v5/studies/{study_id}/participants/search"),
        body = list(phoneFilter = phone,
                    emailFilter = email,
                    pageSize = 100))
  }
  search_results <- httr::content(response)
  return(search_results)
}

#' Delete a Bridge account
#'
#' Only users with a `test_user` data group may be deleted.
#'
#' @param health_code The healthcode of the account to delete.
#' @param user_id The user ID of the account to delete.
#' @export
delete_participant <- function(health_code=NULL, user_id=NULL) {
  if (is.null(health_code) && is.null(user_id)) {
    stop("One of `health_code` or `user_id` must be provided.")
  }
  participant <- get_participant(health_code = health_code,
                                 user_id = user_id)
  if (!("test_user" %in% participant$dataGroups)) {
    stop("This participant is not in the `test_user` data group.")
  }
  response <- bridgeDELETE(glue::glue(
        "/v3/participants/{participant$id}"))
  return(response)
}

#' Create a Bridge account
#'
#' Creates a participant account tied to a phone number.
#'
#' @param phone_number The phone number associated with the account.
#' @param email The email address associated with the account.
#' @param study The study identifier.
#' @param external_id The participant identifier.
#' @param data_groups One or more data groups to assign this user to.
#' @param phone_region_code The CLDR two-letter region code describing the
#' region in which the phone number was issued. By default: "US".
#' @param sharing_scope One of "no_sharing", "sponsors_and_partners",
#' or "all_qualified_researchers". By default: "all_qualified_researchers".
#' @export
create_participant <- function(
    phone_number = NULL,
    email = NULL,
    study = NULL,
    external_id = NULL,
    data_groups = NULL,
    phone_region_code = "US",
    sharing_scope = "all_qualified_researchers") {
  if (is.null(phone_number) && is.null(email)) {
    stop("Either a phone number or email is required.")
  }
  if (is.null(study) && !is.null(external_id) ||
      !is.null(study) && is.null(external_id)) {
    stop("Both a study and an external_id must be supplied.")
  } else if (!is.null(study) && !is.null(external_id)) {
    external_id_list <- list()
    external_id_list[[study]] <- external_id
  } else {
    external_id_list <- NULL
  }
  response <- bridgePOST(
      "/v3/participants",
      body = list(
        "externalIds" = external_id_list,
        "checkForConsent" = FALSE,
        "phone" = list("number" = phone_number,
                       "regionCode" = phone_region_code),
        "dataGroups" = data_groups,
        "sharingScope" = sharing_scope))
  return(response)
}
