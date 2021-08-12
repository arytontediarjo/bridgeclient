#' Get activity events of a study participant (user) using their ID
#'
#' @param user_id The user ID of the participant
#' @export
get_activity_events <- function(user_id) {
  content <- bridgeGET(
    glue::glue("/v3/participants/{user_id}/activityEvents"))
  return(content$items)
}
