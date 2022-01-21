#' Endpoint to gather user adherence from Bridge
#' 
#' @param study_id the Bridge Study ID
#' @param user_id the Bridge User ID
#' 
#' @return response from Bridge adherence/activityStreams endpoint
#' 
#' @export
get_adherence <- function(study_id, user_id){
    if (is.null(study_id) || is.null(user_id)) {
        stop(glue::glue("You need to fill both study_id and user_id"))
    }
    tryCatch({
        bridgeGET(
            glue("/v5/studies/{study_id}/participants/{user_id}/adherence/eventstream"))
    }, error = function(e){
        stop(glue::glue("Adherence from following ",
                        "study_id: {study_id} and user_id: {user_id} ",
                        "is not found"))
    })
}