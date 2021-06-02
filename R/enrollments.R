#' Get enrollments for this study
#'
#' @param study_id The study ID.
#' @param filter One of "enrolled", "withdrawn", or "all". Defaults to "all".
#' @param include_testers Whether to include accounts with the
#' "test_user" data group.
#' @return A list of accounts that are enrolled in that study.
#' @export
get_all_enrollments <- function(
    study_id,
    filter="all",
    include_testers=FALSE) {
  offset_by <- 0
  page_size <- 100
  total_enrolled <- Inf
  all_enrollments <- list()
  while (offset_by <= total_enrolled) {
    these_enrollments <- bridgeGET(
        endpoint = glue::glue("/v5/studies/{study_id}/enrollments"),
        query = list(offsetBy = offset_by,
                     pageSize = page_size,
                     enrollmentFilter = filter,
                     includeTesters = include_testers))
    all_enrollments <- c(all_enrollments, these_enrollments[[1]])
    total_enrolled <- these_enrollments$total
    offset_by <- offset_by + page_size
  }
  return(all_enrollments)
}
