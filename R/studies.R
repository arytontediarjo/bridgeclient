#' Function to get studies of Bridge App
#' 
#' @return response from Bridge /studies endpoint
get_studies <- function(){
    tryCatch({
        bridgeclient:::bridgeGET("/v5/studies") %>% .$items
    }, error = function(e){
        glue::glue("study not available in Bridge App")
    })
}