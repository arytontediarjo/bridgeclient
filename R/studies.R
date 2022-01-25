#' Function to get studies of Bridge App
#' 
#' @return response from Bridge /studies endpoint
#' @export
get_studies <- function(){
    tryCatch({
        bridgeclient:::bridgeGET("/v5/studies") %>% .$items
    }, error = function(e){
        e$message
    })
}