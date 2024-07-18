POLICY_XML_DIR           <- "xml_policy/"

#' get_xml_names
#'
#' Return list of xml names as output for chunk when they are included in csv file
#'
#' @param csv_name CSV file with xml column that contains output xml names
#' @param default_xml A default xml to output if no xml names provided for any output
#' @return Vector of xml names
#' @author Russell Horowitz
get_xml_names <- function(csv_name, default_xml) {
  all_xml_names <- suppressMessages(readr::read_csv(paste0("inst/extdata/", csv_name), comment = "#"))
  all_xml_names <- union(unique(all_xml_names$xml), default_xml)
  names(all_xml_names) <- rep("XML", length(all_xml_names))
  for(i in 1:length(all_xml_names)){
    if (!grepl(".xml", all_xml_names[i])){
      all_xml_names[i] <- paste0(all_xml_names[i], ".xml")
    }
  }
  all_xml_names
}

#' Method to return multiple xmls
#'
#' @param all_xml_names List of xml names
#' @return A list of xmls that a function can output
#' @export
return_multiple_xmls <- function(list_of_xmls, all_xml_names) {
  # for(xml_name in all_xml_names){
  #   list_of_xmls[[xml_name]] <- get(xml_name)
  # }
  names(list_of_xmls) <- all_xml_names
  outlist <- sapply(list_of_xmls, return_data)
  names(outlist) <- all_xml_names

  outlist
}
