# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_ceilings_floors_AGR_xml
#'
#' Construct XML data structure for \code{aeei.xml}.
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{policy_ceilings_floors_AGR.xml}.
module_policy_ceilings_floors_AGR_xml <- function(command, ...) {
  all_xml_names <- get_xml_names("policy/A_ag_constraints.csv", "policy_ceilings_floors_AGR.xml")
  names(all_xml_names) <- rep("XML", length(all_xml_names))

  MODULE_INPUTS <- c("L3011.policy_port_stnd",
                     "L3011.XML_policy_map",
                     "L3011.input_tax",
                     "L3011.input_subsidy")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(all_xml_names)
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]


    # Load required inputs
    get_data_list(all_data, MODULE_INPUTS)

    # Match XML names in A_Policy_XML_Names to policies
    # If no xml listed for given region/market/policy, assign to policy_ceilings_floors.xml
    L3011.policy_port_stnd_xml <- L3011.policy_port_stnd %>%
      left_join(L3011.XML_policy_map, by = c("policy.portfolio.standard", "market")) %>%
      replace_na(list(xml = "policy_ceilings_floors.xml"))

    # ===================================================
    for (xml_name in all_xml_names){
      L3011.policy_port_stnd_tmp <- L3011.policy_port_stnd_xml %>%
        filter(xml == xml_name)

      # Use as filter for other tables
      policy_rgn_tmp <- L3011.policy_port_stnd_tmp %>%
        distinct(region, policy.portfolio.standard, policyType, xml)

      L3011.input_tax_tmp <- L3011.input_tax %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.tax" = "policy.portfolio.standard"))

      L3011.input_subsidy_tmp <- L3011.input_subsidy %>%
        semi_join(policy_rgn_tmp, by = c("region", "input.subsidy" = "policy.portfolio.standard"))

      # Produce output
      assign(xml_name,
             create_xml(xml_name) %>%
               add_xml_data(L3011.policy_port_stnd_tmp, "PortfolioStdConstraint") %>%
               add_xml_data(L3011.input_tax_tmp, "TechInputTax") %>%
               add_xml_data(L3011.input_subsidy_tmp, "TechInputSubsidy") %>%
               add_precursors("L3011.policy_port_stnd",
                              "L3011.input_tax",
                              "L3011.input_subsidy")
      )

    }


    # Need this for loop because having issues with lapply(all_xml_names, get)
    list_of_xmls <- list()
    for(xml_name in all_xml_names){
      list_of_xmls[[xml_name]] <- get(xml_name)
    }
    return_multiple_xmls(list_of_xmls, all_xml_names)
  } else {
    stop("Unknown command")
  }
}
