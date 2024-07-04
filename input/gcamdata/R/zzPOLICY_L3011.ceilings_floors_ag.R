# Copyright 2019 Battelle Memorial Institute; see the LICENSE file.

#' module_policy_L3011.ceilings_floors_ag
#'
#' Produce agricultural demand ceilings/floors and markets by region
#'
#'
#' @param command API command to execute
#' @param ... other optional parameters, depending on command
#' @return Depends on \code{command}: either a vector of required inputs,
#' a vector of output names, or (if \code{command} is "MAKE") all
#' the generated outputs: \code{L3011.policy_port_stnd}, \code{L3011.XML_policy_map},
#' \code{L3011.input_tax}, \code{L3011.input_subsidy}
#' @importFrom assertthat assert_that
#' @importFrom dplyr bind_rows distinct filter if_else left_join mutate select
#' @author RLH April 2023
module_policy_L3011.ceilings_floors_ag <- function(command, ...) {
  MODULE_INPUTS <- c(FILE = "policy/A_ag_constraints",
                     FILE = "policy/mappings/policy_tech_mappings",
                     FILE = "policy/mappings/market_region_mappings",
                     "L240.Production_reg_imp",
                     "L240.Production_reg_dom")
  if(command == driver.DECLARE_INPUTS) {
    return(MODULE_INPUTS)
  } else if(command == driver.DECLARE_OUTPUTS) {
    return(c("L3011.policy_port_stnd",
             "L3011.XML_policy_map",
             "L3011.input_tax",
             "L3011.input_subsidy"))
  } else if(command == driver.MAKE) {

    all_data <- list(...)[[1]]

    # Load required inputs -------------------------
    get_data_list(all_data, MODULE_INPUTS)

    A_ag_constraints <- A_ag_constraints %>%
      left_join(market_region_mappings, by = "market") %>%
      mutate(region = if_else(is.na(region), market, region))

    L3011.Tech_All <- bind_rows(L240.Production_reg_imp, L240.Production_reg_dom) %>%
      # remove techs with all calOutput 0 - this assumes if there was no past import/dom prod
      # there will be none in future as well
      group_by(region, supplysector, subsector, technology) %>%
      # filter(!all(calOutputValue == 0)) %>%
      ungroup %>%
      distinct(region, supplysector, subsector, technology)

    # 1. Extend targets to all desired years and techs --------------------
    # Filter out NA years and interpolate between non-NA years
    L3011.ceilings_floors_NA <- A_ag_constraints %>%
      gather_years(value_col = "constraint") %>%
      filter(!is.na(constraint)) %>%
      # Leaving grouped on purpose here
      group_by(region, market, policy.portfolio.standard, policyType, tech_mapping )

    # Separate out groups with only 1 value since they get turned into NAs with approx_fun
    if (nrow(L3011.ceilings_floors_NA %>% filter(dplyr::n() > 1)) > 1){
      L3011.ceilings_floors_NA_mult <- L3011.ceilings_floors_NA %>%
        filter(dplyr::n() > 1) %>%
        # Interpolates between min and max years for each region/policy combo
        complete(nesting(xml, region, market, policy.portfolio.standard, policyType, tech_mapping ),
                 year = seq(min(year), max(year), 5)) %>%
        mutate(constraint = approx_fun(year, constraint)) %>%
        ungroup
    } else {
      L3011.ceilings_floors_NA_mult <- L3011.ceilings_floors_NA %>% filter(dplyr::n() > 1) %>%  ungroup
    }

    L3011.ceilings_floors <- L3011.ceilings_floors_NA_mult %>%
      bind_rows(L3011.ceilings_floors_NA %>% filter(dplyr::n() == 1) %>%  ungroup) %>%
      left_join(policy_tech_mappings, by = "tech_mapping") %>%
      select(region, market, policy.portfolio.standard, policyType, supplysector, subsector, technology = stub.technology, year, constraint)

    # # Need to drop techs that don't exist, write them out here
    # tech_remove <- L3011.ceilings_floors %>%
    #   anti_join(L3011.Tech_All, by = c("region", "supplysector", "subsector", "technology")) %>%
    #   distinct(region, supplysector, subsector, technology)
    #
    # if (nrow(tech_remove) > 0){
    #   print("Constraint removed for the following sectors:")
    #   for(i in 1:nrow(tech_remove)){
    #     print(paste(tech_remove[i,], collapse = "---"))
    #   }
    #   L3011.ceilings_floors <- L3011.ceilings_floors %>%
    #     anti_join(tech_remove, by = c("region", "supplysector", "subsector", "technology"))
    # }

    # 2. Create policy portfolio standard tables --------------------
    L3011.policy_port_stnd <- L3011.ceilings_floors %>%
      mutate(constraint = if_else(policyType == "RES",
                                  1,
                                  constraint)) %>%
      select(LEVEL2_DATA_NAMES[["PortfolioStdConstraint"]]) %>%
      distinct()

    # 3. Create input tax tables - apply to all model years because of vintages --------------------
    L3011.input_tax <- L3011.ceilings_floors %>%
      semi_join(A_ag_constraints %>% filter(policyType == "tax"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.tax = policy.portfolio.standard, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
      na.omit()

    # 4. Create input subsidy tables - apply to all model years because of vintages --------------------
    L3011.input_subsidy <- L3011.ceilings_floors %>%
      semi_join(A_ag_constraints %>% filter(policyType == "subsidy"),
                by = c("region", "market", "policy.portfolio.standard", "policyType")) %>%
      distinct(region, input.subsidy = policy.portfolio.standard, supplysector, subsector, technology) %>%
      repeat_add_columns(tibble(year = c(MODEL_FINAL_BASE_YEAR, MODEL_FUTURE_YEARS))) %>%
      na.omit()

    # 5. Make mapping of policies to xml file names --------------------
    L3011.XML_policy_map <- distinct(A_ag_constraints, xml, policy.portfolio.standard, market) %>%
      mutate(xml = if_else(grepl(".xml", xml), xml, paste0(xml, ".xml")))

    # Produce outputs ------------
    L3011.policy_port_stnd %>%
      add_title("Policy names and constraints", overwrite = T) %>%
      add_units("Mt") ->
      L3011.policy_port_stnd

    L3011.input_tax %>%
      add_title("Ag Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") ->
      L3011.input_tax

    L3011.input_subsidy %>%
      add_title("Ag Technologies to apply constraint to", overwrite = T) %>%
      add_units("NA") ->
      L3011.input_subsidy

    L3011.XML_policy_map %>%
      add_title("Mapping of policy names to xml", overwrite = T) %>%
      add_units("NA") ->
      L3011.XML_policy_map

    return_data(L3011.policy_port_stnd,
                L3011.input_tax,
                L3011.input_subsidy,
                L3011.XML_policy_map)
  } else {
    stop("Unknown command")
  }
}


