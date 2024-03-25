## ---- drg-proportion
drg_proportion <- function(drg_code, data = data_complete_cases){
  
  data_count <- as.data.frame(prop.table(table(data$group_collapse, 
                                               data$drg), margin = 2)*100)
  
  colnames(data_count)[colnames(data_count) == 'Var1'] <- 'group'
  colnames(data_count)[colnames(data_count) == 'Var2'] <- 'drg'
  colnames(data_count)[colnames(data_count) == 'Freq'] <- 'proportion'
  
  data_subset <- data_count[data_count$drg == drg_code, ]
  
  plot_proportion <- ggplot(data_subset, 
                            aes(x=group, 
                                y = proportion)) + 
    geom_bar(stat="identity", fill = "#245F8A") + 
    labs(title = "Pregnancy outcomes",
         subtitle = paste0("Barplot of cases with ", drg_code, " classification by treatment group"),
         caption = "Data source: Aktionbündis Patientensicherheit") +
    xlab("Treatment group") + 
    ylab("Proportion of cases with DRG classification") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "#205B87"),
          plot.caption = element_text(color = "#5B8AAA", face = "italic")
    )
  
  return(plot_proportion)
  
}

## ---- drg-spend
drg_spend <- function(drg_code, data = data_complete_cases){
  
  data.table::setDT(data)
  
  data_reimbursement <- data[, .(reimbursement = mean(drg_yield), 
                                 actual_spend = mean(internal_cost)), 
                             by = .(group_collapse, drg)]
  
  data_subset <- data_reimbursement[data_reimbursement$drg == drg_code, ]
  
  if(nrow(data_subset) == 1){
    
    if(data_subset[1, "group_collapse"] == "control"){
      
      new_row <- data.frame(group_collapse = "intervention", drg = drg_code, reimbursement = 0, actual_spend = 0)
      
      data_subset <- rbind(data_subset, new_row)
      
    } else if(data_subset[1, "group_collapse"] == "intervention"){
      
      new_row <- data.frame(group_collapse = "control", drg = drg_code, reimbursement = 0, actual_spend = 0)
      
      data_subset <- rbind(data_subset, new_row)
      
    }
    
  } else{
    
    data_subset <- data_subset
    
  }
  
  data_subset_long <- data.table::melt(data_subset, 
                                       id.vars = c("group_collapse", 
                                                   "drg"), 
                                       variable.name = "cost_type", 
                                       value.name = "cost")
  
  plot_costs <- ggplot(data_subset_long, 
                       aes(x=group_collapse, y = cost, fill = cost_type)) + 
    geom_bar(stat="identity", position = "dodge") + 
    labs(title = "Treatment costs",
         subtitle = paste0("Barplot of average spent for ", drg_code, " by treatment group"),
         caption = "Data source: Aktionbündis Patientensicherheit", 
         fill = "Cost type") +
    xlab("Treatment group") + 
    ylab("Costs per patient (in €)") + 
    scale_fill_manual(values=c("#EAF3F9", "#A4BFD2", "#5B8AAA", "#205B87", "#E13530")) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          plot.title = element_text(color = "#E13530", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "#205B87"),
          plot.caption = element_text(color = "#5B8AAA", face = "italic")
    )
    
  
  return(plot_costs)
  
}

## ---- drg-analysis
drg_analysis <- function(drg_code, data = data_complete_cases){
  
  proportion_plot <- drg_proportion(drg_code = drg_code,
                                    data = data)
  
  spend_plot <- drg_spend(drg_code = drg_code,
                          data = data)
  
  
  return(list(proportion_plot, spend_plot))
  
  
}

## ---- drg-description
print_drg <- function(drg_code, data = drg_classification){
  
  data <- data[data$drg == drg_code, ]
  
  german <- data$description_deutsch
  
  english <- data$english_description
  
  return(list(german, english))
  
}

## ---- create-factors
prepare_data_factors <- function(data, to_factor){
  
  for(i in to_factor){
    data[[i]] <- 
      as.factor(data[[i]])    
  }
  
  return(data)
  
}

## ---- build-models
build_model <- function(data, null_model, intervention_var){
  
  intervention_model <- formula_extraction(formula = null_model, additions = paste0(" + ", intervention_var))
  
  null.lm <- lm(null_model, 
                data = data)
  
  null_model <- summary(null.lm)
  
  intervention.lm <- lm(intervention_model, 
                        data = data)
  
  intervention_model <- summary(intervention.lm)
  
  null_intervention_comparison <- anova(null.lm, intervention.lm)
  
  return(list(null_model, intervention_model, null_intervention_comparison))
  
}

## ---- formula-extraction
formula_extraction <- function(formula, additions){
  
  formula_string <- as.character(null_model)
  
  intervention_model <- noquote(paste0(formula_string[2], 
                                       " ~ ", 
                                       formula_string[3], 
                                       additions))
  
  return(intervention_model)
  
}

## ---- transform-var
transform_var <- function(data, var){
  
  data[[paste0(var,"_tx")]] <- (1/data[[var]])
  
  return(data)
  
}

## ---- back-transform-log
back_transform <- function(x){
  
  (exp(x)-1)*100
  
}

## ---- parse-week-pregnancy
parse_week_of_pregnancy <- function(pregnancy_week) {
  preg_week_numbers <- as.numeric(unlist(strsplit(pregnancy_week, "\\+")))
  weeks <- preg_week_numbers[1]
  days <- preg_week_numbers[2]
  return(list(weeks = weeks, days = days))
}

## ---- derive-total-gestation-days
derive_gestation_days <- function(data){
  
  parsed_weeks_days <- lapply(data[["week_pregnancy"]], 
                              parse_week_of_pregnancy)
  
  parsed_df <- do.call(rbind.data.frame, 
                       parsed_weeks_days)
  
  colnames(parsed_df) <- c("gestation_full_weeks", 
                           "gestation_additional_days")
  
  output <- cbind(data, 
                  parsed_df)
  
  output$gestation_days <- 
    output$gestation_full_weeks*7 + output$gestation_additional_days
  
  index_weeks <- which(names(output) == "gestation_full_weeks")
  
  index_days <- which(names(output) == "gestation_additional_days")
  
  output <- output[, -c(index_weeks,index_days)]
  
  return(output)
  
}