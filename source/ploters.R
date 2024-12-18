plot_one_stock <- function(
    dataset, 
    time_variable, 
    stock_variable,
    flow_variables,
    params_text, 
    plot_scale = 1,
    line_size = 1.2,
    stock_linetype = "solid",
    flow_linetype = "dashed",
    plot_title = "") {
  
  n_flow_variables <- length(flow_variables)
  
  maxX = dataset[length(dataset[,time_variable]), time_variable]
  maxY = max(dataset[, stock_variable])
  
  for (i in 1:n_flow_variables) {
    flow_range <- diff(range(dataset[, flow_variables[i]]))
    
    if (flow_range > 0) {
      dataset[, flow_variables[i]] <- dataset[, flow_variables[i]] * maxY / max(dataset[, flow_variables[i]])
    } else {
      dataset[, flow_variables[i]] <- 1
    }
  }
  
  p <- ggplot(data = dataset, aes_string(x = time_variable)) +
    geom_line(aes_string(x = time_variable, y = stock_variable, linetype = "'stock'"), size = line_size)
  
  for (i in 1:n_flow_variables) {
    nameForLegend <- ifelse(grepl("in", flow_variables[i]), "inflow", "outflow")
    nameForLegend <- paste("'", nameForLegend, "'")
    
    p <- p + geom_line(aes_string(x = time_variable, y = flow_variables[i], 
                                  color = nameForLegend, linetype = "'flow'"), size = line_size)
  }
  
  p + scale_linetype_manual(name = "", values = c(flow_linetype, stock_linetype)) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(
      axis.title = element_text(size = plot_scale * 10),
      axis.text = element_text(size = plot_scale * 10),
      legend.key.width = unit(line_size, "cm")
    ) +
    annotate("text",
             x = 0.7 * maxX, 
             y = 0.8 * maxY,
             label = params_text, 
             size = 5, 
             hjust = 0, 
             vjust = 1) +
    ggtitle(plot_title)
}

plot_n_stocks <- function(
    dataset, 
    time_variable, 
    stock_variables,
    flow_variables,
    params_text, 
    plot_scale = 1,
    line_size = 1.2,
    stock_linetype = "solid",
    flow_linetype = "dashed",
    flow_internal_linetype = "dotdash",
    plot_title = "") {
  
  n_stock_variables <- length(stock_variables)
  n_flow_variables <- length(flow_variables)
  
  maxX = dataset[length(dataset[,time_variable]), time_variable]
  maxY = max(dataset[, stock_variables])
  
  for (i in 1:n_flow_variables) {
    flow_range <- diff(range(dataset[, flow_variables[i]]))
    
    if (flow_range > 0) {
      dataset[, flow_variables[i]] <- dataset[, flow_variables[i]] * maxY / max(dataset[, flow_variables[i]])
    } else {
      dataset[, flow_variables[i]] <- 1
    }
  }
  
  # The palette with black:
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  stocks_colors <- cbbPalette[1:n_stock_variables]
  
  valuesForLegend <- c()
  
  p <- ggplot(data = dataset, aes_string(x = time_variable))
  
  for (i in 1:n_stock_variables) {
    nameForLegend <- paste("'", stock_variables[i], "stock", "'")
    valuesForLegendHere <- c(stock_linetype)
    
    p <- p +
      geom_line(aes_string(x = time_variable, y = stock_variables[i], linetype = nameForLegend), 
                color = stocks_colors[i], size = line_size)
    
    flowsOfThisStock <- grep(paste0("^", stock_variables[i]), flow_variables)
    internalFlowsHere <- grep("in|out|loss", flow_variables[flowsOfThisStock])
    mutualFlowsHere <- setdiff(flowsOfThisStock, internalFlowsHere)
    
    for (j in internalFlowsHere) {
      nameForLegend <- flow_variables[j]
      nameForLegend <- paste("'", nameForLegend, "'")
      
      p <- p + geom_line(aes_string(x = time_variable, y = flow_variables[j], 
                                    linetype = nameForLegend), color = stocks_colors[i], size = line_size)
      
      valuesForLegendHere <- c(valuesForLegendHere, flow_linetype)
    }
    
    for (h in mutualFlowsHere) {
      nameForLegend <- flow_variables[h]
      nameForLegend <- paste("'", nameForLegend, "'")
      
      p <- p + geom_line(aes_string(x = time_variable, y = flow_variables[h], 
                                    linetype = nameForLegend), color = stocks_colors[i], size = line_size)
      
      valuesForLegendHere <- c(valuesForLegendHere, flow_internal_linetype)
    }
    
    valuesForLegend <- c(valuesForLegend, valuesForLegendHere)
  }
    
  p + scale_linetype_manual(name = "", values = valuesForLegend) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    theme_bw() +
    theme(
      axis.title = element_text(size = plot_scale * 10),
      axis.text = element_text(size = plot_scale * 10),
      legend.key.width = unit(line_size, "cm")
    ) +
    annotate("text",
             x = 0.7 * maxX, 
             y = 0.8 * maxY,
             label = params_text, 
             size = 5, 
             hjust = 0, 
             vjust = 1) +
    ggtitle(plot_title)
}
