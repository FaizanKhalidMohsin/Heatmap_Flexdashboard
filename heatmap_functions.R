
# Heatmap Functions


# Function: last_n_Quarters_risk_color 
# ====================================

last_n_Quarters_risk_color = function(file_name = "Current Account to GDP.xlsm", 
                                      time_series_name = "Current Account to GDP",
                                      y = "Current Account", 
                                      y_axis_label = "CAD Millions",
                                      Ratio_var_name = "GDP",                
                                      time_series_type = "Quarterly", # "Monthly" or "Quarterly"
                                      start_ts_at = "Q1 1994",        # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
                                      risk = "two_tailed",            # "two_tailed",  "right_tailed",  "left_tailed"
                                      bucket = "Macrobucket", 
                                      sub_bucket = "Extenal Imbalances",
                                      n_Quarters = 4,
                                      end_ts_at = "Q4 2019",
                                      datapath = "../DataFolder",
                                      export_R_calculations = "Yes"
                                      
) {
  
  
  # #Code for Debugging   
  # 
  # file_name = "TSX 243 Return on Equity.xlsm"
  # time_series_name = "TSX 243 Return on Equity"
  # y = "TSX 243 Net Income"
  # y_axis_label = "CAD Millions"
  # Ratio_var_name = "Common Equity"                
  # time_series_type = "Quarterly" # "Monthly" or "Quarterly"
  # start_ts_at = "Q1 1994"       # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
  # risk = "two_tailed"            # "two_tailed",  "right_tailed",  "left_tailed"
  # bucket = "Market, Funding, Solvency Risk" 
  # sub_bucket = "Market Prices"
  # n_Quarters = 4
  # end_ts_at = "Q4 2019"
  # datapath = "./DataFolder"
  
  
  #### Data Preparation
  
  library(dplyr)
  
  ##### Import data
  
  library(readxl)
  sheet_used <- 2
  if (time_series_type == "Quarterly") {
    sheet_used <- 2
    y1 = paste("Quarterly", y)
  } else if (time_series_type == "Monthly") {
    sheet_used <- 3
    y1 = paste("Monthly", y)
  } else if (time_series_type == "Ratio") {
    sheet_used <- 4
    y1 = paste("Ratio", y)
  }
  
  print(paste("time series name:", y))
  print(paste("Ratio:", Ratio_var_name))
  
  
  path = datapath
  # Create dataframe in R -- "time_series_data"
  filename <- paste0(path, "/", file_name)
  time_series_data <- read_excel(filename, sheet = sheet_used,
                                 range = cell_cols("A:B"),
                                 col_names = c("Period", "y"))
  time_series_data <- time_series_data[-(1:2), ]
  time_series_data$y <- as.numeric(time_series_data$y)
  
  if(time_series_type == "Monthly") {
    time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
    time_series_data$Period = format(time_series_data$Period, "%b%y")
    
  }
  
  
  # Denominator, if ratio
  if (time_series_type == "Ratio") {
    sheet_used_ratio <- 4
    ratio_series_data <- read_excel(filename, sheet = sheet_used_ratio,
                                    range = cell_cols("A:B"),
                                    col_names = c("Period_ratio", "y_ratio"))
    ratio_series_data <- ratio_series_data[-(1:2), ]
    ratio_series_data$y_ratio <- as.numeric(ratio_series_data$y_ratio)
    time_series_data$y <- time_series_data$y / ratio_series_data$y_ratio
  }
  
  
  ##### Clean the data
  
  
  which(is.na(time_series_data), arr.ind = T)
  time_series_data <- na.omit(time_series_data)
  
  
  ##### Remove all the data after the last quarter: end_ts_at
  # so all the time series align.
  #str(time_series_data)
  end_ts_index <- which(time_series_data$Period == end_ts_at) 
  #length(time_series_data$Period)
  time_series_data = time_series_data[1:end_ts_index, ]
  #dim(time_series_data)
  #time_series_data$Period = as.character( time_series_data$Period)
  
  
  #### Calculate Time Series Data -- Rates of Growth
  
  
  # Calculating the Year over Year Percent change. 
  
  percent_change <- NULL  # Percent change must always be year over year change for Monthly and Quarterly.
  
  n_rows_lag = 4  # The default percent change is for Quarterly data, hence 4. 
  
  if(time_series_type == "Monthly") {
    n_rows_lag = 12 # For Monthly data it becomes 12. 
  }
  
  # Extract y variable from data frame and store it as a vector under "y_time_series"
  y_time_series <- time_series_data$y
  
  
  for(i in 1:(nrow(time_series_data) - n_rows_lag)) {
    percent_change[i] = 100 * (y_time_series[i + n_rows_lag] - y_time_series[i]) / y_time_series[i]
  }
  
  
  
  # Add percent_change to data.frame "time_series_data" and change the zeros in percent_change to NA to correctly plot the graph.
  
  percent_change_full =  c( rep(NA, nrow(time_series_data) - length(percent_change)), percent_change)
  length(percent_change_full)
  
  # Start percentage change calculation from start_ts_at. 
  
  start_ts_index <- which(time_series_data$Period == start_ts_at) 
  
  percent_change_full[1:(start_ts_index - 1)] = NA # This puts NA's for all the rows before start_ts_at.
  
  percent_change = percent_change_full # The full length percent_change vector with correct NA's.
  
  time_series_data$percent_change <- percent_change # Add percent_change to time series from start_ts_at. 
  
  percent_change = na.omit(percent_change) # Remove the NA's from percent_change to get the final vector to use in the analysis.                  
  
  ##### Plot time series data -- rates of growth
  
  # Add average line to dataframe "time_series_data"
  time_series_data$average <- mean(na.omit(time_series_data$percent_change))
  
  #### Calculate Time Series Data -- Deviation from 3 and 8-year Trend
  
  
  # Create new data.frame with raw data, but excluding calcs 
  time_series_data_orig <- time_series_data[ ,c(1,2)]
  
  # Function for the deviation from average n-year trend data  
  

  deviation_from_trend_fn <- function(n_years_trend = 3,  time_series_data_orig = time_series_data_orig) {
      if(time_series_type == "Monthly") {
        n_months = n_years_trend * 12
        n_rows_lag = n_months
      } else {
        n_quarters = n_years_trend * 4
        n_rows_lag = n_quarters
      }

      start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
      start_ts_index
      # subset(time_series_data, Period > start_ts)
      time_series_data_relevant <- time_series_data_orig[(start_ts_index - (n_rows_lag - 1)) : nrow(time_series_data_orig), ]
      # extract y variable from data frame and store it as a vector under y_time_series
      y_time_series <- time_series_data_relevant$y
      y_mean <- mean(y_time_series)

      n_year_trend = NULL

      # Calculates the 3-year MA trend
      for(i in n_rows_lag : (nrow(time_series_data_relevant))) {
        n_year_trend[i] = mean(y_time_series[(i - (n_rows_lag - 1)) : i])
      }

      length(n_year_trend)

      n_year_trend_f <- as.numeric(na.omit(n_year_trend))

      # Fill NAs before start date in data.frame
      n_year_trend_final <- c(rep(NA, nrow(time_series_data_orig) - length(n_year_trend_f)), n_year_trend_f)

      # Subtraction to create the deviation from trend
      deviation_from_n_year_trend <- time_series_data_orig$y - n_year_trend_final

      mean_deviation <- mean(deviation_from_n_year_trend, na.rm = T)

      library(english)
      v1 = paste0(as.english(n_years_trend), " Year Trend")
      v2 = paste0("Deviation From ", n_years_trend ," Year Trend")
      v3 = paste0("Mean of ", n_years_trend, " Year Trend" )

      dff = data.frame(time_series_data_orig, v1 = n_year_trend_final, v2 = deviation_from_n_year_trend, v3 = mean_deviation)

      colnames(dff)[c(2, 3, 4, 5)] = c(y, v1, v2, v3)

      return(dff)
  }
  
  # Create data for n-years of trend
  
  data3yeartrend <- deviation_from_trend_fn(n_years_trend = 3, time_series_data_orig = time_series_data_orig) 
  data8yeartrend <- deviation_from_trend_fn(n_years_trend = 8, time_series_data_orig = time_series_data_orig)
  
  #### Calculate Time Series Data -- Ratio to GDP
  
  # Function that reads the time series data from the excel file 
  read_ts_data_fn <- function(filename, time_series_type) {
    sheet_used <- 2
    if(time_series_type == "Quarterly") {
      sheet_used <- 2
      y = paste("Quarterly", y)
    } else if (time_series_type == "Monthly") {
      sheet_used <- 3
      y = paste("Monthly", y)
    } else if (time_series_type == "Ratio") {
      sheet_used <- 4
      y = paste("Ratio", y)
    }
    
    time_series_data <- read_excel(filename, sheet = sheet_used,
                                   range = cell_cols("A:B"),
                                   col_names = c("Period", "y"))
    time_series_data <- time_series_data[-(1:2), ]
    time_series_data$y <- as.numeric(time_series_data$y)
    
    if(time_series_type == "Monthly") {
      time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
      time_series_data$Period = format(time_series_data$Period, "%b%y")
    }
    
    # Clean the data by deleting any NA's such as at the end of the series. 
    which(is.na(time_series_data), arr.ind = T)
    time_series_data <- na.omit(time_series_data)
    
    # Remove data (rows) after the last quarter: end_ts_at
    end_ts_index <- which(time_series_data$Period == end_ts_at) 
    time_series_data = time_series_data[1:end_ts_index, ]
    
    return(time_series_data)
    
  }
  
  # Function that creates the ratio data  
  fn_reads_and_creates_ratio_ts <- function(y, Ratio_var_name, filename, start_ts_at, time_series_data_orig = time_series_data_orig) {
    
    
    time_series_Ratio_data <- read_ts_data_fn(filename, time_series_type = "Ratio") 
    start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
    Ratio_var <- time_series_Ratio_data$y[start_ts_index : nrow(time_series_data_orig)]
    Ratio_var <- c(rep(NA, nrow(time_series_data_orig) - length(Ratio_var)), Ratio_var)
    
    # Create the ratio data   
    time_series_Ratio_data <- data.frame(time_series_data_orig, Ratio_var = time_series_Ratio_data$y)
    time_series_Ratio_data$Ratio <- time_series_Ratio_data$y / Ratio_var
    time_series_Ratio_data$Mean_of_Ratio <-  mean(na.omit(time_series_Ratio_data$y / Ratio_var))
    colnames(time_series_Ratio_data)[c(2, 3, 4)] <- c(y, Ratio_var_name, paste("Ratio of", y, "to", Ratio_var_name ))
    
    return(time_series_Ratio_data)
  }
  
  # Create the data.frame in R -- "time_series_GDP_Ratio_data"
  if(time_series_type == "Quarterly") {
    time_series_GDP_Ratio_data <- fn_reads_and_creates_ratio_ts(y = y, Ratio_var_name = Ratio_var_name, filename = filename, start_ts_at = start_ts_at, time_series_data_orig = time_series_data_orig)
  }
  
  
  
  #### Export R calculations back to a new csv file
  
  # Function that merges all four data sets to be exported 
  merge_and_export_data_fn <- function(y = y, trendyears = "3and8", data1, data2, data3, data4, path = "", export = "Yes") {
    
    colnames(data1)[2] = y
    
    if(time_series_type == "Monthly"){ # if data is monthly
      namefile = paste(y, trendyears, "years") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)])
    } else { # if data is anything else i.e. Quarterly.
      namefile = paste(y, trendyears, "years with", Ratio_var_name, "Ratio") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)], data4[,c(-1,-2)])
    }
    
    todaysData = format(Sys.time(), "%d%m%Y")
    dataname = file.path(path, paste0( namefile, " - ", time_series_type ," - " , todaysData, ".csv"))
    
    if(export == "Yes") {
      write.csv(df, dataname, row.names = F)
    }
    
    return(df)
  }
  
  
  
  # Merging and exporting the data 
  
  #Do not export anything
  if(time_series_type == "Monthly"){ time_series_GDP_Ratio_data = NULL }
  
  #Check if directory exists, if not create it. 
  path_R_output_data = file.path("..", "R_output_data")
  dir.create(path_R_output_data, recursive = T)    
  
  #Merge and export function call
  final_data <- merge_and_export_data_fn(y = y, trendyears = "3and8",
                                         data1 = time_series_data,
                                         data2 = data3yeartrend,
                                         data3 = data8yeartrend,
                                         data4 = time_series_GDP_Ratio_data ,
                                         path = path_R_output_data,
                                         export = export_R_calculations
  )
  
  #### Colour Code Observations based on Equal Percentile Buckets
  ##### Calculate percentile cutoffs
  
  
  # Function to color code risk: right-tailed risk, two_tailed risk, or left_tailed risk
  risk_color <- function(ts_y_vector, risk = "right_tailed") {
    col_labels = c("green", "light green",  "yellow", "orange",  "red", "dark red")
    partitions = 6
    colored_labels = col_labels
    
    if(risk == "two_tailed") {
      colored_labels =  c(rev(col_labels), col_labels)
      partitions = 12
      
    } else if(risk == "left_tailed") {
      colored_labels = rev(col_labels)
    }
    
    ts_y_vector <- na.omit(ts_y_vector)
    color_code_percentile <- factor(cut(ts_y_vector,
                                        quantile(ts_y_vector, probs = seq(0, 1, 1/partitions)),
                                        include.lowest = TRUE),
                                    labels = colored_labels)
    #color code observations
    color_code_vec <- as.character(color_code_percentile)
    return(color_code_vec)
  }
  
  #"Risk color per observation -- 3-year trend data"
  risk_color_3years = risk_color(ts_y_vector = data3yeartrend[[4]], risk = risk)
  
  
  #"Risk color per observation -- 8-year trend data"
  risk_color_8years = risk_color(ts_y_vector = data8yeartrend[[4]], risk = risk)
  
  
  "Risk color for each observation -- ratio data"
  if(time_series_type == "Quarterly") {
    risk_color_ratio = risk_color(ts_y_vector = time_series_GDP_Ratio_data[[4]], risk = risk)
  }
  
  risk_color_used = risk_color_ratio
  
  last_4Q_risk_color = tail(risk_color_used, n = n_Quarters)
  Period = tail(time_series_GDP_Ratio_data$Period, n = n_Quarters)
  ratio_ts = tail(time_series_GDP_Ratio_data[[4]], n = n_Quarters)
  
  df = data.frame(time_series_name = time_series_name, Period = Period, risk_color = last_4Q_risk_color, ratio_ts = ratio_ts, 
                  bucket = bucket, sub_bucket = sub_bucket )
  
  return(df)
  
}





# Function: create_subplots_and_last_n_Quarters_risk_color 
# ====================================

create_subplots_and_last_n_Quarters_risk_color = function(file_name = "Current Account to GDP.xlsm", 
                                      time_series_name = "Current Account to GDP",
                                      y = "Current Account", 
                                      y_axis_label = "CAD Millions",
                                      Ratio_var_name = "GDP",                
                                      time_series_type = "Quarterly", # "Monthly" or "Quarterly"
                                      start_ts_at = "Q1 1994",        # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
                                      risk = "two_tailed",            # "two_tailed",  "right_tailed",  "left_tailed"
                                      bucket = "Macrobucket", 
                                      sub_bucket = "Extenal Imbalances",
                                      n_Quarters = 4,
                                      end_ts_at = "Q4 2019",
                                      datapath = "./DataFolder", 
                                      time_points_in_plots = 30, 
                                      export_R_calculations = "Yes"
) {
  
  
  # #Code for Debugging   
  # 
  # file_name = "TSX 243 Return on Equity.xlsm"
  # time_series_name = "TSX 243 Return on Equity"
  # y = "TSX 243 Net Income"
  # y_axis_label = "CAD Millions"
  # Ratio_var_name = "Common Equity"                
  # time_series_type = "Quarterly" # "Monthly" or "Quarterly"
  # start_ts_at = "Q1 1994"       # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
  # risk = "two_tailed"            # "two_tailed",  "right_tailed",  "left_tailed"
  # bucket = "Market, Funding, Solvency Risk" 
  # sub_bucket = "Market Prices"
  # n_Quarters = 4
  # end_ts_at = "Q4 2019"
  # datapath = "./DataFolder"
  
  
  #### Data Preparation
  
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  
  ##### Import data
  
  library(readxl)
  sheet_used <- 2
  if (time_series_type == "Quarterly") {
    sheet_used <- 2
    y1 = paste("Quarterly", y)
  } else if (time_series_type == "Monthly") {
    sheet_used <- 3
    y1 = paste("Monthly", y)
  } else if (time_series_type == "Ratio") {
    sheet_used <- 4
    y1 = paste("Ratio", y)
  }
  
  print(paste("time series name:", y))
  print(paste("Ratio:", Ratio_var_name))
  
  
  path = datapath
  # Create dataframe in R -- "time_series_data"
  filename <- paste0(path, "/", file_name)
  time_series_data <- read_excel(filename, sheet = sheet_used,
                                 range = cell_cols("A:B"),
                                 col_names = c("Period", "y"))
  time_series_data <- time_series_data[-(1:2), ]
  time_series_data$y <- as.numeric(time_series_data$y)
  
  if(time_series_type == "Monthly") {
    time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
    time_series_data$Period = format(time_series_data$Period, "%b%y")
    
  }
  
  
  # Denominator, if ratio
  if (time_series_type == "Ratio") {
    sheet_used_ratio <- 4
    ratio_series_data <- read_excel(filename, sheet = sheet_used_ratio,
                                    range = cell_cols("A:B"),
                                    col_names = c("Period_ratio", "y_ratio"))
    ratio_series_data <- ratio_series_data[-(1:2), ]
    ratio_series_data$y_ratio <- as.numeric(ratio_series_data$y_ratio)
    time_series_data$y <- time_series_data$y / ratio_series_data$y_ratio
  }
  
  
  ##### Clean the data
  
  
  which(is.na(time_series_data), arr.ind = T)
  time_series_data <- na.omit(time_series_data)
  
  
  ##### Remove all the data after the last quarter: end_ts_at
  # so all the time series align.
  #str(time_series_data)
  end_ts_index <- which(time_series_data$Period == end_ts_at) 
  #length(time_series_data$Period)
  time_series_data = time_series_data[1:end_ts_index, ]
  #dim(time_series_data)
  #time_series_data$Period = as.character( time_series_data$Period)
  
  
  
  # Create the human numbers function.
  human_numbers <- function(x = NULL, smbl = "", signif = 1) {
    humanity <- function(y) {
      
      if (!is.na(y)) {
        tn <- round(abs(y) / 1e12, signif)
        b <- round(abs(y) / 1e9, signif)
        m <- round(abs(y) / 1e6, signif)
        k <- round(abs(y) / 1e3, signif)
        
        if ( y >= 0 ) {
          y_is_positive <- ""
        } else {
          y_is_positive <- "-"
        }
        
        if ( k < 1 ) {
          paste0(y_is_positive, smbl, round(abs(y), signif))
        } else if ( m < 1) {
          paste0 (y_is_positive, smbl, k, "k")
        } else if (b < 1) {
          paste0 (y_is_positive, smbl, m, "m")
        } else if(tn < 1) {
          paste0 (y_is_positive, smbl, b, "bn")
        } else {
          paste0 (y_is_positive, smbl, comma(tn), "tn")
        }
      } else if (is.na(y) | is.null(y)) {
        "-"
      }
    }
    
    sapply(x, humanity)
  }
  
  # Human versions of large currency numbers - extensible via smbl 
  
  human_gbp   <- function(x){human_numbers(x, smbl = "£")} 
  human_usd   <- function(x){human_numbers(x, smbl = "$")} 
  human_euro  <- function(x){human_numbers(x, smbl = "€")} 
  human_num   <- function(x){human_numbers(x, smbl = "", signif = 2)} 
  
  
  # Locations to put date labels
  locations = c(floor(seq(from = 1, to = nrow(time_series_data), by = nrow(time_series_data) / time_points_in_plots)), nrow(time_series_data))
  
  # Plot the time series
  plot_levels = ggplot(time_series_data, aes(x = seq_along(Period), y = y)) +
    geom_line(color = "darkred", size = 0.7) +
    labs(title = paste(y, " --  Levels"), x = "Figure 1", y = y_axis_label) + #, tag = "Figure 1") +
    scale_x_continuous(breaks = locations, labels = time_series_data$Period[locations]) +
    scale_y_continuous(labels = human_num) +
    theme(axis.title.y = element_text(face = "plain", size = 9), 
          axis.title.x = element_text(face = "plain", size = 7), 
          axis.text.x = element_text(vjust = 0.5, angle = 90, size = 7), 
          axis.text.y = element_text(size = 7),
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
          #, plot.tag = element_text(size = 10)
          #,  plot.tag.position = c(0.5, 0)
    )
  
  
  #### Calculate Time Series Data -- Rates of Growth
  
  
  # Calculating the Year over Year Percent change. 
  
  percent_change <- NULL  # Percent change must always be year over year change for Monthly and Quarterly.
  
  n_rows_lag = 4  # The default percent change is for Quarterly data, hence 4. 
  
  if(time_series_type == "Monthly") {
    n_rows_lag = 12 # For Monthly data it becomes 12. 
  }
  
  # Extract y variable from data frame and store it as a vector under "y_time_series"
  y_time_series <- time_series_data$y
  
  
  for(i in 1:(nrow(time_series_data) - n_rows_lag)) {
    percent_change[i] = 100 * (y_time_series[i + n_rows_lag] - y_time_series[i]) / y_time_series[i]
  }
  
  
  
  # Add percent_change to data.frame "time_series_data" and change the zeros in percent_change to NA to correctly plot the graph.
  
  percent_change_full =  c( rep(NA, nrow(time_series_data) - length(percent_change)), percent_change)
  length(percent_change_full)
  
  # Start percentage change calculation from start_ts_at. 
  
  start_ts_index <- which(time_series_data$Period == start_ts_at) 
  
  percent_change_full[1:(start_ts_index - 1)] = NA # This puts NA's for all the rows before start_ts_at.
  
  percent_change = percent_change_full # The full length percent_change vector with correct NA's.
  
  time_series_data$percent_change <- percent_change # Add percent_change to time series from start_ts_at. 
  
  percent_change = na.omit(percent_change) # Remove the NA's from percent_change to get the final vector to use in the analysis.                  
  
  ##### Plot time series data -- rates of growth
  
  # Add average line to dataframe "time_series_data"
  time_series_data$average <- mean(na.omit(time_series_data$percent_change))
  
  
  
  #### Calculate Time Series Data -- Deviation from 3 and 8-year Trend
  
  
  # Create new data.frame with raw data, but excluding calcs 
  time_series_data_orig <- time_series_data[ ,c(1,2)]
  
  # Function for the deviation from average n-year trend data  
  
  deviation_from_trend_fn <- function(n_years_trend = 3,  time_series_data_orig = time_series_data_orig) {
    if(time_series_type == "Monthly") {
      n_months = n_years_trend * 12
      n_rows_lag = n_months
    } else {
      n_quarters = n_years_trend * 4
      n_rows_lag = n_quarters
    }
    
    start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
    start_ts_index
    # subset(time_series_data, Period > start_ts)
    time_series_data_relevant <- time_series_data_orig[(start_ts_index - (n_rows_lag - 1)) : nrow(time_series_data_orig), ]
    # extract y variable from data frame and store it as a vector under y_time_series
    y_time_series <- time_series_data_relevant$y
    y_mean <- mean(y_time_series)
    
    n_year_trend = NULL
    
    # Calculates the 3-year MA trend
    for(i in n_rows_lag : (nrow(time_series_data_relevant))) {
      n_year_trend[i] = mean(y_time_series[(i - (n_rows_lag - 1)) : i])
    }
    
    length(n_year_trend)
    
    n_year_trend_f <- as.numeric(na.omit(n_year_trend))
    
    # Fill NAs before start date in data.frame
    n_year_trend_final <- c(rep(NA, nrow(time_series_data_orig) - length(n_year_trend_f)), n_year_trend_f)
    
    # Subtraction to create the deviation from trend
    deviation_from_n_year_trend <- time_series_data_orig$y - n_year_trend_final
    
    mean_deviation <- mean(deviation_from_n_year_trend, na.rm = T)
    
    library(english)
    v1 = paste0(as.english(n_years_trend), " Year Trend")
    v2 = paste0("Deviation From ", n_years_trend ," Year Trend")
    v3 = paste0("Mean of ", n_years_trend, " Year Trend" )
    
    dff = data.frame(time_series_data_orig, v1 = n_year_trend_final, v2 = deviation_from_n_year_trend, v3 = mean_deviation)
    
    colnames(dff)[c(2, 3, 4, 5)] = c(y, v1, v2, v3)
    
    return(dff)
  }
  
  # Create data for n-years of trend
  
  data3yeartrend <- deviation_from_trend_fn(n_years_trend = 3, time_series_data_orig = time_series_data_orig) 
  data8yeartrend <- deviation_from_trend_fn(n_years_trend = 8, time_series_data_orig = time_series_data_orig)
  
  #### Calculate Time Series Data -- Ratio to GDP
  
  # Function that reads the time series data from the excel file 
  read_ts_data_fn <- function(filename, time_series_type) {
    sheet_used <- 2
    if(time_series_type == "Quarterly") {
      sheet_used <- 2
      y = paste("Quarterly", y)
    } else if (time_series_type == "Monthly") {
      sheet_used <- 3
      y = paste("Monthly", y)
    } else if (time_series_type == "Ratio") {
      sheet_used <- 4
      y = paste("Ratio", y)
    }
    
    time_series_data <- read_excel(filename, sheet = sheet_used,
                                   range = cell_cols("A:B"),
                                   col_names = c("Period", "y"))
    time_series_data <- time_series_data[-(1:2), ]
    time_series_data$y <- as.numeric(time_series_data$y)
    
    if(time_series_type == "Monthly") {
      time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
      time_series_data$Period = format(time_series_data$Period, "%b%y")
    }
    
    # Clean the data by deleting any NA's such as at the end of the series. 
    which(is.na(time_series_data), arr.ind = T)
    time_series_data <- na.omit(time_series_data)
    
    # Remove data (rows) after the last quarter: end_ts_at
    end_ts_index <- which(time_series_data$Period == end_ts_at) 
    time_series_data = time_series_data[1:end_ts_index, ]
    
    return(time_series_data)
    
  }
  
  # Function that creates the ratio data  
  fn_reads_and_creates_ratio_ts <- function(y, Ratio_var_name, filename, start_ts_at, time_series_data_orig = time_series_data_orig) {
    
    
    time_series_Ratio_data <- read_ts_data_fn(filename, time_series_type = "Ratio") 
    start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
    Ratio_var <- time_series_Ratio_data$y[start_ts_index : nrow(time_series_data_orig)]
    Ratio_var <- c(rep(NA, nrow(time_series_data_orig) - length(Ratio_var)), Ratio_var)
    
    # Create the ratio data   
    time_series_Ratio_data <- data.frame(time_series_data_orig, Ratio_var = time_series_Ratio_data$y)
    time_series_Ratio_data$Ratio <- time_series_Ratio_data$y / Ratio_var
    time_series_Ratio_data$Mean_of_Ratio <-  mean(na.omit(time_series_Ratio_data$y / Ratio_var))
    colnames(time_series_Ratio_data)[c(2, 3, 4)] <- c(y, Ratio_var_name, paste("Ratio:", y, "to", Ratio_var_name ))
    
    return(time_series_Ratio_data)
  }
  
  # Create the data.frame in R -- "time_series_GDP_Ratio_data"
  if(time_series_type == "Quarterly") {
    time_series_GDP_Ratio_data <- fn_reads_and_creates_ratio_ts(y = y, Ratio_var_name = Ratio_var_name, filename = filename, start_ts_at = start_ts_at, time_series_data_orig = time_series_data_orig)
  }
  
  
  #### Export R calculations back to a new csv file
  
  # Function that merges all four data sets to be exported 
  merge_and_export_data_fn <- function(y = y, trendyears = "3and8", data1, data2, data3, data4, path = "", export = "Yes") {
    
    colnames(data1)[2] = y
    
    if(time_series_type == "Monthly"){ # if data is monthly
      namefile = paste(y, trendyears, "years") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)])
    } else { # if data is anything else i.e. Quarterly.
      namefile = paste(y, trendyears, "years with", Ratio_var_name, "Ratio") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)], data4[,c(-1,-2)])
    }
    
    todaysData = format(Sys.time(), "%d%m%Y")
    dataname = file.path(path, paste0( namefile, " - ", time_series_type ," - " , todaysData, ".csv"))
    
    if(export == "Yes") {
      write.csv(df, dataname, row.names = F)
    }
    
    return(df)
  }
  
  
  
  # Merging and exporting the data 
  
  #Do not export anything
  if(time_series_type == "Monthly"){ time_series_GDP_Ratio_data = NULL }
  
  #Check if directory exists, if not create it. 
  path_R_output_data = file.path("..", "R_output_data")
  dir.create(path_R_output_data, recursive = T)    
  
  #Merge and export function call
  final_data <- merge_and_export_data_fn(y = y, trendyears = "3and8",
                                         data1 = time_series_data,
                                         data2 = data3yeartrend,
                                         data3 = data8yeartrend,
                                         data4 = time_series_GDP_Ratio_data ,
                                         path = path_R_output_data,
                                         export = export_R_calculations
  )
  
  #### Colour Code Observations based on Equal Percentile Buckets
  ##### Calculate percentile cutoffs
  
  
  # Function to color code risk: right-tailed risk, two_tailed risk, or left_tailed risk
  risk_color <- function(ts_y_vector, risk = "right_tailed") {
    col_labels = c("green", "light green",  "yellow", "orange",  "red", "dark red")
    partitions = 6
    colored_labels = col_labels
    
    if(risk == "two_tailed") {
      colored_labels =  c(rev(col_labels), col_labels)
      partitions = 12
      
    } else if(risk == "left_tailed") {
      colored_labels = rev(col_labels)
    }
    
    ts_y_vector <- na.omit(ts_y_vector)
    color_code_percentile <- factor(cut(ts_y_vector,
                                        quantile(ts_y_vector, probs = seq(0, 1, 1/partitions)),
                                        include.lowest = TRUE),
                                    labels = colored_labels)
    #color code observations
    color_code_vec <- as.character(color_code_percentile)
    return(color_code_vec)
  }
  
  "Risk color per observation -- 3-year trend data"
  risk_color_3years = risk_color(ts_y_vector = data3yeartrend[[4]], risk = risk)
  
  
  "Risk color per observation -- 8-year trend data"
  risk_color_8years = risk_color(ts_y_vector = data8yeartrend[[4]], risk = risk)
  
  
  "Risk color for each observation -- ratio data"
  if(time_series_type == "Quarterly") {
    risk_color_ratio = risk_color(ts_y_vector = time_series_GDP_Ratio_data[[4]], risk = risk)
  }
  
  
  #### Plot 3 and 8 Year Deviation from Trend
  
  
  #Function to plot dots only for deviation from n-year trend and ratio data
  ggplot_ts_point_line = function(time_series_data = time_series_data,
                                  time_points_in_plots = 40,
                                  risk = "right_tailed",
                                  ylabel = "default",
                                  tag = "") {
    
    Ratio_plot = F
    if(colnames(time_series_data)[5] == "Mean_of_Ratio") {
      Ratio_plot = T
    }
    
    # Prepare the data for plotting
    time_series_data = na.omit(time_series_data)
    ts_y_vector = na.omit(time_series_data[[4]])
    risk_color_vec =  risk_color(ts_y_vector = ts_y_vector, risk = risk)
    yname = colnames(time_series_data)[4]  #get name from the fourth column of data.frame
    colnames(time_series_data)[c(4,5)] = c("yvar", "average")
    
    if(ylabel == "default") {
      ylabel = y_axis_label
    }
    
    #Locations to put date labels
    locations <- c(floor(seq(from = 1,to = nrow(na.omit(time_series_data)),
                             by = (nrow(na.omit(time_series_data)) / time_points_in_plots))),
                   nrow(na.omit(time_series_data)))
    
    #Plot the time series.
    p <- ggplot(na.omit(time_series_data), aes(x = seq_along(Period), y = yvar)) +
      geom_line(color = "black") +
      geom_point(color = risk_color_vec, size = 2) +
      #geom_line(aes(y = average), color = "steelblue") +
      geom_line(aes(y = (min(yvar) - sd(yvar) * 0.5)), color = risk_color_vec, size =
                  4.5) +
      labs(title = yname, y = ylabel, x = tag) +
      scale_x_continuous(breaks = locations, labels = time_series_data$Period[locations]) +
      scale_y_continuous(labels = human_num) +
      theme(axis.title.y = element_text(face = "plain", size = 9),
            axis.title.x = element_text(face = "plain", size = 7), 
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            axis.text.x = element_text(vjust = 0.5, angle = 90, size = 7),
            axis.text.y = element_text(size = 7)  )
    
    # if(Ratio_plot == F) { #Colin commented this out in his code.
    #   p = p + geom_line(aes(y = 0), color = "black", linetype = "dashed")
    # }
    
    return(p)
  }
  
  #### Plot Ratio to GDP
  plot_ratio = ggplot_ts_point_line(time_series_data = time_series_GDP_Ratio_data, ylabel = "", time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 2")
  
  ### Plot the 3 year and 8 year deviation from trend.
  plot_3yeartrend = ggplot_ts_point_line(time_series_data = data3yeartrend, time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 3")
  plot_8yeartrend = ggplot_ts_point_line(time_series_data = data8yeartrend, time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 4")
  
  
  
  ### Place the plots in their repective bucket folder as one plot. 
  
  #Check if directory exists, if not create one. 
  
  path_dir = file.path("..", "Heatmap_Subplots", bucket)
  dir.create(path_dir, recursive = T)
  
  # Save under directory as pdf/jpeg.
  
  mypath <- file.path(path_dir, paste0( time_series_name, ".pdf"))
  
  pdf(file = mypath, width = 10 , height = 10 )
  
  figure1 = ggarrange(plot_levels,plot_ratio, plot_3yeartrend, plot_8yeartrend, ncol = 2, nrow = 2 ) 
  print(annotate_figure(figure1, top = text_grob(paste0( time_series_name, " -- ", bucket, ": ", sub_bucket, " \n  " ), face = "bold", size = 14)))
  
  dev.off()
  
  # Create the data frame to export for creating the heatmap
  
  risk_color_used = risk_color_ratio
  
  last_4Q_risk_color = tail(risk_color_used, n = n_Quarters)
  Period = tail(time_series_GDP_Ratio_data$Period, n = n_Quarters)
  ratio_ts = tail(time_series_GDP_Ratio_data[[4]], n = n_Quarters)
  
  df = data.frame(time_series_name = time_series_name, Period = Period, risk_color = last_4Q_risk_color, ratio_ts = ratio_ts, 
                  bucket = bucket, sub_bucket = sub_bucket )
  
  return(df)
  
}

# Function: 
##############################




# Function: create_subplots
# ====================================

create_subplots = function(file_name = "Current Account to GDP.xlsm", 
                                                          time_series_name = "Current Account to GDP",
                                                          y = "Current Account", 
                                                          y_axis_label = "CAD Millions",
                                                          Ratio_var_name = "GDP",                
                                                          time_series_type = "Quarterly", # "Monthly" or "Quarterly"
                                                          start_ts_at = "Q1 1994",        # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
                                                          risk = "two_tailed",            # "two_tailed",  "right_tailed",  "left_tailed"
                                                          bucket = "Macrobucket", 
                                                          sub_bucket = "Extenal Imbalances",
                                                          n_Quarters = 4,
                                                          end_ts_at = "Q4 2019",
                                                          datapath = "./DataFolder", 
                                                          time_points_in_plots = 30, 
                                                          export_merged_data = "No"
) {
  
  
  # #Code for Debugging   
  # 
  # file_name = "TSX 243 Return on Equity.xlsm"
  # time_series_name = "TSX 243 Return on Equity"
  # y = "TSX 243 Net Income"
  # y_axis_label = "CAD Millions"
  # Ratio_var_name = "Common Equity"                
  # time_series_type = "Quarterly" # "Monthly" or "Quarterly"
  # start_ts_at = "Q1 1994"       # if Quarterly: "Q1 1994", "Q1 1983", if Monthly: "Jan03"
  # risk = "two_tailed"            # "two_tailed",  "right_tailed",  "left_tailed"
  # bucket = "Market, Funding, Solvency Risk" 
  # sub_bucket = "Market Prices"
  # n_Quarters = 4
  # end_ts_at = "Q4 2019"
  # datapath = "./DataFolder"
  # export_merged_data = "No"
  
  
  #### Data Preparation
  
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  
  ##### Import data
  
  library(readxl)
  sheet_used <- 2
  if (time_series_type == "Quarterly") {
    sheet_used <- 2
    y1 = paste("Quarterly", y)
  } else if (time_series_type == "Monthly") {
    sheet_used <- 3
    y1 = paste("Monthly", y)
  } else if (time_series_type == "Ratio") {
    sheet_used <- 4
    y1 = paste("Ratio", y)
  }
  
  #print(paste("time series name:", y))
  #print(paste("Ratio:", Ratio_var_name))
  
  
  path = datapath
  # Create dataframe in R -- "time_series_data"
  filename <- paste0(path, "/", file_name)
  time_series_data <- read_excel(filename, sheet = sheet_used,
                                 range = cell_cols("A:B"),
                                 col_names = c("Period", "y"))
  time_series_data <- time_series_data[-(1:2), ]
  time_series_data$y <- as.numeric(time_series_data$y)
  
  if(time_series_type == "Monthly") {
    time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
    time_series_data$Period = format(time_series_data$Period, "%b%y")
    
  }
  
  
  # Denominator, if ratio
  if (time_series_type == "Ratio") {
    sheet_used_ratio <- 4
    ratio_series_data <- read_excel(filename, sheet = sheet_used_ratio,
                                    range = cell_cols("A:B"),
                                    col_names = c("Period_ratio", "y_ratio"))
    ratio_series_data <- ratio_series_data[-(1:2), ]
    ratio_series_data$y_ratio <- as.numeric(ratio_series_data$y_ratio)
    time_series_data$y <- time_series_data$y / ratio_series_data$y_ratio
  }
  
  
  ##### Clean the data
  
  
  which(is.na(time_series_data), arr.ind = T)
  time_series_data <- na.omit(time_series_data)
  
  
  ##### Remove all the data after the last quarter: end_ts_at
  # so all the time series align.
  #str(time_series_data)
  end_ts_index <- which(time_series_data$Period == end_ts_at) 
  #length(time_series_data$Period)
  time_series_data = time_series_data[1:end_ts_index, ]
  #dim(time_series_data)
  #time_series_data$Period = as.character( time_series_data$Period)
  
  
  
  # Create the human numbers function.
  human_numbers <- function(x = NULL, smbl = "", signif = 1) {
    humanity <- function(y) {
      
      if (!is.na(y)) {
        tn <- round(abs(y) / 1e12, signif)
        b <- round(abs(y) / 1e9, signif)
        m <- round(abs(y) / 1e6, signif)
        k <- round(abs(y) / 1e3, signif)
        
        if ( y >= 0 ) {
          y_is_positive <- ""
        } else {
          y_is_positive <- "-"
        }
        
        if ( k < 1 ) {
          paste0(y_is_positive, smbl, round(abs(y), signif))
        } else if ( m < 1) {
          paste0 (y_is_positive, smbl, k, "k")
        } else if (b < 1) {
          paste0 (y_is_positive, smbl, m, "m")
        } else if(tn < 1) {
          paste0 (y_is_positive, smbl, b, "bn")
        } else {
          paste0 (y_is_positive, smbl, comma(tn), "tn")
        }
      } else if (is.na(y) | is.null(y)) {
        "-"
      }
    }
    
    sapply(x, humanity)
  }
  
  # Human versions of large currency numbers - extensible via smbl 
  
  human_gbp   <- function(x){human_numbers(x, smbl = "£")} 
  human_usd   <- function(x){human_numbers(x, smbl = "$")} 
  human_euro  <- function(x){human_numbers(x, smbl = "€")} 
  human_num   <- function(x){human_numbers(x, smbl = "", signif = 2)} 
  
  
  # Locations to put date labels
  locations = c(floor(seq(from = 1, to = nrow(time_series_data), by = nrow(time_series_data) / time_points_in_plots)), nrow(time_series_data))
  
  # Plot the time series
  plot_levels = ggplot(time_series_data, aes(x = seq_along(Period), y = y)) +
    geom_line(color = "darkred", size = 0.7) +
    labs(title = paste(y, " --  Levels"), x = "Figure 1", y = y_axis_label) + #, tag = "Figure 1") +
    scale_x_continuous(breaks = locations, labels = time_series_data$Period[locations]) +
    scale_y_continuous(labels = human_num) +
    theme(axis.title.y = element_text(face = "plain", size = 9), 
          axis.title.x = element_text(face = "plain", size = 7), 
          axis.text.x = element_text(vjust = 0.5, angle = 90, size = 7), 
          axis.text.y = element_text(size = 7),
          plot.title = element_text(hjust = 0.5, size = 10, face = "bold")
          #, plot.tag = element_text(size = 10)
          #, plot.tag.position = c(0.5, 0)
    )
  
  
  #### Calculate Time Series Data -- Rates of Growth
  
  
  # Calculating the Year over Year Percent change. 
  
  percent_change <- NULL  # Percent change must always be year over year change for Monthly and Quarterly.
  
  n_rows_lag = 4  # The default percent change is for Quarterly data, hence 4. 
  
  if(time_series_type == "Monthly") {
    n_rows_lag = 12 # For Monthly data it becomes 12. 
  }
  
  # Extract y variable from data frame and store it as a vector under "y_time_series"
  y_time_series <- time_series_data$y
  
  
  for(i in 1:(nrow(time_series_data) - n_rows_lag)) {
    percent_change[i] = 100 * (y_time_series[i + n_rows_lag] - y_time_series[i]) / y_time_series[i]
  }
  
  
  
  # Add percent_change to data.frame "time_series_data" and change the zeros in percent_change to NA to correctly plot the graph.
  
  percent_change_full =  c( rep(NA, nrow(time_series_data) - length(percent_change)), percent_change)
  length(percent_change_full)
  
  # Start percentage change calculation from start_ts_at. 
  
  start_ts_index <- which(time_series_data$Period == start_ts_at) 
  
  percent_change_full[1:(start_ts_index - 1)] = NA # This puts NA's for all the rows before start_ts_at.
  
  percent_change = percent_change_full # The full length percent_change vector with correct NA's.
  
  time_series_data$percent_change <- percent_change # Add percent_change to time series from start_ts_at. 
  
  percent_change = na.omit(percent_change) # Remove the NA's from percent_change to get the final vector to use in the analysis.                  
  
  ##### Plot time series data -- rates of growth
  
  # Add average line to dataframe "time_series_data"
  time_series_data$average <- mean(na.omit(time_series_data$percent_change))
  
  
  
  #### Calculate Time Series Data -- Deviation from 3 and 8-year Trend
  
  
  # Create new data.frame with raw data, but excluding calcs 
  time_series_data_orig <- time_series_data[ ,c(1,2)]
  
  # Function for the deviation from average n-year trend data  
  
  deviation_from_trend_fn <- function(n_years_trend = 3,  time_series_data_orig = time_series_data_orig) {
    if(time_series_type == "Monthly") {
      n_months = n_years_trend * 12
      n_rows_lag = n_months
    } else {
      n_quarters = n_years_trend * 4
      n_rows_lag = n_quarters
    }
    
    start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
    start_ts_index
    # subset(time_series_data, Period > start_ts)
    time_series_data_relevant <- time_series_data_orig[(start_ts_index - (n_rows_lag - 1)) : nrow(time_series_data_orig), ]
    # extract y variable from data frame and store it as a vector under y_time_series
    y_time_series <- time_series_data_relevant$y
    y_mean <- mean(y_time_series)
    
    n_year_trend = NULL
    
    # Calculates the 3-year MA trend
    for(i in n_rows_lag : (nrow(time_series_data_relevant))) {
      n_year_trend[i] = mean(y_time_series[(i - (n_rows_lag - 1)) : i])
    }
    
    length(n_year_trend)
    
    n_year_trend_f <- as.numeric(na.omit(n_year_trend))
    
    # Fill NAs before start date in data.frame
    n_year_trend_final <- c(rep(NA, nrow(time_series_data_orig) - length(n_year_trend_f)), n_year_trend_f)
    
    # Subtraction to create the deviation from trend
    deviation_from_n_year_trend <- time_series_data_orig$y - n_year_trend_final
    
    mean_deviation <- mean(deviation_from_n_year_trend, na.rm = T)
    
    library(english)
    v1 = paste0(as.english(n_years_trend), " Year Trend")
    v2 = paste0("Deviation From ", n_years_trend ," Year Trend")
    v3 = paste0("Mean of ", n_years_trend, " Year Trend" )
    
    dff = data.frame(time_series_data_orig, v1 = n_year_trend_final, v2 = deviation_from_n_year_trend, v3 = mean_deviation)
    
    colnames(dff)[c(2, 3, 4, 5)] = c(y, v1, v2, v3)
    
    return(dff)
  }
  
  # Create data for n-years of trend
  
  data3yeartrend <- deviation_from_trend_fn(n_years_trend = 3, time_series_data_orig = time_series_data_orig) 
  data8yeartrend <- deviation_from_trend_fn(n_years_trend = 8, time_series_data_orig = time_series_data_orig)
  
  #### Calculate Time Series Data -- Ratio to GDP
  
  # Function that reads the time series data from the excel file 
  read_ts_data_fn <- function(filename, time_series_type) {
    sheet_used <- 2
    if(time_series_type == "Quarterly") {
      sheet_used <- 2
      y = paste("Quarterly", y)
    } else if (time_series_type == "Monthly") {
      sheet_used <- 3
      y = paste("Monthly", y)
    } else if (time_series_type == "Ratio") {
      sheet_used <- 4
      y = paste("Ratio", y)
    }
    
    time_series_data <- read_excel(filename, sheet = sheet_used,
                                   range = cell_cols("A:B"),
                                   col_names = c("Period", "y"))
    time_series_data <- time_series_data[-(1:2), ]
    time_series_data$y <- as.numeric(time_series_data$y)
    
    if(time_series_type == "Monthly") {
      time_series_data$Period = as.Date(as.numeric(time_series_data$Period), origin = "1899-12-30")
      time_series_data$Period = format(time_series_data$Period, "%b%y")
    }
    
    # Clean the data by deleting any NA's such as at the end of the series. 
    which(is.na(time_series_data), arr.ind = T)
    time_series_data <- na.omit(time_series_data)
    
    # Remove data (rows) after the last quarter: end_ts_at
    end_ts_index <- which(time_series_data$Period == end_ts_at) 
    time_series_data = time_series_data[1:end_ts_index, ]
    
    return(time_series_data)
    
  }
  
  # Function that creates the ratio data  
  fn_reads_and_creates_ratio_ts <- function(y, Ratio_var_name, filename, start_ts_at, time_series_data_orig = time_series_data_orig) {
    
    
    time_series_Ratio_data <- read_ts_data_fn(filename, time_series_type = "Ratio") 
    start_ts_index <- which(time_series_data_orig$Period == start_ts_at)
    Ratio_var <- time_series_Ratio_data$y[start_ts_index : nrow(time_series_data_orig)]
    Ratio_var <- c(rep(NA, nrow(time_series_data_orig) - length(Ratio_var)), Ratio_var)
    
    # Create the ratio data   
    time_series_Ratio_data <- data.frame(time_series_data_orig, Ratio_var = time_series_Ratio_data$y)
    time_series_Ratio_data$Ratio <- time_series_Ratio_data$y / Ratio_var
    time_series_Ratio_data$Mean_of_Ratio <-  mean(na.omit(time_series_Ratio_data$y / Ratio_var))
    colnames(time_series_Ratio_data)[c(2, 3, 4)] <- c(y, Ratio_var_name, paste("Ratio:", y, "to", Ratio_var_name ))
    
    return(time_series_Ratio_data)
  }
  
  # Create the data.frame in R -- "time_series_GDP_Ratio_data"
  if(time_series_type == "Quarterly") {
    time_series_GDP_Ratio_data <- fn_reads_and_creates_ratio_ts(y = y, Ratio_var_name = Ratio_var_name, filename = filename, start_ts_at = start_ts_at, time_series_data_orig = time_series_data_orig)
  }
  
  
  #### Export R calculations back to a new csv file
  
  # Function that merges all four data sets to be exported 
  merge_and_export_data_fn <- function(y = y, trendyears = "3and8", data1, data2, data3, data4, path = "", export = "Yes") {
    
    colnames(data1)[2] = y
    
    if(time_series_type == "Monthly"){ # if data is monthly
      namefile = paste(y, trendyears, "years") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)])
    } else { # if data is anything else i.e. Quarterly.
      namefile = paste(y, trendyears, "years with", Ratio_var_name, "Ratio") 
      df = cbind(data1, data2[, c(-1, -2)], data3[, c(-1, -2)], data4[,c(-1,-2)])
    }
    
    todaysData = format(Sys.time(), "%d%m%Y")
    dataname = file.path(path, paste0( namefile, " - ", time_series_type ," - " , todaysData, ".csv"))
    
    if(export == "Yes") {
      write.csv(df, dataname, row.names = F)
    }
    
    return(df)
  }
  
  
  
  # Merging and exporting the data 
  
  #Do not export anything
  if(time_series_type == "Monthly"){ time_series_GDP_Ratio_data = NULL }
  
  #Check if directory exists, if not create it. 
  path_R_output_data = file.path("..", "R_output_data")
  dir.create(path_R_output_data, recursive = T)    
  
  #Merge and export function call
  final_data <- merge_and_export_data_fn(y = y, trendyears = "3and8",
                                         data1 = time_series_data,
                                         data2 = data3yeartrend,
                                         data3 = data8yeartrend,
                                         data4 = time_series_GDP_Ratio_data ,
                                         path = path_R_output_data,
                                         export = export_merged_data
  )
  
  #### Colour Code Observations based on Equal Percentile Buckets
  ##### Calculate percentile cutoffs
  
  
  # Function to color code risk: right-tailed risk, two_tailed risk, or left_tailed risk
  risk_color <- function(ts_y_vector, risk = "right_tailed") {
    col_labels = c("green", "light green",  "yellow", "orange",  "red", "dark red")
    partitions = 6
    colored_labels = col_labels
    
    if(risk == "two_tailed") {
      colored_labels =  c(rev(col_labels), col_labels)
      partitions = 12
      
    } else if(risk == "left_tailed") {
      colored_labels = rev(col_labels)
    }
    
    ts_y_vector <- na.omit(ts_y_vector)
    color_code_percentile <- factor(cut(ts_y_vector,
                                        quantile(ts_y_vector, probs = seq(0, 1, 1/partitions)),
                                        include.lowest = TRUE),
                                    labels = colored_labels)
    #color code observations
    color_code_vec <- as.character(color_code_percentile)
    return(color_code_vec)
  }
  
  "Risk color per observation -- 3-year trend data"
  risk_color_3years = risk_color(ts_y_vector = data3yeartrend[[4]], risk = risk)
  
  
  "Risk color per observation -- 8-year trend data"
  risk_color_8years = risk_color(ts_y_vector = data8yeartrend[[4]], risk = risk)
  
  
  "Risk color for each observation -- ratio data"
  if(time_series_type == "Quarterly") {
    risk_color_ratio = risk_color(ts_y_vector = time_series_GDP_Ratio_data[[4]], risk = risk)
  }
  
  
  #### Plot 3 and 8 Year Deviation from Trend
  
  
  #Function to plot dots only for deviation from n-year trend and ratio data
  ggplot_ts_point_line = function(time_series_data = time_series_data,
                                  time_points_in_plots = 40,
                                  risk = "right_tailed",
                                  ylabel = "default",
                                  tag = "") {
    
    Ratio_plot = F
    if(colnames(time_series_data)[5] == "Mean_of_Ratio") {
      Ratio_plot = T
    }
    
    # Prepare the data for plotting
    time_series_data = na.omit(time_series_data)
    ts_y_vector = na.omit(time_series_data[[4]])
    risk_color_vec =  risk_color(ts_y_vector = ts_y_vector, risk = risk)
    yname = colnames(time_series_data)[4]  #get name from the fourth column of data.frame
    colnames(time_series_data)[c(4,5)] = c("yvar", "average")
    
    if(ylabel == "default") {
      ylabel = y_axis_label
    }
    
    #Locations to put date labels
    locations <- c(floor(seq(from = 1,to = nrow(na.omit(time_series_data)),
                             by = (nrow(na.omit(time_series_data)) / time_points_in_plots))),
                   nrow(na.omit(time_series_data)))
    
    #Plot the time series.
    p <- ggplot(na.omit(time_series_data), aes(x = seq_along(Period), y = yvar)) +
      geom_line(color = "black") +
      geom_point(color = risk_color_vec, size = 2) +
      #geom_line(aes(y = average), color = "steelblue") +
      geom_line(aes(y = (min(yvar) - sd(yvar) * 0.5)), color = risk_color_vec, size =
                  4.5) +
      labs(title = yname, y = ylabel, x = tag) +
      scale_x_continuous(breaks = locations, labels = time_series_data$Period[locations]) +
      scale_y_continuous(labels = human_num) +
      theme(axis.title.y = element_text(face = "plain", size = 9),
            axis.title.x = element_text(face = "plain", size = 7), 
            plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
            axis.text.x = element_text(vjust = 0.5, angle = 90, size = 7),
            axis.text.y = element_text(size = 7)  )
    
    # if(Ratio_plot == F) { #Colin commented this out in his code.
    #   p = p + geom_line(aes(y = 0), color = "black", linetype = "dashed")
    # }
    
    return(p)
  }
  
  #### Plot Ratio to GDP
  plot_ratio = ggplot_ts_point_line(time_series_data = time_series_GDP_Ratio_data, ylabel = "", time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 2")
  
  ### Plot the 3 year and 8 year deviation from trend.
  plot_3yeartrend = ggplot_ts_point_line(time_series_data = data3yeartrend, time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 3")
  plot_8yeartrend = ggplot_ts_point_line(time_series_data = data8yeartrend, time_points_in_plots = time_points_in_plots, risk = risk, tag = "Figure 4")

  
  ### Arrage the subplots into one plot. 
  
  
  figure1 = ggarrange(plot_levels,plot_ratio, plot_3yeartrend, plot_8yeartrend, ncol = 2, nrow = 2 ) 
  fig1 = annotate_figure(figure1, top = text_grob(paste0( time_series_name, " -- ", bucket, ": ", sub_bucket, " \n  " ), face = "bold", size = 14))
  #print(fig1)
  
  return(fig1)
}



# Function: heatmap_cat_data()
##############################
# Custom created heatmap plot to plot categorical data such as colours as the data and not numbers.

heatmap_cat_data = function(risk_data, bucket) {
  
  windowsFonts(Times=windowsFont("Times New Roman"))
  
  data = risk_data[risk_data$bucket == bucket,]
  
  
  ratio_ts_keep = as.character(tail(unique(data$Period), 2))
  
  second_last_Q = ratio_ts_keep[1]
  last_Q = ratio_ts_keep[2]
  
  # Create the arrow_data_frame which will be used to create the arrow plots.
  arrow_data_frame = data[data$Period == last_Q, ]
  
  last_ratio_ts = as.numeric(data$ratio_ts[data$Period == last_Q ])
  second_last_ratio_ts =  as.numeric(data$ratio_ts[data$Period == second_last_Q ])
  
  arrow_vector = last_ratio_ts - second_last_ratio_ts
  !all(arrow_vector == 0)
  #arrow = ifelse(arrow_vector>0, paste0("\\","u2B9D"), paste0("\\", "u2B9F") ) # different type
  
  arrow = ifelse(arrow_vector>0, 24, 25)
  arrow_col = ifelse(arrow == 24, "grey", "white")
  arrow_data_frame$arrow = arrow ; arrow_data_frame$ratio_ts_diff = arrow_vector ; arrow_data_frame$arrow_col = arrow_col
  
  p1 = ggplot(data, aes(x = Period, y = time_series_name))+
    geom_point(size = 8, color = data$risk_color, shape = "square") +
    labs(title = paste("Financial Vulnerability", "--", bucket),  y = "", x = "") +
    theme(text=element_text(family="Times"),
          axis.text.y = element_text(hjust = 1, face = "plain", size = 13), 
          axis.text.x = element_text(face="bold", size = 11), 
          axis.ticks.x=element_blank(),
          plot.title = element_text(family="Times", face = "bold", size = 16),
          plot.title.position = "plot") +
    scale_x_discrete(position = "top") +
    facet_grid(sub_bucket~., space = "free", scales = "free", shrink = F, switch = "y") # labeller = label_wrap_gen()
  #p1 = ggplotly(p1)
  
  p2 = ggplot(arrow_data_frame, aes(x = Period, y = time_series_name))+
    geom_point( shape = arrow_data_frame$arrow, size = 4, fill=arrow_data_frame$arrow_col) + #
    labs(  y = "", x = "") +   
    theme(axis.title.x=element_blank(),
          axis.text.x=element_text(family = "Times",face="bold", size = 13),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          aspect.ratio = 10/1) +
    scale_x_discrete(breaks=c("Q4 2019"),labels=c("Trend"), position = "top") +
    facet_grid(sub_bucket~., space = "free", scales = "free", shrink = F) + 
    theme(strip.background = element_blank(),  strip.text.y = element_blank())
  #p2 = p2 + clean_theme()
  #p2 = ggplotly(p2)
  figure = ggarrange(p1,p2, ncol = 2, nrow = 1, widths = c(10, 1), align = "h") 
  #figure = ggplotly(figure)
  print(annotate_figure(figure))
} 




















