# -----------------------------------------------------------------------------------------------

# Definition Keyword Search Function ----

keyword_search <- function(data, keywords)
{
  keywords.search <- grepl(keywords, data$definition, ignore.case=T) # data$definition limits search to definition column while c(yada yada yada includes all variables)
  data.keywords <- data[keywords.search, c("root","root2","category","definition","X1970.f","X1970.s","X1980.f","X1980.s","X1990.f","X1990.s","X2000.f","X2000.s","X2010.f","X2010.s" )] 
  return(data.keywords) # results of function
  }

# -----------------------------------------------------------------------------------------------

# Category Keyword Search Function ----

cat_search <- function(data, keywords)
{
  keywords.search <- grepl(keywords, data$category, ignore.case=T) # data$category limits search to category column while c(yada yada yada includes all variables)
  data.keywords <- data[keywords.search, c("root","root2","category","definition","X1970.f","X1970.s","X1980.f","X1980.s","X1990.f","X1990.s","X2000.f","X2000.s","X2010.f","X2010.s" )] 
  return(data.keywords) # results of function
}

# -----------------------------------------------------------------------------------------------

# Filter Variables by Time Period Function ----


time_periods <- function(dd, start, finish)
{
  
  # 1970 Time Period
  
  period <- grepl("70$", dd$root, ignore.case = T)
  seventy <- data.frame(dd[period,]$root)
  
  # 1980 Time Period
  
  period <- grepl("80$", dd$root, ignore.case = T)
  eighty <- data.frame(dd[period,]$root)
  
  # 1990 Time Period
  
  period <- grepl("90$", dd$root, ignore.case = T)
  ninety <- data.frame(dd[period,]$root)
  
  # 2000 Time Period
  
  period <- grepl("00$", dd$root, ignore.case = T)
  thous <- data.frame(dd[period,]$root)
  
  # 2010 Time Period
  
  period <- grepl("10$", dd$root, ignore.case = T)
  tens <- data.frame(dd[period,]$root)
  
  #Combining Time Periods
  combo <- rbind.data.frame(seventy, eighty, ninety, thous, tens)
  colnames(combo) <- c("variable")
  final <- combo
  
  # Code to verify if date is in within the timeframe.
  
  final$seventy <- final$variable %in% unlist(seventy) 
  final$eighty <- final$variable %in% unlist(eighty)
  final$ninety <- final$variable %in% unlist(ninety)
  final$thous <- final$variable %in% unlist(thous)
  final$tens <- final$variable %in% unlist(tens)
  
  final$year <- ifelse(
                      final$seventy==TRUE, final$year <- 1970,
                      ifelse(
                            final$eighty==TRUE, final$year <- 1980,
                            ifelse(
                                  final$ninety==TRUE, final$year <- 1990,
                                  ifelse(
                                        final$thous==TRUE, final$year <- 2000,
                                        ifelse(
                                              final$tens==TRUE, final$year >- 2010, NA
                                              )
                                        )
                                  )
                            )
                      )
                
  # Limit Table to Variable and Year Columns
  
  final <- final[, c("variable","year")]
  
  # Assigns date1 variable based on start year input
  
  if((start == 70)|(start== 1970))
    {
      date1 <- 1970  
    }
    else if((start == 80)|(start == 1980))
        {
          date1 <- 1980
        }
        else if((start == 90)|(start == 1990))
            {
              date1 <- 1990
            }
            else if((start == 00)|(start == 2000))
                {
                  date1 <- 2000
                }
                else if((start == 10)|(start == 2010))
                    {
                      date1 <- 2010
                }
                
  # Assigns date2 variable based on finish year input
                
  if((finish == 70)|(finish == 1970))
    {
      date2 <- 1970  
    }
    else if((finish == 80)|(finish == 1980))
        {
          date2 <- 1980
        }
        else if((finish == 90)|(finish == 1990))
            {
              date2 <- 1990
            }
            else if((finish == 00)|(finish == 2000))
                {
                  date2 <- 2000
                }
                else if((finish == 10)|(finish == 2010))
                    {
                      date2 <- 2010
                    }              
                
            
  # Filter data by date inputs and save to table
  
  final <- final %>%              
  filter(final$year >= date1 & final$year <= date2)
  results <- final
  
  ifelse( nrow(results) >0, return(as.data.frame(results)),print("Data unavailable"))
}




# -----------------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------------------------
