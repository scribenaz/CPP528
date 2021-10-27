# -----------------------------------------------------------------------------------------------

# Definition Keyword Search Function ----

keyword_search <- function(data, keywords)
{
  keywords.search <- grepl(keywords, data$definition, ignore.case=T) 
  data.keywords <- data[keywords.search, c("root","root2","category","definition","X1970.f","X1970.s","X1980.f","X1980.s","X1990.f","X1990.s","X2000.f","X2000.s","X2010.f","X2010.s" )] 
  return(data.keywords) # results of function
  }

# -----------------------------------------------------------------------------------------------

# Category Keyword Search Function ----

cat_search <- function(data, keywords)
{
  keywords.search <- grepl(keywords, data$category, ignore.case=T) 
  data.keywords <- data[keywords.search, c("root","root2","category","definition","X1970.f","X1970.s","X1980.f","X1980.s","X1990.f","X1990.s","X2000.f","X2000.s","X2010.f","X2010.s" )] 
  return(data.keywords) # results of function
}

# -----------------------------------------------------------------------------------------------

# Filter Variables by Time Period Function




# -----------------------------------------------------------------------------------------------



# -----------------------------------------------------------------------------------------------