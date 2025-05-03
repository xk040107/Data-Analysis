library(DataExplorer)
library(ggplot2)
library(dplyr)
library(randomForest)
library(stringr)

fileURL = "C:\\Users\\Xin Kher\\Documents\\apu\\data analysis (r)\\assignment\\hackingData.csv"

df = read.csv(fileURL)
df

plot_missing(df)
View(df)
df_og <- df


# Replacing blanks 
column_names <- colnames(df)
column_names <- setdiff(column_names, c("Notify"))

for (name in column_names){
  
  # replace empty string with NA 
  df[[name]] = replace(df[[name]], df[[name]] == "", NA)
  # replace blank or whitespace-only strings with NA 
  df[[name]] <- gsub("^\\s*$", NA, df[[name]])
  
}
plot_missing(df)
colSums(is.na(df))


for (name in column_names){
  
  # replace empty string with NA 
  df[[name]] = replace(df[[name]], (is.null(df[[name]]) | trimws(df[[name]]) == "NULL"), NA) # == NULL
}
plot_missing(df)
colSums(is.na(df))

summary(df_og)
summary(df)
View(df[!rowSums(is.na(df)) <= 5,])
length(df[!rowSums(is.na(df)) <= 5,])
View(df_og[!rowSums(is.na(df_og)) <= 6,])


df <- df[rowSums(is.na(df)) <= 6,]
plot_missing(df)
colSums(is.na(df))

# ============================ Duplication Removal ============================ 
# Action: Remove full duplicates only
# Signifies multiple diff attacks in a day, diff downtime indicates system recovery at different times and system parts 

duplicates <- df[duplicated(df),]
View(duplicates)
dim(duplicates)
df <- unique(df)
plot_missing(df)

# ============================ Feature Cleaning  ============================ 
imputation_NA_replacement <- function(DF_COLUMN, IMPUTATION_VALUE){
  # DF_NEW = replace(is.na(DF[[COLUMN_NAME]]), IMPUTATION_VALUE) #DF[[COLUMN_NAME]]
  #df[[name]] = replace(df[[name]], (is.null(df[[name]]) | trimws(df[[name]]) == "NULL"), NA) # == NULL
  DF_COLUMN = replace(DF_COLUMN, is.na(DF_COLUMN), IMPUTATION_VALUE)
  return(DF_COLUMN)
}

### IP 
### URL 
df$IP = imputation_NA_replacement(df$IP, "Unknown")
df$URL = imputation_NA_replacement(df$URL, "Unknown")
plot_missing(df)

### RANSOM
df$Ransom = imputation_NA_replacement(df$Ransom, 0)

df$Ransom <- as.numeric(df$Ransom)
summary(df$Ransom) # after numeric changes 
View(df)
plot_missing(df)

### Country 
as.data.frame((table(df$Country, useNA = "ifany")))
as.data.frame(sort((table(df$Country, useNA = "ifany"))))

# Convert to lowercase and remove spaces, special characters before mapping
df$Country <- tolower(df$Country)           # Convert to lowercase
df$Country <- gsub("[[:punct:]]", "", df$Country)  # Remove punctuation
df$Country <- gsub("\\s+", "", df$Country)  # Remove all spaces
df$Country <- gsub('"', "", df$Country)     # Remove quotes specifically

# Create mapping dictionary - expanded based on the frequency table
country_mapping <- c(
  # Fix major duplicates and variations
  "unitedkingd" = "United Kingdom",
  "unitedkingdom" = "United Kingdom",
  "unitedkingdomom" = "United Kingdom",
  "unitedstate" = "United States",
  "unitedstates" = "United States",
  "russianfede" = "Russian Federation",
  "russianfederation" = "Russian Federation",
  "russianfederationration" = "Russian Federation",
  "czechrepubl" = "Czech Republic",
  "czechrepublic" = "Czech Republic",
  "czechrepublicic" = "Czech Republic",
  "netherland" = "Netherlands",
  "netherlands" = "Netherlands",
  "netherlandss" = "Netherlands",
  
  # Fix concatenated country names
  "americansamoa" = "American Samoa",
  "antiguaandbarbuda" = "Antigua and Barbuda",
  "bosniaandherzegovina" = "Bosnia and Herzegovina",
  "bruneidarussalam" = "Brunei Darussalam",
  "burkinafaso" = "Burkina Faso",
  "caymanislan" = "Cayman Islands",
  "cotedivoire" = "Cote d'Ivoire",
  "dominicanrepublic" = "Dominican Republic",
  "equatorialguinea" = "Equatorial Guinea",
  "faroeislands" = "Faroe Islands",
  "papuanewguinea" = "Papua New Guinea",
  "saudiarabia" = "Saudi Arabia",
  "southafrica" = "South Africa",
  "srilanka" = "Sri Lanka",
  "trinidadandtobago" = "Trinidad and Tobago",
  "unitedarabemirates" = "United Arab Emirates",
  
  # Fix truncated and malformed names
  "iranislami" = "Iran",
  "moldovarep" = "Moldova",
  "taiwanprov" = "Taiwan",
  "newzealand" = "New Zealand",
  "newcaledoni" = "New Caledonia",
  "newcaledonia" = "New Caledonia",
  "newcaledoniaa" = "New Caledonia",
  "frenchpolyn" = "French Polynesia",
  "macedoniat" = "Macedonia",
  "korearepub" = "South Korea",
  "southkorea" = "South Korea",
  
  # Fix islands and territories
  "virginislan" = "Virgin Islands",
  "virginislands" = "Virgin Islands",
  "virginislandsbritish" = "Virgin Islands (British)",
  "virginislandsus" = "Virgin Islands (U.S.)",
  "virginislandsds" = "Virgin Islands",
  "netherlandsantilles" = "Netherlands Antilles",
  "norfolkisland" = "Norfolk Island",
  "northernmarianaislands" = "Northern Mariana Islands",
  "turksandcaicosislands" = "Turks and Caicos Islands",
  
  # Fix special cases
  "laopeoplesdemocraticrepublic" = "Laos",
  "syrianarabrepublic" = "Syria",
  "syrianarab" = "Syria",
  "palestinianterritory" = "Palestine",
  "libyanarabjamahiriya" = "Libya",
  "anonymousproxy" = "Anonymous Proxy",
  "satelliteprovider" = "Satellite Provider",
  "vaticancitystate" = "Vatican City",
  "saintkittsnevis" = "Saint Kitts and Nevis",
  "saotomeandprincipe" = "Sao Tome and Principe",
  
  # Fix other common variations
  "capeverde" = "Cape Verde",
  "cookislands" = "Cook Islands",
  "costarica" = "Costa Rica",
  "elsalvador" = "El Salvador",
  "hongkong" = "Hong Kong",
  "puertorico" = "Puerto Rico",
  "sanmarino" = "San Marino",
  
  # Standardize regions
  "asia" = "Asia Pacific",
  "asiapacific" = "Asia Pacific",
  "asiapacificregion" = "Asia Pacific",
  "eastasia" = "East Asia",
  "easteuro" = "Eastern Europe",
  "westeuro" = "Western Europe",
  "easttimor" = "East Timor",
  "europe" = "European Union",
  "europeanuni" = "European Union",
  "europeanunion" = "European Union",
  "europeanunionon" = "European Union",
  "southamerica" = "South America",
  "northamerica" = "North America",
  "middleeast" = "Middle East",
  
  # Handle unknown values
  "nana" = "Unknown"
)

# Clean the data
df$Country <- coalesce(df$Country, "Unknown")
df$Country <- ifelse(is.na(df$Country) | df$Country == "" | df$Country == "unknown", "Unknown", df$Country) # Convert NA to "Unknown" because "Unknown" is the mode
#### df$Country <- str_replace_all(df$Country, setNames(country_mapping, names(country_mapping)))

df$Country <- recode(df$Country, !!!country_mapping)

# Proper case function
proper_case <- function(x) {
  # Handle NA values correctly
  #x[is.na(x)] <- NA
  
  # Split on space and handle each word
  words <- strsplit(tolower(x), " ")
  sapply(words, function(word) {
    # Special handling for specific words
    articles <- c("and", "of", "the", "in", "on", "at", "to")
    word <- sapply(word, function(w) {
      if(w %in% articles) {
        return(w)
      } else {
        return(paste0(toupper(substr(w, 1, 1)), substr(w, 2, nchar(w))))
      }
    })
    paste(word, collapse = " ")
  })
}

# Apply proper case
df$Country <- proper_case(df$Country)

as.data.frame(table(df$Country, useNA = "ifany"))
as.data.frame(sort(table(df$Country, useNA = "ifany")))

### WEBSERVER
as.data.frame(sort(table(df$WebServer, useNA = "ifany")))
as.data.frame(sort(unique(df$WebServer)))
# ggplot(df, aes(x = WebServer)) + 
#   geom_bar() +
#   facet_wrap(~ WebServer, scales = "free")
clean_webserver_data <- function(DF) {
  # Assuming your data frame has a column named 'WebServer'
  cleaned_data <- DF %>%
    mutate(
      WebServer = case_when(
        # Apache category
        grepl("Apache", WebServer, ignore.case = TRUE) ~ "Apache",
        
        # IIS category (all versions)
        grepl("IIS|Microsoft-IIS", WebServer, ignore.case = TRUE) ~ "Microsoft-IIS",
        
        # Nginx category
        grepl("nginx|cloudflare-nginx", WebServer, ignore.case = TRUE) ~ "Nginx",
        
        # LiteSpeed category
        grepl("LiteSpeed", WebServer, ignore.case = TRUE) ~ "LiteSpeed",
        
        # Lighttpd category
        grepl("lighttpd", WebServer, ignore.case = TRUE) ~ "Lighttpd",
        
        # Enterprise/Commercial Servers
        grepl("Oracle|Sun-Java|Sun-ONE|IBM|Zeus", WebServer, ignore.case = TRUE) ~ "Enterprise",
        
        # Security focused servers
        grepl("Security|Firewall|ModSecurity|Safe3", WebServer, ignore.case = TRUE) ~ "Security-Focused",
        
        # Unknown/Missing values
        is.na(WebServer) | WebServer == "Unknown" | 
          WebServer == "NOYB" | WebServer == "<NA>" ~ "Unknown",
        
        # Custom/Minor servers (everything else)
        TRUE ~ "Other"
      )
    )
}

df = clean_webserver_data(df)
colSums(is.na(df))
summary(df$WebServer)
View(df)
plot_missing(df)
as.data.frame(sort(table(df$WebServer, useNA = "ifany")))

### OS 
as.data.frame(sort(table(df$OS, useNA = "ifany")))
as.data.frame(sort(unique(df$OS)))

clean_os_data <- function(DF) {
  # Assuming your data frame has a column named 'OS'
  cleaned_data <- DF %>%
    mutate(
      OS = case_when(
        # Linux/Unix Family
        grepl("Linux|BSD|Unix|UNIX|Solar|SunOS|AIX|aix|HP-UX|Tru64", # |linux|LINUX
              OS, ignore.case = TRUE) & 
          !grepl("Microsoft|Windows|Win", OS, ignore.case = TRUE) ~ "Unix/Linux",
        
        # Windows Family
        grepl("Win|Windows|NT|Microsoft", OS, ignore.case = TRUE) ~ "Windows",
        
        # Mac Family
        grepl("Mac|Apple", OS, ignore.case = TRUE) ~ "Mac",
        
        # Embedded/Device OS
        grepl("embedded|Router|WAP|Switch|device|adapter|modem", 
              OS, ignore.case = TRUE) ~ "Embedded",
        
        # Network Devices
        grepl("F5|Cisco|Juniper|NetGear|D-Link|Linksys", 
              OS, ignore.case = TRUE) ~ "Network Devices",
        
        # Unknown/Missing values
        is.na(OS) | 
          grepl("Unknown|Unkno|unknown", OS, ignore.case = TRUE) | 
          OS == "<NA>" ~ "Unknown",
        
        # Everything else
        TRUE ~ "Other"
      )
    )
}

df = clean_os_data(df)
colSums(is.na(df))
summary(df$OS)
View(df)
plot_missing(df)
as.data.frame(sort(table(df$OS, useNA = "ifany")))

### Lang
as.data.frame(sort(table(df$Lang, useNA = "ifany")))
as.data.frame(sort(unique(df$Lang)))


clean_lang_data <- function(DF) {
  # Assuming your data frame has a column named 'Lang'
  cleaned_data <- DF %>%
    mutate(
      Lang = case_when(
        # Arabic
        grepl("^lang=ar$", Lang, ignore.case = TRUE) ~ "Arabic",
        
        # German
        grepl("^lang=de$", Lang, ignore.case = TRUE) ~ "German",
        
        # English (handling case differences like lang=en, lang=EN)
        grepl("^lang=en$", Lang, ignore.case = TRUE) ~ "English",
        
        # Spanish
        grepl("^lang=es$", Lang, ignore.case = TRUE) ~ "Spanish",
        
        # Italian
        grepl("^lang=it$", Lang, ignore.case = TRUE) ~ "Italian",
        
        # Portuguese
        grepl("^lang=pt$", Lang, ignore.case = TRUE) ~ "Portuguese",
        
        # Font Names (irrelevant data)
        grepl("Lucida|Times New Roman|Helvetica|Courier", Lang, ignore.case = TRUE) ~ "Font",
        
        # Unknown/Missing values
        is.na(Lang) | Lang == "<NA>" | 
          grepl("Unknown|Unkno|unknown", Lang, ignore.case = TRUE) ~ "Unknown",
        
        # Everything else
        TRUE ~ "Other"
      )
    )
}
df = clean_lang_data(df)
as.data.frame(sort(table(df$Lang, useNA = "ifany")))
View(df)
plot_missing(df)

### Encoding
as.data.frame(sort(table(df$Encoding, useNA = "ifany")))
as.data.frame(sort(unique(df$Encoding)))

clean_encoding_data <- function(DF) {
  cleaned_data <- DF %>%
    mutate(
      Encoding = case_when(
        # UTF Family
        Encoding %in% c("utf-8", "utf-16", "utf-16le", "utf-7") ~ "UTF",
        
        # ISO-8859 Family
        Encoding %in% c("iso-8859-1", "iso-8859-2", "ISO-8859-2", "iso-8859-5", "ISO-8859-5", 
                        "iso-8859-7", "iso-8859-8", "iso-8859-9", "iso-8859-11", "iso-8859-15") ~ "ISO-8859",
        
        # Windows Encodings
        Encoding %in% c("windows-1250", "windows-1251", "windows-1252", "windows-1253", 
                        "windows-1254", "windows-1255", "windows-1256", "windows-1257", "windows-874") ~ "Windows",
        
        # # Chinese Encodings
        # Encoding %in% c("GB2312", "gb2312", "Big5", "big5") ~ "Chinese",
        # 
        # # Japanese Encodings
        # Encoding %in% c("shift_jis", "EUC-JP") ~ "Japanese",
        # 
        # # Korean Encodings
        # Encoding %in% c("EUC-KR") ~ "Korean",
        # 
        # # Thai Encodings
        # Encoding %in% c("TIS-620", "tis-620") ~ "Thai",
        
        # ASCII & Similar
        Encoding %in% c("ascii", "us-ascii") ~ "ASCII",
        
        
        # Other Encodings
        Encoding %in% c("IBM855", "asmo-708", "KOI8-R") ~ "Other",
        
        # Unknown/Missing values + Irrelevant data merged
        is.na(Encoding) | Encoding %in% c("<NA>", "N", "LiteSpeed") ~ "Unknown",
        
        # Everything else
        TRUE ~ "Other"
      )
    )
}

df = clean_encoding_data(df)
as.data.frame(sort(table(df$Encoding, useNA = "ifany")))
View(df)
plot_missing(df)


### loss 
rf_loss_mice_optimized <- function(df, predictor_vars, 
                                   num_iterations = 3,     # Reduced from 5
                                   num_trees = 50,         # Reduced from 100
                                   min_improvement = 0.01, # Early stopping threshold
                                   sample_size = 0.7) {    # Random Forest subsample
  # Verify required packages
  if (!require(randomForest)) {
    stop("randomForest package is required")
  }
  
  # Initialize
  temp_df <- df
  target_var <- "Loss"
  
  # Verify predictor variables
  if (!all(predictor_vars %in% names(df))) {
    stop("Some predictor variables not found in dataset")
  }
  
  # Initial simple imputation for Loss
  temp_df[[target_var]] <- as.numeric(as.character(temp_df[[target_var]]))
  mean_val <- mean(temp_df[[target_var]], na.rm = TRUE)
  temp_df[[target_var]][is.na(temp_df[[target_var]])] <- mean_val
  
  # Store metrics and previous predictions
  evaluation_metrics <- list()
  previous_predictions <- NULL
  original_na <- is.na(df[[target_var]])
  n_samples <- floor(nrow(df) * sample_size)
  
  # MICE iterations
  for(iteration in 1:num_iterations) {
    # Subsample data for faster training
    sample_indices <- sample(1:nrow(temp_df), n_samples)
    train_data <- temp_df[sample_indices, ]
    
    formula_str <- paste(target_var, "~", paste(predictor_vars, collapse = " + "))
    
    tryCatch({
      # Train random forest with optimized parameters
      rf_model <- randomForest(
        formula = as.formula(formula_str),
        data = train_data,
        ntree = num_trees,
        nodesize = 5,         # Faster training
        sampsize = n_samples * 0.7,  # Further subsample each tree
        importance = FALSE    # Skip if not needed
      )
      
      # Predictions for missing values
      current_predictions <- predict(rf_model, temp_df[original_na, ])
      
      # Check for convergence
      if (!is.null(previous_predictions)) {
        improvement <- mean(abs(current_predictions - previous_predictions)) / 
          mean(abs(previous_predictions))
        
        if (improvement < min_improvement) {
          message("Early stopping at iteration ", iteration, 
                  " due to small improvement: ", round(improvement, 4))
          break
        }
      }
      
      # Store metrics and update values
      evaluation_metrics[[paste0("iteration_", iteration)]] <- list(
        oob_error = rf_model$mse[length(rf_model$mse)]
      )
      
      temp_df[original_na, target_var] <- current_predictions
      previous_predictions <- current_predictions
      
    }, error = function(e) {
      warning(paste("Error in iteration", iteration, ":", e$message))
    })
  }
  
  # Final evaluation
  final_evaluation <- list(
    mean_oob_error = mean(sapply(evaluation_metrics, function(x) x$oob_error)),
    iterations_completed = length(evaluation_metrics)
  )
  
  return(list(
    imputed_data = temp_df,
    final_evaluation = final_evaluation
  ))
}

result <- rf_loss_mice_optimized(
  df = df,
  predictor_vars = c("Ransom", "DownTime", "OS", "WebServer"),
  num_iterations = 3,
  num_trees = 50,
  min_improvement = 0.01,
  sample_size = 0.7
)

# Get imputed data
df <- result$imputed_data
View(df)
plot_missing(df)


### Changing the data types
summary(df)
summary(df_og)
df$DownTime <- as.numeric(df$DownTime)

write.csv(df, "C:\\Users\\Xin Kher\\Documents\\apu\\data analysis (r)\\assignment\\cleaned_hackingData.csv", row.names = FALSE)
