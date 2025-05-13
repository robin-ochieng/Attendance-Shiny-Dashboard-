# data_processing.R

read_and_process_data <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("text", "text", "date", "text", "text", "text", "text", "text", "numeric"))

  Data <- Data %>%
    mutate(Time_hms = hms(as.numeric(Time) * 86400),  # Convert to period
            Date_only = as.Date(`Date/Time`, tz = "Africa/Nairobi"),
            # Combine the date + time fraction to get a full POSIXct datetime
            FullDateTime = as.POSIXct(
              paste(Date_only, format(Time_hms, "%H:%M:%S")),
              tz = "Africa/Nairobi"
            ),
           Hour = hour(Time_hms),  # Extract hour component
           Min  = minute(Time_hms),
           Sec  = second(Time_hms),
           Time_of_Day = if_else(
            Hour > 6 & Hour < 19 | (Hour == 6 & Min > 0) | (Hour == 19 & Min == 0),
            "Day",
            "Night"
            ),  # Determine Time of Day
           Time_Period = case_when(
             Hour >= 4 & Hour < 10 ~ "Morning (6-10)",
             Hour >= 10 & Hour < 12 ~ "Mid-Morning (10-12)",
             Hour >= 12 & Hour < 13 ~ "Noon (12-13)",
             Hour >= 13 & Hour < 16 ~ "Afternoon (13-16)",
             Hour >= 16 & Hour < 19 ~ "Evening (16-19)",
             Hour >= 19 | Hour < 4 ~ "Night (19-00)",  # Adjusted for logical OR as midnight spans from 19 to before 4
             TRUE ~ "Unknown"  # Default case
           ),
           Day_of_week = weekdays(`Date/Time`),  # Extract the day of the week from 'Date/Time'
           Division_Name = paste(Division, Name, sep = " - "), # Combine 'Division' and 'Name'
           FirstSignInTime = FullDateTime
         )
  
  return(Data)
}

# Function to calculate attendance metrics
calculate_metrics <- function(Data) {
  attendance_metrics <- Data %>%
    summarise(
      Total_Sign_In = sum(Status == "Sign In", na.rm = TRUE),
      Total_Sign_Out = sum(Status == "Sign Out", na.rm = TRUE),
      Unique_Days_Counted = n_distinct(`Date/Time`)
    ) %>%
    mutate(
      Sign_In_Frequency = Total_Sign_In / Unique_Days_Counted,
      Sign_In_Rate = Total_Sign_In / (Total_Sign_In + Total_Sign_Out),
      Sign_Out_Frequency = Total_Sign_Out / Unique_Days_Counted,
      Sign_Out_Rate = Total_Sign_Out / (Total_Sign_In + Total_Sign_Out)
    )
  
  return(attendance_metrics)
}


# Function to summarize early sign-in times
calculate_early_sign_ins <- function(Data) {
  EarlySignInDetails <- Data %>%
    filter(Status == "Sign In") %>%
    group_by(Date_only, Name, Division) %>%
    summarise(FirstSignInTime = min(FullDateTime), .groups = 'drop') %>%
    filter(FirstSignInTime < as.POSIXct(paste(Date_only, "08:15:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi"))
  
  return(EarlySignInDetails)
}


# Function to calculate late sign-in details
calculate_late_sign_ins <- function(Data) {
  LateSignInDetails <- Data %>%
    filter(Status == "Sign In") %>%  # Filter only sign-in records
    group_by(Date_only, Name, Division) %>%  # Group by Date/Time, Name, and Division
    summarise(FirstSignInTime = min(FullDateTime), .groups = "drop") %>%  # Calculate minimum sign-in time for each group
    filter(FirstSignInTime > as.POSIXct(paste(Date_only, "08:16:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi"))  # Filter for late sign-ins after 8:15 AM

  return(LateSignInDetails)
}

# Function to summarize early sign-in times
calculate_early_sign_outs <- function(Data) {
  EarlySignOutsDetails <- Data %>%
    filter(Status == "Sign Out") %>%
    group_by(Date_only, Name, Division) %>%
    summarise(FirstSignOutTime = max(FullDateTime), .groups = 'drop') %>%
    filter(
      FirstSignOutTime >= as.POSIXct(paste(Date_only, "16:45:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi"),
      FirstSignOutTime <  as.POSIXct(paste(Date_only, "17:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
    )
  
  return(EarlySignOutsDetails)
}


# Function to calculate late sign-in details
calculate_late_sign_outs <- function(Data) {
  LateSignOutsDetails <- Data %>%
    filter(Status == "Sign Out") %>%  # Filter only sign-in records
    group_by(Date_only, Name, Division) %>%  # Group by Date/Time, Name, and Division
    summarise(FirstSignOutTime = max(FullDateTime), .groups = "drop") %>%  # Calculate minimum sign-in time for each group
    filter(
      FirstSignOutTime >= as.POSIXct(paste(Date_only, "17:01:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi") &
      FirstSignOutTime <  as.POSIXct(paste(as.Date(Date_only) + 1, "02:00:00"), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
    )  # Filter for late sign-ins after 8:15 AM
  
  return(LateSignOutsDetails)
}