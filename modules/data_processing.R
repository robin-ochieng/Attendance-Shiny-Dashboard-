# data_processing.R

read_and_process_data <- function(filepath) {
  Data <- read_excel(filepath, col_types = c("text", "text", "date", "text", "text", "text", "text", "text", "numeric"))

  Data <- Data %>%
    mutate(Time = as.numeric(Time),  # Ensure Time is numeric
           Time = seconds_to_period(Time * 86400),  # Convert to period
           Hour = hour(Time),  # Extract hour component
           Time_of_Day = if_else(hour(Time) > 6 & hour(Time) < 19 |
                                     (hour(Time) == 6 & minute(Time) > 0) |
                                     (hour(Time) == 19 & minute(Time) == 0),
                                   "Day", "Night"),  # Determine Time of Day
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
           Division_Name = paste(Division, Name, sep = " - "),  # Combine 'Division' and 'Name'
           Time_str = sprintf("%02d:%02d:%02d", as.integer(hour(Time)), as.integer(minute(Time)), as.integer(second(Time))),
           FirstSignInTime = as.POSIXct(paste(`Date/Time`, Time_str), format = "%Y-%m-%d %H:%M:%S", tz = "Africa/Nairobi")
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
    dplyr::filter(Status == "Sign In") %>%
    group_by(`Date/Time`, Name, Division) %>%
    summarise(FirstSignInTime = min(FirstSignInTime), .groups = 'drop') %>%
    dplyr::filter(FirstSignInTime < as.POSIXct(paste(`Date/Time`, "08:15:00"), format = "%Y-%m-%d %H:%M:%S"))
  
  return(EarlySignInDetails)
}


# Function to calculate late sign-in details
calculate_late_sign_ins <- function(Data) {
  LateSignInDetails <- Data %>%
    filter(Status == "Sign In") %>%  # Filter only sign-in records
    group_by(`Date/Time`, Name, Division) %>%  # Group by Date/Time, Name, and Division
    summarise(FirstSignInTime = min(FirstSignInTime), .groups = "drop") %>%  # Calculate minimum sign-in time for each group
    filter(FirstSignInTime > as.POSIXct(paste(`Date/Time`, "08:16:00"), format = "%Y-%m-%d %H:%M:%S"))  # Filter for late sign-ins after 8:15 AM

  return(LateSignInDetails)
}
