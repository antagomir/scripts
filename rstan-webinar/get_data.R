get_loans_data <- function(zip_file_name = "LoanStats3a.csv.zip") {
  if (!file.exists(zip_file_name)) {
    download.file(paste0("https://resources.lendingclub.com/", zip_file_name), 
                  destfile = zip_file_name, quiet = TRUE,
                  method = ifelse(.Platform$OS.type == "windows", "wininet", "wget"))
  }
  if (!file.exists(sub(".zip", "", zip_file_name, fixed = TRUE))) {
    unzip(zip_file_name)
  }
  
  Loans <- read.csv("LoanStats3a.csv", stringsAsFactors = FALSE, 
                    skip = 1, fileEncoding = "latin1")
  
  Loans$id <- NULL
  Loans$member_id <- NULL
  Loans$funded_amnt <- NULL
  Loans$funded_amnt_inv <- NULL
  Loans$int_rate <- as.numeric(sub("%", "", Loans$int_rate))
  
  LC <- Loans[,c("grade", "sub_grade")]
  Loans$grade <- NULL
  Loans$sub_grade <- NULL
  
  Loans$emp_title <- NULL
  Loans$emp_length <- gsub("(^[[:digit:]]+).*$", "\\1", Loans$emp_length)
  Loans$emp_length[Loans$emp_length == "n/a"] <- NA_character_
  Loans$emp_length[Loans$emp_length == "< 1 year"] <- "0"
  Loans$emp_length <- as.numeric(Loans$emp_length)
  
  Loans$issue_d <- NULL
  Loans$pymnt_plan <- NULL
  Loans$url <- NULL
  Loans$desc <- NULL
  Loans$title <- NULL
  
  Loans$zip_code <- gsub("xx$", "", Loans$zip_code)
  Loans$dti <- NULL
  Loans$dti_joint <- NULL
  Loans$earliest_cr_line <- as.numeric(gsub("^[[:alpha:]]+-", "", Loans$earliest_cr_line))
  Loans$mths_since_last_delinq <- NULL
  Loans$mths_since_last_record <- NULL
  Loans$initial_list_status <- NULL
  Loans$out_prncp <- NULL
  Loans$out_prncp_inv <- NULL
  Loans$total_pymnt <- NULL
  Loans$total_rec_int <- NULL
  Loans$total_pymnt_inv <- NULL
  Loans$total_rec_prncp <- NULL
  Loans$total_rec_late_fee <- NULL
  Loans$recoveries <- NULL
  Loans$collection_recovery_fee <- NULL
  Loans$last_pymnt_d <- NULL
  Loans$last_pymnt_amnt <- NULL
  Loans$next_pymnt_d <- NULL
  Loans$last_credit_pull_d <- NULL
  Loans$collections_12_mths_ex_med <- NULL
  Loans$mths_since_last_major_derog <- NULL
  Loans$policy_code <- NULL
  Loans$application_type <- NULL
  Loans$annual_inc_joint <- NULL
  Loans$verification_status_joint <- NULL
  Loans$revol_util <- NULL
  
  Loans$y <- Loans$loan_status %in% unique(Loans$loan_status)[c(2,4:7)]
  Loans$y[Loans$loan_status %in% unique(Loans$loan_status)[8:11]] <- NA
  Loans$loan_status <- NULL
  
  miss <- is.na(Loans$y) | is.na(Loans$emp_length)
  Loans <- Loans[!miss,]

  Loans$term <- as.factor(Loans$term)
  Loans$emp_length <- as.integer(Loans$emp_length)
  Loans$home_ownership <- as.factor(Loans$home_ownership)
  Loans$verification_status <- as.factor(Loans$verification_status)
  Loans$purpose <- as.factor(Loans$purpose)
  Loans$zip_code <- as.factor(Loans$zip_code)
  Loans$addr_state <- as.factor(Loans$addr_state)
  Loans$delinq_2yrs <- as.integer(Loans$delinq_2yrs)
  Loans$inq_last_6mths <- as.integer(Loans$inq_last_6mths)
  Loans$open_acc <- as.integer(Loans$open_acc)
  Loans$pub_rec <- as.integer(Loans$pub_rec)
  Loans$total_acc <- as.integer(Loans$total_acc)
  Loans$y <- as.integer(Loans$y)
  Loans$y <- factor(Loans$y, levels = 0:1, labels = c("NoProblem", "Problem"))
  Loans <- Loans[Loans$home_ownership %in% c("MORTGAGE", "OWN", "RENT"), ]
  return(Loans[,c("y", "loan_amnt", "term", "home_ownership", "annual_inc", 
                  "emp_length", "installment")])
}
