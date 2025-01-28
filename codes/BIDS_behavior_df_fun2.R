BIDS_behavior_df_fun2 = function(behavior_acronym, main_behavior_spreadsheet, 
                             behavior_dictionary, 
                             task_dictionary, sub_path_partial) {
  ### for testing start ###
  # behavior_acronym = task_name
  # main_behavior_spreadsheet = main_behavior_spreadsheet
  # behavior_dictionary = behavior_dictionary
  # task_dictionary = task_dictionary
  # sub_path_partial = OSF_sub_path_partial
  ### for testing start ###
  
  
  behavior_dictionary_current = behavior_dictionary[behavior_dictionary$Task == behavior_acronym, ]
  
  ############################### item extraction recursive function start #######################################
  item_extract_fun = function(input_i) {
    # for testing
    # input_i = var_i
    # for testing
    input_i_parse = str_split(input_i, "\\[|arm", simplify = TRUE)
    if (nrow(input_i_parse)*ncol(input_i_parse) == 1) {
      input_type = behavior_dictionary_current$FieldType[which(behavior_dictionary_current$Variable_FieldName == input_i_parse[1,1])]
      if (input_type == "calc") {
        input_i_cal = behavior_dictionary_current$Choices_Calculations_SliderLabels[which(behavior_dictionary_current$Variable_FieldName == input_i_parse[1,1])]
        item_extract_fun(input_i_cal)
      } else {
        items_BIDS_keep.env$items_BIDS_keep[nrow(items_BIDS_keep.env$items_BIDS_keep)+1,1] = input_i_parse
      }
    } else {
      input_i_parse = input_i_parse %>% str_match("\\s*(.*?)\\s*\\]") %>% na.omit %>% .[,2] %>% .[!startsWith(., "_")]
      for (k in 1:length(input_i_parse)) {
        input_k = input_i_parse[k]
        item_extract_fun(input_k)
      }
    }
  }
  ############################### item extraction recursive function end #######################################    
  
  # create empty dataframe to save items
  assign('items_BIDS_keep', data.frame(matrix(ncol = 1, nrow = 0)), envir=items_BIDS_keep.env)
  colnames(items_BIDS_keep.env$items_BIDS_keep) = "REDCap_name"
  
  ## check if acronym exist
  behavior_acronym_check_main = na.omit(main_behavior_spreadsheet$Acronym) == behavior_acronym
  behavior_acronym_check_scoring = na.omit(task_dictionary$Acronym) == behavior_acronym
  
  if(sum(behavior_acronym_check_main) != 1) {
    stop("acronym doesn't exist in the main spreadsheet dictionary")
  } else {
    if(sum(behavior_acronym_check_scoring) == 0) {
      stop("acronym doesn't exist in task dictionary")
    }
  }
  
  ## extract scale level description
  behavior_main_rnum = which(main_behavior_spreadsheet$Acronym == behavior_acronym)
  behavior_main_description = main_behavior_spreadsheet$Description[behavior_main_rnum]
  behavior_main_URL = main_behavior_spreadsheet$DOI[behavior_main_rnum]
  behavior_main_json_name = main_behavior_spreadsheet$json[behavior_main_rnum]
  # behavior_main_session = main_behavior_spreadsheet$`When is it administered`[behavior_main_rnum]
  
  ## extract items from all scoring var that has been tagged by behavior_acronym
  scoring_rnum = which(task_dictionary$Acronym == behavior_acronym)
  for (i in 1:length(scoring_rnum)) {
    ### for testing start ###
    # i = 2
    # print(i)
    ### for testing end ###
    var_i = task_dictionary$Name[scoring_rnum[i]]
    item_extract_fun(var_i)
  }
  
  # keep only unique items
  items_BIDS = items_BIDS_keep.env$items_BIDS_keep %>% unique()
  n_items = nrow(items_BIDS)
  
  ## put together JSON
  ### create empty df for first level nesting
  df_JSON = data.frame(matrix(ncol = n_items+1, nrow = 1))
  colnames(df_JSON) = c("MeasurementToolMetadata", items_BIDS)
  df_JSON$MeasurementToolMetadata = data.frame(Description = behavior_main_description,
                                               TermURL = behavior_main_URL)
  
  for (t in 2:(n_items+1)) {
    # find item levels
    ### for testing start ###
    # t = 3
    # print(t)
    ### for testing end ###
    item_var_name = items_BIDS[t-1,1]
    # item_var_description = behavior_dictionary_current[behavior_dictionary_current$Variable_FieldName == item_var_name, 
    #                                                    "FieldLabel"] %>% trimws()
    item_var_description = behavior_dictionary_current[behavior_dictionary_current$Variable_FieldName == item_var_name, 
                                                       "Content"] %>% trimws()
    
    # extract levels from the aggregated coding cell
    item_levels = 
      behavior_dictionary_current[behavior_dictionary_current$Variable_FieldName == item_var_name, 
                                  "Choices_Calculations_SliderLabels"]
    # if (item_levels == "" | is.na(item_levels)) {
    if (item_levels == "Units") {
      df_JSON[[item_var_name]] = data.frame(matrix(ncol = 2, nrow = 1))
      df_JSON[[item_var_name]]$Description = item_var_description
      df_JSON[[item_var_name]]$Unit = as.character(
        behavior_dictionary_current[behavior_dictionary_current$Variable_FieldName == item_var_name, "Unit"]
        )
    } else {
      ## split level description and number
      item_levels_split = str_split(item_levels, "\\|", simplify = TRUE)
      ## transform the vector into matrix, col 1 is code, col 2 is level description
      n_levels = length(item_levels_split)
      # item_levels_reshape <- matrix(item_levels_split, nrow = 2, byrow = FALSE)
      
      df_JSON[[item_var_name]] = data.frame(matrix(ncol = 2, nrow = 1))
      df_JSON[[item_var_name]]$Description = item_var_description
      df_JSON[[item_var_name]]$Levels = data.frame(matrix(ncol = n_levels, nrow = 1))
      
      for (k in 1:n_levels) {
        ### for testing start ###
        # k = 1
        ### for testing end ###
        # level_name = as.character(k)
        level_split = strsplit(item_levels_split[k], ",")[[1]]
        level_name = level_split[[1]] %>% trimws()
        df_JSON[[item_var_name]]$Levels[[level_name]] = paste(level_split[-1], collapse = ",") %>% trimws()
      }
    }
  }
  
  jsonData_raw = jsonlite::toJSON(df_JSON, pretty = TRUE)
  
  # remove the "[" and "]" at the beginning of saved json
  jsonData <- gsub("^\\[|\\]$", "", as.character(jsonData_raw))
  
  write(jsonData, paste(sub_path_partial, "_", behavior_main_json_name, sep = ""))

  
  # return(list(df_JSON, jsonData, n_items, items_BIDS))
  return(jsonData)
  }
  