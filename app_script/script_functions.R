# This script contains key functions for processing scripts


# This function reads in a script and returns an organized list
process_script <- function(script){
  # Get act & scene info
  act_breaks <- which(str_detect(script, "^ACT\\s\\d+\\s+SCENE\\s\\d+"))
  
  # Split script into scenes
  script_scenes <- vector(mode = "list", length = length(act_breaks))
  for (i in 1:length(act_breaks)){
    scene_start = act_breaks[i]
    scene_end = ifelse(i == length(act_breaks), length(script), act_breaks[i + 1] - 1)
    script_scenes[[i]] <- script[scene_start:scene_end]
  }
  
  # Get act and scene numbers
  act_num <- sapply(
    script_scenes,
    \(i) str_extract(i[1], "(?<=ACT\\s)\\d{1,2}(?=\\sSCENE\\s\\d{1,2})")
  )
  
  scene_num <- sapply(
    script_scenes,
    \(i) str_extract(i[1], "(?<=ACT\\s\\d{1,2}\\sSCENE\\s)\\d{1,2}")
  )
  
  # Initialize output
  processed_script <- list() #vector(mode = "list", length = length(unique(act_num)))
  
  for (act in unique(act_num)) {
    act_scene <- scene_num[which(act_num == act)]
    #processed_script[[paste0("Act ", act)]] <- vector(mode = "list", length = length(act_scene))
    
    for (scene in act_scene){
      # Get scene index
      scene_index <- which(act_num == act & scene_num == scene)
      
      # Pull scene lines
      scene_lines <- script_scenes[[scene_index]]
      
      setting =  str_extract(scene_lines[1], "(?<=ACT\\s\\d{1,2}\\sSCENE\\s\\d{1,2}\\.?\\s).+")
      
      # Get actor and character info
      if (str_detect(scene_lines[2], "\\#")) {
        
        # Convert actor and chracter info to df
        roles <- scene_lines[2] |> 
          str_split_1("\\s") |>
          as_tibble() |>
          separate(value, into = c("character", "actor"), sep = "\\#")
        
        # Pull info
        characters = roles |> pull(character)
        actors = roles |> pull (actor)
        
      } else {
        
        # If role labels are missing, assume line 2 is characters
        characters <- str_split_1(scene_lines[2], "\\s")
        actors <- NULL
      }
      
      characters <- gsub("\\_", " ", characters)
      
      # Process lines here
      clean_lines <- scene_lines[-c(1:2)]
      #line_breaks <- which(clean_lines %in% characters)
      character_speak <- which(str_detect(clean_lines, "^[[[:upper:]]\\s]+$"))
      
      if (!(character_speak[1] == 1)) stop(paste0("Warning: script alignment is incorrect. Character line is: ", clean_lines[1]))
      
      lines <- vector(mode = "numeric", length = length(character_speak))
      for (j in 1:length(character_speak)) {
        line_start = character_speak[j] + 1
        line_end = ifelse(j == length(character_speak), length(clean_lines), character_speak[j + 1] - 1)
        lines[j] <- clean_lines[line_start:line_end] |> 
          paste0(collapse = " ") 
          
      }
      
      script <- tibble(
        character = clean_lines[character_speak],
        line = lines
      )
      
      
      # Add info to list
      processed_script[[paste0("Act ", act)]][[paste0("Scene ", scene)]] <- list(
        "Setting" = setting,
        "Characters" = characters,
        "Actors" = actors,
        "Script" = script
        )
    }
  }
  
  return(processed_script)
}

# This returns the ending
get_cue_line <- function(line) {
  word_count = str_count(line, "\\w+")
  
  if (word_count > 5) {
    cue <- paste0("...", paste(word(line, -6:-1), collapse = " "))
  } else cue <- line
  
  return(cue)
}

# This function takes in a processed scene and a character, and returns only that 
# character's lines with cues
call_lines_and_cues <- function(lines, character) {
  # Add scene start to cue first line
  lines <- rbind(
    tibble(character = "SCENE START", line = " "), 
    lines
  )
  
  speaking_lines <- which(str_detect(lines$character, character))
  
  cue_df <- tibble(
    cue_character = lines$character[speaking_lines - 1],
    cue_line = lines$line[speaking_lines - 1],
    character = lines$character[speaking_lines],
    line = lines$line[speaking_lines]
  ) 
  
  cue_df <- cue_df|> 
    mutate(cue_line = map(cue_line, get_cue_line)) |> 
    unnest(cue_line)
  return(cue_df)
}


# This function compares a user entered line to the source line
# Note: add a "?" exception
check_line <- function(original_line, entered_line) {
  original_clean <- original_line |> 
    gsub("[[:punct:]]", "", x = _) |> # May want to change this to a full alphabetic-only
    str_to_lower()
  
  entered_clean <- entered_line |> 
    gsub("[[:punct:]]", "", x = _) |> 
    str_to_lower()

  # Check for perfect line
  if (original_clean == entered_clean) {
    
    print("Good Job!") #return(NULL)
    
  } else {
    
    # Initialize notes
    notes <- tibble(problem = c(), line = c())
    
    # Split lines into words
    ref_words <- str_split_1(original_line, " ")
    original_words <- str_split_1(original_clean, " ")
    entered_words <- str_split_1(entered_clean, " ")
    
    # Indels
    inserted_words <- setdiff(entered_words, original_words)
    deleted_words <- setdiff(original_words, entered_words)
    
    word_mod = 0
    
    for (word in inserted_words) {
      line_markup <- str_replace(entered_words, word, paste0("<b>", word, "</b>"))
      
      for (i in which(entered_words == word)) {
        snippet_start = max(i - 2, 1)
        snippet_end = min(i + 2, length(entered_words))
        
        line_note = paste(line_markup[snippet_start:snippet_end], collapse = " ")
        notes <- rbind(notes,
                       tibble(problem = "Inserted word",line_note))
        word_mod = word_mod - 1
      }
      
    }
    for (word in deleted_words) {
      line_markup <- str_replace(original_words, word, paste0("<b>", word, "</b>"))
      
      for (i in which(original_words == word)) {
        snippet_start = max(i - 2, 1)
        snippet_end = min(i + 2, length(original_words))
        
        line_note = paste(line_markup[snippet_start:snippet_end], collapse = " ")
        notes <- rbind(notes,
                       tibble(problem = "Deleted word",line_note))
        word_mod = word_mod - 1
      }
      
    }    
    
    # Word order
    if ((length(entered_words) == length(original_words)) & setequal(entered_words, original_words)) {
      notes <- rbind(notes,
                     tibble(problem = "Word Order", original_line))
    }
    
  }
  return(notes)
}
