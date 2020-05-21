# References:
#   https://r-pkgs.org/
#   https://kjhealy.github.io/covdata/
#
# This script will be for reading and cleaning all of the data from the Vox Machina Campaign of CritRoleStats:
#    https://www.critrolestats.com/stats-vm

library(tidyverse)
library(here) # needed to check if directory exists
library(googlesheets4) # lets you download multiple worksheets from google sheets https://cran.r-project.org/web/packages/googlesheets4/googlesheets4.pdf
library(stringr) # needed for word() function to separate character strings
library(rvest) # needed to scrape html tables
library(usethis) # needed for writing packages

# Check for data/ directory
ifelse(!dir.exists(here("data-raw/data")),
       dir.create(file.path("data-raw/data")),
       FALSE)

# since the google sheets on the site are public, no need to force user to log in to Google
sheets_deauth()

#### DATA-GETTING FUNCTIONS ####

#### Crits and Rolls ####
# We'll just take the All Rolls, since that's the most interesting data (encompassing PC Nat1s and Nat 20s)

#### All Rolls https://docs.google.com/spreadsheets/d/1OEg29XbL_YpO0m5JrLQpOPYTnxVsIg8iP67EYUrtRJg/edit?usp=sharing ####
# This dataset is a google sheet listing all of the rolls by every PC (Player Character) in the Vox Machina Campaign.
# It is split into separate sheets for each episode.
# Oh god even between the sheets, the columns are different, though not by too much.
#    Will I have to go through each of the 115 episodes manually to see which columns to read?
#    Sometimes the column names are different too even though it's the same value... this is gross
# Also, unknown values are written as "Unknown"
# Natural 1's and 20's are written as Nat1 and Nat20, which we'll have to convert as well

# For the time being, let's have our consolidated column names be:
# ep (dbl): episode number
# time (dbl): time in seconds ellapsed in episode until roll
# char (chr): name of character who rolled
# type (chr): type of roll being made (persuasion, attack, etc.)
# total (dbl): total value of roll after adding any modifiers
# nat (dbl): "natural" value of roll before adding modifiers
# damage (dbl): amount of damage dealt if an attack roll (damage currently a string, will have to scrape and convert)
# notes (chr): memos written by the dataset creator
# crit (dbl): binary noting whether or not roll was a crit (nat 1 or nat 20)

# get metadata for the google sheet so we can loop through every worksheet
all_rolls_metadata <- sheets_get("https://docs.google.com/spreadsheets/d/1OEg29XbL_YpO0m5JrLQpOPYTnxVsIg8iP67EYUrtRJg/edit")

tibble_list_rolls <- NULL
# takes a longass time, but reads all the sheets (let's never do this again)
for (x in 1:dim(all_rolls_metadata$sheets['name'])[1]) {
  # We start by just making a list of tibbles - much easier to bind them all at the end rather than as we go
  tibble_list_rolls[[x]] <- sheets_read("https://docs.google.com/spreadsheets/d/1OEg29XbL_YpO0m5JrLQpOPYTnxVsIg8iP67EYUrtRJg/edit",
                                 sheet = x,
                                 col_types = "c", # need to read all cols as chars to avoid miscasting
                                 na = c("", "Unknown") # deal with annoying non-standard NA values
                                 )
  Sys.sleep(1.5) # sleep 1.5s between each sheet read so Google doesn't get mad at us and throw Client error: (429) RESOURCE_EXHAUSTED
}
# bind them tibbles
all_rolls <- bind_rows(tibble_list_rolls)

# the columns we want are:
# ep -> Episode (1)
# time -> Time (2)
# char -> Character (3)
# type -> Type of Roll (4)
# total -> Total Value (5)
# nat -> Natural Value (6)
# damage -> Damage Dealt (8), skip Crit? (7) because we're making our own column for that
# notes -> Notes (9)
all_rolls <- all_rolls %>%
  select(Episode, # select columns by name in case order changes
         Time,
         Character,
         'Type of Roll', # need quotes for multiple word column names
         'Total Value',
         'Natural Value',
         'Damage Dealt',
         Notes) %>%
  rename(ep = Episode, # rename columns to be not horrible
         time = Time,
         char = Character,
         type = 'Type of Roll',
         total = 'Total Value',
         nat = "Natural Value",
         damage = "Damage Dealt",
         notes = Notes)

# Now to clean the data

# Looks like there was an empty sheet yielding several all NA rows - let's remove those
all_rolls <- filter_all(all_rolls, any_vars(!is.na(.)))

# Not all ep are just numbers - some have been split into parts (i.e. 31 p1) - let's just keep the integer value, since we don't care about these splits, and make it numeric
all_rolls$ep <- as.numeric(word(all_rolls$ep, 1))

# We have the complex problem of crits - natural 1's (Nat1) and natural 20s (Nat20).
# Not all of them have recorded total values, since crits (usually) automatically fail/succeed.
# But it'd be nice to note crits when they happen, so we'll use a binary crit variable
# > This was noted only for attack rolls in the original file, but any crit rolled sounds more interesting, so we'll write a new column.
all_rolls$crit <- if_else((all_rolls$nat == 1 | all_rolls$nat == 20), 1, 0, 0) # 1 if crit, 0 otherwise

# Change the Nat1 and Nat20 in the total column to just 1 and 20
# Need to check if na to avoid errors being thrown
# TODO: but maybe it'd be better to determine modifiers from previous rolls and add them? Sounds... hard
all_rolls[!is.na(all_rolls$total) & all_rolls$total == "Nat1", 'total'] <- "1"
all_rolls[!is.na(all_rolls$total) & all_rolls$total == "Nat20", 'total'] <- "20"

# Change damage to numeric; no need for type or target
# damage follows format of "<amount> <damage type> to <character>", so should just be able to take first word as the value
# Warning NAs introduced by coercion, but maybe that's ok. My guess is that happens if damage just has a memo without a value, so it should become NA anyway
all_rolls$damage <- as.numeric(word(all_rolls$damage, 1))

# Change column types for total and nat to numeric
# Warning NAs introduced by coercion, but maybe that's ok. My guess is that happens if damage just has a memo without a value, so it should become NA anyway
all_rolls[, c('total', 'nat')] <- sapply(all_rolls[, c('total', 'nat')], as.numeric)

# Change column time for time to time
# TODO: have a datetime now, but sets date as current date which is technically incorrect... maybe don't do this?
all_rolls$time <- as.POSIXct(all_rolls$time, format = "%H:%M:%S")

# make list of episode names for the Vox Machina campaign ("ep1", "ep2", ... "ep115")
episodes <- unique(all_rolls$ep)
episodes <- paste("ep", 1:length(vm_ep_list), sep = "")

#### Rankings ####
#### Total Kills https://www.critrolestats.com/vm-kills ####
# this dataset is just an html table, so no need to go through google sheets

# I think it's best to structure this tibble as follows:
# Col1: char (name of character)
# Col2: total (number of total kills in campaign)
# Col3 ~: ep1 ~ (number of kills in each episode (1 onward))
## e.g.
## char       total   ep1   ep2   ep3   ep4   ep5   ~
## Keyleth    116     0     0     6.5   0     1
## ~

# table is structured by character:
# > first line in cell is character name (i.e. "Keyleth") + ":" + kill count (i.e. "34")
# > remaining lines per cell are episode number (i.e. "Ep1") + ":" + kill count for that episode (i.e. "2")
# > note 0.5 kill represents a shared kill between two characters

# Had to read from embedded google docs, hence URL change
total_kills_list <- read_html("https://docs.google.com/document/d/1UB9QMA0sMmp5BwzZ5flQLbZPurcu6oGgM4s74O8NBLY/pub?embedded=true") %>%
  html_nodes("table") %>%
  html_table()

# change total_kills from list of 1 to list of character strings - each item is for a different player character
total_kills_list <- paste(unlist(total_kills_list))

## create tibble for each character in tibble list
## char name, total kills, ep1 kills, ep2 kills, etc
## then bind each as a row, change column names to char, total, ep1, ep2, etc.
tibble_list_kills <- NULL

for (x in 2:length(total_kills_list)) {

  # Currently, list is formatted without space between episode values
  # > i.e. "Grog: 64.5Ep1: 2Ep2: 1Ep4: 1Ep6: 2Ep15: 1Ep18: 3Ep19: 1...
  # So let's separate by every "Ep
  char_kills_list <- str_split_fixed(total_kills_list[x], pattern = "Ep", n = length(vm_ep_list))
  # and now we have a list of strings with the following format:
  # > item 1: "<character name>: <total kills>"
  # > item 2 onward: "<ep number>: <ep kills>"

  # # Start with the easier components to separate out: character name and total kills
  tibble_list_kills[[x]] <- tibble(
    char = word(char_kills_list[1], 1, sep = ":"), # gets character name and removes ":"
    total = as.numeric(word(char_kills_list[1], 2, sep = " "))) # gets total number of kills after char name

  # add empty columns for episode kills
  tibble_list_kills[[x]][,episodes] <- NA

  # get kills per individual episode
  for(i in 2:length(char_kills_list)) {

    ep <- as.numeric(word(char_kills_list[i], 1, sep = ":")) # episode number

    if(!is.na(ep)) { # check that ep is not NA (in other words, check that we have not exceeded number of episodes in campaign, since not all eps have kills)
      # whitespace was a "non-breaking space," hence the need for str_trim from stringr
      ep_kills <- as.numeric(str_trim(word(char_kills_list[i], 2, sep = ":"))) # episode kills

      tibble_list_kills[[x]][,paste("ep", ep, sep = "")] <- ep_kills
    }
  }
}

# bind them tibbles
all_kills <- bind_rows(tibble_list_kills)


#### Damage Dealt https://docs.google.com/spreadsheets/d/152k1UMyTCtwGcTJt_SvYenXvdYzGZJGvLmLpYLj9yYI/edit#gid=0 ####
# back to google sheets

# these sheets are formatted differently:
# > The first sheet is total and average damage dealt for each player, as well as individual damage amounts
# Player  Keyleth   Pike    ...
# Average 23.8      21.71   ...
# Total   6126      1390    ...
#         4         16      ...
#         ...       ...
# > Then, each sheet after that lists damage done each episode, laid out in the same way as above (with total and avg of that episode)
# >> there is also a total for the episode in the bottom left corner, so be sure to ignore that
# > Note: every episode has its own sheet, even if no damage has been dealt

# get metadata for the google sheet so we can loop through every worksheet
all_damage_dealt_metadata <- sheets_get("https://docs.google.com/spreadsheets/d/152k1UMyTCtwGcTJt_SvYenXvdYzGZJGvLmLpYLj9yYI/edit")

tibble_list_damage_dealt <- NULL

for (x in 1:dim(all_damage_dealt_metadata$sheets['name'])[1]) {
  # We start by just making a list of tibbles - much easier to bind them all at the end rather than as we go
  tibble_list_damage_dealt[[x]] <- sheets_read("https://docs.google.com/spreadsheets/d/152k1UMyTCtwGcTJt_SvYenXvdYzGZJGvLmLpYLj9yYI/edit",
                                        sheet = x,
                                        col_types = "c", # need to read all cols as chars to avoid miscasting
                                        na = c("", "Unknown", "-"), # deal with annoying non-standard NA values
                                        range = "B1:AB" # skip first column, which just includes labels we will ignore
                                        )
  Sys.sleep(1.5) # sleep 1.5s between each sheet read so Google doesn't get mad at us and throw Client error: (429) RESOURCE_EXHAUSTED
}

# Let's go with the following layout for the final table, where each ep# indicates amount of damage dealt that episode by that character
# char      avg     total     ep1   ep2   ep3   ...
# Keyleth   23.8    6126      4     53    0     ...

# Let's start with the info from sheet 1 - that will get us our first three columns (char, total, avg)
# TODO: currently have some empty columns that list names as "...9" and "...14". Difficulty reading ellipsis, so maybe we can delete them later
charnames <- names(tibble_list_damage_dealt[[1]])
avg <- as.numeric(tibble_list_damage_dealt[[1]][1,])
total <- as.numeric(tibble_list_damage_dealt[[1]][2,])

all_damage_dealt <- tibble(
  char = charnames,
  avg = avg,
  total = total
)

# add empty columns for episode damage
# we assign it as 0 to avoid assignment type errors later
all_damage_dealt[, episodes] <- 0

# Next, let's look at total damage per episode per character
# iterate through the sheets for each episode
for (i in 2:length(tibble_list_damage_dealt)) {
  # iterate through each character for each episode
  for (char in names(tibble_list_damage_dealt[[i]])) {
    ep <- episodes[i - 1]
    all_damage_dealt[all_damage_dealt$char == char, ep] <- as.numeric(tibble_list_damage_dealt[[i]][2, char])
  }
}

# Now to clean. We have some empty rows (from the empty columns in the first sheet), so let's remove those
# > These empty rows have NA as a value for avg
# Let's also replace all remaining NA values with 0
# > These NA values came from '-' cells in the dataset, which indicated that a character did not deal damage that episode
all_damage_dealt <- all_damage_dealt %>%
  drop_na('avg') %>%
  mutate_each_(funs(replace(., which(is.na(.)), 0)), names(all_damage_dealt))

#### Damage Taken https://docs.google.com/spreadsheets/d/1yqRaiwoEuUocZkj2oySmIgmoEpIr6Ap18qNSe_F6G6o/edit#gid=0 ####


#### Spells Cast https://docs.google.com/spreadsheets/d/1Y7FB0rEUX8Ik0MfGUtsdItoFWcvlgpGVcSJ0l9-dGDw/edit#gid=1159219189 ####

#### Time and Attendance ####

#### WRITE DATA ####
usethis::use_data(all_rolls, overwrite = TRUE, compress = "xz")
usethis::use_data(all_kills, overwrite = TRUE, compress = "xz")
usethis::use_data(all_damage_dealt, overwrite = TRUE, compress = "xz")

