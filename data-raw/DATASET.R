# References:
#   https://r-pkgs.org/
#   https://kjhealy.github.io/covdata/
#
# This script will be for reading and cleaning all of the data from the Vox Machina Campaign of CritRoleStats:
#    https://www.critrolestats.com/stats-vm

library(tidyverse)
library(gsheet) # what a godsend lets you download google sheet using url https://github.com/maxconway/gsheet
library(naniar) # needed to replace "Unknown" with NA

# Check for data/ directory
ifelse(!dir.exists(here("data-raw/data")),
       dir.create(file.path("data-raw/data")),
       FALSE)

# DATA-GETTING FUNCTIONS

## Crits and Rolls

### All Rolls
#### This dataset is a google sheet listing all of the rolls by every PC (Player Character) in the Vox Machina Campaign.
#### It is split into separate sheets for each episode.
#### Oh god even between the sheets, the columns are different, though not by too much.
####    Will I have to go through each of the 115 episodes manually to see which columns to read?
####    Sometimes the column names are different too even though it's the same value... this is gross
#### Also, unknown values are written as "Unknown"
#### Natural 1's and 20's are written as Nat1 and Nat20, which we'll have to convert as well

#### For the time being, let's have our consolidated column names be:
##### ep (int): episode number
##### time (datetime): real time in episode of roll
##### char (string): name of character who rolled
##### type (string): type of roll being made (persuasion, attack, etc.)
##### total (int): total value of roll after adding any modifiers
##### nat (int): "natural" value of roll before adding modifiers
##### damage (int): amount of damage dealt if an attack roll (damage currently a string, will have to scrape and convert)
##### notes (string): memos written by the dataset creator

all_rolls <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1OEg29XbL_YpO0m5JrLQpOPYTnxVsIg8iP67EYUrtRJg/edit')
#### currently, this just gets the first sheet. But we'll start with that and do some cleaning
#### the columns we want are:
##### ep -> Episode (1)
##### time -> Time (2)
##### char -> Character (3)
##### type -> Type of Roll (4)
##### total -> Total Value (5)
##### nat -> Natural Value (6)
##### damage -> Damage Dealt (8), skip Crit? (7) because we're making our own column for that
##### notes -> Notes (9)
all_rolls <- all_rolls %>% rename(ep = Episode, # rename columns to be not horrible
                                  time = Time,
                                  char = Character,
                                  type = 'Type of Roll',
                                  total = 'Total Value',
                                  nat = "Natural Value",
                                  damage = "Damage Dealt",
                                  notes = Notes)

#### Now to clean. For starters, let's replace all "Unknown" with NA
all_rolls <- all_rolls %>% replace_with_na_all(condition = ~.x == "Unknown")

#### Now for the more complex problem of crits - natural 1's (Nat1) and natural 20s (Nat20).
#### Not all of them have recorded total values, since crits automatically fail/succeed.
#### But it'd be nice to note crits when they happen, so we'll use a binary crit variable
##### Note this is noted only for attack rolls in the original file, but any crit rolled sounds more interesting,
#####  so we'll write a new column.
all_rolls$crit <- if_else((all_rolls$nat == 1 | all_rolls$nat == 20), 1, 0, 0) # 1 if crit, 0 otherwise

#### Finally, we change the Nat1 and Nat20 in the total column to just 1 and 20
all_rolls[all_rolls$total == "Nat1", 'total'] <- 1
all_rolls[all_rolls$total == "Nat20", 'total'] <- 20

