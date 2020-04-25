# References:
#   https://r-pkgs.org/
#   https://kjhealy.github.io/covdata/
#
# This script will be for reading and cleaning all of the data from the Vox Machina Campaign of CritRoleStats:
#    https://www.critrolestats.com/stats-vm

library(gsheet) # what a godsend lets you download google sheet using url https://github.com/maxconway/gsheet

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
##### damage -> Damage Dealt (8), skip 7 Crit? because that's a useless varialbe
##### notes -> Notes (9)
all_rolls <- all_rolls %>% select(Episode, # select columns by name in case order changes
                     Time,
                     Character,
                     'Type of Roll', # need quotes for multiple word column names
                     'Total Value',
                     'Natural Value',
                     'Damage Dealt',
                     Notes) %>% rename(ep = Episode, # rename columns to be not horrible
                                  time = Time,
                                  char = Character,
                                  type = 'Type of Roll',
                                  total = 'Total Value',
                                  nat = "Natural Value",
                                  damage = "Damage Dealt",
                                  notes = Notes)

