library(dplyr)
library(httr)
library(jsonlite)

source("apikeys.R")

# this is the base uri used to get the needed data from the propublica api
base_uri <- "https://api.propublica.org/congress/v1/"

# takes in a string "topic" and searches for the 20 most recent bills that are about that topic, returns this as a data frame
get_bill_data <- function(topic) {
  resource <- "bills/search.json"
  query_params <- list(query = topic)
  
  # sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
  response <- GET(paste0(base_uri, resource), query = query_params, add_headers('X-API-Key' = propublica_key))
  
  # parses the data into JSON format
  parsed_data <- content(response, "text") %>% 
    fromJSON()
  
  # makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
  results <- parsed_data$results$bills[[1]] %>% 
    flatten() %>% 
    mutate("sponsor_info" = paste0("Main sponsor: ", sponsor_name, ", From: ", 
                                          sponsor_state, ", Party: ", sponsor_party, ". Democratic cosponsors: ", cosponsors_by_party.D,
                                          ", Republican cosponsors: ", cosponsors_by_party.R, "."))
}

# obtains all members in the house of representatives representing the state of washington
resource_house <- "115/house/members.json"
# sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
response_house <- GET(paste0(base_uri, resource_house), add_headers('X-API-Key' = propublica_key))
# parses the data into JSON format
parsed_data_house <- content(response_house, "text") %>%
  fromJSON()
# makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
results_house <- parsed_data_house$results$members[[1]] %>%
  flatten() %>%
  filter(state == "WA")

# obtains all members in senate representing the state of washington
resource_senate <- "115/senate/members.json"
# sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
response_senate <- GET(paste0(base_uri, resource_senate), add_headers('X-API-Key' = propublica_key))
# parses the data into JSON format
parsed_data_senate <- content(response_senate, "text") %>%
  fromJSON()
# makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
results_senate <- parsed_data_senate$results$members[[1]] %>%
  flatten() %>%
  filter(state == "WA")

# takes in a member id and returns a data frame with 1 row represnting all data on that representative
get_legislature_data <- function(member_id) {
  resource <- paste0("members/", member_id, ".json")
  
  # sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
  response <- GET(paste0(base_uri, resource), add_headers('X-API-Key' = propublica_key))
  
  # parses the data into JSON format
  parsed <- content(response, "text") %>%
    fromJSON()
  
  # makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
  results <- parsed$results %>% 
    flatten()
}

# takes in a member id and returns all bills that member has sponsored or cosponsored in a data frame
get_bills_by_member <- function(member_id) {
  resource <- paste0("members/", member_id, "/bills/introduced.json")
  
  # sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
  response <- GET(paste0(base_uri, resource), add_headers('X-API-Key' = propublica_key))
  
  # parses the data into JSON format
  parsed <- content(response, "text") %>%
    fromJSON()
  
  # makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
  result <- parsed$results$bills[[1]] %>% 
    flatten() %>% 
    select("Bill ID" = bill_id, "Title" = title)
}

# returns the 20 most recent votes that have happened in congress in a data frame
recent_vote_data_all <- function(){
  resource <- paste0("house/votes/recent.json")
  
  # sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
  response <- GET(paste0(base_uri, resource), add_headers('X-API-Key' = propublica_key))
  
  # parses the data into JSON format
  parsed <- content(response, "text") %>%
    fromJSON()
  
  # makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
  result <- parsed$results$votes %>% 
    flatten() %>% 
    select("bill_id" = bill.bill_id, "d_majority" = democratic.majority_position, "r_majority" = republican.majority_position)
}

# takes in a member id and returns that member's 20 most recent votes in congress in a data frame
recent_vote_member <- function(member_id) {
  resource <- paste0("members/", member_id, "/votes.json")
  
  # sends a get request to the appropriate endpoint of the api based on the resource proveded, also includes the api key
  response <- GET(paste0(base_uri, resource), add_headers('X-API-Key' = propublica_key))
  
  # parses the data into JSON format
  parsed <- content(response, "text") %>%
    fromJSON()
  
  # makes the data into a dataframe, then flattens this data frame, and includes the appropriate columns
  result <- parsed$results$votes[[1]] %>% 
    flatten() %>% 
    select("bill_id" = bill.bill_id, "member_position" = position)
}

# takes in a member_id and returns the percentage of votes that the given member has voted with the opposing party based on the 20
# most recnt votes.
vote_with_opp_majority <- function(member_id) {
  all_votes <- recent_vote_data_all()
  member_votes <- recent_vote_member(member_id)
  combined_votes <- left_join(all_votes, member_votes)
  # finds the party of the given member
  member_party <- get_legislature_data(member_id)$current_party
  votes_with_opp <- combined_votes
  
  # uses the given member's party to extract all votes where the member voted with the opposint party
  if(member_party == 'D') {
    votes_with_opp <- combined_votes %>% 
      filter(member_position == r_majority)
  }
  if(member_party == 'R') {
    votes_with_opp <- combined_votes %>% 
      filter(member_position == d_majority)
  }
  if(member_party == 'I') {
    votes_with_opp <- combined_votes %>% 
      filter(member_position == d_majority | member_position == r_majority)
  }
  
  # takes the number of rows from where the member voted with the opposite party and divides it by the total number of votes (20)
  # multiplies this number by 100 and rounds to obtain the percentage
  percentage <- round((nrow(votes_with_opp) / nrow(combined_votes)) * 100)
}

