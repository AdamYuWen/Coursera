library(dplyr)
library(tidyr)

subject <- function(ld_df, id) UseMethod("subject")
visit   <- function(subject, visit_num) UseMethod("visit")
room    <- function(visit, room_name) UseMethod("room")

# LongitudionalData Class #####################################################
make_LD <- function(data) {
  ld_df <- data %>% nest(-id)
  structure(ld_df, class = "LongitudinalData")
}

subject.LongitudinalData <- function(ld_df, id) {
  if (!id %in% ld_df[["id"]])
    stop("It is an invalid ID for subject.")
  index <- which(ld_df[["id"]] == id)
  structure(list(id = id, data = ld_df[["data"]][[index]]), class = "Subject")
}

print.LongitudinalData <- function(ld_df) {
  paste("There are", length(ld_df[["id"]]), "subjects.")
}

# Subject Class ###############################################################
visit.Subject <- function(subject_df, visit_id) {
  if (!visit_id %in% 0:2)
    stop("It is an invalid ID for visit.")
  data = subject_df[["data"]] %>%
    filter(visit == visit_id)
  structure(list(id = subject_df[["id"]], visit_id = visit_id, data = data), class = "Visit")
}

summary.Subject <- function(subject_df) {
  data <- subject_df[["data"]] %>%
    group_by(visit, room) %>%
    summarise(value = mean(value)) %>%
    spread(room, value)
  structure(list(id = subject_df[["id"]], data = data), class = "Summary")
}

print.Subject <- function(subject_df) {
  paste("Subject ID:", subject_df[["id"]])
}

# Visit Class #################################################################
room.Visit <- function(visit_df, room_id) {
  if (!room_id %in% visit_df[["data"]][["room"]])
    stop("It is an invalid ID for visit.")
  data <- visit_df[["data"]] %>%
    filter(room == room_id)
  structure(list(id = visit_df["id"], visit_id = visit_df[["visit_id"]], room = room_id, data = data), class = "Room")
}

print.Visit <- function(visit_df) {
  paste("Subject ID:", visit_df[["id"]], "and Visit ID:", visit_df[["visit_id"]])
}

# Room Class ##################################################################
summary.Room <- function(room_df) {
  data <- summary(room_df[["data"]][["value"]])
  structure(list(id = room_df[["id"]], data = data), class = "Summary")
}

print.Room <- function(room_df) {
  paste("Subject ID:", room_df[["id"]], "and Visit ID:", room_df[["visit_id"]], "and Room ID:", room_df[["room"]])
}

# Summary Class ###############################################################
print.Summary <- function(room_df) {
  paste("ID", room_df[[1]], print(room_df[[2]]))
}