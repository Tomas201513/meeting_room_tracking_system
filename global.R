# Meeting Room Booking System - Global Configuration
# This file is loaded before ui.R and server.R

# ===== Required Packages =====
library(shiny)
library(bslib)
library(toastui)
library(DT)
library(RSQLite)
library(shinyWidgets)
library(shinymanager)

# ===== Authentication Credentials =====
credentials <- data.frame(
  user = c("admin", "user"),
  password = c("admin", "user"),
  start = c(NA, NA),
  expire = c(NA, NA),
  admin = c(TRUE, FALSE),
  comment = c("Administrator", "Regular User"),
  stringsAsFactors = FALSE
)

# ===== Database Initialization =====
init_db <- function() {
  con <- dbConnect(SQLite(), "scheduler.db")
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  # rooms table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS rooms (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_name TEXT NOT NULL UNIQUE,
      accomodate INTEGER,
      description TEXT,
      status INTEGER DEFAULT 1
    );
  ")
  
  # meeting_schedule table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meeting_schedule (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_id INTEGER NOT NULL,
      start_datetime TEXT NOT NULL,
      end_datetime TEXT NOT NULL,
      organiser TEXT,
      meeting_purpose TEXT,
      FOREIGN KEY(room_id) REFERENCES rooms(id) ON DELETE CASCADE
    );
  ")
  
  dbDisconnect(con)
}

# Initialize database on app start
init_db()

# ===== Database Helper Functions =====
db_conn <- function() {
  con <- dbConnect(SQLite(), "scheduler.db")
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

# ---- Room Functions ----
get_rooms <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "rooms")) return(data.frame())
  dbReadTable(con, "rooms")
}

add_room <- function(room_name, accomodate, description, status) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con,
    "INSERT INTO rooms (room_name, accomodate, description, status)
     VALUES (?, ?, ?, ?)",
    params = list(room_name, accomodate, description, ifelse(isTRUE(status), 1, 0))
  )
}

delete_room <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM rooms WHERE id = ?", params = list(id))
}

# ---- Meeting Functions ----
get_meetings_joined <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "meeting_schedule")) return(data.frame())
  dbGetQuery(con, "
    SELECT m.id,
           r.room_name,
           m.start_datetime,
           m.end_datetime,
           m.organiser,
           m.meeting_purpose,
           m.room_id
    FROM meeting_schedule m
    JOIN rooms r ON r.id = m.room_id
    ORDER BY datetime(m.start_datetime) DESC
  ")
}

meeting_overlaps <- function(room_id, start_dt, end_dt) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  count <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM meeting_schedule
    WHERE room_id = ?
      AND datetime(?) < datetime(end_datetime)
      AND datetime(?) > datetime(start_datetime)
  ", params = list(room_id, start_dt, end_dt))$n
  count > 0
}

add_meeting <- function(room_id, start_dt, end_dt, organiser, purpose) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    INSERT INTO meeting_schedule (room_id, start_datetime, end_datetime, organiser, meeting_purpose)
    VALUES (?, ?, ?, ?, ?)
  ", params = list(room_id, start_dt, end_dt, organiser, purpose))
}

delete_meeting <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM meeting_schedule WHERE id = ?", params = list(id))
}
