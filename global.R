# Resource Scheduler - Global Configuration & Database Functions

library(shiny)
library(bslib)
library(toastui)
library(DT)
library(RSQLite)
library(shinyWidgets)
library(shinymanager)
library(colourpicker)
library(shinyjs)
library(sodium)

# ---------------------------------------------------------------------------
# Database Initialization
# ---------------------------------------------------------------------------

# Creates database tables if they don't exist
init_db <- function() {
  con <- dbConnect(SQLite(), "scheduler.db")
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  
  # Departments table for authentication

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS departments (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      department_name TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      is_admin INTEGER DEFAULT 0,
      room_permission TEXT DEFAULT 'read',
      car_permission TEXT DEFAULT 'read',
      status INTEGER DEFAULT 1,
      created_at TEXT DEFAULT CURRENT_TIMESTAMP
    );
  ")
  
  # Create default admin if no departments exist
  admin_exists <- dbGetQuery(con, "SELECT COUNT(*) as n FROM departments WHERE is_admin = 1")$n
  if (admin_exists == 0) {
    default_password <- password_store("admin123")
    dbExecute(con, "
      INSERT INTO departments (department_name, password_hash, is_admin, room_permission, car_permission, status)
      VALUES (?, ?, 1, 'create', 'create', 1)
    ", params = list("Admin", default_password))
  }
  
  # Rooms table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS rooms (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_name TEXT NOT NULL UNIQUE,
      accomodate INTEGER,
      description TEXT,
      color TEXT DEFAULT '#4285F4',
      status INTEGER DEFAULT 1
    );
  ")
  
  # Meeting schedule table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS meeting_schedule (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      room_id INTEGER NOT NULL,
      start_datetime TEXT NOT NULL,
      end_datetime TEXT NOT NULL,
      organiser TEXT,
      meeting_purpose TEXT,
      created_by_dept INTEGER,
      FOREIGN KEY(room_id) REFERENCES rooms(id) ON DELETE CASCADE,
      FOREIGN KEY(created_by_dept) REFERENCES departments(id) ON DELETE SET NULL
    );
  ")
  
  # Car table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS car (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      car_name TEXT NOT NULL,
      car_model TEXT NOT NULL,
      car_plate_no TEXT NOT NULL UNIQUE,
      accomodate TEXT NOT NULL,
      description TEXT,
      color TEXT DEFAULT '#0F9D58',
      status INTEGER DEFAULT 1
    );
  ")
  
  # Car schedule table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS car_schedule (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      car_plate_no TEXT NOT NULL,
      start_datetime TEXT NOT NULL,
      end_datetime TEXT NOT NULL,
      passanger_name TEXT NOT NULL,
      department TEXT NOT NULL,
      trip_purpose TEXT NOT NULL,
      no_of_passangers INTEGER NOT NULL,
      share INTEGER DEFAULT 1,
      pickup_location TEXT NOT NULL,
      dropoff_location TEXT NOT NULL,
      comments TEXT,
      created_by_dept INTEGER,
      FOREIGN KEY(car_plate_no) REFERENCES car(car_plate_no) ON DELETE CASCADE,
      FOREIGN KEY(created_by_dept) REFERENCES departments(id) ON DELETE SET NULL
    );
  ")
  
  # Migration: Add created_by_dept column to existing tables if not present
  meeting_cols <- dbListFields(con, "meeting_schedule")
  if (!"created_by_dept" %in% meeting_cols) {
    dbExecute(con, "ALTER TABLE meeting_schedule ADD COLUMN created_by_dept INTEGER")
  }
  
  car_schedule_cols <- dbListFields(con, "car_schedule")
  if (!"created_by_dept" %in% car_schedule_cols) {
    dbExecute(con, "ALTER TABLE car_schedule ADD COLUMN created_by_dept INTEGER")
  }
  
  dbDisconnect(con)
}

init_db()

# Returns a new database connection with foreign keys enabled
db_conn <- function() {
  con <- dbConnect(SQLite(), "scheduler.db")
  dbExecute(con, "PRAGMA foreign_keys = ON;")
  con
}

# ---------------------------------------------------------------------------
# Department Functions (Authentication)
# ---------------------------------------------------------------------------

# Hash a password using sodium
hash_password <- function(password) {
  password_store(password)
}

# Verify a password against stored hash
verify_password <- function(password, hash) {
  tryCatch({
    password_verify(hash, password)
  }, error = function(e) {
    FALSE
  })
}

# Get department by name
get_department_by_name <- function(dept_name) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "
    SELECT * FROM departments WHERE department_name = ? AND status = 1
  ", params = list(dept_name))
}

# Get all departments
get_departments <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "departments")) return(data.frame())
  dbReadTable(con, "departments")
}

# Add a new department
add_department <- function(department_name, password, is_admin = FALSE, 
                           room_permission = "read", car_permission = "read", status = TRUE) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  password_hash <- hash_password(password)
  dbExecute(con, "
    INSERT INTO departments (department_name, password_hash, is_admin, room_permission, car_permission, status)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(
    department_name, 
    password_hash, 
    ifelse(isTRUE(is_admin), 1, 0),
    room_permission,
    car_permission,
    ifelse(isTRUE(status), 1, 0)
  ))
}

# Update department
update_department <- function(id, department_name = NULL, password = NULL, is_admin = NULL,
                              room_permission = NULL, car_permission = NULL, status = NULL) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  
  updates <- c()
  params <- list()
  
  if (!is.null(department_name)) {
    updates <- c(updates, "department_name = ?")
    params <- c(params, list(department_name))
  }
  if (!is.null(password) && password != "") {
    updates <- c(updates, "password_hash = ?")
    params <- c(params, list(hash_password(password)))
  }
  if (!is.null(is_admin)) {
    updates <- c(updates, "is_admin = ?")
    params <- c(params, list(ifelse(isTRUE(is_admin), 1, 0)))
  }
  if (!is.null(room_permission)) {
    updates <- c(updates, "room_permission = ?")
    params <- c(params, list(room_permission))
  }
  if (!is.null(car_permission)) {
    updates <- c(updates, "car_permission = ?")
    params <- c(params, list(car_permission))
  }
  if (!is.null(status)) {
    updates <- c(updates, "status = ?")
    params <- c(params, list(ifelse(isTRUE(status), 1, 0)))
  }
  
  if (length(updates) > 0) {
    params <- c(params, list(id))
    query <- paste0("UPDATE departments SET ", paste(updates, collapse = ", "), " WHERE id = ?")
    dbExecute(con, query, params = params)
  }
}

# Delete department by ID
delete_department <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM departments WHERE id = ?", params = list(id))
}

# Custom credential checker for shinymanager
check_credentials_db <- function() {
  function(user, password) {
    dept <- get_department_by_name(user)
    if (is.null(dept) || nrow(dept) == 0) {
      return(list(result = FALSE))
    }
    if (!verify_password(password, dept$password_hash)) {
      return(list(result = FALSE))
    }
    list(
      result = TRUE,
      user_info = list(
        user = dept$department_name,
        dept_id = dept$id,
        admin = dept$is_admin == 1,
        room_permission = dept$room_permission,
        car_permission = dept$car_permission
      )
    )
  }
}

# ---------------------------------------------------------------------------
# Room Functions
# ---------------------------------------------------------------------------

# Returns all rooms from database
get_rooms <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "rooms")) return(data.frame())
  dbReadTable(con, "rooms")
}

# Inserts a new room
add_room <- function(room_name, accomodate, description, color, status) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "INSERT INTO rooms (room_name, accomodate, description, color, status)
     VALUES (?, ?, ?, ?, ?)",
            params = list(room_name, accomodate, description, color, ifelse(isTRUE(status), 1, 0))
  )
}

# Deletes a room by ID (cascades to meetings)
delete_room <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM rooms WHERE id = ?", params = list(id))
}

# ---------------------------------------------------------------------------
# Meeting Functions
# ---------------------------------------------------------------------------

# Returns all meetings joined with room names and department info
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
           m.room_id,
           m.created_by_dept,
           d.department_name as creator_dept_name
    FROM meeting_schedule m
    JOIN rooms r ON r.id = m.room_id
    LEFT JOIN departments d ON d.id = m.created_by_dept
    ORDER BY datetime(m.start_datetime) DESC
  ")
}

# Checks if a time slot overlaps with existing meetings for a room
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

# Inserts a new meeting with department tracking
add_meeting <- function(room_id, start_dt, end_dt, organiser, purpose, created_by_dept = NULL) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    INSERT INTO meeting_schedule (room_id, start_datetime, end_datetime, organiser, meeting_purpose, created_by_dept)
    VALUES (?, ?, ?, ?, ?, ?)
  ", params = list(room_id, start_dt, end_dt, organiser, purpose, created_by_dept))
}

# Deletes a meeting by ID
delete_meeting <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM meeting_schedule WHERE id = ?", params = list(id))
}

# Get meeting by ID
get_meeting_by_id <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM meeting_schedule WHERE id = ?", params = list(id))
}

# Check if department can modify a meeting
can_modify_meeting <- function(meeting_id, current_dept_id, is_admin) {
  if (is_admin) return(TRUE)
  meeting <- get_meeting_by_id(meeting_id)
  if (nrow(meeting) == 0) return(FALSE)
  if (is.na(meeting$created_by_dept)) return(FALSE)
  return(meeting$created_by_dept == current_dept_id)
}

# ---------------------------------------------------------------------------
# Car Functions
# ---------------------------------------------------------------------------

# Returns all cars from database
get_cars <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "car")) return(data.frame())
  dbReadTable(con, "car")
}

# Inserts a new car
add_car <- function(car_name, car_model, car_plate_no, accomodate, description, color, status) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con,
            "INSERT INTO car (car_name, car_model, car_plate_no, accomodate, description, color, status)
     VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(car_name, car_model, car_plate_no, accomodate, description, color, ifelse(isTRUE(status), 1, 0))
  )
}

# Deletes a car by ID (cascades to bookings)
delete_car <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM car WHERE id = ?", params = list(id))
}

# ---------------------------------------------------------------------------
# Car Booking Functions
# ---------------------------------------------------------------------------

# Returns all car bookings joined with car details and department info
get_car_schedules_joined <- function() {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  if (!dbExistsTable(con, "car_schedule")) return(data.frame())
  dbGetQuery(con, "
    SELECT cs.id,
           c.car_name,
           c.car_plate_no,
           cs.start_datetime,
           cs.end_datetime,
           cs.passanger_name,
           cs.department,
           cs.trip_purpose,
           cs.no_of_passangers,
           cs.share,
           cs.pickup_location,
           cs.dropoff_location,
           cs.comments,
           cs.created_by_dept,
           d.department_name as creator_dept_name
    FROM car_schedule cs
    JOIN car c ON c.car_plate_no = cs.car_plate_no
    LEFT JOIN departments d ON d.id = cs.created_by_dept
    ORDER BY datetime(cs.start_datetime) DESC
  ")
}

# Checks if a time slot overlaps with existing bookings for a car
car_booking_overlaps <- function(car_plate_no, start_dt, end_dt) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  count <- dbGetQuery(con, "
    SELECT COUNT(*) AS n
    FROM car_schedule
    WHERE car_plate_no = ?
      AND datetime(?) < datetime(end_datetime)
      AND datetime(?) > datetime(start_datetime)
  ", params = list(car_plate_no, start_dt, end_dt))$n
  count > 0
}

# Inserts a new car booking with department tracking
add_car_schedule <- function(car_plate_no, start_datetime, end_datetime, passanger_name,
                            department, trip_purpose, no_of_passangers,
                            share = TRUE, pickup_location, dropoff_location, comments,
                            created_by_dept = NULL) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    INSERT INTO car_schedule (car_plate_no, start_datetime, end_datetime,
                              passanger_name, department, trip_purpose,
                              no_of_passangers, share, pickup_location,
                              dropoff_location, comments, created_by_dept)
    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
  ", params = list(car_plate_no, start_datetime, end_datetime, passanger_name,
                   department, trip_purpose, no_of_passangers,
                   ifelse(isTRUE(share), 1, 0), pickup_location,
                   dropoff_location, comments, created_by_dept))
}

# Deletes a car booking by ID
delete_car_schedule <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbExecute(con, "DELETE FROM car_schedule WHERE id = ?", params = list(id))
}

# Get car booking by ID
get_car_schedule_by_id <- function(id) {
  con <- db_conn()
  on.exit(dbDisconnect(con))
  dbGetQuery(con, "SELECT * FROM car_schedule WHERE id = ?", params = list(id))
}

# Check if department can modify a car booking
can_modify_car_booking <- function(booking_id, current_dept_id, is_admin) {
  if (is_admin) return(TRUE)
  booking <- get_car_schedule_by_id(booking_id)
  if (nrow(booking) == 0) return(FALSE)
  if (is.na(booking$created_by_dept)) return(FALSE)
  return(booking$created_by_dept == current_dept_id)
}
