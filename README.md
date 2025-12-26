# Resource Scheduler

A Shiny web application for booking meeting rooms and vehicles. Built with a clean, calendar-based interface inspired by Google Calendar.

## What it does

- Book meeting rooms and cars through a weekly calendar view
- See availability at a glance with color-coded events
- Prevent double-bookings automatically
- Manage resources through an admin panel
- Department-based authentication with role-based permissions

## Quick Start

1. Install the required packages:
```r
source("install_packages.R")
```

2. Run the app:
```r
shiny::runApp()
```

3. Log in with the default admin credentials:

| Role  | Department Name | Password  |
|-------|-----------------|-----------|
| Admin | Admin           | admin123  |

## Permission System

Each department can have separate permissions for rooms and cars:

| Permission | Can View | Can Create | Edit/Delete Own | Edit/Delete Others |
|------------|----------|------------|-----------------|-------------------|
| Read       | Yes      | No         | No              | No                |
| Create     | Yes      | Yes        | Yes             | No                |
| Admin      | Yes      | Yes        | Yes             | Yes               |

- **Read**: Can only view existing bookings
- **Create**: Can create bookings and manage their own bookings (edit/delete)
- **Admin**: Full control over all bookings and can manage departments

## How to use

**Booking a room or car (requires Create permission):**
- Click on any time slot in the calendar
- Fill in the details in the popup form
- Click Save

**Navigating the calendar:**
- Use the mini calendar on the left to jump to a date
- Use Previous/Next buttons for week navigation
- Filter visible rooms or cars using the checkboxes

**Managing resources (admin only):**
- Go to "Manage Rooms" or "Manage Cars" tab
- Add new resources with the form on the left
- Delete resources from the table

**Managing departments (admin only):**
- Go to "Manage Departments" tab
- Create new department accounts with name and password
- Set room and car permissions (Read or Create)
- Share credentials with departments after creation

## Project Files

```
app.R           - Entry point
global.R        - Database setup and helper functions
ui.R            - User interface
server.R        - Server logic
www/style.css   - Styling
scheduler.db    - SQLite database (created on first run)
```

## Tech Stack

- R Shiny with bslib for Bootstrap 5 theming
- toastui for the calendar component
- RSQLite for data persistence
- shinymanager for authentication
- sodium for secure password hashing

## Notes

- Working hours are set to 8 AM - 5 PM, Monday through Friday
- The database file (`scheduler.db`) is created automatically on first run
- Passwords are securely hashed using the sodium library
- Default admin account is created on first run (Admin / admin123)
- Delete the `scheduler.db` file to reset the database
