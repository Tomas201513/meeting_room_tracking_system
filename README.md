# Meeting Room Tracking System

A modern, Google Calendar-inspired meeting room booking application built with R Shiny. Features a visual weekly calendar view, real-time room availability, and role-based access control.

## Features

- **Weekly Calendar View**: Google Calendar-like interface with 7-day week display
- **Time Block Scheduling**: 8 AM - 5 PM working hours with 30-minute intervals
- **Room Management**: Add, edit, and delete meeting rooms (admin only)
- **Visual Room Filtering**: Color-coded rooms with checkbox filters
- **Overlap Prevention**: Automatic detection of booking conflicts
- **Weekend Restriction**: Saturday and Sunday displayed as unavailable
- **Authentication**: Built-in user authentication with admin/user roles
- **SQLite Database**: Persistent storage for rooms and meetings

## Screenshots

<!-- Add screenshots of your application here -->

## Installation

### Prerequisites

- R (>= 4.0.0)
- RStudio (recommended)

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/Tomas201513/meeting_room_tracking_system.git
   cd meeting_room_tracking_system
   ```

2. Install required packages:
   ```r
   source("install_packages.R")
   ```

3. Run the application:
   ```r
   shiny::runApp()
   ```

## Configuration

### Default Credentials

| Role | Username | Password |
|------|----------|----------|
| Admin | `admin` | `admin` |
| User | `user` | `user` |

### Database

The application automatically creates a SQLite database (`scheduler.db`) on first run with the following tables:

- `rooms`: Meeting room definitions
- `meeting_schedule`: Scheduled meetings

## Usage

### Calendar View

- Click on any time slot to create a new meeting
- Use the sidebar calendar to navigate to specific dates
- Filter visible rooms using the checkbox list
- Use Today/Previous/Next buttons for quick navigation

### Room Management (Admin Only)

1. Navigate to "Manage Rooms" tab
2. Fill in room details (name, capacity, description)
3. Click "Add Room" to create

### Booking a Meeting

1. Click on desired time slot in calendar
2. Select room from dropdown
3. Enter meeting subject and organizer
4. Adjust start/end times if needed
5. Click "Save Meeting"

## Project Structure

```
meeting_room_tracking_system/
├── app.R                        # Main application entry point
├── global.R                     # Global configuration and database functions
├── ui.R                         # User interface definition
├── server.R                     # Server-side logic
├── install_packages.R           # Package installation script
├── .gitignore                   # Git ignore rules
├── Meeting-room tracker.Rproj   # RStudio project file
└── README.md                    # This file
```

## Technologies Used

- **R Shiny**: Web application framework
- **bslib**: Bootstrap 5 theming
- **toastui**: TOAST UI Calendar integration
- **shinymanager**: Authentication system
- **RSQLite**: SQLite database interface
- **DT**: Interactive data tables
- **shinyWidgets**: Enhanced UI components

## Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## License

This project is open source. See the repository for license details.

## Acknowledgments

- [TOAST UI Calendar](https://ui.toast.com/tui-calendar) for the calendar component
- [Shiny](https://shiny.rstudio.com/) for the web framework
- [Bootstrap](https://getbootstrap.com/) for styling

