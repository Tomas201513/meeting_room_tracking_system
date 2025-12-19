# Meeting Room Booking System - Server
# Calendar with toastui + Admin room management

library(shiny)
library(toastui)
library(DT)

# Predefined colors for rooms
ROOM_COLORS <- c("#4285F4", "#0F9D58", "#DB4437", "#F4B400", "#AB47BC", 
                 "#00ACC1", "#FF7043", "#5C6BC0", "#26A69A", "#EC407A")

server <- function(input, output, session) {
  
  # ===== Authentication =====
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Check if user is admin
  is_admin <- reactive({
    auth <- reactiveValuesToList(res_auth)
    isTRUE(auth$admin)
  })
  
  # ===== Reactive Triggers =====
  rooms_trig <- reactiveVal(0)
  meetings_trig <- reactiveVal(0)
  
  # =========================================
  # ===== CALENDAR SECTION =====
  # =========================================
  
  # Get rooms with colors assigned
  rooms_with_colors <- reactive({
    rooms_trig()
    rooms <- get_rooms()
    if (nrow(rooms) == 0) return(rooms)
    
    # Assign colors to rooms
    rooms$color <- ROOM_COLORS[((seq_len(nrow(rooms)) - 1) %% length(ROOM_COLORS)) + 1]
    rooms
  })
  
  # Track which rooms are visible (for checkbox filtering)
  visible_rooms <- reactiveVal(NULL)
  
  # Initialize visible rooms when rooms load
  observe({
    rooms <- rooms_with_colors()
    if (nrow(rooms) > 0 && is.null(visible_rooms())) {
      active_ids <- as.character(subset(rooms, status == 1)$id)
      visible_rooms(active_ids)
    }
  })
  
  # Room checkboxes UI (Google Calendar style)
  output$room_checkboxes_ui <- renderUI({
    rooms <- rooms_with_colors()
    active_rooms <- subset(rooms, status == 1)
    
    if (nrow(active_rooms) == 0) {
      return(div(
        style = "color: #5f6368; font-size: 13px; padding: 8px 0;",
        "No rooms available.",
        br(),
        "Admin can add rooms in 'Manage Rooms' tab."
      ))
    }
    
    # Create checkbox for each room
    checkboxes <- lapply(seq_len(nrow(active_rooms)), function(i) {
      room <- active_rooms[i, ]
      div(
        class = "room-checkbox-item",
        tags$input(
          type = "checkbox",
          id = paste0("room_vis_", room$id),
          class = "room-checkbox",
          checked = "checked",
          `data-room-id` = room$id,
          style = "margin-right: 8px;"
        ),
        div(
          class = "room-color-dot",
          style = paste0("background-color: ", room$color, ";")
        ),
        tags$label(
          class = "room-checkbox-label",
          `for` = paste0("room_vis_", room$id),
          room$room_name
        )
      )
    })
    
    tagList(
      tags$ul(class = "room-checkbox-list", checkboxes),
      # JavaScript to handle checkbox changes
      tags$script(HTML('
        $(document).on("change", ".room-checkbox", function() {
          var visibleRooms = [];
          $(".room-checkbox:checked").each(function() {
            visibleRooms.push($(this).data("room-id").toString());
          });
          Shiny.setInputValue("visible_rooms_input", visibleRooms, {priority: "event"});
        });
      '))
    )
  })
  
  # Update visible rooms when checkboxes change
  observeEvent(input$visible_rooms_input, {
    visible_rooms(input$visible_rooms_input)
  })
  
  # Week display (shows current week range)
  output$week_display_ui <- renderUI({
    # This will be updated by calendar navigation
    current_date <- input$mini_calendar
    if (is.null(current_date)) current_date <- Sys.Date()
    
    # Calculate week start and end
    week_start <- current_date - as.integer(format(current_date, "%u")) + 1
    week_end <- week_start + 6
    
    # Format: "December 15-21, 2025"
    if (format(week_start, "%B") == format(week_end, "%B")) {
      display_text <- paste0(
        format(week_start, "%B "),
        format(week_start, "%d"), "-",
        format(week_end, "%d"), ", ",
        format(week_end, "%Y")
      )
    } else {
      display_text <- paste0(
        format(week_start, "%b %d"), " - ",
        format(week_end, "%b %d, %Y")
      )
    }
    
    div(class = "gcal-week-display", display_text)
  })
  
  # Get meetings formatted for calendar (Google Calendar-like blocks)
  calendar_schedules <- reactive({
    meetings_trig()
    rooms_trig()
    
    meetings <- get_meetings_joined()
    rooms <- rooms_with_colors()
    
    if (nrow(meetings) == 0) return(data.frame())
    
    # Format times for toastui (needs ISO format for proper block rendering)
    format_for_calendar <- function(dt_string) {
      # Convert "YYYY-MM-DD HH:MM" to ISO format
      dt <- as.POSIXct(dt_string, format = "%Y-%m-%d %H:%M", tz = "UTC")
      format(dt, "%Y-%m-%dT%H:%M:%S")
    }
    
    # Build display title: "Subject | Organizer" or fallback
    build_title <- function(subject, organizer) {
      subj <- if (is.na(subject) || subject == "") NULL else subject
      org <- if (is.na(organizer) || organizer == "") NULL else organizer
      
      if (!is.null(subj) && !is.null(org)) {
        paste0(subj, "\n👤 ", org)
      } else if (!is.null(subj)) {
        subj
      } else if (!is.null(org)) {
        paste0("👤 ", org)
      } else {
        "Meeting"
      }
    }
    
    # Create schedule data for toastui with proper time blocks
    data.frame(
      id = as.character(meetings$id),
      calendarId = as.character(meetings$room_id),
      title = mapply(build_title, meetings$meeting_purpose, meetings$organiser, USE.NAMES = FALSE),
      body = paste0(
        "<div style='padding: 8px;'>",
        "<div style='font-size: 14px; font-weight: 600; margin-bottom: 8px;'>",
        ifelse(is.na(meetings$meeting_purpose) | meetings$meeting_purpose == "", "Meeting", meetings$meeting_purpose),
        "</div>",
        "<div style='font-size: 13px; color: #5f6368; margin-bottom: 4px;'>",
        "<span style='margin-right: 6px;'>👤</span>",
        ifelse(is.na(meetings$organiser) | meetings$organiser == "", "Not specified", meetings$organiser),
        "</div>",
        "<div style='font-size: 13px; color: #5f6368;'>",
        "<span style='margin-right: 6px;'>📍</span>",
        meetings$room_name,
        "</div>",
        "</div>"
      ),
      start = sapply(meetings$start_datetime, format_for_calendar),
      end = sapply(meetings$end_datetime, format_for_calendar),
      location = ifelse(is.na(meetings$organiser), "", meetings$organiser),
      category = "time",
      isAllday = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  # Render the calendar
  output$booking_calendar <- renderCalendar({
    rooms <- rooms_with_colors()
    schedules <- calendar_schedules()
    vis_rooms <- visible_rooms()
    
    # Force week view with time grid
    # DISABLE built-in creation popup - we'll use custom modal
    cal <- calendar(
      defaultView = "week",
      useDetailPopup = TRUE,
      useCreationPopup = FALSE,  # Disabled! Using custom modal instead
      isReadOnly = FALSE,
      view = "week"
    )
    
    # Add room properties (calendars) - only visible ones
    active_rooms <- subset(rooms, status == 1)
    if (nrow(active_rooms) > 0) {
      for (i in seq_len(nrow(active_rooms))) {
        room_id <- as.character(active_rooms$id[i])
        # Set visibility based on checkbox state
        is_visible <- is.null(vis_rooms) || room_id %in% vis_rooms
        
        cal <- cal %>% cal_props(
          id = room_id,
          name = active_rooms$room_name[i],
          color = "#fff",
          backgroundColor = active_rooms$color[i],
          borderColor = active_rooms$color[i]
        )
      }
    }
    
    # Filter schedules based on visible rooms
    if (nrow(schedules) > 0 && !is.null(vis_rooms)) {
      schedules <- schedules[schedules$calendarId %in% vis_rooms, ]
    }
    
    # Add schedules if any
    if (nrow(schedules) > 0) {
      cal <- cal %>% cal_schedules(schedules)
    }
    
    # Add week options for Google Calendar-like display
    # 7-day week view from 8 AM to 5 PM in 1-hour blocks
    cal <- cal %>% cal_week_options(
      startDayOfWeek = 1,      # Start week on Monday
      hourStart = 8,           # 8 AM
      hourEnd = 17,            # 5 PM
      showNowIndicator = TRUE,
      eventView = TRUE,        # Show events in time grid
      taskView = FALSE,        # No task view
      narrowWeekend = FALSE,   # Full width weekends
      workweek = FALSE         # Show all 7 days
    )
    
    cal
  })
  
  # Handle mini calendar date changes
  observeEvent(input$mini_calendar, {
    cal_proxy_date("booking_calendar", input$mini_calendar)
  })
  
  # Store clicked date/time for custom modal
  clicked_datetime <- reactiveValues(date = NULL, start_time = NULL, end_time = NULL)
  
  
  # When user clicks on calendar time grid, show custom modal
  observeEvent(input$calendar_click, {
    event <- input$calendar_click
    if (is.null(event)) return()
    
    
    # Check if weekend
    if (isTRUE(event$isWeekend)) {
      showNotification(
        "Weekends are not available for booking. Please select a weekday (Mon-Fri).",
        type = "warning",
        duration = 4
      )
      return()
    }
    
    # Store date and times from JavaScript click handler
    clicked_datetime$date <- event$date
    clicked_datetime$start_time <- event$startTime
    clicked_datetime$end_time <- event$endTime
    
    # Parse for display
    date_obj <- as.Date(event$date)
    
    # Get active rooms for dropdown
    rooms <- rooms_with_colors()
    active_rooms <- subset(rooms, status == 1)
    room_choices <- setNames(active_rooms$id, active_rooms$room_name)
    
    # Show custom modal with TIME ONLY pickers
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("calendar-plus", style = "color: #4285F4;"),
        paste("New Meeting -", format(date_obj, "%A, %B %d, %Y"))
      ),
      
      # Room selector
      selectInput("modal_room", "Select Room:", choices = room_choices, width = "100%"),
      
      # Subject
      textInput("modal_subject", "Subject:", placeholder = "Meeting subject", width = "100%"),
      
      # Organizer
      textInput("modal_organizer", "Organizer:", placeholder = "Your name", width = "100%"),
      
      # TIME ONLY pickers in a row (8 AM to 5 PM only)
      fluidRow(
        column(6,
          selectInput(
            "modal_start_time",
            "Start Time:",
            choices = c(
              "08:00" = "08:00", "08:30" = "08:30",
              "09:00" = "09:00", "09:30" = "09:30",
              "10:00" = "10:00", "10:30" = "10:30",
              "11:00" = "11:00", "11:30" = "11:30",
              "12:00" = "12:00", "12:30" = "12:30",
              "13:00" = "13:00", "13:30" = "13:30",
              "14:00" = "14:00", "14:30" = "14:30",
              "15:00" = "15:00", "15:30" = "15:30",
              "16:00" = "16:00", "16:30" = "16:30"
            ),
            selected = clicked_datetime$start_time,
            width = "100%"
          )
        ),
        column(6,
          selectInput(
            "modal_end_time",
            "End Time:",
            choices = c(
              "08:30" = "08:30",
              "09:00" = "09:00", "09:30" = "09:30",
              "10:00" = "10:00", "10:30" = "10:30",
              "11:00" = "11:00", "11:30" = "11:30",
              "12:00" = "12:00", "12:30" = "12:30",
              "13:00" = "13:00", "13:30" = "13:30",
              "14:00" = "14:00", "14:30" = "14:30",
              "15:00" = "15:00", "15:30" = "15:30",
              "16:00" = "16:00", "16:30" = "16:30",
              "17:00" = "17:00"
            ),
            selected = clicked_datetime$end_time,
            width = "100%"
          )
        )
      ),
      
      # Note about working hours
      div(
        style = "color: #5f6368; font-size: 12px; margin-top: 8px;",
        icon("clock"),
        " Working hours: 8:00 AM - 5:00 PM (Mon-Fri)"
      ),
      
      br(),
      div(
        style = "color: #5f6368; font-size: 13px;",
        icon("info-circle"),
        paste(" Date:", event$date, "(from calendar selection)")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("modal_save_meeting", "Save Meeting", 
                     class = "btn-primary",
                     style = "background: #4285F4; border: none;")
      ),
      
      size = "m",
      easyClose = TRUE
    ))
  })
  
  # Handle custom modal save
  observeEvent(input$modal_save_meeting, {
    # Get values
    room_id <- as.integer(input$modal_room)
    subject <- input$modal_subject
    organizer <- input$modal_organizer
    start_time <- input$modal_start_time
    end_time <- input$modal_end_time
    date <- clicked_datetime$date
    
    
    # Validate
    if (is.null(room_id) || is.na(room_id)) {
      showNotification("Please select a room.", type = "error")
      return()
    }
    
    if (is.null(start_time) || start_time == "" || is.null(end_time) || end_time == "") {
      showNotification("Please select start and end times.", type = "error")
      return()
    }
    
    # Validate working hours (8 AM - 5 PM)
    start_hour <- as.integer(substr(start_time, 1, 2))
    end_hour <- as.integer(substr(end_time, 1, 2))
    
    if (start_hour < 8 || start_hour > 16) {
      showNotification("Start time must be between 8:00 AM and 4:00 PM.", type = "error")
      return()
    }
    
    if (end_hour < 9 || end_hour > 17) {
      showNotification("End time must be between 9:00 AM and 5:00 PM.", type = "error")
      return()
    }
    
    # Build datetime strings
    start_dt <- paste(date, start_time)
    end_dt <- paste(date, end_time)
    
    # Check if end is after start
    if (as.POSIXct(start_dt) >= as.POSIXct(end_dt)) {
      showNotification("End time must be after start time.", type = "error")
      return()
    }
    
    # Check overlap
    if (meeting_overlaps(room_id, start_dt, end_dt)) {
      showNotification("This room already has a meeting at this time.", type = "error")
      return()
    }
    
    # Add meeting
    
    tryCatch({
      add_meeting(room_id, start_dt, end_dt, organizer, subject)
      meetings_trig(meetings_trig() + 1)
      removeModal()
      showNotification("Meeting scheduled!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # Handle meeting deletion
  observeEvent(input$booking_calendar_delete, {
    event <- input$booking_calendar_delete
    if (!is.null(event$id)) {
      tryCatch({
        delete_meeting(as.integer(event$id))
        meetings_trig(meetings_trig() + 1)
        showNotification("Meeting deleted.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    }
  })
  
  # Handle meeting update (drag/resize)
  observeEvent(input$booking_calendar_update, {
    event <- input$booking_calendar_update
    if (!is.null(event)) {
      # For now, just refresh - full update would need more DB work
      meetings_trig(meetings_trig() + 1)
    }
  })
  
  # Refresh button
  observeEvent(input$btn_refresh, {
    meetings_trig(meetings_trig() + 1)
    rooms_trig(rooms_trig() + 1)
    showNotification("Calendar refreshed.", type = "message")
  })
  
  # Today button
  observeEvent(input$btn_today, {
    cal_proxy_today("booking_calendar")
  })
  
  # Previous button
  observeEvent(input$btn_prev, {
    cal_proxy_prev("booking_calendar")
  })
  
  # Next button
  observeEvent(input$btn_next, {
    cal_proxy_next("booking_calendar")
  })
  
  # Jump to date
  observeEvent(input$goto_date, {
    cal_proxy_date("booking_calendar", input$goto_date)
  })
  
  # =========================================
  # ===== ROOMS ADMIN SECTION =====
  # =========================================
  
  # Admin-only rooms UI
  output$rooms_admin_ui <- renderUI({
    if (!is_admin()) {
      return(
        div(class = "alert alert-warning mt-4",
          icon("lock"),
          " This section is only accessible to administrators.",
          br(), br(),
          "Please contact an admin if you need to add or modify rooms."
        )
      )
    }
    
    # Admin UI
    layout_sidebar(
      sidebar = sidebar(
        h4("Add New Room"),
        textInput("room_name", "Room name:", placeholder = "e.g. Board Room"),
        numericInput("accomodate", "Capacity:", value = 10, min = 1, max = 1000),
        textAreaInput("room_desc", "Description:", placeholder = "Optional notes", resize = "vertical"),
        checkboxInput("room_status", "Active?", value = TRUE),
        br(),
        actionButton("add_room_btn", "Add Room", class = "btn-primary", width = "100%"),
        br(), br(),
        actionButton("rooms_refresh_btn", "Refresh", class = "btn-secondary", width = "100%")
      ),
      card(
        card_header("Existing Rooms"),
        card_body(DTOutput("rooms_table"))
      ),
      # Delete room JS handler
      tags$script(HTML("
        $(document).on('click', '.delete-room-btn', function() {
          var rid = $(this).attr('data-id');
          if (confirm('Delete this room? (All meetings for this room will also be deleted)')) {
            Shiny.setInputValue('delete_room_id', rid, {priority: 'event'});
          }
        });
      "))
    )
  })
  
  # Rooms data for table
  rooms_data <- reactive({
    rooms_trig()
    df <- get_rooms()
    if (nrow(df) == 0) return(df)
    
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm delete-room-btn" data-id="%s">Delete</button>',
      df$id
    )
    df$status <- ifelse(df$status == 1, "Active", "Inactive")
    df[, c("Delete", "room_name", "accomodate", "description", "status")]
  })
  
  # Render rooms table
  output$rooms_table <- renderDT({
    if (!is_admin()) return(NULL)
    
    if (is.null(rooms_data()) || nrow(rooms_data()) == 0) {
      return(DT::datatable(
        data.frame(Message = "No rooms yet. Add one from the left."),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    DT::datatable(
      rooms_data(),
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = 'tip',
        columnDefs = list(
          list(orderable = FALSE, targets = 0),
          list(width = '80px', targets = 0)
        )
      ),
      rownames = FALSE,
      colnames = c("Action", "Room", "Capacity", "Description", "Status")
    )
  })
  
  # Add room handler
  observeEvent(input$add_room_btn, {
    if (!is_admin()) {
      showNotification("Only admins can add rooms.", type = "error")
      return()
    }
    
    rn <- trimws(input$room_name %||% "")
    if (rn == "") {
      showNotification("Please provide a room name.", type = "error")
      return()
    }
    
    cap <- suppressWarnings(as.integer(input$accomodate))
    if (is.na(cap) || cap <= 0) {
      showNotification("Capacity must be a positive integer.", type = "error")
      return()
    }
    
    desc <- if (is.null(input$room_desc) || is.na(input$room_desc)) "" else input$room_desc
    st <- isTRUE(input$room_status)
    
    tryCatch({
      add_room(rn, cap, desc, st)
      updateTextInput(session, "room_name", value = "")
      updateNumericInput(session, "accomodate", value = 10)
      updateTextAreaInput(session, "room_desc", value = "")
      updateCheckboxInput(session, "room_status", value = TRUE)
      rooms_trig(rooms_trig() + 1)
      showNotification("Room added!", type = "message")
    }, error = function(e) {
      msg <- e$message
      if (grepl("UNIQUE constraint failed", msg, fixed = TRUE)) {
        showNotification("Room name already exists.", type = "error")
      } else {
        showNotification(paste("Error:", msg), type = "error")
      }
    })
  })
  
  # Refresh rooms
  observeEvent(input$rooms_refresh_btn, {
    rooms_trig(rooms_trig() + 1)
    showNotification("Rooms refreshed.", type = "message")
  })
  
  # Delete room
  observeEvent(input$delete_room_id, {
    if (!is_admin()) {
      showNotification("Only admins can delete rooms.", type = "error")
      return()
    }
    
    rid <- suppressWarnings(as.numeric(input$delete_room_id))
    if (!is.na(rid)) {
      tryCatch({
        delete_room(rid)
        rooms_trig(rooms_trig() + 1)
        meetings_trig(meetings_trig() + 1)
        showNotification("Room deleted.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    }
  })
}
