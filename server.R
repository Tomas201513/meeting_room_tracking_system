# Resource Scheduler - Server Logic

library(shiny)
library(toastui)
library(DT)

# Default color palette for calendar events
COLORS <- c("#4285F4", "#0F9D58", "#DB4437", "#F4B400", "#AB47BC", 
            "#00ACC1", "#FF7043", "#5C6BC0", "#26A69A", "#EC407A")

server <- function(input, output, session) {
  
  # ---------------------------------------------------------------------------
  # Authentication
  # ---------------------------------------------------------------------------
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Returns TRUE if current user has admin privileges
  is_admin <- reactive({
    auth <- reactiveValuesToList(res_auth)
    isTRUE(auth$admin)
  })
  
  # Hide admin-only tabs for regular users
  observe({
    if (!is_admin()) {
      shinyjs::runjs('
        $(".navbar-nav .nav-item a:contains(\\"Manage Rooms\\")").parent().hide();
        $(".navbar-nav .nav-item a:contains(\\"Manage Cars\\")").parent().hide();
      ')
    } else {
      shinyjs::runjs('
        $(".navbar-nav .nav-item a:contains(\\"Manage Rooms\\")").parent().show();
        $(".navbar-nav .nav-item a:contains(\\"Manage Cars\\")").parent().show();
      ')
    }
  })
  
  # ---------------------------------------------------------------------------
  # Reactive Triggers (for refreshing data)
  # ---------------------------------------------------------------------------
  
  rooms_trig <- reactiveVal(0)
  meetings_trig <- reactiveVal(0)
  cars_trig <- reactiveVal(0)
  car_bookings_trig <- reactiveVal(0)
  
  # ---------------------------------------------------------------------------
  # Data: Rooms & Cars with Colors
  # ---------------------------------------------------------------------------
  
  # Returns rooms with color assignments
  rooms_with_colors <- reactive({
    rooms_trig()
    rooms <- get_rooms()
    if (nrow(rooms) == 0) return(rooms)
    
    if (!"color" %in% names(rooms)) {
      rooms$color <- COLORS[((seq_len(nrow(rooms)) - 1) %% length(COLORS)) + 1]
    } else {
      na_idx <- is.na(rooms$color) | rooms$color == ""
      if (any(na_idx)) {
        rooms$color[na_idx] <- COLORS[((which(na_idx) - 1) %% length(COLORS)) + 1]
      }
    }
    rooms
  })
  
  # Returns cars with color assignments
  cars_with_colors <- reactive({
    cars_trig()
    cars <- get_cars()
    if (nrow(cars) == 0) return(cars)
    
    if (!"color" %in% names(cars)) {
      cars$color <- COLORS[((seq_len(nrow(cars)) - 1) %% length(COLORS)) + 1]
    } else {
      na_idx <- is.na(cars$color) | cars$color == ""
      if (any(na_idx)) {
        cars$color[na_idx] <- COLORS[((which(na_idx) - 1) %% length(COLORS)) + 1]
      }
    }
    cars
  })
  
  # ---------------------------------------------------------------------------
  # Calendar Visibility Filters
  # ---------------------------------------------------------------------------
  
  visible_rooms <- reactiveVal(NULL)
  
  observe({
    rooms <- rooms_with_colors()
    if (nrow(rooms) > 0 && is.null(visible_rooms())) {
      active_ids <- as.character(subset(rooms, status == 1)$id)
      visible_rooms(active_ids)
    }
  })
  
  visible_cars <- reactiveVal(NULL)
  
  observe({
    cars <- cars_with_colors()
    if (nrow(cars) > 0 && is.null(visible_cars())) {
      visible_cars(as.character(subset(cars, status == 1)$car_plate_no))
    }
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Sidebar Checkboxes
  # ---------------------------------------------------------------------------
  
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
  
  observeEvent(input$visible_rooms_input, {
    visible_rooms(input$visible_rooms_input)
  })
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Sidebar Checkboxes
  # ---------------------------------------------------------------------------
  
  output$car_checkboxes_ui <- renderUI({
    cars <- cars_with_colors()
    active_cars <- subset(cars, status == 1)
    
    if (nrow(active_cars) == 0) {
      return(div("No cars available"))
    }
    
    tagList(
      lapply(seq_len(nrow(active_cars)), function(i) {
        car <- active_cars[i, ]
        div(
          class = "room-checkbox-item",
          tags$input(
            type = "checkbox",
            checked = "checked",
            class = "car-checkbox",
            `data-car` = car$car_plate_no
          ),
          div(class = "room-color-dot",
              style = paste0("background:", car$color)),
          tags$label(paste(car$car_name, "-", car$car_plate_no))
        )
      }),
      tags$script(HTML("
      $(document).on('change', '.car-checkbox', function(){
        let cars = [];
        $('.car-checkbox:checked').each(function(){
          cars.push($(this).data('car').toString());
        });
        Shiny.setInputValue('visible_cars_input', cars, {priority:'event'});
      });
    "))
    )
  })
  
  observeEvent(input$visible_cars_input, {
    visible_cars(input$visible_cars_input)
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Week Display
  # ---------------------------------------------------------------------------
  
  output$week_display_ui <- renderUI({
    current_date <- input$mini_calendar
    if (is.null(current_date)) current_date <- Sys.Date()
    
    week_start <- current_date - as.integer(format(current_date, "%u")) + 1
    week_end <- week_start + 6
    
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
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Schedule Data
  # ---------------------------------------------------------------------------
  
  # Formats meetings for the toastui calendar component
  calendar_schedules <- reactive({
    meetings_trig()
    rooms_trig()
    
    meetings <- get_meetings_joined()
    rooms <- rooms_with_colors()
    
    if (nrow(meetings) == 0) return(data.frame())
    
    format_for_calendar <- function(dt_string) {
      dt <- as.POSIXct(dt_string, format = "%Y-%m-%d %H:%M", tz = "UTC")
      format(dt, "%Y-%m-%dT%H:%M:%S")
    }
    
    data.frame(
      id = as.character(meetings$id),
      calendarId = as.character(meetings$room_id),
      title = ifelse(is.na(meetings$meeting_purpose) | meetings$meeting_purpose == "",
                     meetings$room_name, meetings$meeting_purpose),
      body = paste0(
        "<strong>Organizer:</strong> ", 
        ifelse(is.na(meetings$organiser) | meetings$organiser == "", "Not specified", meetings$organiser),
        "<br><strong>Room:</strong> ", meetings$room_name
      ),
      start = sapply(meetings$start_datetime, format_for_calendar),
      end = sapply(meetings$end_datetime, format_for_calendar),
      location = ifelse(is.na(meetings$organiser), "", meetings$organiser),
      category = "time",
      isAllday = FALSE,
      stringsAsFactors = FALSE
    )
  })
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Schedule Data
  # ---------------------------------------------------------------------------
  
  # Formats car bookings for the toastui calendar component
  car_calendar_schedules <- reactive({
    car_bookings_trig()
    cars_trig()
    
    bookings <- get_car_schedules_joined()
    cars <- cars_with_colors()
    
    if (nrow(bookings) == 0) return(data.frame())
    
    to_iso <- function(x) {
      format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%S")
    }
    
    schedules_df <- data.frame(
      id = as.character(bookings$id),
      calendarId = bookings$car_plate_no,
      title = paste0(bookings$trip_purpose),
      body = paste0(
        "<strong>Passenger:</strong> ", bookings$passanger_name,
        "<br><strong>Department:</strong> ", bookings$department,
        "<br><strong>Purpose:</strong> ", bookings$trip_purpose,
        "<br><strong>Passengers:</strong> ", bookings$no_of_passangers,
        "<br><strong>Pickup:</strong> ", bookings$pickup_location,
        "<br><strong>Drop-off:</strong> ", bookings$dropoff_location,
        ifelse(is.na(bookings$comments) | bookings$comments == "", "", 
               paste0("<br><strong>Notes:</strong> ", bookings$comments))
      ),
      location = bookings$passanger_name,
      start = sapply(bookings$start_datetime, to_iso),
      end = sapply(bookings$end_datetime, to_iso),
      category = "time",
      isAllday = FALSE,
      stringsAsFactors = FALSE
    )
    
    schedules_df
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Render
  # ---------------------------------------------------------------------------
  
  output$booking_calendar <- renderCalendar({
    rooms <- rooms_with_colors()
    schedules <- calendar_schedules()
    vis_rooms <- visible_rooms()
    
    cal <- calendar(
      defaultView = "week",
      useDetailPopup = TRUE,
      useCreationPopup = FALSE,
      isReadOnly = FALSE,
      view = "week"
    )
    
    active_rooms <- subset(rooms, status == 1)
    if (nrow(active_rooms) > 0) {
      for (i in seq_len(nrow(active_rooms))) {
        room_id <- as.character(active_rooms$id[i])
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
    
    if (nrow(schedules) > 0 && !is.null(vis_rooms)) {
      schedules <- schedules[schedules$calendarId %in% vis_rooms, ]
    }
    
    if (nrow(schedules) > 0) {
      cal <- cal %>% cal_schedules(schedules)
    }
    
    cal <- cal %>% cal_week_options(
      startDayOfWeek = 1,
      hourStart = 8,
      hourEnd = 17,
      showNowIndicator = TRUE,
      eventView = "time",
      taskView = FALSE,
      narrowWeekend = FALSE,
      workweek = TRUE
    )
    
    cal
  })
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Render
  # ---------------------------------------------------------------------------
  
  output$car_calendar <- renderCalendar({
    cars <- cars_with_colors()
    schedules <- car_calendar_schedules()
    vis <- visible_cars()
    
    cal <- calendar(
      defaultView = "week",
      useDetailPopup = TRUE,
      useCreationPopup = FALSE,
      isReadOnly = FALSE,
      view = "week"
    )
    
    active_cars <- subset(cars, status == 1)
    for (i in seq_len(nrow(active_cars))) {
      cal <- cal %>% cal_props(
        id = active_cars$car_plate_no[i],
        name = paste(active_cars$car_name[i], "-", active_cars$car_plate_no[i]),
        backgroundColor = active_cars$color[i],
        borderColor = active_cars$color[i]
      )
    }
    
    if (!is.null(vis)) {
      schedules <- schedules[schedules$calendarId %in% vis, ]
    }
    
    if (nrow(schedules) > 0) {
      cal <- cal %>% cal_schedules(schedules)
    }
    
    cal %>% cal_week_options(
      startDayOfWeek = 1,
      hourStart = 8,
      hourEnd = 17,
      showNowIndicator = TRUE,
      eventView = "time",
      taskView = FALSE,
      narrowWeekend = FALSE,
      workweek = TRUE
    )
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Navigation
  # ---------------------------------------------------------------------------
  
  observeEvent(input$mini_calendar, {
    cal_proxy_date("booking_calendar", input$mini_calendar)
  })
  
  observeEvent(input$btn_refresh, {
    meetings_trig(meetings_trig() + 1)
    rooms_trig(rooms_trig() + 1)
    showNotification("Calendar refreshed.", type = "message")
  })
  
  observeEvent(input$btn_today, {
    cal_proxy_today("booking_calendar")
  })
  
  observeEvent(input$btn_prev, {
    cal_proxy_prev("booking_calendar")
  })
  
  observeEvent(input$btn_next, {
    cal_proxy_next("booking_calendar")
  })
  
  observeEvent(input$goto_date, {
    cal_proxy_date("booking_calendar", input$goto_date)
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Create Meeting
  # ---------------------------------------------------------------------------
  
  clicked_datetime <- reactiveValues(date = NULL, start_time = NULL, end_time = NULL)
  
  # Opens booking modal when user clicks on calendar
  observeEvent(input$calendar_click, {
    event <- input$calendar_click
    if (is.null(event)) return()
    
    if (isTRUE(event$isWeekend)) {
      showNotification(
        "Weekends are not available for booking. Please select a weekday (Mon-Fri).",
        type = "warning",
        duration = 4
      )
      return()
    }
    
    clicked_datetime$date <- event$date
    clicked_datetime$start_time <- event$startTime
    clicked_datetime$end_time <- event$endTime
    
    date_obj <- as.Date(event$date)
    
    rooms <- rooms_with_colors()
    active_rooms <- subset(rooms, status == 1)
    room_choices <- setNames(active_rooms$id, active_rooms$room_name)
    
    showModal(modalDialog(
      title = div(
        style = "display: flex; align-items: center; gap: 10px;",
        icon("calendar-plus", style = "color: #4285F4;"),
        paste("New Meeting -", format(date_obj, "%A, %B %d, %Y"))
      ),
      
      selectInput("modal_room", "Select Room:", choices = room_choices, width = "100%"),
      textInput("modal_subject", "Subject:", placeholder = "Meeting subject", width = "100%"),
      textInput("modal_organizer", "Organizer:", placeholder = "Your name", width = "100%"),
      
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
  
  # Validates and saves the new meeting
  observeEvent(input$modal_save_meeting, {
    room_id <- as.integer(input$modal_room)
    subject <- input$modal_subject
    organizer <- input$modal_organizer
    start_time <- input$modal_start_time
    end_time <- input$modal_end_time
    date <- clicked_datetime$date
    
    if (is.null(room_id) || is.na(room_id)) {
      showNotification("Please select a room.", type = "error")
      return()
    }
    
    if (is.null(start_time) || start_time == "" || is.null(end_time) || end_time == "") {
      showNotification("Please select start and end times.", type = "error")
      return()
    }
    
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
    
    start_dt <- paste(date, start_time)
    end_dt <- paste(date, end_time)
    
    if (as.POSIXct(start_dt) >= as.POSIXct(end_dt)) {
      showNotification("End time must be after start time.", type = "error")
      return()
    }
    
    if (meeting_overlaps(room_id, start_dt, end_dt)) {
      showNotification("This room already has a meeting at this time.", type = "error")
      return()
    }
    
    tryCatch({
      add_meeting(room_id, start_dt, end_dt, organizer, subject)
      meetings_trig(meetings_trig() + 1)
      removeModal()
      showNotification("Meeting scheduled!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # ---------------------------------------------------------------------------
  # Room Calendar: Delete & Update
  # ---------------------------------------------------------------------------
  
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
  
  observeEvent(input$booking_calendar_update, {
    event <- input$booking_calendar_update
    if (!is.null(event)) {
      meetings_trig(meetings_trig() + 1)
    }
  })
  
  # ---------------------------------------------------------------------------
  # Room Admin: UI
  # ---------------------------------------------------------------------------
  
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
    
    layout_sidebar(
      sidebar = sidebar(
        h4("Add New Room"),
        textInput("room_name", "Room name:", placeholder = "e.g. Board Room"),
        numericInput("accomodate", "Capacity:", value = 10, min = 1, max = 1000),
        textAreaInput("room_desc", "Description:", placeholder = "Optional notes", resize = "vertical"),
        colourpicker::colourInput("room_color", "Color:", value = "#4285F4", 
                    palette = "limited",
                    allowedCols = c("#4285F4", "#0F9D58", "#DB4437", "#F4B400", "#AB47BC", 
                                   "#00ACC1", "#FF7043", "#5C6BC0", "#26A69A", "#EC407A")),
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
  
  # ---------------------------------------------------------------------------
  # Room Admin: Table Data
  # ---------------------------------------------------------------------------
  
  rooms_data <- reactive({
    rooms_trig()
    df <- get_rooms()
    if (nrow(df) == 0) return(df)
    
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm delete-room-btn" data-id="%s">Delete</button>',
      df$id
    )
    color_vals <- if ("color" %in% names(df)) df$color else rep("#4285F4", nrow(df))
    color_vals[is.na(color_vals) | color_vals == ""] <- "#4285F4"
    df$Color <- sprintf(
      '<span style="display:inline-block;width:20px;height:20px;background:%s;border-radius:4px;"></span>',
      color_vals
    )
    df$status <- ifelse(df$status == 1, "Active", "Inactive")
    df[, c("Delete", "Color", "room_name", "accomodate", "description", "status")]
  })
  
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
      colnames = c("Action", "Color", "Room", "Capacity", "Description", "Status")
    )
  })
  
  # ---------------------------------------------------------------------------
  # Room Admin: Add & Delete
  # ---------------------------------------------------------------------------
  
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
    color <- input$room_color %||% "#4285F4"
    st <- isTRUE(input$room_status)
    
    tryCatch({
      add_room(rn, cap, desc, color, st)
      updateTextInput(session, "room_name", value = "")
      updateNumericInput(session, "accomodate", value = 10)
      updateTextAreaInput(session, "room_desc", value = "")
      colourpicker::updateColourInput(session, "room_color", value = "#4285F4")
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
  
  observeEvent(input$rooms_refresh_btn, {
    rooms_trig(rooms_trig() + 1)
    showNotification("Rooms refreshed.", type = "message")
  })
  
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
  
  # ---------------------------------------------------------------------------
  # Car Admin: UI
  # ---------------------------------------------------------------------------
  
  output$cars_admin_ui <- renderUI({
    if (!is_admin()) {
      return(
        div(class = "alert alert-warning mt-4",
            icon("lock"),
            " This section is only accessible to administrators.",
            br(), br(),
            "Please contact an admin if you need to add or modify cars."
        )
      )
    }
    
    layout_sidebar(
      sidebar = sidebar(
        h4("Add New Car"),
        textInput("car_name", "Car name:", placeholder = "e.g. Toyota Corolla"),
        textInput("car_model", "Model:", placeholder = "e.g. 2023"),
        textInput("car_plate_no", "Plate number:", placeholder = "e.g. ABC-1234"),
        numericInput("car_accomodate", "Capacity:", value = 4, min = 1, max = 50),
        textAreaInput("car_desc", "Description:", placeholder = "Optional notes", resize = "vertical"),
        colourpicker::colourInput("car_color", "Color:", value = "#0F9D58", 
                    palette = "limited",
                    allowedCols = c("#4285F4", "#0F9D58", "#DB4437", "#F4B400", "#AB47BC", 
                                   "#00ACC1", "#FF7043", "#5C6BC0", "#26A69A", "#EC407A")),
        checkboxInput("car_status", "Active?", value = TRUE),
        br(),
        actionButton("add_car_btn", "Add Car", class = "btn-primary", width = "100%"),
        br(), br(),
        actionButton("cars_refresh_btn", "Refresh", class = "btn-secondary", width = "100%")
      ),
      card(
        card_header("Existing Cars"),
        card_body(DTOutput("cars_table"))
      ),
      tags$script(HTML("
        $(document).on('click', '.delete-car-btn', function() {
          var cid = $(this).attr('data-id');
          if (confirm('Delete this car? (All bookings for this car will also be deleted)')) {
            Shiny.setInputValue('delete_car_id', cid, {priority: 'event'});
          }
        });
      "))
    )
  })
  
  # ---------------------------------------------------------------------------
  # Car Admin: Table Data
  # ---------------------------------------------------------------------------
  
  cars_data <- reactive({
    cars_trig()
    df <- get_cars()
    if (nrow(df) == 0) return(df)
    
    df$Delete <- sprintf(
      '<button class="btn btn-danger btn-sm delete-car-btn" data-id="%s">Delete</button>',
      df$id
    )
    color_vals <- if ("color" %in% names(df)) df$color else rep("#0F9D58", nrow(df))
    color_vals[is.na(color_vals) | color_vals == ""] <- "#0F9D58"
    df$Color <- sprintf(
      '<span style="display:inline-block;width:20px;height:20px;background:%s;border-radius:4px;"></span>',
      color_vals
    )
    df$status <- ifelse(df$status == 1, "Active", "Inactive")
    df[, c("Delete", "Color", "car_name", "car_model", "car_plate_no", "accomodate", "description", "status")]
  })
  
  output$cars_table <- renderDT({
    if (!is_admin()) return(NULL)
    
    if (is.null(cars_data()) || nrow(cars_data()) == 0) {
      return(DT::datatable(
        data.frame(Message = "No cars yet. Add one from the left."),
        options = list(dom = 't', ordering = FALSE),
        rownames = FALSE
      ))
    }
    DT::datatable(
      cars_data(),
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
      colnames = c("Action", "Color", "Name", "Model", "Plate No", "Capacity", "Description", "Status")
    )
  })
  
  # ---------------------------------------------------------------------------
  # Car Admin: Add & Delete
  # ---------------------------------------------------------------------------
  
  observeEvent(input$add_car_btn, {
    if (!is_admin()) {
      showNotification("Only admins can add cars.", type = "error")
      return()
    }
    
    cn <- trimws(input$car_name %||% "")
    if (cn == "") {
      showNotification("Please provide a car name.", type = "error")
      return()
    }
    
    cm <- trimws(input$car_model %||% "")
    if (cm == "") {
      showNotification("Please provide a car model.", type = "error")
      return()
    }
    
    cp <- trimws(input$car_plate_no %||% "")
    if (cp == "") {
      showNotification("Please provide a plate number.", type = "error")
      return()
    }
    
    cap <- suppressWarnings(as.integer(input$car_accomodate))
    if (is.na(cap) || cap <= 0) {
      showNotification("Capacity must be a positive integer.", type = "error")
      return()
    }
    
    desc <- if (is.null(input$car_desc) || is.na(input$car_desc)) "" else input$car_desc
    color <- input$car_color %||% "#0F9D58"
    st <- isTRUE(input$car_status)
    
    tryCatch({
      add_car(cn, cm, cp, as.character(cap), desc, color, st)
      updateTextInput(session, "car_name", value = "")
      updateTextInput(session, "car_model", value = "")
      updateTextInput(session, "car_plate_no", value = "")
      updateNumericInput(session, "car_accomodate", value = 4)
      updateTextAreaInput(session, "car_desc", value = "")
      colourpicker::updateColourInput(session, "car_color", value = "#0F9D58")
      updateCheckboxInput(session, "car_status", value = TRUE)
      cars_trig(cars_trig() + 1)
      showNotification("Car added!", type = "message")
    }, error = function(e) {
      msg <- e$message
      if (grepl("UNIQUE constraint failed", msg, fixed = TRUE)) {
        showNotification("Car with this plate number already exists.", type = "error")
      } else {
        showNotification(paste("Error:", msg), type = "error")
      }
    })
  })
  
  observeEvent(input$cars_refresh_btn, {
    cars_trig(cars_trig() + 1)
    showNotification("Cars refreshed.", type = "message")
  })
  
  observeEvent(input$delete_car_id, {
    if (!is_admin()) {
      showNotification("Only admins can delete cars.", type = "error")
      return()
    }
    
    cid <- suppressWarnings(as.numeric(input$delete_car_id))
    if (!is.na(cid)) {
      tryCatch({
        delete_car(cid)
        cars_trig(cars_trig() + 1)
        car_bookings_trig(car_bookings_trig() + 1)
        showNotification("Car deleted.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    }
  })
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Week Display
  # ---------------------------------------------------------------------------
  
  output$week_display_car_ui <- renderUI({
    current_date <- input$mini_calendar_car
    if (is.null(current_date)) current_date <- Sys.Date()
    
    week_start <- current_date - as.integer(format(current_date, "%u")) + 1
    week_end <- week_start + 6
    
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
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Create Booking
  # ---------------------------------------------------------------------------
  
  clicked_car_datetime <- reactiveValues(
    date = NULL,
    start_time = NULL,
    end_time = NULL
  )
  
  # Opens booking modal when user clicks on car calendar
  observeEvent(input$car_calendar_click, {
    event <- input$car_calendar_click
    if (is.null(event)) return()
    
    if (isTRUE(event$isWeekend)) {
      showNotification(
        "Weekends are not available for car booking.",
        type = "warning"
      )
      return()
    }
    
    clicked_car_datetime$date <- event$date
    clicked_car_datetime$start_time <- event$startTime
    clicked_car_datetime$end_time <- event$endTime
    
    cars <- cars_with_colors()
    active_cars <- subset(cars, status == 1)
    
    car_choices <- setNames(
      active_cars$car_plate_no,
      paste(active_cars$car_name, "-", active_cars$car_plate_no)
    )
    
    showModal(modalDialog(
      title = paste(
        "New Car Booking -",
        format(as.Date(event$date), "%A, %B %d, %Y")
      ),
      
      selectInput("modal_car_plate", "Select Car:",
                  choices = car_choices, width = "100%"),
      
      textInput("modal_passenger", "Passenger name:", width = "100%"),
      textInput("modal_department", "Department:", width = "100%"),
      textInput("modal_trip_purpose", "Trip purpose:", width = "100%"),
      
      numericInput("modal_passengers_no", "Number of passengers:",
                   value = 1, min = 1),
      
      fluidRow(
        column(6,
               selectInput(
                 "modal_car_start_time",
                 "Start time:",
                 choices = sprintf("%02d:%02d",
                                   rep(8:16, each = 2),
                                   rep(c(0, 30), 9)),
                 selected = clicked_car_datetime$start_time
               )
        ),
        column(6,
               selectInput(
                 "modal_car_end_time",
                 "End time:",
                 choices = sprintf("%02d:%02d",
                                   rep(8:17, each = 2),
                                   rep(c(0, 30), 10)),
                 selected = clicked_car_datetime$end_time
               )
        )
      ),
      
      textInput("modal_pickup", "Pickup location:", width = "100%"),
      textInput("modal_dropoff", "Drop-off location:", width = "100%"),
      textAreaInput("modal_car_comment", "Comments:",
                    resize = "vertical"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "save_car_booking",
          "Save Booking",
          class = "btn-primary"
        )
      ),
      
      size = "m",
      easyClose = TRUE
    ))
  })
  
  # Validates and saves the new car booking
  observeEvent(input$save_car_booking, {
    req(clicked_car_datetime$date)
    
    start_dt <- paste(clicked_car_datetime$date,
                      input$modal_car_start_time)
    end_dt <- paste(clicked_car_datetime$date,
                    input$modal_car_end_time)
    
    if (as.POSIXct(start_dt) >= as.POSIXct(end_dt)) {
      showNotification("End time must be after start time.", type = "error")
      return()
    }
    
    overlap <- car_booking_overlaps(
      input$modal_car_plate,
      start_dt,
      end_dt
    )
    
    if (overlap) {
      showNotification("Car already booked for this time.", type = "error")
      return()
    }
    
    tryCatch({
      add_car_schedule(
        car_plate_no   = input$modal_car_plate,
        start_datetime = start_dt,
        end_datetime   = end_dt,
        passanger_name = input$modal_passenger,
        department     = input$modal_department,
        trip_purpose   = input$modal_trip_purpose,
        no_of_passangers = input$modal_passengers_no,
        pickup_location = input$modal_pickup,
        dropoff_location = input$modal_dropoff,
        comments = input$modal_car_comment
      )
      
      car_bookings_trig(car_bookings_trig() + 1)
      removeModal()
      
      updateDateInput(session, "mini_calendar_car",
                      value = as.Date(clicked_car_datetime$date))
      cal_proxy_date("car_calendar",
                     as.Date(clicked_car_datetime$date))
      
      showNotification("Car booked successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # ---------------------------------------------------------------------------
  # Car Calendar: Delete & Navigation
  # ---------------------------------------------------------------------------
  
  observeEvent(input$car_calendar_delete, {
    event <- input$car_calendar_delete
    if (is.null(event$id)) return()
    
    tryCatch({
      delete_car_schedule(as.integer(event$id))
      car_bookings_trig(car_bookings_trig() + 1)
      showNotification("Car booking deleted.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  observeEvent(input$btn_today_car, {
    cal_proxy_today("car_calendar")
  })
  
  observeEvent(input$btn_prev_car, {
    cal_proxy_prev("car_calendar")
  })
  
  observeEvent(input$btn_next_car, {
    cal_proxy_next("car_calendar")
  })
  
  observeEvent(input$mini_calendar_car, {
    cal_proxy_date("car_calendar", input$mini_calendar_car)
  })
  
  observeEvent(input$btn_refresh_car, {
    car_bookings_trig(car_bookings_trig() + 1)
    cars_trig(cars_trig() + 1)
    showNotification("Car calendar refreshed.", type = "message")
  })
  
}
