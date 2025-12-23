# Meeting Room Booking System - UI
# Google Calendar-like layout with left sidebar

library(shiny)
library(bslib)
library(toastui)
library(DT)

# Theme configuration
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#4285F4",
  "navbar-bg" = "#2C3E50"
)

ui <- page_navbar(
  title = "Meeting Room Scheduler",
  theme = theme,
  
  # ===== Calendar Tab (Main Dashboard) =====
  nav_panel(
    "Calendar",
    
    # Custom CSS for Google Calendar-like layout
    tags$head(
      tags$style(HTML("
        /* ========================================
           Google Calendar Layout
           ======================================== */
        
        .gcal-container {
          display: flex;
          height: calc(100vh - 120px);
          gap: 0;
        }
        
        /* Left Sidebar */
        .gcal-sidebar {
          width: 260px;
          min-width: 260px;
          background: #fff;
          border-right: 1px solid #dadce0;
          padding: 16px;
          overflow-y: auto;
        }
        
        .gcal-sidebar h6 {
          color: #3c4043;
          font-size: 11px;
          font-weight: 500;
          text-transform: uppercase;
          letter-spacing: 0.8px;
          margin: 20px 0 12px 0;
        }
        
        /* Mini calendar styling */
        .mini-calendar {
          margin-bottom: 20px;
        }
        
        .mini-calendar .form-control {
          border: none;
          background: transparent;
          padding: 0;
        }
        
        /* Room checkboxes */
        .room-checkbox-list {
          list-style: none;
          padding: 0;
          margin: 0;
        }
        
        .room-checkbox-item {
          display: flex;
          align-items: center;
          padding: 8px 0;
          cursor: pointer;
        }
        
        .room-checkbox-item:hover {
          background: #f1f3f4;
          margin: 0 -16px;
          padding: 8px 16px;
        }
        
        .room-color-dot {
          width: 12px;
          height: 12px;
          border-radius: 2px;
          margin-right: 12px;
          flex-shrink: 0;
        }
        
        .room-checkbox-label {
          font-size: 14px;
          color: #3c4043;
          flex-grow: 1;
        }
        
        /* Main Content Area */
        .gcal-main {
          flex: 1;
          display: flex;
          flex-direction: column;
          background: #fff;
          overflow: hidden;
        }
        
        /* Top Toolbar */
        .gcal-toolbar {
          display: flex;
          align-items: center;
          padding: 8px 16px;
          border-bottom: 1px solid #dadce0;
          gap: 16px;
        }
        
        .gcal-toolbar-left {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .gcal-toolbar-center {
          flex: 1;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .gcal-toolbar-right {
          display: flex;
          align-items: center;
          gap: 8px;
        }
        
        .gcal-week-display {
          font-size: 22px;
          font-weight: 400;
          color: #3c4043;
        }
        
        .btn-today {
          border: 1px solid #dadce0;
          background: #fff;
          color: #3c4043;
          font-weight: 500;
          padding: 8px 16px;
          border-radius: 4px;
        }
        
        .btn-today:hover {
          background: #f1f3f4;
        }
        
        .btn-nav {
          width: 36px;
          height: 36px;
          border-radius: 50%;
          border: none;
          background: transparent;
          color: #5f6368;
          display: flex;
          align-items: center;
          justify-content: center;
        }
        
        .btn-nav:hover {
          background: #f1f3f4;
        }
        
        .btn-new-event {
          background: #fff;
          border: 1px solid #dadce0;
          border-radius: 24px;
          padding: 8px 24px 8px 12px;
          font-weight: 500;
          color: #3c4043;
          display: flex;
          align-items: center;
          gap: 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.12);
        }
        
        .btn-new-event:hover {
          background: #f6fafe;
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
        }
        
        .btn-new-event .plus-icon {
          width: 36px;
          height: 36px;
          background: linear-gradient(135deg, #4285F4 25%, #34A853 25%, #34A853 50%, #FBBC05 50%, #FBBC05 75%, #EA4335 75%);
          border-radius: 50%;
          display: flex;
          align-items: center;
          justify-content: center;
          color: white;
          font-size: 24px;
          font-weight: 300;
        }
        
        /* Calendar Container */
        .gcal-calendar-wrapper {
          flex: 1;
          padding: 0;
          overflow: hidden;
        }
        
        /* ========================================
           Event Block Styling
           ======================================== */
        
        .toastui-calendar-event-time {
          border-radius: 4px !important;
          opacity: 0.9 !important;
          border-left: 4px solid rgba(0,0,0,0.15) !important;
        }
        
        .toastui-calendar-event-time:hover {
          opacity: 1 !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.2) !important;
        }
        
        .toastui-calendar-event-time-content {
          font-weight: 500 !important;
          font-size: 12px !important;
          padding: 4px 6px !important;
          color: white !important;
        }
        
        /* Current time indicator */
        .toastui-calendar-timegrid-now-indicator {
          border-top: 2px solid #EA4335 !important;
        }
        
        .toastui-calendar-timegrid-now-indicator-bullet {
          background-color: #EA4335 !important;
          width: 12px !important;
          height: 12px !important;
          border-radius: 50% !important;
          margin-left: -6px !important;
          margin-top: -5px !important;
        }
        
        /* Today column highlight */
        .toastui-calendar-day-name-date.toastui-calendar-today {
          background-color: #1a73e8 !important;
          color: white !important;
          border-radius: 50% !important;
          width: 46px !important;
          height: 46px !important;
        }
        
        /* Grid lines */
        .toastui-calendar-timegrid-gridline {
          border-color: #e8eaed !important;
        }
        
        /* ========================================
           Weekend (Saturday/Sunday) Styling
           ======================================== */
        
        /* Shade weekend columns - last 2 columns (Sat, Sun) */
        .toastui-calendar-column:nth-last-child(1),
        .toastui-calendar-column:nth-last-child(2) {
          background: repeating-linear-gradient(
            45deg,
            #e0e0e0,
            #e0e0e0 8px,
            #d5d5d5 8px,
            #d5d5d5 16px
          ) !important;
        }
        
        /* Weekend day headers - last 2 */
        .toastui-calendar-day-names .toastui-calendar-day-name:nth-last-child(1),
        .toastui-calendar-day-names .toastui-calendar-day-name:nth-last-child(2) {
          color: #9e9e9e !important;
          background: #f0f0f0 !important;
        }
        
        /* Weekend column overlay for darker shade */
        .toastui-calendar-column:nth-last-child(1)::after,
        .toastui-calendar-column:nth-last-child(2)::after {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          bottom: 0;
          background: rgba(100, 100, 100, 0.15);
          pointer-events: none;
          z-index: 1;
        }
        
        /* Detail popup styling (for viewing events) */
        .toastui-calendar-popup-container {
          border-radius: 8px !important;
          box-shadow: 0 8px 28px rgba(0,0,0,0.28) !important;
        }
        
        /* Custom modal styling for meeting creation */
        .modal-content {
          border-radius: 12px !important;
          border: none !important;
        }
        
        .modal-header {
          border-bottom: 1px solid #e8eaed !important;
          padding: 16px 24px !important;
        }
        
        .modal-body {
          padding: 24px !important;
        }
        
        .modal-footer {
          border-top: 1px solid #e8eaed !important;
          padding: 16px 24px !important;
        }
        
        /* Time input styling in modal */
        input[type='time'] {
          padding: 10px 12px !important;
          font-size: 16px !important;
          border: 1px solid #dadce0 !important;
          border-radius: 4px !important;
        }
        
        input[type='time']:focus {
          outline: none !important;
          border-color: #4285F4 !important;
          box-shadow: 0 0 0 2px rgba(66,133,244,0.2) !important;
        }
      ")),
      
      # JavaScript to capture calendar clicks and send to Shiny
      tags$script(HTML('
        $(document).ready(function() {
          
          // Wait for calendar to be initialized
          var checkCalendar = setInterval(function() {
            var calendarEl = document.querySelector(".toastui-calendar-layout");
            if (calendarEl) {
              clearInterval(checkCalendar);
              
              // Add click listener to time grid
              $(document).on("mouseup", ".toastui-calendar-timegrid-gridline, .toastui-calendar-timegrid-time-column, .toastui-calendar-column", function(e) {
                // Get the clicked position to determine time
                var $target = $(e.target).closest(".toastui-calendar-column");
                if ($target.length === 0) return;
                
                // Find the day column
                var $columns = $(".toastui-calendar-column");
                var colIndex = $columns.index($target);
                
                // Get today and calculate clicked date
                var today = new Date();
                var dayOfWeek = today.getDay();
                var monday = new Date(today);
                monday.setDate(today.getDate() - (dayOfWeek === 0 ? 6 : dayOfWeek - 1));
                
                var clickedDate = new Date(monday);
                clickedDate.setDate(monday.getDate() + colIndex);
                
                // Calculate time from Y position
                var $gridContainer = $target.find(".toastui-calendar-timegrid-column");
                if ($gridContainer.length === 0) $gridContainer = $target;
                
                var offset = e.offsetY || 0;
                var containerHeight = $gridContainer.height() || 540;
                var hoursDisplayed = 9; // 8 AM to 5 PM = 9 hours
                var startHour = 8;
                
                var clickedHour = startHour + (offset / containerHeight) * hoursDisplayed;
                var hour = Math.floor(clickedHour);
                var minutes = Math.round((clickedHour - hour) * 60 / 30) * 30; // Round to 30 min
                if (minutes >= 60) { hour++; minutes = 0; }
                
                // Enforce working hours: 8 AM to 5 PM
                if (hour < 8) hour = 8;
                if (hour > 16) hour = 16; // Max start time is 4 PM for 1-hour meeting
                
                var startTime = ("0" + hour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                var endHour = hour + 1;
                if (endHour > 17) endHour = 17; // End time max is 5 PM
                var endTime = ("0" + endHour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                
                // Check if weekend (Sat = 6, Sun = 0)
                var dayOfWeekClicked = clickedDate.getDay();
                var isWeekend = (dayOfWeekClicked === 0 || dayOfWeekClicked === 6);
                
                var dateStr = clickedDate.getFullYear() + "-" + 
                              ("0" + (clickedDate.getMonth() + 1)).slice(-2) + "-" + 
                              ("0" + clickedDate.getDate()).slice(-2);
                
                
                // Send to Shiny
                Shiny.setInputValue("calendar_click", {
                  date: dateStr,
                  startTime: startTime,
                  endTime: endTime,
                  isWeekend: isWeekend,
                  timestamp: new Date().getTime()
                }, {priority: "event"});
              });
            }
          }, 500);
          
          // Customize detail popup icons
          setInterval(function() {
            $(".toastui-calendar-ic-location").each(function() {
              if ($(this).attr("data-icon-changed") !== "true") {
                $(this).css({"background-image": "none", "font-size": "14px"}).html("👤").attr("data-icon-changed", "true");
              }
            });
          }, 100);
        });
      '))
    ),
    
    # Main Google Calendar-like container
    div(class = "gcal-container",
      
      # ===== Left Sidebar =====
      div(class = "gcal-sidebar",
        # Mini Month Calendar
        div(class = "mini-calendar",
          dateInput(
            inputId = "mini_calendar",
            label = NULL,
            value = Sys.Date(),
            width = "100%"
          )
        ),
        
        hr(style = "margin: 16px 0; border-color: #dadce0;"),
        
        # My Calendars (Rooms) Section
        h6(icon("chevron-down", style = "margin-right: 4px;"), "My Calendars"),
        uiOutput("room_checkboxes_ui")
      ),
      
      # ===== Main Content =====
      div(class = "gcal-main",
        
        # Top Toolbar
        div(class = "gcal-toolbar",
          # Left: Today + Nav
          div(class = "gcal-toolbar-left",
            actionButton("btn_today", "Today", class = "btn-today"),
            actionButton("btn_prev", icon("chevron-left"), class = "btn-nav"),
            actionButton("btn_next", icon("chevron-right"), class = "btn-nav")
          ),
          
          # Center: Week Display
          div(class = "gcal-toolbar-center",
            uiOutput("week_display_ui")
          ),
          
          # Right: Refresh button
          div(class = "gcal-toolbar-right",
            tags$span("Week View", style = "color: #5f6368; font-weight: 500; margin-right: 12px;"),
            actionButton("btn_refresh", icon("sync"), 
              class = "btn-nav", 
              title = "Refresh"
            )
          )
        ),
        
        # Calendar
        div(class = "gcal-calendar-wrapper",
          calendarOutput("booking_calendar", height = "100%")
        )
      )
    )
  ),
  
  # ===== Rooms Tab (Admin Only) =====
  nav_panel(
    "Manage Rooms",
    value = "rooms_tab",
    uiOutput("rooms_admin_ui")
  )
)
