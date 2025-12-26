# Resource Scheduler - User Interface

library(shiny)
library(bslib)
library(toastui)
library(DT)

# Bootstrap 5 theme
theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#4285F4",
  "navbar-bg" = "#2C3E50"
)

ui <- page_navbar(
  title = "Resource Scheduler",
  theme = theme,
  
  # ---------------------------------------------------------------------------
  # Global: Styles & Tooltip Script
  # ---------------------------------------------------------------------------
  
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      # Font Awesome for icons (required for shinymanager logout button)
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css",
        integrity = "sha512-iecdLmaskl7CVkqkXNQ/ZH/XLlvWZOJyj7Yy7tcenmpD1ypASozpmT/E0iPtmFIB46ZmdtAc9eNBvH0H/ZpiBw==",
        crossorigin = "anonymous"
      ),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    
    # Hover tooltip for calendar events
    tags$script(HTML('
      $(document).ready(function() {
        var tooltip = $("<div id=\\"event-tooltip\\"></div>").css({
          position: "fixed",
          background: "rgba(32, 33, 36, 0.95)",
          color: "white",
          padding: "12px 16px",
          borderRadius: "8px",
          fontSize: "13px",
          lineHeight: "1.6",
          maxWidth: "320px",
          zIndex: 10000,
          display: "none",
          boxShadow: "0 6px 20px rgba(0,0,0,0.35)",
          pointerEvents: "none"
        }).appendTo("body");
        
        $(document).on("mouseenter", ".toastui-calendar-event-time", function(e) {
          var $event = $(this);
          var title = $event.find(".toastui-calendar-event-time-content").text() || "Booking";
          var eventColor = $event.css("background-color");
          
          var content = "<div style=\\"border-left: 4px solid " + eventColor + "; padding-left: 10px;\\">";
          content += "<div style=\\"font-weight: 600; font-size: 14px; margin-bottom: 6px;\\">" + title + "</div>";
          content += "<div style=\\"color: #9aa0a6; font-size: 11px;\\">Click for full details</div>";
          content += "</div>";
          
          var x = e.pageX + 15;
          var y = e.pageY + 15;
          
          if (x + 320 > $(window).width()) x = e.pageX - 335;
          if (y + 100 > $(window).height()) y = e.pageY - 80;
          
          tooltip.html(content).css({
            left: x,
            top: y
          }).fadeIn(100);
        });
        
        $(document).on("mousemove", ".toastui-calendar-event-time", function(e) {
          var x = e.pageX + 15;
          var y = e.pageY + 15;
          if (x + 320 > $(window).width()) x = e.pageX - 335;
          if (y + 100 > $(window).height()) y = e.pageY - 80;
          
          tooltip.css({ left: x, top: y });
        });
        
        $(document).on("mouseleave", ".toastui-calendar-event-time", function() {
          tooltip.fadeOut(80);
        });
        
        $(document).on("click", ".toastui-calendar-event-time", function() {
          tooltip.hide();
        });
      });
    '))
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Tab: Book Meeting Room
  # ---------------------------------------------------------------------------
  
  nav_panel(
    "Book Meeting Room",
    
    # Click handler for calendar time grid
    tags$head(
      tags$script(HTML('
        $(document).ready(function() {
          
          var checkCalendar = setInterval(function() {
            var calendarEl = document.querySelector(".toastui-calendar-layout");
            if (calendarEl) {
              clearInterval(checkCalendar);
              
              $(document).on("mouseup", "#booking_calendar .toastui-calendar-timegrid-gridline, #booking_calendar .toastui-calendar-timegrid-time-column, #booking_calendar .toastui-calendar-column", function(e) {
                var $target = $(e.target).closest(".toastui-calendar-column");
                if ($target.length === 0) return;
                
                var $columns = $("#booking_calendar .toastui-calendar-column");
                var colIndex = $columns.index($target);
                
                var today = new Date();
                var dayOfWeek = today.getDay();
                var monday = new Date(today);
                monday.setDate(today.getDate() - (dayOfWeek === 0 ? 6 : dayOfWeek - 1));
                
                var clickedDate = new Date(monday);
                clickedDate.setDate(monday.getDate() + colIndex);
                
                var $gridContainer = $target.find(".toastui-calendar-timegrid-column");
                if ($gridContainer.length === 0) $gridContainer = $target;
                
                var offset = e.offsetY || 0;
                var containerHeight = $gridContainer.height() || 540;
                var hoursDisplayed = 9;
                var startHour = 8;
                
                var clickedHour = startHour + (offset / containerHeight) * hoursDisplayed;
                var hour = Math.floor(clickedHour);
                var minutes = Math.round((clickedHour - hour) * 60 / 30) * 30;
                if (minutes >= 60) { hour++; minutes = 0; }
                
                if (hour < 8) hour = 8;
                if (hour > 16) hour = 16;
                
                var startTime = ("0" + hour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                var endHour = hour + 1;
                if (endHour > 17) endHour = 17;
                var endTime = ("0" + endHour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                
                var dateStr = clickedDate.getFullYear() + "-" + 
                              ("0" + (clickedDate.getMonth() + 1)).slice(-2) + "-" + 
                              ("0" + clickedDate.getDate()).slice(-2);
                
                Shiny.setInputValue("calendar_click", {
                  date: dateStr,
                  startTime: startTime,
                  endTime: endTime,
                  isWeekend: false,
                  timestamp: new Date().getTime()
                }, {priority: "event"});
              });
            }
          }, 500);
          
          setInterval(function() {
            $(".toastui-calendar-ic-location").each(function() {
              if ($(this).attr("data-icon-changed") !== "true") {
                $(this).css({"background-image": "none", "font-size": "14px"}).html("").attr("data-icon-changed", "true");
              }
            });
          }, 100);
        });
      '))
    ),
    
    # Layout: Sidebar + Calendar
    div(class = "gcal-container", style = "display: flex !important; flex-direction: row !important; height: calc(100vh - 56px); width: 100%; overflow: hidden;",
      
      # Sidebar
      div(class = "gcal-sidebar", style = "width: 260px; min-width: 260px; flex-shrink: 0; background: #fff; border-right: 1px solid #dadce0; padding: 16px; overflow-y: auto;",
        div(class = "mini-calendar",
          dateInput(
            inputId = "mini_calendar",
            label = NULL,
            value = Sys.Date(),
            width = "100%"
          )
        ),
        
        hr(style = "margin: 16px 0; border-color: #dadce0;"),
        
        h6(icon("chevron-down", style = "margin-right: 4px;"), "My Calendars"),
        uiOutput("room_checkboxes_ui")
      ),
      
      # Main content
      div(class = "gcal-main", style = "flex: 1; display: flex; flex-direction: column; background: #fff; overflow: hidden;",
        
        # Toolbar
        div(class = "gcal-toolbar", style = "display: flex; align-items: center; padding: 8px 16px; border-bottom: 1px solid #dadce0; gap: 16px;",
          div(class = "gcal-toolbar-left",
            actionButton("btn_today", "Today", class = "btn-today"),
            actionButton("btn_prev", icon("chevron-left"), class = "btn-nav"),
            actionButton("btn_next", icon("chevron-right"), class = "btn-nav")
          ),
          
          div(class = "gcal-toolbar-center",
            uiOutput("week_display_ui")
          ),
          
          div(class = "gcal-toolbar-right",
            tags$span("Week View", style = "color: #5f6368; font-weight: 500; margin-right: 12px;"),
            actionButton("btn_refresh", icon("sync"), 
              class = "btn-nav", 
              title = "Refresh"
            )
          )
        ),
        
        # Calendar
        div(class = "gcal-calendar-wrapper", style = "flex: 1; min-height: 0;",
          calendarOutput("booking_calendar", height = "100%")
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Tab: Book Car
  # ---------------------------------------------------------------------------
  
  nav_panel(
    "Book Car",
    value = "car_calendar_tab",
    
    # Click handler for car calendar time grid
    tags$head(
      tags$script(HTML('
        $(document).ready(function() {
          $(document).on("shown.bs.tab", function(e) {
            if (e.target && e.target.textContent && e.target.textContent.includes("Book Car")) {
              setTimeout(function() {
                $(window).trigger("resize");
              }, 100);
            }
          });
          
          var checkCarCalendar = setInterval(function() {
            var carCalendarEl = document.querySelector("#car_calendar .toastui-calendar-layout");
            if (carCalendarEl) {
              clearInterval(checkCarCalendar);
              
              $(document).on("mouseup", "#car_calendar .toastui-calendar-timegrid-gridline, #car_calendar .toastui-calendar-timegrid-time-column, #car_calendar .toastui-calendar-column", function(e) {
                var $target = $(e.target).closest(".toastui-calendar-column");
                if ($target.length === 0) return;
                
                var $columns = $("#car_calendar .toastui-calendar-column");
                var colIndex = $columns.index($target);
                
                var today = new Date();
                var dayOfWeek = today.getDay();
                var monday = new Date(today);
                monday.setDate(today.getDate() - (dayOfWeek === 0 ? 6 : dayOfWeek - 1));
                
                var clickedDate = new Date(monday);
                clickedDate.setDate(monday.getDate() + colIndex);
                
                var $gridContainer = $target.find(".toastui-calendar-timegrid-column");
                if ($gridContainer.length === 0) $gridContainer = $target;
                
                var offset = e.offsetY || 0;
                var containerHeight = $gridContainer.height() || 1200;
                var hoursDisplayed = 24;
                var startHour = 0;
                
                var clickedHour = startHour + (offset / containerHeight) * hoursDisplayed;
                var hour = Math.floor(clickedHour);
                var minutes = Math.round((clickedHour - hour) * 60 / 30) * 30;
                if (minutes >= 60) { hour++; minutes = 0; }
                
                if (hour < 0) hour = 0;
                if (hour > 23) hour = 23;
                
                var startTime = ("0" + hour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                var endHour = hour + 1;
                if (endHour > 24) endHour = 24;
                var endTime = ("0" + endHour).slice(-2) + ":" + ("0" + minutes).slice(-2);
                
                var dateStr = clickedDate.getFullYear() + "-" + 
                              ("0" + (clickedDate.getMonth() + 1)).slice(-2) + "-" + 
                              ("0" + clickedDate.getDate()).slice(-2);
                
                Shiny.setInputValue("car_calendar_click", {
                  date: dateStr,
                  startTime: startTime,
                  endTime: endTime,
                  isWeekend: false,
                  timestamp: new Date().getTime()
                }, {priority: "event"});
              });
            }
          }, 500);
        });
      '))
    ),
    
    # Layout: Sidebar + Calendar
    div(class = "gcal-container", style = "display: flex !important; flex-direction: row !important; height: calc(100vh - 56px); width: 100%; overflow: hidden;",
      
      # Sidebar
      div(class = "gcal-sidebar", style = "width: 260px; min-width: 260px; flex-shrink: 0; background: #fff; border-right: 1px solid #dadce0; padding: 16px; overflow-y: auto;",
        div(class = "mini-calendar",
          dateInput(
            inputId = "mini_calendar_car",
            label = NULL,
            value = Sys.Date(),
            width = "100%"
          )
        ),
        
        hr(style = "margin: 16px 0; border-color: #dadce0;"),
        
        h6(icon("chevron-down", style = "margin-right: 4px;"), "Available Cars"),
        uiOutput("car_checkboxes_ui")
      ),
      
      # Main content
      div(class = "gcal-main", style = "flex: 1; display: flex; flex-direction: column; background: #fff; overflow: hidden;",
        
        # Toolbar
        div(class = "gcal-toolbar", style = "display: flex; align-items: center; padding: 8px 16px; border-bottom: 1px solid #dadce0; gap: 16px;",
          div(class = "gcal-toolbar-left",
            actionButton("btn_today_car", "Today", class = "btn-today"),
            actionButton("btn_prev_car", icon("chevron-left"), class = "btn-nav"),
            actionButton("btn_next_car", icon("chevron-right"), class = "btn-nav")
          ),
          
          div(class = "gcal-toolbar-center",
            uiOutput("week_display_car_ui")
          ),
          
          div(class = "gcal-toolbar-right",
            tags$span("Week View", style = "color: #5f6368; font-weight: 500; margin-right: 12px;"),
            actionButton("btn_refresh_car", icon("sync"), 
              class = "btn-nav", 
              title = "Refresh"
            )
          )
        ),
        
        # Calendar
        div(class = "gcal-calendar-wrapper", style = "flex: 1; min-height: 0;",
          calendarOutput("car_calendar", height = "100%")
        )
      )
    )
  ),
  
  # ---------------------------------------------------------------------------
  # Tab: Manage Rooms (Admin)
  # ---------------------------------------------------------------------------
  
  nav_panel(
    "Manage Rooms",
    value = "rooms_tab",
    uiOutput("rooms_admin_ui")
  ),
  
  # ---------------------------------------------------------------------------
  # Tab: Manage Cars (Admin)
  # ---------------------------------------------------------------------------
  
  nav_panel(
    "Manage Cars",
    value = "cars_tab",
    uiOutput("cars_admin_ui")
  ),
  
  # ---------------------------------------------------------------------------
  # Tab: Manage Departments (Admin)
  # ---------------------------------------------------------------------------
  
  nav_panel(
    "Manage Departments",
    value = "depts_tab",
    uiOutput("depts_admin_ui")
  )
)
