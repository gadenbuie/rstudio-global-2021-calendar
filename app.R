library(shiny)
library(bslib)
library(lubridate)
library(calendar)
library(reactable)
library(glue)
source("R/stack.R")
source("R/tz.R")

Sys.setenv(TZ = "UTC")

theme <- bs_theme(
  version = 4,
  bootswatch = "lux",
  primary = "#447099",
  secondary = "#75AADB",
  success = "#A4C689",
  warning = "#fdbe4b",
  info = "#CBD4DD",
  "font-size-base" = "1rem"
)

schedule <- readr::read_csv("data/schedule.csv")
schedule$id <- seq_len(nrow(schedule))
year(schedule$time_gmt) <- 2021

ui <- navbarPage(
  'rstudio::global("schedule")',
  theme = theme,
  collapsible = TRUE,
  tabPanel(
    title = "Schedule",
    id = "schedule",
    div(
      class = "container-fluid",
      style = "max-width: 1600px",
      div(
        class = "row",
        div(
          class = "col-lg-3 order-1 order-lg-2 sidebar",
          uiOutput("your_talks"),
          selectInput("tz", "Your Timezone", choices = available_timezones(), selected = "UTC", width = "100%"),
          div(
            class = "row",
            div(
              class = "col-6 col-lg-12",
              textInput("sch_search", "Search", width = "100%"),
              radioButtons("sch_day", "Day", c("First" = "one", "Second" = "two", "All" = "all"), inline = TRUE, selected = c("all"), width = "100%"),
              sliderInput("sch_hours", "Hours in Your Time Zone", value = c(0, 24), min = 0, max = 24, step = 1, post = ":00", width = "100%")
            ),
            div(
              class = "col-6 col-lg-12",
              selectizeInput("sch_type", "Talk Type", choices = sort(unique(schedule$type)), multiple = TRUE, width = "100%"),
              selectizeInput("sch_topic", "Talk Topic", choices = sort(unique(schedule$topic)), multiple = TRUE, width = "100%"),
              selectizeInput("sch_presenter", "Presenter", choices = sort(unique(schedule$name)), multiple = TRUE, width = "100%")
            )
          )
        ),
        div(
          class = "col-lg-9 order-2 order-lg-1",
          reactable::reactableOutput("schedule"),
          singleton(
            tags$script(HTML(
              "$(document).on('click', '.btn-talk-more-info, .btn-talk-more-info i', function(ev) {
                Shiny.setInputValue('talk_more_info', ev.target.closest('.btn').dataset.value, {priority: 'event'})
              })

              document.querySelector('.navbar-brand').classList.add('text-monospace')

              $(document).on('shiny:sessioninitialized', function() {
                Shiny.setInputValue('browser_tz', Intl.DateTimeFormat().resolvedOptions().timeZone)
              })
              "))
          )
        )
      )
    )
  ),
  tabPanel(
    title = "About",
    id = "about",
    div(
      class = "container-fluid",
      style = "max-width: 900px",
      h2(
        class = "text-monospace",
        "community %>% tidyr::gather()"
      ),
      p("January 21, 2021 at 8am PT / 16:00 GMT / 01:00 JST"),
      p(
        "Our goal is to make rstudio::global(2021) our most inclusive and",
        "global event, making the most of the freedom from geographical and",
        "economic constraints that comes with an online event. That means that",
        "the conference will be free, designed around participation from every",
        "time zone, and have speakers from around the world."
      ),
      p(
        a(
          "Register Now",
          href = "https://global.rstudio.com/student/authentication/register",
          class = "btn btn-primary"
        ),
        a(
          tags$a(
            href = "https://global.rstudio.com/student/all_events",
            class = "btn btn-success",
            "Official Schedule"
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  selected_talks <- Stack$new()
  selected_in_current_view <- reactiveVal()

  schedule_view <- reactive({
    if (isTruthy(input$sch_day)) {
      if (input$sch_day == "one") {
        schedule <- schedule[schedule$time_gmt < ymd_hms("2021-01-22 04:00:00", tz = "UTC"), ]
      } else if (input$sch_day == "two") {
        schedule <- schedule[schedule$time_gmt >= ymd_hms("2021-01-22 04:00:00", tz = "UTC"), ]
      }
    }
    schedule$time <- with_tz(schedule$time_gmt, input$tz)
    if (isTruthy(input$sch_hours)) {
      schedule <- schedule[
        hour(schedule$time) >= input$sch_hours[1] & hour(schedule$time) <= input$sch_hours[2],
      ]
    }
    if (shiny::isTruthy(input$sch_search)) {
      schedule <- schedule[grepl(input$sch_search, tolower(paste(schedule$title_text, schedule$abstract_text))), ]
    }
    if (isTruthy(input$sch_type)) {
      schedule <- schedule[schedule$type %in% input$sch_type, ]
    }
    if (isTruthy(input$sch_topic)) {
      schedule <- schedule[schedule$topic %in% input$sch_topic, ]
    }
    if (isTruthy(input$sch_presenter)) {
      schedule <- schedule[schedule$name %in% input$sch_presenter, ]
    }
    schedule$info <- schedule$talk_id
    schedule <- schedule[, c("id", "talk_id", "time", "duration", "type", "track", "info", "title_text", "name", "topic")]
    schedule
  })

  selected_by_user_current_view <- reactive(getReactableState("schedule", "selected"))

  observeEvent(selected_by_user_current_view(), {
    current <- selected_talks$stack()
    on.exit(ignore_schedule_change(FALSE))
    if (!is.null(current) && is.null(selected_by_user_current_view()) && ignore_schedule_change()) {
      return()
    }
    in_view <- intersect(current, schedule_view()$id)

    if (is.null(selected_by_user_current_view()) && length(in_view)) {
      selected_talks$remove(in_view)
      return()
    }

    selected <- schedule_view()$id[selected_by_user_current_view()]

    talks_to_add <- setdiff(selected, current)
    talks_to_drop <- setdiff(in_view, selected)

    if (length(talks_to_add)) {
      selected_talks$add(talks_to_add)
    }
    if (length(talks_to_drop)) {
      selected_talks$remove(talks_to_drop)
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)

  output$your_talks <- renderUI({
    req(selected_talks$stack())
    tagList(
      downloadButton(
        "download_calendar",
        class = "d-block mb-3",
        glue("Download Calendar ({n} talks)", n = length(selected_talks$stack()))
      ),
      p(class = "text-right", actionLink("reset", "Reset Selection"))
    )
  })

  output$download_calendar <- downloadHandler(
    filename = "rstudio-global-talks.ics",
    content = function(file) {
      talks <- schedule[schedule$id %in% selected_talks$stack(), ]
      talks$start_time <- with_tz(talks$time_gmt, tzone = input$tz)
      talks$end_time <- talks$start_time + seconds(talks$duration)
      talk_events <- lapply(seq_len(nrow(talks)), function(idx) {
        ev <- calendar::ic_event(
          start_time = talks$start_time[[idx]],
          end_time = talks$end_time[[idx]],
          summary = talks$title_text[[idx]]
        )
        ev$DESCRIPTION <- paste0(talks$abstract_text[[idx]], "\n\nPresenter: ", talks$name[[idx]], "\n\nLink: ", talks$url[[idx]])
        ev
      })
      calendar::ic_write(do.call(rbind, talk_events), file)
    }
  )

  observeEvent(input$reset, {
    selected_talks$update(NULL)
    reactable::updateReactable(
      "schedule",
      selected = NA
    )
  })

  ignore_schedule_change <- reactiveVal(FALSE)

  output$schedule <- reactable::renderReactable({
    ignore_schedule_change(TRUE)
    reactable(
      schedule_view(),
      selection = "multiple",
      defaultSelected = which(schedule_view()$id %in% isolate(selected_talks$stack())),
      columns = list(
        talk_id = colDef(show = FALSE),
        id = colDef(show = FALSE),
        time = colDef(
          name = "Time",
          html = TRUE,
          cell = function(value) strftime(value, '<span class="white-space:pre;">%a %b %d</span> %H:%M', tz = input$tz)
        ),
        duration = colDef(
          name = "Length",
          minWidth = 80,
          cell = function(value, index) prettyunits::pretty_sec((value %/% 60) * 60)
        ),
        type = colDef(
          name = "Type",
          html = TRUE,
          cell = function(value) {
            value <- paste(value)
            glue(
              '<span class="badge badge-pill badge-{type}">{value}</span>',
              type = switch(
                value,
                keynote = "primary",
                lightning = "warning",
                talk = "success",
                "light"
              ),
              value = paste0(toupper(substr(value, 1, 1)), substr(value, 2, nchar(value)))
            )
          }
        ),
        track = colDef(
          name = "Track",
          html = TRUE,
          minWidth = 80,
          cell = function(value) {
            if (!is.na(value)) {
              glue(
                '<span class="badge badge-pill badge-{type}">{value}</span>',
                type = switch(
                  paste(value),
                  A = "secondary",
                  B = "info",
                  C = "dark",
                  "light"
                )
              )
            }
          }
        ),
        topic = colDef(name = "Topic", minWidth = 150),
        name = colDef(name = "Presenter", minWidth = 200),
        title_text = colDef(name = "Title", minWidth = 300),
        info = colDef(
          name = "",
          html = TRUE,
          minWidth = 80,
          cell = function(value) {
            if (!isTruthy(value)) return()
            tags$button(
              class = "btn btn-light btn-talk-more-info",
              `data-value` = value,
              title = "More info...",
              icon("info")
            )
          }
        )
      )
    )
  })

  observeEvent(input$talk_more_info, {
    talk <- schedule[!is.na(schedule$talk_id) & schedule$talk_id == as.numeric(input$talk_more_info), ]
    req(nrow(talk))

    speaker_img_slug <- tolower(gsub(" ", "", talk$name[[1]]))
    speaker_img <- if (file.exists(file.path("www", "speakers", paste0(speaker_img_slug, ".png")))) {
      file.path("speakers", paste0(speaker_img_slug, ".png"))
    } else if (file.exists(file.path("www", "speakers", paste0(speaker_img_slug, ".jpg")))) {
      file.path("speakers", paste0(speaker_img_slug, ".jpg"))
    }

    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        title = talk$title_text[[1]],
        h2("Abstract"),
        HTML(talk$abstract_html[[1]]),
        h2(talk$name[[1]]),
        if (!is.null(speaker_img)) {
          div(
            class = "row",
            div(
              class = "col-sm-3 order-1 order-sm-2",
              tags$img(src = speaker_img, style = "max-width: 100%", class = "rounded-lg")
            ),
            div(
              class = "col-sm-9 order-2 order-sm-1",
              HTML(talk$bio_html[[1]])
            )
          )
        } else HTML(talk$bio_html[[1]]),
        footer = list(
          tags$a(
            href = talk$url[[1]],
            class = "btn btn-success",
            target = "_blank",
            "Go To Talk Page"
          ),
          modalButton("OK")
        )
      )
    )
  })

  observeEvent(input$browser_tz, {
    if (input$browser_tz %in% OlsonNames()) {
      updateSelectInput(session, "tz", selected = input$browser_tz)
    }
  })
}

shinyApp(ui = ui, server = server)
