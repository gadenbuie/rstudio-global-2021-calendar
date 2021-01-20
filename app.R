library(shiny)
library(bslib)
library(lubridate)
library(calendar)
library(reactable)
library(glue)
source("R/stack.R")
source("R/tz.R")
source("R/cards.R")

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
schedule$name <- gsub("\n", ", ", schedule$name)

ui <- navbarPage(
  'rstudio::global("schedule")',
  theme = theme,
  collapsible = TRUE,
  tabPanel(
    title = "Schedule",
    id = "schedule",
    # skip link
    a(
      "If you're using a screen reader, you may find the official ",
      "RStudio Global conference website is better suited. Do you want to go there now?",
      class = "screenreader-text",
      `tab-index` = 1,
      href = "https://global.rstudio.com/student/all_events"
    ),
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
          helpText(
            class = "mt-3",
            tags$a(
              href = "https://global.rstudio.com",
              code("rstudio::global")
            ),
            "will be held Thursday 2021-01-21 and Friday 2021-01-22."
          ),
          htmltools::htmlDependency(
            name = "rstudio-global-calendar",
            version = "0.0.1",
            src = "www",
            script = "extra.js",
            stylesheet = "extra.css"
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
      ),
      tags$hr(class = "my-4"),
      h2("About this app", class = "text-monospace"),
      p(
        HTML("This app was built with &#x2665;&#xFE0F; and &#x2615; by"),
        tags$a(href = "https://www.garrickadenbuie.com", "Garrick Aden-Buie", .noWS = "after"),
        ", using the packages listed below. Check out",
        tags$a(href = "https://github.com/gadenbuie/rstudio-global-2021-calendar", "the full source code"),
        "on Github."
      ),
      div(
        class = "d-flex flex-wrap align-items-stretch justify-content-between",
        card("shiny", rstudio_hex("shiny"), "https://shiny.rstudio.com", "Shiny is an R package that makes it easy to build interactive web apps straight from R."),
        card("renv", rstudio_hex("renv"), "https://rstudio.github.io/renv", "The renv package helps you create reproducible environments for your R projects. Use renv to make your R projects more: isolated, portable, and reproducible."),
        card("bslib", list(src = "https://camo.githubusercontent.com/3a4d3fbd6458e2fe5c0f5fb2df62878b9f74c2531c340a37599d875ae43a7d0e/68747470733a2f2f692e696d6775722e636f6d2f4b4c4b793173302e676966", alt = "Animated gif of bslib features"), "https://rstudio.github.io/bslib/", "Tools for creating custom Bootstrap themes, making it easier to style Shiny apps & R Markdown documents directly from R without writing unruly CSS and HTML."),
        card("R6", rstudio_hex("R6"), "https://r6.r-lib.org/", "Encapsulated object-oriented programming for R."),
        card("glue", rstudio_hex("glue"), "https://glue.tidyverse.org", "Glue strings to data in R. Small, fast, dependency free interpreted string literals."),
        card("lubridate", rstudio_hex("lubridate"), "https://lubridate.tidyverse.org", "Make working with dates in R just that little bit easier."),
        card("calendar", NULL, "https://github.com/ATFutures/calendar", "Create, read, write, and work with iCalander (.ics, .ical or similar) files in R."),
        card("reactable", NULL, "https://glin.github.io/reactable/index.html", "Interactive data tables for R, based on the React Table library and made with reactR."),
        card("prettyunits", NULL, "https://github.com/r-lib/prettyunits", "Pretty, human readable formatting of quantities."),
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
    schedule <- schedule[, c("id", "info", "talk_id", "type", "title_text", "name", "time", "duration", "track", "topic")]
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
        class = "d-block mb-3 btn-primary",
        glue(
          "Download Calendar ({n} talk{s})",
          n = length(selected_talks$stack()),
          s = if (length(selected_talks$stack()) == 1) "" else "s"
        )
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
        desc <- paste0("Presenter: ", talks$name[[idx]], "\n\n", talks$abstract_text[[idx]])
        desc <- gsub("\n", "\\n", desc, fixed = TRUE)
        desc <- strwrap(desc, 75)
        desc <- paste(desc, collapse = "\n ")
        desc <- gsub(",", "\\,", desc)
        ev <- calendar::ic_event(
          start_time = talks$start_time[[idx]],
          end_time = talks$end_time[[idx]],
          summary = talks$title_text[[idx]],
          more_properties = TRUE,
          event_properties = c(
            DESCRIPTION = desc,
            URL = talks$url[[idx]]
          )
        )
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
      highlight = TRUE,
      borderless = TRUE,
      columns = list(
        talk_id = colDef(show = FALSE),
        id = colDef(show = FALSE),
        time = colDef(
          name = "Time",
          html = TRUE,
          cell = function(value) strftime(value, '<span class="white-space:pre;">%a</span> %H:%M', tz = input$tz)
        ),
        duration = colDef(
          name = "Length",
          minWidth = 80,
          cell = function(value, index) prettyunits::pretty_sec((value %/% 60) * 60)
        ),
        type = colDef(
          name = "Type",
          html = TRUE,
          align = "center",
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
          align = "center",
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
        topic = colDef(name = "Topic", minWidth = 100, align = "center"),
        name = colDef(name = "Presenter", minWidth = 200),
        title_text = colDef(name = "Title", minWidth = 300),
        info = colDef(
          name = "",
          html = TRUE,
          minWidth = 60,
          sortable = FALSE,
          class = "cell-info-button",
          cell = function(value) {
            if (!isTruthy(value)) return()
            tags$button(
              class = "btn btn-light btn-talk-more-info",
              `data-value` = value,
              title = "More info...",
              icon("info")
            )
          },
          style = list(position = "sticky", left = 30, background = "#fff", zIndex = 1,
            borderRight = "2px solid #eee"),
          headerStyle = list(position = "sticky", left = 30, background = "#fff", zIndex = 1,
            borderRight = "2px solid #eee")
        ),
        .selection = colDef(
          width = 30,
          style = list(cursor = "pointer", position = "sticky", left = 0, background = "#fff", zIndex = 1),
          headerStyle = list(cursor = "pointer", position = "sticky", left = 0, background = "#fff", zIndex = 1)
        )
      )
    )
  })

  observeEvent(input$talk_more_info, {
    talk <- schedule[!is.na(schedule$talk_id) & schedule$talk_id == as.numeric(input$talk_more_info), ]
    req(nrow(talk))

    speaker_names <- strsplit(talk$name[[1]], ", ")[[1]]
    speaker_bios <- strsplit(talk$bio_html[[1]], "\n</p>\n<p>")[[1]]
    if (length(speaker_bios) == 2) {
      speaker_bios[1] <- paste0(speaker_bios[1], "</p>")
      speaker_bios[2] <- paste0("<p>", speaker_bios[2])
    }


    html_speaker_bio <- function(idx) {
      spkr_name <- speaker_names[idx]
      spkr_bio <- speaker_bios[idx]
      spkr_img <- tolower(gsub("[ '-]", "", spkr_name))
      spkr_img <- if (file.exists(file.path("www", "speakers", paste0(spkr_img, ".png")))) {
        file.path("speakers", paste0(spkr_img, ".png"))
      } else if (file.exists(file.path("www", "speakers", paste0(spkr_img, ".jpg")))) {
        file.path("speakers", paste0(spkr_img, ".jpg"))
      }
      tagList(
        h2(spkr_name),
        if (!is.null(spkr_img)) {
          div(
            class = "row",
            div(
              class = "col-sm-3 order-1 order-sm-2",
              tags$img(src = spkr_img, style = "max-width: 100%", class = "rounded-lg")
            ),
            div(
              class = "col-sm-9 order-2 order-sm-1",
              HTML(spkr_bio)
            )
          )
        } else HTML(spkr_bio)
      )
    }

    showModal(
      modalDialog(
        size = "l",
        easyClose = TRUE,
        title = talk$title_text[[1]],
        h2("Abstract"),
        HTML(talk$abstract_html[[1]]),
        lapply(seq_along(speaker_names), html_speaker_bio),
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
