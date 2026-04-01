######## Course info ########
library(dplyr)

# Start of semester
start_semester <- "2026-03-02"

# Week of mid-semester break
mid_semester_break <- "2026-04-06"

# Schedule
schedule <- tibble(
  Week = seq(12),
  Topic = c(
    "R tools and data structures",
    "Foundations of R programming",
    "R package development",
    "R programming with LLMs",
    "Debugging",
    "Functional programming",
    "Measuring and improving performance",
    "Object-oriented programming with S3",
    "Object-oriented programming with vctrs",
    "Metaprogramming",
    "Rewriting R code in C++",
    "Package hackathon"
  )
)

# Add mid-semester break
# Date here is Monday of each week
calendar <- tibble(
  Date = seq(as.Date(start_semester), by = "1 week", length.out = 13)
) |>
  mutate(
    Week = row_number(),
    Week = if_else(Date < mid_semester_break, Week, Week - 1),
    #Week =
  )

# Add calendar to schedule
schedule <- schedule |>
  left_join(calendar, by = "Week") |>
  mutate(
    Week = if_else(Date == mid_semester_break, NA, Week),
    Topic = if_else(Date == mid_semester_break, "Mid-semester break", Topic)
  ) |>
  select(Week, Date, everything())

# Add assignment details
lastmon <- function(x) {
  7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
}

assignments <- readr::read_csv(here::here("assignments.csv")) |>
  mutate(
    Date = lastmon(Due),
    Moodle = paste0(
      "https://learning.monash.edu/mod/assign/view.php?id=",
      Moodle
    ),
    File = paste0(
      "assignments/",
      tools::file_path_sans_ext(File),
      ".html"
    )
  )

schedule <- schedule |>
  full_join(assignments, by = "Date")

week_details <- tibble::tribble(
  ~Week, ~Summary, ~Prepare, ~ResourceLabel, ~ResourceUrl,
  1L, "Get your tooling in place and build a foundation in R data structures and everyday workflow.", "Install Positron, take the quick tour, and watch the intro videos before the workshop.", "Advanced R foundations", "https://adv-r.hadley.nz/foundations-intro.html",
  2L, "Work through subsetting, control flow, functions, environments, and conditions in base R.", "Watch the subsetting videos and finish the `linear()` warm-up exercise before class.", "Advanced R foundations", "https://adv-r.hadley.nz/foundations-intro.html",
  3L, "Learn the package-development workflow: metadata, documentation, checks, tests, and CI.", "Watch the package-building video and come ready to work inside an R package project.", "R Packages", "https://r-pkgs.org",
  4L, "Use LLM tooling productively for R programming inside Positron and with the ellmer package.", "Review the Positron AI videos and the ellmer documentation before the session.", "ellmer documentation", "https://ellmer.tidyverse.org/",
  5L, "Practice debugging workflows in R, including reprexes, traceback, browser, errors, and warnings.", "Watch the debugging videos and skim the online support resources before class.", "Advanced R debugging", "https://adv-r.hadley.nz/debugging.html",
  6L, "Use functional programming patterns, function arguments, and function factories to write cleaner code.", "Read the functional programming chapter and arrive ready to discuss reusable function design.", "Advanced R functional programming", "https://adv-r.hadley.nz/fp.html",
  7L, "Measure performance, profile bottlenecks, and improve the speed of R code where it matters.", "Read the Efficient R material and come with questions about profiling and optimisation.", "Efficient R", "https://csgillespie.github.io/efficientR/",
  8L, "Learn object-oriented programming concepts in R with a focus on S3 design and methods.", "Read the S3 chapter and skim the comparison resources if you need a broader OO refresher.", "Advanced R S3", "https://adv-r.hadley.nz/s3.html",
  9L, "Extend object-oriented programming with vctrs-based classes and a stronger type system.", "Review the vctrs documentation and the related Advanced R material before class.", "vctrs documentation", "https://vctrs.r-lib.org",
  10L, "Work with non-standard evaluation, abstract syntax trees, tidyselect, and code generation safely.", "Read the metaprogramming chapters in Advanced R before the workshop.", "Advanced R metaprogramming", "https://adv-r.hadley.nz/metaprogramming.html",
  11L, "Rewrite performance-critical code in C++ using Rcpp and related tooling.", "Make sure your compiler toolchain is installed before class so you can complete the exercises.", "Rcpp FAQ", "https://cran.r-project.org/package=Rcpp/vignettes/Rcpp-FAQ.pdf",
  12L, "Use the hackathon week to finish, polish, and troubleshoot your final package work in teams.", "Bring your package backlog, open issues, and final integration tasks to the workshop.", "Assignment 3", "./assignments/A3.html"
)

course_weeks <- schedule |>
  filter(!is.na(Date), !is.na(Topic)) |>
  arrange(Date)

get_current_week_context <- function(reference_date = Sys.Date()) {
  today <- as.Date(reference_date)
  current <- course_weeks |>
    filter(today >= Date, today < Date + 7)

  if (NROW(current) == 0) {
    if (today < min(course_weeks$Date)) {
      current <- course_weeks |>
        slice_head(n = 1)
    } else {
      current <- course_weeks |>
        filter(Date <= today) |>
        slice_tail(n = 1)
    }
  }

  next_week <- course_weeks |>
    filter(!is.na(Week), Date > current$Date) |>
    slice_head(n = 1)

  next_assignment <- assignments |>
    filter(Due >= today) |>
    arrange(Due) |>
    slice_head(n = 1)

  due_this_week <- assignments |>
    filter(Due >= current$Date, Due < current$Date + 7) |>
    arrange(Due)

  details <- tibble::tibble()
  if (!is.na(current$Week)) {
    details <- week_details |>
      filter(Week == current$Week)
  }

  list(
    today = today,
    current = current,
    details = details,
    next_week = next_week,
    next_assignment = next_assignment,
    due_this_week = due_this_week
  )
}

show_week_dashboard <- function(reference_date = Sys.Date()) {
  context <- get_current_week_context(reference_date)
  current <- context$current
  details <- context$details
  next_week <- context$next_week
  next_assignment <- context$next_assignment
  due_this_week <- context$due_this_week

  is_break <- is.na(current$Week)
  heading <- if (is_break) {
    "Mid-semester break"
  } else {
    paste0("Week ", current$Week, " · ", current$Topic)
  }

  summary <- if (is_break) {
    "No workshop this week. Use the break to catch up on recent material and prepare for the next teaching block."
  } else {
    details$Summary[[1]]
  }

  prepare <- if (is_break) {
    "Catch up on recordings, consolidate notes from earlier weeks, and get ahead on the next topic."
  } else {
    details$Prepare[[1]]
  }

  workshop <- if (is_break) {
    "No live workshop this week."
  } else {
    paste0(
      format(current$Date + 3, "%A %d %B"),
      " · 9:00am to 11:00am · LTB Room 121"
    )
  }

  next_up <- if (NROW(next_week) == 0) {
    "Final teaching week reached."
  } else {
    paste0(
      "Week ",
      next_week$Week,
      " starts ",
      format(next_week$Date + 3, "%d %b"),
      ": ",
      next_week$Topic
    )
  }

  assessment_text <- if (NROW(due_this_week) > 0) {
    if (due_this_week$Due[[1]] < context$today) {
      paste0(
        "This week's deadline: ",
        due_this_week$Assignment[[1]],
        " was due ",
        format(due_this_week$Due[[1]], "%A %d %B")
      )
    } else {
      paste0(
        due_this_week$Assignment[[1]],
        " is due ",
        format(due_this_week$Due[[1]], "%A %d %B")
      )
    }
  } else if (NROW(next_assignment) > 0) {
    paste0(
      "Next assessment: ",
      next_assignment$Assignment[[1]],
      " on ",
      format(next_assignment$Due[[1]], "%A %d %B")
    )
  } else {
    "No remaining assessment deadlines in the schedule."
  }

  week_page <- if (is_break) {
    if (NROW(next_week) > 0) paste0("./week", next_week$Week, "/index.html") else "./index.html"
  } else {
    paste0("./week", current$Week, "/index.html")
  }

  week_page_label <- if (is_break) "Preview next teaching week" else "Open week page"

  slides_url <- if (!is_break && fs::file_exists(here::here(paste0("docs/week", current$Week, "/slides.pdf")))) {
    paste0("./week", current$Week, "/slides.pdf")
  } else {
    NULL
  }

  resource_url <- if (!is_break) details$ResourceUrl[[1]] else if (NROW(next_assignment) > 0) next_assignment$File[[1]] else "./index.html"
  resource_label <- if (!is_break) details$ResourceLabel[[1]] else "Next assessment"

  html <- c(
    "<div class='week-dashboard'>",
    "<div class='week-dashboard__header'>",
    if (is_break) "<span class='week-dashboard__status week-dashboard__status--break'>Break week</span>" else "<span class='week-dashboard__status'>This week</span>",
    paste0("<h2>", heading, "</h2>"),
    paste0("<p>", summary, "</p>"),
    "</div>",
    "<div class='week-dashboard__grid'>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Prepare before class</span>",
    paste0("<p>", prepare, "</p>"),
    "</div>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Workshop</span>",
    paste0("<p>", workshop, "</p>"),
    "</div>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Assessment</span>",
    paste0("<p>", assessment_text, "</p>"),
    "</div>",
    "<div class='week-dashboard__card'>",
    "<span class='week-dashboard__label'>Coming up next</span>",
    paste0("<p>", next_up, "</p>"),
    "</div>",
    "</div>",
    "<div class='week-dashboard__actions'>",
    paste0("<a class='button-link' href='", week_page, "'>", week_page_label, "</a>"),
    if (!is.null(slides_url)) paste0("<a class='button-link button-link--ghost' href='", slides_url, "'>Slides PDF</a>"),
    paste0("<a class='button-link button-link--ghost' href='", resource_url, "'>", resource_label, "</a>"),
    "<a class='button-link button-link--ghost' href='https://learning.monash.edu/mod/lti/view.php?id=5219874'>Recordings</a>",
    "</div>",
    "</div>"
  )

  cat(paste(html, collapse = ""))
}

show_assignments <- function(week) {
  ass <- schedule |>
    filter(
      Week >= week,
      !is.na(Assignment),
    ) |>
    filter(Week == min(Week) | Week - week <= 2) |>
    select(Assignment:File)
  if (NROW(ass) > 0) {
    cat("\n\n## Assignments\n\n")
    for (i in seq(NROW(ass))) {
      cat(
        "* [",
        ass$Assignment[i],
        "](../",
        ass$File[i],
        ") is due on ",
        format(ass$Due[i], "%A %d %B.\n"),
        sep = ""
      )
    }
  }
}

submit <- function(schedule, assignment) {
  ass <- schedule |>
    filter(Assignment == assignment)
  due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
  url <- ass$Moodle
  button <- paste0(
    "<div class='assignment-cta'>",
    "<div>",
    "<p class='assignment-cta__label'>Submission</p>",
    "<p class='assignment-cta__due'>Due ", due, "</p>",
    "<p class='assignment-cta__text'>Open the Moodle submission page for instructions, links and final upload details.</p>",
    "</div>",
    "<a href='", url, "' class='button-link assignment-cta__button'>Open submission page</a>",
    "</div>"
  )
  cat(button)
}

show_slides <- function(week) {
  qmd_file <- here::here(paste0("week", week, "/slides.qmd"))
  slides_exist <- fs::file_exists(qmd_file)
  if (slides_exist) {
    pdf_file <- paste0("https://arp.numbat.space/week", week, "/slides.pdf")
    embed <- paste0(
      "<iframe class='slides-frame' src='",
      pdf_file,
      "' title='Slides for week ",
      week,
      "' loading='lazy'></iframe>"
    )
    button <- paste0(
      "<div class='resource-actions'><a href='",
      pdf_file,
      "' class='button-link button-link--ghost'>Download slides PDF</a></div>"
    )
    cat(paste0("## Slides for week\n\n", embed, "\n", button))
  }
}
