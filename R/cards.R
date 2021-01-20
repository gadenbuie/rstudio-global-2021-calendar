card <- function(title, image, link, ..., class = "p-2 mb-3 border rounded w-100") {
  div(
    class = "col-sm-6 col-md-4",
    div(
      class = class,
      if (!is.null(image)) {
        div(
          class = "mb-3",
          style = "height: 200px",
          tags$img(class = "mx-auto d-block", src = image$src, alt = image$alt, style = "max-height: 200px; max-width: 100%"),
        )
      },
      div(
        h3(class = "text-uppercase", title),
        p(class = "card-text", ...),
        a(href = link, title, target = "_blank", class = "btn btn-secondary")
      )
    )
  )
}

rstudio_hex <- function(pkg) {
  list(
    src = glue("https://github.com/rstudio/hex-stickers/raw/master/PNG/{pkg}.png"),
    alt = glue("{pkg} R package hex sticker")
  )
}
