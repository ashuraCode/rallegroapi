#' A package for scrapping data from allegro.pl.
#'
#' @section Functions:
#' \code{\link{allegro_page_dump}}
#'
#' \code{\link{allegro_serch_dump}}
#'
#' \code{\link{allegro_category_dump}}
#'
#' \code{\link{allegro_user_dump}}
#'
#' \code{\link{allegro_categories}}
#'
#' @docType package
#' @name rallegroapi
NULL


.onLoad <- function(libname, pkgname){

}

#' Allegro sorting orders
#'
#' \describe{
#'   \item{trafnosc}{Sorting by accuracy}
#'   \item{cena_rosnaco}{Sorting by price ascending}
#'   \item{cena_malejaco}{Sorting by price descending}
#'   \item{cena_z_przesylka_rosnaco}{Sorting by price with supply ascending}
#'   \item{cena_z_przesylka_malejaco}{Sorting by price with supply descending}
#'   \item{popularnosc}{Sorting by popularity}
#'   \item{czas_do_konca}{Sorting by time to the end ascending}
#'   \item{czas_dodania}{Sorting by time of auction}
#' }
#'
#' @examples
#' allegro_order$trafnosc
#'
#' @export
allegro_order <- list(
  'trafnosc' = 'm',
  'cena_rosnaco' = 'p',
  'cena_malejaco' = 'pd',
  'cena_z_przesylka_rosnaco' = 'd',
  'cena_z_przesylka_malejaco' = 'dd',
  'popularnosc' = 'qd',
  'czas_do_konca' = 't',
  'czas_dodania' = 'n'
)

#' String to number conversion
#'
#' @param num A string
#' @return Number or Na
as_int <- function(num) {
  if (is.null(num))
    return (NA_integer_)
  as.integer(num)
}

#' String to number conversion
#'
#' @param dbl A string
#' @return Number or Na
as_dbl <- function(dbl) {
  if (is.null(dbl))
    return (NA_real_)
  as.double(dbl)
}

#' String to logic value conversion
#'
#' @param lgl A string
#' @return True or false
as_lgl <- function(lgl) {
  if (is.null(lgl))
    return (FALSE)
  as.logical(lgl)
}

#' Dump request to prettified json file
#'
#' @param base_url A url
#' @param query Query list
#' @param file A file name
#' @examples
#' allegro_page_dump("https://allegro.pl/mapa-strony/kategorie", list(), "kategorie.json")
#'
#' @import httr jsonlite
#' @export
allegro_page_dump <- function(base_url, query, file) {
  url <- base_url

  handle_reset(url)
  handle <- handle_find(url)

  response <- GET(url, query = query, handle = handle)
  content <- content(response, as = 'text')

  content <- prettify(content, indent = 4)

  write(content, file)
}

#' Scrapping auctions
#'
#' @param base_url A url
#' @param query Query list
#' @param pageLimit Page limit (-1 means all)
#' @return Tibble of auctions
#' @examples
#' allegro_scrap_pages("https://allegro.pl/kategoria/elektronika-67193", list(), 1)
#'
#' @import tidyverse httr dplyr tibble purrr
#' @export
allegro_scrap_pages <- function(base_url, query, pageLimit = -1) {
  url <- base_url
  query$p <- 1

  handle_reset(url)
  handle <- handle_find(url)

  response <- GET(url, query = query, handle = handle)
  content <- content(response)

  pageCount <- content[['listing']][['searchMeta']][['lastAvailablePage']]
  if (is.null(pageCount)) {
    pageCount <- content[['listing-user-layout']][['searchMeta']][['lastAvailablePage']]
  }

  if (pageLimit > 0) {
    if (pageLimit < pageCount){
      pageCount <- pageLimit
    }
  }

  data <- tibble::tibble()

  repeat {
    items <- content[['pagination bottom']][['collection']][['items']] %>%
      { c(.$promoted, .$products, .$bundled, .$sponsored, .$regular, .$featured) } %>%
      { tibble::tibble(
        id = map(., "id"),
        name = map(., "name"),
        url = map(., "url"),
        location = map(., function(entry){ entry$location$city }),
        seller = map(., function(entry){ entry$seller$login }),
        sellerId = map(., function(entry){ entry$seller$id }),
        superSeller = map_lgl(., function(entry){ as_lgl(entry$seller$superSeller) }),
        sellerRating = map_dbl(., function(entry){ as_dbl(entry$seller$positiveFeedbackPercent) }),
        price = map_dbl(., function(entry){ as_dbl(entry$sellingMode$buyNow$price$amount) }),
        currency = map(., function(entry){ entry$sellingMode$buyNow$price$currency }),
        rating = map_dbl(., function(entry){ as_dbl(entry$productReview$rating$average) }),
        ratingCount = map_int(., function(entry){ as_int(entry$productReview$rating$count) }),
        quantity = map_int(., function(entry){ as_int(entry$quantity$value) }),
        type = map(., function(entry){ entry$type })
      )}
    data <- bind_rows(data, items)

    query$p <- query$p + 1
    if (query$p > pageCount){
      break
    }

    response <- GET(url, query = query, handle = handle)
    content <- content(response)
  }

  return (data)
}

#' Scrapping auctions by search value
#'
#' @param string A search value
#' @param file A file name
#' @param order One of values from \link{allegro_order}
#' @param pageLimit Page limit (-1 means all)
#' @examples
#' allegro_serch_dump("herbata", "herbata.xlsx", allegro_order$trafnosc, 1)
#'
#' @import openxlsx
#' @export
allegro_serch_dump <- function(string, file, order = 'm', pageLimit = -1) {
  url <- "https://allegro.pl/listing"
  query <- list(
    string = string,
    bmatch = 'baseline-eyesa2-dict43-sup-1-2-0205',
    order = order,
    p = 1
  )

  data <- allegro_scrap_pages(url, query, pageLimit)

  write.xlsx(data, file, asTable = T)
}

#' Scrapping auctions by category
#'
#' @param string A category
#' @param file A file name
#' @param order One of values from allegro_order
#' @param pageLimit Page limit (-1 means all)
#' @examples
#' allegro_category_dump("elektronika-67193", "elektronika-67193.xlsx", allegro_order$trafnosc, 1)
#'
#' @import openxlsx
#' @export
allegro_category_dump <- function(category, file, order = 'm', pageLimit = -1) {
  url <- str_c("https://allegro.pl/kategoria", category, sep = '/')
  query <- list(
    bmatch = 'baseline-eyesa2-dict43-sup-1-2-0205',
    order = order,
    p = 1
  )

  data <- allegro_scrap_pages(url, query, pageLimit)

  write.xlsx(data, file, asTable = T)
}

#' Scrapping auctions by user auctions
#'
#' @param string A user name
#' @param file A file name
#' @param order One of values from allegro_order
#' @param pageLimit Page limit (-1 means all)
#' @examples
#' allegro_user_dump("X-KOM_PL", "X-KOM_PL.xlsx", allegro_order$trafnosc, 1)
#'
#' @import openxlsx
#' @export
allegro_user_dump <- function(user, file, order = 'm', pageLimit = -1) {
  url <- str_c("https://allegro.pl/uzytkownik", user, sep = '/')
  query <- list(
    bmatch = 'baseline-eyesa2-dict43-sup-1-2-0205',
    order = order,
    p = 1
  )

  data <- allegro_scrap_pages(url, query, pageLimit)

  write.xlsx(data, file, asTable = T)
}

#' Scrapping all allegro categories
#'
#' @return Tibble of categories
#' @examples
#' allegro_categories()
#'
#' @import tidyverse httr
#' @export
allegro_categories <- function() {
  url <- "https://allegro.pl/mapa-strony/kategorie"
  query <- list()

  handle_reset(url)
  handle <- handle_find(url)

  response <- GET(url, query = query, handle = handle)
  content <- content(response)


  collect_kategorie <- function(items) {
    kategorie <- tibble(name = character(), url = character())
    for (item in items) {
      keys <- names(item)
      if (has_element(keys, 'children') && has_element(keys, 'name') &&
          has_element(keys, 'openInNewWindow') && has_element(keys, 'url')) {
        children <- item[['children']]
        if (length(children) != 0) {
          kategorie <- bind_rows(kategorie, collect_kategorie(children))
        } else {
          url <- item[['url']]
          if(str_starts(url, '/kategoria/.*')) {
            kategorie <- add_row(kategorie, name = item[['name']], url = str_sub(url, 12))
          }
        }
      }
    }
    return (kategorie)
  }

  kategorie <- tibble(name = character(), url = character())

  for (name in names(content)) {
    entry <- content[[name]]
    keys <- names(entry)
    if (has_element(keys, 'items')) {
      kategorie <- bind_rows(kategorie, collect_kategorie(entry[['items']]))
    }
  }

  return (kategorie)
}
