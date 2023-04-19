#' Wrap url with html <a> tag
#'
#' Creates an html hyperlink with the <a>
#' tag and target attribute set to open the link in a new tab.
#'
#' @param url URL of the page the link goes to.
#' @param text Character string to display. Defaults to url.
#'
#' @returns Character string with <a> tag.
href <- function(url, text = url) {
  paste0("<a href='", url, "' target='_blank'>", text, "</a>")
}
