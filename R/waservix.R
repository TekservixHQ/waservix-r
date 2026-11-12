#' waservix: Client for the 'WAservix' 'WhatsApp' API Gateway
#'
#' WAservix WhatsApp API Gateway by Tekservix: send WhatsApp texts, images,
#' documents and locations from your own number, connected by QR code. You need
#' a Tekservix account with WAservix and a connected number. Each number has
#' its own device API key (it starts with `wpk_`) under WAservix -> Devices.
#' Create an account at <https://tekservix.com/waservix/>. API reference:
#' <https://tekservix.com/waservix/docs/>.
#'
#' Start with [waservix_client()], then call the other `waservix_*` functions.
#' Every call returns the `data` part of the API answer as a list. API errors
#' are raised as conditions of class `waservix_error`, which carry the HTTP
#' `status`, the API `message` and the raw `body`.
#'
#' @keywords internal
"_PACKAGE"

#' Create a WAservix client
#'
#' @param key The device API key of your WhatsApp number (starts with `wpk_`).
#'   Defaults to the `WASERVIX_API_KEY` environment variable.
#' @param base_url API base URL. Change it only for tests or a proxy.
#' @return An object of class `waservix_client`, used by every other function.
#' @export
#' @examples
#' client <- waservix_client("wpk_your_device_key")
waservix_client <- function(key = Sys.getenv("WASERVIX_API_KEY"),
                            base_url = "https://tekservix.com/waservix/api") {
  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop("A WAservix device API key is required (WAservix -> Devices in your Tekservix account).",
         call. = FALSE)
  }
  structure(list(key = key, base_url = sub("/+$", "", base_url)),
            class = "waservix_client")
}

#' @export
print.waservix_client <- function(x, ...) {
  cat("<waservix_client> ", x$base_url, "\n", sep = "")
  invisible(x)
}

#' Call any WAservix endpoint
#'
#' Low-level access for endpoints that have no helper of their own.
#'
#' @param client A client from [waservix_client()].
#' @param method HTTP method, e.g. `"GET"` or `"POST"`.
#' @param path Path after the base URL, e.g. `"/status"`.
#' @param body Optional list sent as JSON.
#' @param query Optional named list of query parameters.
#' @return The `data` part of the API answer, as a list.
#' @export
#' @examples
#' \dontrun{
#' client <- waservix_client("wpk_your_device_key")
#' waservix_request(client, "GET", "/status")
#' }
waservix_request <- function(client, method, path, body = NULL, query = NULL) {
  check_client(client)
  req <- httr2::request(paste0(client$base_url, path))
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(req, Authorization = paste("Bearer", client$key),
                            Accept = "application/json", .redact = "Authorization")
  req <- httr2::req_user_agent(req, "waservix-r/1.0.0")
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  query <- Filter(Negate(is.null), as.list(query))
  if (length(query)) req <- do.call(httr2::req_url_query, c(list(req), query))
  if (!is.null(body)) req <- httr2::req_body_json(req, body, auto_unbox = TRUE, null = "null")
  resp <- httr2::req_perform(req)
  handle_response(resp)
}

#' Send any WhatsApp message
#'
#' Sends to a number (`"+250788123456"`) or a group ID (ending in `@g.us`).
#' Field names follow the API: `text`, `imageUrl`, `videoUrl`, `audioUrl`,
#' `documentUrl`, `fileName`, `location`, `poll`, `replyTo`, `mentions`,
#' `viewOnce`.
#'
#' @inheritParams waservix_request
#' @param to Phone number in international format, or a group ID.
#' @param ... Message fields, named as in the API. `NULL` fields are dropped.
#' @return A list with the queued message ID and status.
#' @export
#' @examples
#' \dontrun{
#' client <- waservix_client("wpk_your_device_key")
#' waservix_send_message(client, "+250788123456",
#'                       videoUrl = "https://example.com/demo.mp4", text = "Demo")
#' }
waservix_send_message <- function(client, to, ...) {
  body <- compact(c(list(to = to), list(...)))
  waservix_request(client, "POST", "/send-message", body = body)
}

#' Send a text message
#'
#' @inheritParams waservix_send_message
#' @param text The message text.
#' @return A list with the queued message ID and status.
#' @export
#' @examples
#' \dontrun{
#' waservix_send_text(waservix_client("wpk_your_device_key"),
#'                    "+250788123456", "Your order #1042 has shipped.")
#' }
waservix_send_text <- function(client, to, text) {
  waservix_send_message(client, to, text = text)
}

#' Send an image
#'
#' @inheritParams waservix_send_message
#' @param url Public URL of the image.
#' @param caption Optional caption.
#' @return A list with the queued message ID and status.
#' @export
#' @examples
#' \dontrun{
#' waservix_send_image(waservix_client("wpk_your_device_key"), "+250788123456",
#'                     "https://example.com/catalogue.jpg", caption = "New arrivals")
#' }
waservix_send_image <- function(client, to, url, caption = NULL) {
  waservix_send_message(client, to, imageUrl = url, text = caption)
}

#' Send a document
#'
#' @inheritParams waservix_send_message
#' @param url Public URL of the file.
#' @param file_name Optional file name shown to the recipient.
#' @param caption Optional caption.
#' @return A list with the queued message ID and status.
#' @export
#' @examples
#' \dontrun{
#' waservix_send_document(waservix_client("wpk_your_device_key"), "+250788123456",
#'                        "https://example.com/inv-1042.pdf",
#'                        file_name = "Invoice-1042.pdf", caption = "Your invoice")
#' }
waservix_send_document <- function(client, to, url, file_name = NULL, caption = NULL) {
  waservix_send_message(client, to, documentUrl = url, fileName = file_name, text = caption)
}

#' Send a location pin
#'
#' @inheritParams waservix_send_message
#' @param latitude,longitude Coordinates in decimal degrees.
#' @param name Optional place name.
#' @return A list with the queued message ID and status.
#' @export
#' @examples
#' \dontrun{
#' waservix_send_location(waservix_client("wpk_your_device_key"), "+250788123456",
#'                        -1.9441, 30.0619, name = "Our shop")
#' }
waservix_send_location <- function(client, to, latitude, longitude, name = NULL) {
  loc <- compact(list(latitude = latitude, longitude = longitude, name = name))
  waservix_send_message(client, to, location = loc)
}

#' Get a sent message's status
#'
#' @inheritParams waservix_request
#' @param message_id Message ID.
#' @return The message and its status (queued, sent, delivered, read, played
#'   or failed).
#' @export
#' @examples
#' \dontrun{
#' waservix_get_message_info(waservix_client("wpk_your_device_key"), 5473)
#' }
waservix_get_message_info <- function(client, message_id) {
  waservix_request(client, "GET", paste0("/messages/", id(message_id), "/info"))
}

#' Edit a sent text
#'
#' WhatsApp allows edits for about 15 minutes after sending.
#'
#' @inheritParams waservix_get_message_info
#' @param text The new text.
#' @return The API confirmation.
#' @export
#' @examples
#' \dontrun{
#' waservix_edit_message(waservix_client("wpk_your_device_key"), 5473, "Corrected text")
#' }
waservix_edit_message <- function(client, message_id, text) {
  waservix_request(client, "PUT", paste0("/messages/", id(message_id)), body = list(text = text))
}

#' Delete a message for everyone
#'
#' @inheritParams waservix_get_message_info
#' @return The API confirmation.
#' @export
#' @examples
#' \dontrun{
#' waservix_delete_message(waservix_client("wpk_your_device_key"), 5473)
#' }
waservix_delete_message <- function(client, message_id) {
  waservix_request(client, "DELETE", paste0("/messages/", id(message_id)))
}

#' Resend a failed message
#'
#' @inheritParams waservix_get_message_info
#' @return The new send attempt.
#' @export
#' @examples
#' \dontrun{
#' waservix_resend_message(waservix_client("wpk_your_device_key"), 5473)
#' }
waservix_resend_message <- function(client, message_id) {
  waservix_request(client, "POST", paste0("/messages/", id(message_id), "/resend"))
}

#' Check whether a number uses WhatsApp
#'
#' @inheritParams waservix_request
#' @param phone Phone number in international format.
#' @return A list telling whether the number is on WhatsApp.
#' @export
#' @examples
#' \dontrun{
#' waservix_on_whatsapp(waservix_client("wpk_your_device_key"), "+250788123456")
#' }
waservix_on_whatsapp <- function(client, phone) {
  waservix_request(client, "GET", paste0("/on-whatsapp/", enc(phone)))
}

#' Get your number's connection status
#'
#' @inheritParams waservix_request
#' @return The connection status of the number behind the API key.
#' @export
#' @examples
#' \dontrun{
#' waservix_get_status(waservix_client("wpk_your_device_key"))
#' }
waservix_get_status <- function(client) {
  waservix_request(client, "GET", "/status")
}

#' List contacts
#'
#' @inheritParams waservix_request
#' @return A list of your number's contacts.
#' @export
#' @examples
#' \dontrun{
#' waservix_list_contacts(waservix_client("wpk_your_device_key"))
#' }
waservix_list_contacts <- function(client) {
  waservix_request(client, "GET", "/contacts")
}

#' Get one contact
#'
#' @inheritParams waservix_on_whatsapp
#' @return The contact.
#' @export
#' @examples
#' \dontrun{
#' waservix_get_contact(waservix_client("wpk_your_device_key"), "+250788123456")
#' }
waservix_get_contact <- function(client, phone) {
  waservix_request(client, "GET", paste0("/contacts/", enc(phone)))
}

#' List groups
#'
#' @inheritParams waservix_request
#' @return The groups your number is in; IDs end in `@g.us`.
#' @export
#' @examples
#' \dontrun{
#' waservix_list_groups(waservix_client("wpk_your_device_key"))
#' }
waservix_list_groups <- function(client) {
  waservix_request(client, "GET", "/groups")
}

#' Get a group's details
#'
#' @inheritParams waservix_request
#' @param group_id Group ID, ending in `@g.us`.
#' @return The group's name, participants and settings.
#' @export
#' @examples
#' \dontrun{
#' waservix_get_group_metadata(waservix_client("wpk_your_device_key"),
#'                             "120363000000000000@g.us")
#' }
waservix_get_group_metadata <- function(client, group_id) {
  waservix_request(client, "GET", paste0("/groups/", trimws(group_id), "/metadata"))
}

#' Verify a webhook delivery
#'
#' Compares the `X-Webhook-Signature` header with your number's webhook
#' secret. The comparison takes the same time whether or not they match.
#'
#' @param signature Value of the `X-Webhook-Signature` header.
#' @param secret Your number's webhook secret.
#' @return `TRUE` when the header matches the secret, otherwise `FALSE`.
#' @export
#' @examples
#' waservix_verify_webhook("my-secret", "my-secret")
#' waservix_verify_webhook("wrong", "my-secret")
waservix_verify_webhook <- function(signature, secret) {
  if (is.null(signature) || length(signature) != 1L || is.na(signature)) return(FALSE)
  if (is.null(secret) || length(secret) != 1L || is.na(secret) || !nzchar(secret)) return(FALSE)
  same_bytes(charToRaw(enc2utf8(signature)), charToRaw(enc2utf8(secret)))
}

# ---- internal ----------------------------------------------------------------

check_client <- function(client) {
  if (!inherits(client, "waservix_client")) {
    stop("`client` must come from waservix_client().", call. = FALSE)
  }
}

compact <- function(x) Filter(Negate(is.null), x)

id <- function(x) format(as.numeric(x), scientific = FALSE, trim = TRUE)

enc <- function(x) utils::URLencode(trimws(as.character(x)), reserved = TRUE)

same_bytes <- function(a, b) {
  if (length(a) != length(b) || length(a) == 0L) return(FALSE)
  sum(as.integer(xor(a, b))) == 0L
}

handle_response <- function(resp) {
  status <- httr2::resp_status(resp)
  text <- if (httr2::resp_has_body(resp)) httr2::resp_body_string(resp) else ""
  parsed <- if (nzchar(text)) {
    tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  }
  if (status >= 400) {
    msg <- if (is.list(parsed) && is.character(parsed$message)) parsed$message else paste("HTTP", status)
    cond <- structure(
      class = c("waservix_error", "error", "condition"),
      list(message = sprintf("WAservix API error %d: %s", status, msg), call = NULL,
           status = status, api_message = msg, body = text)
    )
    stop(cond)
  }
  if (is.list(parsed) && !is.null(names(parsed)) && "data" %in% names(parsed)) parsed$data else parsed
}
