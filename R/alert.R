#' @title Alert
#'
#' @description
#' Creates an Alert class
#'
#' @export
#'
Alert <- R6::R6Class(
  classname = "Alert",
  public = list(
    build = function(header, section, color = "#FFAA00") {
      header <- list(type = "header", text = list(type = "plain_text", text = header))
      section <- list(type = "section", text = list(type = "mrkdwn", text = substr(section, 1L, 3000L)))
      private$.message <- list(color = color, blocks = list(header, section))
      invisible(self)
    },
    send = function(url = Sys.getenv("SLACK_URL"),
                    token = Sys.getenv("SLACK_TOKEN"),
                    channel = Sys.getenv("SLACK_CHANNEL"),
                    username = Sys.getenv("SLACK_USERNAME"),
                    emoji = Sys.getenv("SLACK_EMOJI")) {
      url <- as.character(x = url)
      token <- as.character(x = token)
      channel <- as.character(x = channel)
      username <- as.character(x = username)
      if (is.null(private$.message)) stop("Build the alert first")
      body <- list(channel = channel,
                   username = username,
                   icon_emoji = emoji,
                   attachments = list(private$.message))
      httr::POST(url = url, httr::content_type_json(),
                 httr::add_headers(Authorization = token),
                 body = yyjsonr::write_json_str(body, opts = list(auto_unbox = TRUE)))
    }
  ),
  active = list(
    message = function() private$.message
  ),
  private = list(
    .message = NULL
  )
)
