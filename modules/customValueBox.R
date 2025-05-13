# customValueBox.R
# customValueBox <- function(title, value, box_color) {
#   # Create a card-like HTML block using the provided title, value, and box color.
#   HTML(paste0(
#     "<div style='padding: 20px; border: 2px solid ", box_color, 
#          "; border-radius: 5px; background-color: #f9f9f9;'>",
#       "<h4 style='font-size: 20px; margin-top: 0; margin-bottom: 10px; color: ", box_color, ";'>", title, "</h4>",
#       "<p style='font-size: 16px; font-weight: bold; margin-bottom: 0; color: ", box_color, ";'>", 
#         value,
#       "</p>",
#     "</div>"
#   ))
# }


customValueBox <- function(title, value, box_color) {
  HTML(paste0(
    "<div class='custom-value-box' style='border-color:", box_color, "'>",
      "<h4 class='custom-value-title' style='color:", box_color, "'>", title, "</h4>",
      "<p class='custom-value-value' style='color:", box_color, "'>", value, "</p>",
    "</div>"
  ))
}
