
if (!require(readxl)) install.packages("readxl")

library(readxl)
library(ona)
library(tma)

data <- read_excel("C:\\Users\\X\\Data\\LAK.xlsx", sheet = 1)

my_units <- c("Condition", "Participant_ID") 

my_codes = c(
  'Gamma',
  'Theta',
  'Wrong_Answer',
  'Alpha',
  'Beta',
  'Correct_Answer')


my_hoo_rules <- conversation_rules(
  (Counter %in% UNIT$Counter))


window_size = 3

metaCols = c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")


accum.ona <-
  contexts(data, 
           units_by = my_units, 
           hoo_rules = my_hoo_rules) |>
  accumulate_contexts(codes = my_codes, 
                      decay.function = decay(simple_window, window_size = 3),
                      meta.data = metaCols,
                      return.ena.set = FALSE)


set.ona <- 
  model(accum.ona)


set.ona <-
  model(accum.ona,
        rotate.using ="mean",
        rotation.params = 
          list(Feedback=accum.ona$meta.data$Condition=="Feedback",
               `NoFeedback`=accum.ona$meta.data$Condition=="NoFeedback")   
  )

node_size_multiplier = 0.4
node_position_multiplier = 1
point_position_multiplier =1.5
edge_arrow_saturation_multiplier = 1.5
edge_size_multiplier = 1


plot(set.ona, title = "Feedback (red) mean network") |>
  edges(
    weights =set.ona$line.weights$Condition$Feedback,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("red")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("red"))


plot(set.ona, title = "No-Feedback (blue) mean network") |>
  edges(
    weights = set.ona$line.weights$Condition$NoFeedback,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("blue")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("blue"))


plot(set.ona, title = "Subtracted mean network: `Feedback` (red) vs `No-Feedback` (blue)") |>
  edges(
    weights = (colMeans(set.ona$line.weights$Condition$Feedback) - colMeans(set.ona$line.weights$Condition$NoFeedback))*2,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("red","blue")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("red","blue"))


plot(set.ona, title = "points (dots), mean point (square), and confidence interval") |>
  units(
    points=set.ona$points$Condition$Feedback, 
    points_color = c("red"),
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE)


plot(set.ona, title = "Feedback (red) mean network") |>
  units(
    points=set.ona$points$Condition$Feedback, 
    points_color = c("red"),
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE) |>
  edges(
    weights =set.ona$line.weights$Condition$Feedback,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("red")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("red"))


plot(set.ona, title = "points (dots), mean point (square), and confidence interval") |>
  units(
    points=set.ona$points$Condition$NoFeedback, 
    points_color = c("blue"),
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE)


plot(set.ona, title = "No-Feedback (blue) mean network") |>
  units(
    points=set.ona$points$Condition$NoFeedback, 
    points_color = "blue", 
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE) |>
  edges(
    weights = set.ona$line.weights$Condition$NoFeedback,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("blue")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("blue"))


plot(set.ona, title = "Difference: `Feedback` (red) vs `No-Feedback` (blue)") |>
  units(
    points = set.ona$points$Condition$Feedback, 
    points_color = "red",
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE) |>
  units(
    points = set.ona$points$Condition$NoFeedback, 
    points_color = "blue",
    show_mean = TRUE, show_points = TRUE, with_ci = TRUE) |>
  edges(
    weights = (colMeans(set.ona$line.weights$Condition$Feedback) - colMeans(set.ona$line.weights$Condition$NoFeedback))*2,
    edge_size_multiplier = edge_size_multiplier,
    edge_arrow_saturation_multiplier = edge_arrow_saturation_multiplier,
    node_position_multiplier = node_position_multiplier,
    edge_color = c("red","blue")) |>
  nodes(
    node_size_multiplier = node_size_multiplier,
    node_position_multiplier = node_position_multiplier,
    self_connection_color = c("red","blue"))
