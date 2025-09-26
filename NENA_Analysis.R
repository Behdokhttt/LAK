
library(rENA)
library(tma)

data <- readxl::read_excel("C:\\Users\\kiafa\\Data\\LAK.xlsx", sheet = 1)

unitCols = c("Condition", "Participant_ID")
unique(data[, unitCols])

codeCols = c('Gamma','Theta','Wrong_Answer','Alpha','Beta','Correct_Answer')
conversationCols = c("Counter")
window.size.back = 73
groupVar = "Condition"
groups = c("Feedback", "NoFeedback")
mean = TRUE
metaCols = c("CONFIDENCE.Change","CONFIDENCE.Pre","CONFIDENCE.Post","C.Change")

accum.ena = ena.accumulate.data(
  text_data = RS.data[, 'text'],
  units = data[,unitCols],
  conversation = data[,conversationCols],
  codes = data[,codeCols],
  window.size.back = 3
)

set.ena = ena.make.set(
  enadata = accum.ena,
  rotation.by = ena.rotate.by.mean,
  rotation.params = list(
    accum.ena$meta.data$Condition=="Feedback",
    accum.ena$meta.data$Condition=="NoFeedback"
  )
)

first.game.lineweights = as.matrix(set.ena$line.weights$Condition$Feedback)
second.game.lineweights = as.matrix(set.ena$line.weights$Condition$NoFeedback)

first.game.mean = as.vector(colMeans(first.game.lineweights))
second.game.mean = as.vector(colMeans(second.game.lineweights))

ena.plot(set.ena, title = "Feedback mean plot") |> ena.plot.network(network = first.game.mean, colors = c("red"))
ena.plot(set.ena, title = "NoFeedback mean plot") |> ena.plot.network(network = second.game.mean, colors = c("blue"))

subtracted.mean = first.game.mean - second.game.mean
ena.plot(set.ena, title = "Subtracted: `Feedback` (red) - `NoFeedback` (blue)")  |>
  ena.plot.network(network = subtracted.mean * 2, colors = c("red", "blue"))

first.game.points = as.matrix(set.ena$points$Condition$Feedback)
second.game.points = as.matrix(set.ena$points$Condition$NoFeedback)

ena.plot(set.ena, title = " points (dots), mean point (square), and confidence interval (box)") |> 
  ena.plot.points(points = first.game.points, colors = c("red")) |> 
  ena.plot.group(point = first.game.points, colors =c("red"), confidence.interval = "box")

ena.plot(set.ena, title = "Feedback mean network and its points") |> 
  ena.plot.network(network = first.game.mean, colors = c("red")) |>
  ena.plot.points(points = first.game.points, colors = c("red")) |> 
  ena.plot.group(point = first.game.points, colors =c("red"), confidence.interval = "box") 

ena.plot(set.ena, title = " points (dots), mean point (square), and confidence interval (box)") |> 
  ena.plot.points(points = second.game.points, colors = c("blue")) |> 
  ena.plot.group(point = second.game.points, colors =c("blue"), confidence.interval = "box")

ena.plot(set.ena, title = "NoFeedback mean network and its points") |> 
  ena.plot.network(network = second.game.mean, colors = c("blue")) |>
  ena.plot.points(points = second.game.points, colors = c("blue")) |> 
  ena.plot.group(point = second.game.points, colors =c("blue"), confidence.interval = "box")

ena.plot(set.ena, title = "Subtracted mean network: `Feedback` (red) - `NoFeedback` (blue)")  |>
  ena.plot.network(network = subtracted.mean * 2, colors = c("red", "blue")) |>
  ena.plot.points(points = first.game.points, colors = c("red")) |> 
  ena.plot.group(point = first.game.points, colors =c("red"), confidence.interval = "box") |>
  ena.plot.points(points = second.game.points, colors = c("blue")) |> 
  ena.plot.group(point = second.game.points, colors =c("blue"), confidence.interval = "box")

unit.A.line.weights = as.matrix(set.ena$line.weights$ENA_UNIT$`Feedback.P2`)
unit.A.point = as.matrix(set.ena$points$ENA_UNIT$`Feedback.P2`)

ena.plot(set.ena, title = "Individual network: `Feedback.P2") |> 
  ena.plot.network(network = unit.A.line.weights, colors = c("red")) |>
  ena.plot.points(points = unit.A.point, colors = c("red"))

unit.B.line.weights = as.matrix(set.ena$line.weights$ENA_UNIT$`NoFeedback.P4`)
unit.B.point = as.matrix(set.ena$points$ENA_UNIT$`NoFeedback.P4`)

ena.plot(set.ena, title = "Individual network: `NoFeedback.P4") |> 
  ena.plot.network(network = unit.B.line.weights, colors = c("blue")) |>
  ena.plot.points(points = unit.B.point, colors = c("blue"))

ena.plot(set.ena, title = "Subtracted network: `Feedback.P2 (red) - `NoFeedback.P4 (blue)")  |>
  ena.plot.network(network = (unit.A.line.weights - unit.B.line.weights) * 5, colors = c("red", "blue")) |>
  ena.plot.points(points = unit.A.point, colors = c("red")) |> 
  ena.plot.points(points = unit.B.point, colors = c("blue"))

p <- ena.plotter(set.ena,
               points = T,
               mean = T, 
               network = T,
               print.plots = T,
               groupVar = "Condition",
               groups = c("NoFeedback","Feedback"),
               subtractionMultiplier = 2)
class(p) <- c("plotly", "enaplot", "html-fill-item-overflow-hidden", "html-fill-item", class(p))

library(lsr)

ena_first_points_d1 = as.matrix(set.ena$points$Condition$Feedback)[,1]
ena_second_points_d1 = as.matrix(set.ena$points$Condition$NoFeedback)[,1]
ena_first_points_d2 = as.matrix(set.ena$points$Condition$Feedback)[,2]
ena_second_points_d2 = as.matrix(set.ena$points$Condition$NoFeedback)[,2]

t_test_d1 = t.test(ena_first_points_d1, ena_second_points_d1)
t_test_d1
t_test_d2 = t.test(ena_first_points_d2, ena_second_points_d2)
t_test_d2

set.ena$model$variance
ena.correlations(set.ena)

regression_data = set.ena$points
regression_data$CONFIDENCE.Change = as.numeric(regression_data$CONFIDENCE.Change)
condition_regression = lm(CONFIDENCE.Change ~ MR1 + SVD2 + Condition, data = regression_data, na.action = na.omit)
summary(condition_regression)
