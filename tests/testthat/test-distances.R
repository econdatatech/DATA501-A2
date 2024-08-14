x1 <- rnorm(100, 50, 9)
x2 <- rnorm(100, 200, 64)
error <- rnorm(100, 0, 16)
# Generate the dependent variable (b0=150, b1=-4, b2=2.5)
y1 <- 150 - (4 * x1) + (2.5 * x2) + error

randomdata <- data.frame(y1, x1, x2)
# Fit a linear regression model
# Predicting y1 based on x1 and x2
model <- lm(y1 ~ x1 + x2, data = randomdata)

test_that("we test wrong data type of the first parameter of distances()", {
  expect_error(
    distances(data = c(2, 3, NA, 2, 4, 4), model = model),
    "The input data must be of type data.frame."
  )
})

test_that("we test wrong data type of the second parameter of distances()", {
  expect_error(
    distances(data = randomdata, model = TRUE),
    "The input model must be an object of class lm"
  )
})

test_that("we test wrong data type of the third parameter of distances()", {
  expect_error(
    distances(data = randomdata, model = model, plots = "VERYTRUE"),
    "The input parameter plots must be a boolean"
  )
})

test_that("We test distances() with correct input", {
  expect_no_error(distances(data = randomdata, model = model, plots = TRUE))
})
