test_that("formatted_date", {
  # Caso 1: Chequear si formatea correctamente una fecha
  expect_equal(formatted_date("2022-12-21 12:30:00"), "2022-12-21")
  
  # Caso 2: Puede manejar otros formatos?
  expect_equal(formatted_date("2023/01/15 08:45:00"), "2023-01-15")
  
  # Caso 3: Medianoche
  expect_equal(formatted_date("2024-05-10 00:00:00"), "2024-05-10")
})
