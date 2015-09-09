context("load_images")

## TODO: Rename context
## TODO: Add more tests

test_that("PNG image loading works", {
  demo_directory <- system.file('extdata', 'Demo-Eye', 'Pre', package = 'EyeCanalogram')
  demo_root <- file.path(demo_directory, 'Pre-')
  images <- read.images(demo_root, low = 10, n = 3)

  expect_is(images, 'CanalogramImages')
})
