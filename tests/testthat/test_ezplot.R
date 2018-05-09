context("ezplot basics")

library(magrittr)
td <- data.frame(f1 = sample(letters[1:5], 100, replace = TRUE),
                 f2 = sample(letters[6:10], 100, replace = TRUE),
                 c1 = rnorm(100) + 10,
                 c2 = rnorm(100) + 10)

test_that("ez_plot can make basic bar",
  {
    ez <- ezplot(td, x = "f1", y = "c1")
    gg <- ggplot(td, aes(x = f1, y = c1)) + geom_bar(stat = "identity")
    
    # plot_env is different, so skip
    ez$plot_env <- NULL
    gg$plot_env <- NULL
    expect_equal(ez, gg)
  })

test_that("ez_plot can make grouped bar",
          {
            ez <- ezplot(td, x = "f1", y = "c1", group = "f2")
            gg <- ggplot(td, aes(x = f1, y = c1)) + geom_bar(aes(group = f2, fill = f2), stat = "identity")
            
            # plot_env is different, so skip
            ez$plot_env <- NULL
            gg$plot_env <- NULL
            expect_equal(ez, gg)
          })

test_that("ez_plot can make clustered bar",
          {
            ez <- ezplot(td, x = "f1", y = "c1", group = "f2", group_method = "dodge")
            gg <- ggplot(td, aes(x = f1, y = c1)) + geom_bar(aes(group = f2, fill = f2), stat = "identity", position = "dodge")
            
            # plot_env is different, so skip
            ez$plot_env <- NULL
            gg$plot_env <- NULL
            expect_equal(ez, gg)
          })

test_that("ez_plot can facet",
          {
            ez <- ezplot(td, x = "f1", y = "c1", facet = "f2")
            gg <- ggplot(td, aes(x = f1, y = c1)) + geom_bar(stat = "identity") +
              facet_grid(~ f2)
            
            # plot_env is different, so skip
            ez$plot_env <- NULL
            gg$plot_env <- NULL
            expect_equal(ez, gg)
          })

test_that("ez_plot can make basic line",
          {
            ez <- ezplot(td, x = "c1", y = "c2", plot_type = "line")
            gg <- ggplot(td, aes(x = c1, y = c2)) + geom_line()
            
            # plot_env is different, so skip
            ez$plot_env <- NULL
            gg$plot_env <- NULL
            expect_equal(ez, gg)
          })

test_that("ez_plot can make grouped line",
          {
            ez <- ezplot(td, x = "c1", y = "c2", group = "f1", plot_type = "line")
            gg <- ggplot(td, aes(x = c1, y = c2)) + geom_line(aes(group = f1, color = f1))
            
            # plot_env is different, so skip
            ez$plot_env <- NULL
            gg$plot_env <- NULL
            expect_equal(ez, gg)
          })

test_that("ez_plot can make rectangles",
  {
    td_rect <- td %>%
      group_by(f1) %>%
      summarize(fmin = min(c1),
                fmax = max(c1)) %>%
      mutate(row_index = row_number(),
             xmin = row_index - .45,
             xmax = row_index + .45)
    
    td_rect_plot <- td_rect %>%
      ggplot + geom_rect(aes(x = f1, xmin = xmin, xmax = xmax, ymin = fmin, ymax = fmax))
    
    ezplot(data = td_rect, plot_type = "rect", x = "f1", xmin = "xmin", xmax = "xmax", ymin = "fmin", ymax = "fmax")
  })