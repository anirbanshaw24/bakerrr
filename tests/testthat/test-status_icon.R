
library(testthat)
library(bakerrr)

describe("status icons are correct", {

  it("status icons loaded and returned correctly", {
    expected <- list(
      created   = "🔄",
      running   = "⏳",
      completed = "✅",
      failed    = "❌",
      default   = "🔍"
    )
    icons <- config::get(
      file = bakerrr_path("constants", "constants.yml")
    )$print$emojis
    expect_equal(icons, expected)

    expect_equal(icons$created, "🔄")
    expect_equal(icons$running, "⏳")
    expect_equal(icons$completed, "✅")
    expect_equal(icons$failed, "❌")
    expect_equal(icons$default, "🔍") # fallback to default
  })
})
