context("generator for list of stopping conditions")

test_that("generator for list of stopping conditions works well", {
    # at least on of {max.time, max.iter} must be finite
    expect_error(setupTerminators())

    scs = setupTerminators(100L, 1e6)
    expect_is(scs, "list")
    expect_equal(length(scs), 2L)
    lapply(scs, function(sc) {
        expect_is(sc, "ecr_terminator")
    })

})

