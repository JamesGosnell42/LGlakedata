test_that("test data access", {

  for(name in names(LGlakedata:::filenames)){
    expect_is(LGdata(name), 'data.frame')
  }

})
