context("nsr pol divs")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_checklist", {
    
    nsr_checklists <- NSR_political_divisions()
    
  })

  testthat::expect_s3_class(object = nsr_checklists,
                            class = "data.frame")
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(nsr_checklists) == "data.frame")
  expect_gt(object = nrow(nsr_checklists), expected = 1)
})

test_that("example 2 works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_poldivs", {
    
    nsr_poldivs <- NSR_political_divisions(checklist = F)
    
  })
  
  testthat::expect_s3_class(object = nsr_poldivs,
                            class = "data.frame")
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(nsr_poldivs) == "data.frame")
  expect_gt(object = nrow(nsr_poldivs), expected = 1)
})

test_that("example 3 works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_checklist_canada", {
    
    nsr_checklists_canada <- NSR_political_divisions(country = "Canada")
    
  })
  
  testthat::expect_s3_class(object = nsr_checklists_canada,
                            class = "data.frame")
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(nsr_checklists_canada) == "data.frame")
  expect_gt(object = nrow(nsr_checklists_canada), expected = 1)
})

