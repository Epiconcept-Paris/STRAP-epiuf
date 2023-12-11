library(epiuf)

test_that("create Modify Keyring", {
  # besure that test not exists
  try(keyring::deleteKeyring("test_package"),silent=TRUE)

  expect_equal(createKeyring("test_package","testsecret"),"testsecret")
  expect_output(createKeyring("test_package","testsecret"),"Keyring Exist")
  expect_equal(modifyKeyring("test_package","testpassword"),"testpassword")

  # verify that _epiufkeyring postfix is added correctly
  expect_match(listKeyring("test_package_epiufkeyring"),"test_package")
  
  # cleanup
  deleteKeyring("test_package")
})

test_that("grabing Keyring", {
  expect_warning(grabKeyring("none"))
  createKeyring("test_package","testpassword")
  expect_equal(grabKeyring("test_package"),"testpassword")

  expect_equal(length(listKeyring("test_package")),1)
  createKeyring("test_package2","testpassword")
  expect_equal(length(listKeyring("test_package")),2)
  
  # cleanup
  deleteKeyring("test_package")    
  deleteKeyring("test_package2")    
  
})  

test_that("delete Keyring", {
  expect_warning(deleteKeyring("none"))
  # we create an entry for deletion
  createKeyring("test_package","testsecret")
  expect_output(deleteKeyring("test_package"),"Secret deleted")
  
  createKeyring("test_package_epiuf","testsecret")
  createKeyring("deux_package_epiuf","deuxsecret")
  #verify output is correct
  expect_output(deleteKeyring("test_package_epiuf"),"Secret")
  expect_output(deleteKeyring("deux_package_epiuf"),"Secret")
  # verify it is deleted
  expect_equal(length(listKeyring("_package_epiuf")),0)
})

