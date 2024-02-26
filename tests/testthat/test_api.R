library(epiuf)

test_that("create Modify Keyring", {
  # be sure that 'test' does not exists
  suppressWarnings(epiuf::deleteKeyring("test_package"))

  expect_message(createKeyring("test_package","testsecret"),"Keyring successfully created.")
  expect_warning(createKeyring("test_package","testsecret"))
  expect_message(modifyKeyring("test_package","testpassword"),"Keyring successfully modified.")

  # verify that _epiufkeyring postfix is added correctly
  expect_match(listKeyring("test_package_epiufkeyring"),"test_package")
  
  # cleanup
  deleteKeyring("test_package")
})

test_that("grabing Keyring", {
  expect_error(grabKeyring("none"))
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
  expect_message(deleteKeyring("test_package"),"Secret deleted")
  
  createKeyring("test_package_epiuf","testsecret")
  createKeyring("deux_package_epiuf","deuxsecret")
  #verify output is correct
  expect_message(deleteKeyring("test_package_epiuf"),"Secret deleted")
  expect_message(deleteKeyring("deux_package_epiuf"),"Secret deleted")
  # verify it is deleted
  expect_equal(length(listKeyring("_package_epiuf")),0)
})

