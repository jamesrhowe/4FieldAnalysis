app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(upload = "Test_1") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(treatmentID = "Condition")
app$setInputs(treatmentID = "Condition_")
app$setInputs(treatmentID = "Condition_1")
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_2") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$setInputs(treatmentID = "Condition_")
app$setInputs(treatmentID = "Condition_2")
app$uploadFile(upload = "Test_3") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_4") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$snapshot()
