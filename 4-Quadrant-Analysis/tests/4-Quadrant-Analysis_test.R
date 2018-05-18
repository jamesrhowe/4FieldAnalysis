app <- ShinyDriver$new("../")
app$snapshotInit("4-Quadrant-Analysis_test")

app$uploadFile(upload = "Test_1") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(treatmentID = "cond1")
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_2") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$snapshot()
app$setInputs(treatmentID = "cond2")
app$uploadFile(upload = "Test_3") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_4") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$snapshot()
