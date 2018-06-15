app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(treatmentID = "Cond")
app$setInputs(treatmentID = "Cond_1")
app$uploadFile(upload = "Test_1") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_2") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$setInputs(treatmentID = "Cond_2")
app$uploadFile(upload = "Test_3") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$uploadFile(upload = "Test_4") # <-- This should be the path to the file, relative to the app's tests/ directory
app$setInputs(analyze = "click")
app$snapshot()
