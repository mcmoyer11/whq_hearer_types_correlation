PennController.ResetPrefix(null); // Initiates PennController

// BOTT & NOVECK TRIALS

Sequence("intro", randomize("experiment"), "send", "final")

newTrial("intro",
    
    defaultText
        .print()
    ,    
    newText("<p>Welcome!</p>")
    ,
    newText("<p>In this experiment, you will have to report whether you agree or disagree with a statement.</p>")
    ,
    newText("<p>Press the <strong>F</strong> key to Agree, or the <strong>J</strong> key to Disagree.</p>")
    ,
    newText("<p>Please enter your SONA ID and then click the button below to start the experiment.</p>")
    ,
    newTextInput("inputID")
        .print()
    ,
    newButton("Start")
        .print()
        .wait()
    ,
    newVar("ID")
        .global()
        .set( getTextInput("inputID") )
)

.log( "ID" , getVar("ID") )

// What is in Header happens at the beginning of every single trial
Header(
    // We will use this global Var element later to store the participant's name
    newVar("ID")
        .global()
    ,
    // Delay of 250ms before every trial
    newTimer(250)
        .start()
        .wait()
)
.log( "ID" , getVar("ID") )
// This log command adds a column reporting the participant's name to every line saved to the results


Template( "experiment" =>
    newTrial( "none_frame",
        newTimer(500)
            .start()
            .wait()
        ,
        newText(variable.Sentence)
            .print()
        ,
        newText("<p> Press <strong>F</strong> to <strong>Agree</strong> or <strong>J</strong> to <strong>Disagree</strong><p>")
            .print()
        ,
        newSelector()
            .add("Agree", "Disagree")
            .keys("F", "J")
            .log()
            .wait()
        ,
        newTimer(500)
            .start()
            .wait()
    )
    .log( "ID" , getVar("ID") )
    .log( "Token" , variable.Token )
    .log( "Category" , variable.Category )
    .log( "Sentence" , variable.Sentence )
    .log( "Quantifier" , variable.Quantifier )
    .log( "Type" , variable.sent_type )
    .log( "Verb" , variable.Verb )
    .log( "Group"  , variable.Group  )
    ,
    newTrial( "know_frame",
        newTimer(500)
            .start()
            .wait()
        ,
        newText(variable.Sentence) // Zoologists know that ______
            .print()
        ,
        newText("<p> Press <strong>F</strong> to <strong>Agree</strong> or <strong>J</strong> to <strong>Disagree</strong><p>")
            .print()
        ,
        newSelector()
            .add("Agree", "Disagree")
            .keys("F", "J")
            .log()
            .wait()
        ,
        newTimer(500)
            .start()
            .wait()
    )
    .log( "ID" , getVar("ID") )
    .log( "Token" , variable.Token )
    .log( "Category" , variable.Category )
    .log( "Sentence" , variable.Sentence )
    .log( "Quantifier" , variable.Quantifier )
    .log( "Type" , variable.sent_type )
    .log( "Verb" , variable.Verb )
    .log( "Group"  , variable.Group  )
    ,
    newTrial( "say_frame",
        newTimer(500)
            .start()
            .wait()
        ,
        newText(variable.Sentence) // Zoologists say that _____
            .print()
        ,
        newText("<p> Press <strong>F</strong> to <strong>Agree</strong> or <strong>J</strong> to <strong>Disagree</strong><p>")
            .print()
        ,
        newSelector()
            .add("Agree", "Disagree")
            .keys("F", "J")
            .log()
            .wait()
        ,
        newTimer(500)
            .start()
            .wait()
    )
    .log( "ID" , getVar("ID") )
    .log( "Token" , variable.Token )
    .log( "Category" , variable.Category )
    .log( "Sentence" , variable.Sentence )
    .log( "Quantifier" , variable.Quantifier )
    .log( "Type" , variable.sent_type )
    .log( "Verb" , variable.Verb )
    .log( "Group"  , variable.Group  )

)

SendResults( "send" )

newTrial("end",
    newText("<p>Thank you for your participation!</p>")
        .print()
    ,
    newText("Your participation code is: 3bc8068f")
        .print()
    ,
    newText("<p><a href='https://rutgerslinguistics.sona-systems.com/Default.aspx?ReturnUrl=%2f'>Click here to validate your participation.</a></p>")
        .print()
    ,
    newButton("void")
        .wait()
)
.setOption( "countsForProgressBar" , false )
// Make sure the progress bar is full upon reaching this last (non-)trial



// This is a simple demo script, feel free to edit or delete it
// Find a tutorial and the list of availalbe elements at:
// https://www.pcibex.net/documentation/

PennController.ResetPrefix(null) // Shorten command names (keep this line here)

// Show the 'intro' trial first, then all the 'experiment' trials in a random order
// then send the results and finally show the trial labeled 'bye'
Sequence( "intro", randomize("experiment") , SendResults() , "bye" )


// What is in Header happens at the beginning of every single trial
Header(
    // We will use this global Var element later to store the participant's name
    newVar("ParticipantName")
        .global()
    ,
    // Delay of 250ms before every trial
    newTimer(250)
        .start()
        .wait()
)
.log( "Name" , getVar("ParticipantName") )
// This log command adds a column reporting the participant's name to every line saved to the results


newTrial( "intro" ,
    newImage("pcibex-logo.png")
        .size( 150 , 200 )      // Resize the image to 150x250px
        .print()
    ,
    newText("<p>Welcome to the PCIbex demo experiment.</p><p>Please enter your name below and press Enter:</p>")
        .print()
    ,
    newTextInput()
        .print()
        .wait()                 // The next command won't be executed until Enter is pressed
        .setVar( "ParticipantName" )
        // This setVar command stores the value from the TextInput element into the Var element
)


// This Template command generates as many trials as there are rows in myTable.csv
Template( "myTable.csv" ,
    // Row will iteratively point to every row in myTable.csv
    row => newTrial( "experiment" ,
        // The trials are minimal: choose a pronoun from a DropDown list
        newDropDown("pronoun", "...")
            .before( newText(row.Sentence) )    // Print the sentence to the left of the list
            .add( row.Pronoun1 )
            .add( row.Pronoun2 )
            .shuffle()                          // Randomly order the options in the list (Pronoun1 and Pronoun2)
            .once()                             // Disable the list after the first selection
            .print()
            .wait()
            .log()                              // Make sure to log the participant's selection
        ,
        newButton("Next")
            .print()
            .wait()
    )
    .log( "Sentence" , row.Sentence )
    .log( "Pronoun1" , row.Pronoun1 )
    .log( "Pronoun2" , row.Pronoun2 )
    // Add these three columns to the results lines of these Template-based trials
)


// Spaces and linebreaks don't matter to the script: we've only been using them for the sake of readability
newTrial( "bye" ,
    newText("Thank you for your participation!").print(),
    newButton().wait()  // Wait for a click on a non-displayed button = wait here forever
)

