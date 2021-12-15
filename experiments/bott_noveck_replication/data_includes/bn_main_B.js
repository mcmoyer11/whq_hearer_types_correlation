PennController.ResetPrefix(null); // Initiates PennController
PennController.DebugOff()
// BOTT & NOVECK TRIALS
// B SEQUENCE

Sequence("intro","instructions", "Train", "end_train", rshuffle("test"), "send", "end")

// This only works when I don't embed a DashedSentence in a newController()
var defaults = [
    "DashedSentence", {mode: "speeded acceptability", wordTime: 250}
    ];


newTrial("intro",
    defaultText
        .center()
        .print()
    ,
    newText("<p>Welcome to the Second Part of the Experiment!</p>")
    ,
    newTextInput("ID")
        .log()
        .before( newText("before", "<p>Please enter your unique participant ID. </p>") )
        .center()
        .print()
    ,
    newText("warning", "Please enter your ID first")
        .color("red")
        .bold()
    ,
    newButton("continue button", "Click to continue.")
        .center()
        .print()
        .wait(  // Make sure the TextInput has been filled
            getTextInput("ID")
                .testNot.text("")
                .failure( getText("warning").print() )
        )
    ,   // Create a Var element before going to the next screen
    newVar("ID")
        .global()          // Make it globally accessible
        .set( getTextInput("ID") )
)
.log( "ID" , getVar("ID") )
.setOption("hideProgressBar", true); // Do not show the progress bar on first screen


newTrial("instructions",
    defaultText
        .center()
        .print()
    ,
    newText("<p>As a reminder, we will ask you to decide whether you agree or disagree with a statement.</p>")
    ,
    newText("<p>If you <strong>Agree</strong> with the sentence, press <strong>F</strong>. If you <strong>Disagree</strong>, then press <strong>J</strong><p>")
    ,
    newText("<p>The statement will be revealed <strong>automatically</strong>, one word at a time.</p>")
    ,
    newText("<p><strong>This will happen quickly, so be sure to pay attention</strong>.</p>")
    ,
    newText("<p>Let's practice.</p>")
    ,
    newButton("Click here to proceed to the practice.")
        .center()
        .print()
        .wait()
)


Template( "train_table.csv", row =>
    [
        "Train",
        "DashedSentence", {s: row.Sentence},
        "PennController", newTrial("question",    
            newText("<p> Press <strong>F</strong> to <strong>Agree</strong> or <strong>J</strong> to <strong>Disagree</strong><p>")
                .center()
                .print()
            ,
            newText(row.Feedback)
                .center()
                .print()
                .log( "Feedback" , row.Feedback )
            ,
            newSelector()
                .add( newText("Agree"), newText("Disagree"))
                .keys("F", "J")
                .log("first")
                .wait()
            )
            .log( "ID" , getVar("ID") )
            .log( "Type" , row.Type )
            .log( "Sentence" , row.Sentence )
            .log( "Feedback" , row.Feedback )
    ]
)


newTrial("end_train",
    defaultText
        .center()
        .print()
    ,    
    newText("<p>Great Job!</p>")
    ,
    newText("<p><strong>Once you begin the experiment, you should not stop until you finish.</strong></p>")
    ,
    newText("<p>Only begin when you are ready.</p>")
    ,
    newButton("Click here to start the experiment")
        .center()
        .print()
        .wait()
)

// This works to get speeded acceptability
Template( "bn_table.csv", row =>
    ["test",
        "DashedSentence", {s: `${row.Matrix} ${row.Quantifier} ${row[row.Subject.replace("Category",row.WhichCategory+'Category')]} are ${row[row.Predicate.replace("Category",row.WhichCategory+'Category')]}`},
        "PennController", newTrial("question",    
            newText("<p> Press <strong>F</strong> to <strong>Agree</strong> or <strong>J</strong> to <strong>Disagree</strong><p>")
                .center()
                .print()
            ,
            newSelector()
                .add( newText("Agree"), newText("Disagree"))
                .keys("F", "J")
                .log("first")
                .wait()
        )
        .log( "ID" , getVar("ID") )
        .log( "Study" , row.Study )
        .log( "SentNumber" , row.SentNumber )
        .log( "Quantifier" , row.Quantifier )
        .log( "Matrix" , row.Matrix )
        .log( "WhichCategory" , row.WhichCategory )
        .log( "Subject" , row.Subject )
        .log( "Predicate" , row.Predicate )
        .log( "SentType" , row.SentType )
        .log( "Verb" , row.Verb )
        .log( "Embedded" , row.Embedded )
        .log( "Token" , row.Token )
        .log( "MatchingCategory" , row.MatchingCategory )
        .log( "MismatchingCategory" , row.MismatchingCategory )
        .log( "Group"  , row.Group  )
    ]
)


SendResults( "send" )

newTrial("end",
    newText("You have just completed the second part of the experiment!")
        .center()
        .print()
    ,
    newText("<p>If you need to take a break, do so before now proceeding to the third and final part.</p>")
        .center()
        .print()
    ,
    newText("<p><a href='https://expt.pcibex.net/ibexexps/mcmoyer11/Q-RT/experiment.html'>Click here for Part 3/3.</a></p>")
        .center()
        .print()
    ,
    newButton("void")
        .wait()
)
.setOption( "countsForProgressBar" , false )
// Make sure the progress bar is full upon reaching this last (non-)trial


