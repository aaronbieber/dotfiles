// My Slate configuration

// Store current window states.
var windowStates = {};

// Left screen edge operations.
var leftBarThird = S.op("push",
                        {"direction": "left",
                         "style": "bar-resize:screenSizeX/3"});
var leftBarHalf = S.op("push",
                       {"direction": "left",
                        "style": "bar-resize:screenSizeX/2"});
var leftBarTwoThirds = S.op("push",
                            {"direction": "left",
                             "style": "bar-resize:screenSizeX/3*2"});

// Right screen edge operations.
var rightBarThird = S.op("push",
                         {"direction": "right",
                          "style": "bar-resize:screenSizeX/3"});
var rightBarHalf = S.op("push",
                        {"direction": "right",
                         "style": "bar-resize:screenSizeX/2"});
var rightBarTwoThirds = S.op("push",
                             {"direction": "right",
                              "style": "bar-resize:screenSizeX/3*2"});

// Top screen edge operations.
var bottomHalf = S.op("push",
                      {"direction": "down",
                       "style": "bar-resize:screenSizeY/2"});

/* This closure returns a function that will cycle a window through
 * the states given, assuming that each state is the string name of a
 * valid Slate operation object created with `S.op()'.
 *
 * The function returned is expected to be bound to a key and thus
 * accepts one argument, the window object.
 */
function getCycleStates(states) {
    return function(win) {
        if (win.pid in windowStates &&
            windowStates[win.pid] !== undefined &&
            states.indexOf(windowStates[win.pid]) !== -1
           ) {
            var nextIndex = (states.indexOf(windowStates[win.pid]) + 1) % 3;
            var nextState = states[nextIndex];
            windowStates[win.pid] = nextState;
        } else {
            windowStates[win.pid] = states[0];
        }

        win.doOperation(windowStates[win.pid]);
    };
}

// Window cycling functions.
var leftCycleOp = getCycleStates([leftBarHalf, leftBarThird, leftBarTwoThirds]);
var rightCycleOp = getCycleStates([rightBarHalf, rightBarThird, rightBarTwoThirds]);

// Bind the window cycling functions.
S.bind("left:cmd", leftCycleOp);
S.bind("right:cmd", rightCycleOp);
S.bind(
    "up:cmd",
    S.op("move",
         {"x": "windowTopLeftX",
          "y": "screenOriginY",
          "width": "windowSizeX",
          "height": "screenSizeY/2"
         })
);
S.bind(
    "down:cmd",
    S.op("move",
         {"x": "windowTopLeftX",
          "y": "screenOriginY+(screenSizeY)/2",
          "width": "windowSizeX",
          "height": "screenSizeY/2"
         })
)

// Full-screen.
S.bind(
    "up:cmd,shift",
    S.op(
        "move",
        {
            "x": "screenOriginX",
            "y": "screenOriginY",
            "width": "screenSizeX",
            "height": "screenSizeY"
        }
    )
);

// Throw to other monitors
S.bind(
    "m:cmd,shift",
    S.op("chain", {
        'operations': [
            S.op("move", {
                "screen": 0,
                "x": "screenOriginX",
                "y": "screenOriginY",
                "width": "windowSizeX",
                "height": "windowSizeY"
            }),
            S.op("move", {
                "screen": 1,
                "x": "screenOriginX",
                "y": "screenOriginY",
                "width": "windowSizeX",
                "height": "windowSizeY"
            })
        ]
    })
);
