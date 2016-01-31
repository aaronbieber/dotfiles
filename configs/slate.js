// My Slate configuration

var windowStates = {};

var leftBarThird = S.op("push",
                        {"direction": "left",
                         "style": "bar-resize:screenSizeX/3"});
var leftBarHalf = S.op("push",
                       {"direction": "left",
                        "style": "bar-resize:screenSizeX/2"});
var leftBarTwoThirds = S.op("push",
                            {"direction": "left",
                             "style": "bar-resize:screenSizeX/3*2"});

var rightBarThird = S.op("push",
                         {"direction": "right",
                          "style": "bar-resize:screenSizeX/3"});
var rightBarHalf = S.op("push",
                        {"direction": "right",
                         "style": "bar-resize:screenSizeX/2"});
var rightBarTwoThirds = S.op("push",
                             {"direction": "right",
                              "style": "bar-resize:screenSizeX/3*2"});

function getCycleStates(states) {
    var states = states;

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

        eval(windowStates[win.pid] + '.run()');
    };
}
var leftCycleOp = getCycleStates(['leftBarHalf', 'leftBarThird', 'leftBarTwoThirds']);
var rightCycleOp = getCycleStates(['rightBarHalf', 'rightBarThird', 'rightBarTwoThirds']);

S.bind('h:cmd', leftCycleOp);
S.bind('l:cmd', rightCycleOp);

S.bind('f:ctrl,shift',
       S.op('move',
            {
                'x': 0,
                'y': 0,
                'width': 'screenSizeX',
                'height': 'screenSizeY'
            })
      );
