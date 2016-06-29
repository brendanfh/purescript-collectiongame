"use strict";

//module Main

exports.requestAnimationFrame = function(f) {
    return function() {
        window.requestAnimationFrame(function(dt) {
            f();
        });
    }
};

exports.onKeyChange = function(code) {
    //TODO Rewrite so there is only ever 1 handler
    return function(f) {
        return function() {
            document.addEventListener("keydown", function(e) {
                if (e.keyCode == code) {
                    f(true)();
                }
            }, false);
            document.addEventListener("keyup", function(e) {
                if (e.keyCode == code) {
                    f(false)();
                }
            }, false);
        }
    }
}
