// Use strict mode if available.
"use strict";

/*
 * Console placeholder.
 */

if (console === undefined)
{
    var console = {
        log:   function() { },
        error: function() { },
        warn:  function() { },
        info:  function() { }
    };
}
