// Use strict mode if available.
"use strict";

// Implement forEach for arrays.
if (!Array.prototype.forEach)
{
    Array.prototype.forEach = function(callback, scope)
    {
        if (typeof callback !== 'function')
            throw new TypeError();
        
        var length = this.length;
        for (var i = 0; i < length; ++i)
        {
            if (i in this)
            {
                callback.call(scope, this[i], i, this);
            }
        }
    };
}
