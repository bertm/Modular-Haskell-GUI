// Use strict mode if available.
"use strict";

/**
 * Singleton representing the state of the screen.
 */
Singleton.define('Screen', {
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        // NOTE: Use of browser internals (window.resize).
        
        // Check for a resize event on the window.
        var body = Element.getBody();
        var self = this;
        window.onresize = function()
        {
            // Remove cache, get new size.
            body.invalidate();
            self.size = body.getSize();
            
            // Emit signals.
            self.emitPropertyChangeSignals('size');
        };
        
        // Set size.
        this.size = body.getSize();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The size of the screen.
         *
         * @type Dimension
         */
        size: {
            read: function()
            {
                return this.size;
            }
        }
        /*
        ,
        orientation: {
            read: function()
            {
                // TODO: ScreenOrientation.
            }
        }
        */
    }
});
