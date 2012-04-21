// Use strict mode if available.
"use strict";

/**
 * Screen class.
 */
Singleton.define('Screen', {
    initialize: function()
    {
        // Screen emits resize events.
        var body = Element.getBody();
        var self = this;
        window.onresize = function()
        {
            // Remove cache, get new size.
            body.invalidate();
            self.size = body.getSize();
            
            // TODO: Generalize.
            self.signalDispatcher.emit('size-change', self);
            self.signalDispatcher.emit('property-change', self, 'size');
            self.signalDispatcher.emit('change', self);
        };
        
        // Set size.
        this.size = body.getSize();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        size: {
            read: function()
            {
                return this.size;
            }
        },
        orientation: {
            read: function()
            {
                // TODO: ScreenOrientation.
            }
        }
    }
});
