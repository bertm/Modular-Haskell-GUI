// Use strict mode if available.
"use strict";

/*
 * Misc class.
 */

// TODO: Remove.

Class.define('Misc', {
    extend: 'Widget',
    
    /*
     * Layouting.
     */
    
    getFrameSize: function()
    {
        var frameSize = Misc.base.getFrameSize.call(this);
        
        // Add padding.
        frameSize.width  += 2 * this.xPadding;
        frameSize.height += 2 * this.yPadding;
        
        return frameSize;
    },
    
    allocateSize: function(allocation)
    {
        // TODO: Handle underallocations.
        
        // Get request size.
        var requesition = this.requestSize(false);
        
        // Set our size and position.
        this.el.setSize({width: requesition.width - this.xPadding * 2, height: requesition.height - this.yPadding * 2});
        this.el.setPosition({
            x: allocation.x + this.xPadding + this.xAlign * (allocation.width  - requesition.width),
            y: allocation.y + this.yPadding + this.yAlign * (allocation.height - requesition.height)
        });
        
        // Store allocation.
        this.allocation = allocation;
    },
    
    /*
     * Properties.
     */
    
    properties: {
        'x-align': {
            write: function(xAlign)
            {
                this.xAlign = xAlign;
                
                this.layout();
            },
            read: true,
            defaultValue: 0.5
        },
        'y-align': {
            write: function(yAlign)
            {
                this.yAlign = yAlign;
                
                this.layout();
            },
            read: true,
            defaultValue: 0.5
        },
        'x-padding': {
            write: function(xPadding)
            {
                this.xPadding = xPadding;
                
                this.layout();
            },
            read: true,
            defaultValue: 0
        },
        'y-padding': {
            write: function(yPadding)
            {
                this.yPadding = yPadding;
                
                this.layout();
            },
            read: true,
            defaultValue: 0
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
    }
});
