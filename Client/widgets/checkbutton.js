// Use strict mode if available.
"use strict";

/*
 * Check button class.
 */

Class.define('CheckButton', {
    extend: 'ToggleButton',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        CheckButton.base.initialize.call(this);
        
        // Get control element.
        this.controlEl = this.el.find('.x-control');
    },
    
    getHtml: function()
    {
        var html =
            '<div tabindex="-1" class="x-widget x-checkbutton">' +
                '<div class="x-control" />' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getFrameSize: function()
    {
        var frameSize = CheckButton.base.getFrameSize.call(this);
        
        // Add control width.
        frameSize.width += this.controlEl.getSize().width;
        
        return frameSize;
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Allocate size for child.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
            {
                // Give us only the requested width, thereby essentially left aligning content.
                // Also, vertically center child.
                var requisition = child.requestSize();
                
                allocation.y      = (allocation.height - requisition.height) * 0.5;
                allocation.width  = requisition.width;
                allocation.height = requisition.height;
                
                // Set body size.
                this.bodyEl.setPosition({x: 0, y: allocation.y});
                this.bodyEl.setInnerSize({width: allocation.width, height: allocation.height});
                
                // Set child allocation.
                var padding = this.bodyEl.getPadding();
                var margin  = child.margin;
                
                allocation.x = margin.left + padding.left;
                allocation.y = margin.top  + padding.top;
                
                allocation.width  -= margin.left + margin.right;
                allocation.height -= margin.top  + margin.bottom;
                
                child.allocateSize(allocation);
                
                return;
            }
        }
        
        this.bodyEl.setSize({width: 0, height: 0});
    }
});
