// Use strict mode if available.
"use strict";

/**
 * A check button.
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
        frameSize = Util.cloneShallow(frameSize);
        frameSize.width += this.controlEl.getSize().width;
        
        return frameSize;
    },
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Allocate size for child.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
            {
                // Give us only the requested width, thereby essentially left aligning content.
                // Also, vertically center child.
                var requisition = child.getSizeRequisition();
                
                var availableHeight = allocation.height;
                
                allocation.width  = Math.min(allocation.width,  requisition.natural.width);
                allocation.height = Math.min(allocation.height, requisition.natural.height);
                allocation.y      = Math.round((availableHeight - allocation.height) * 0.5);
                
                // Set body size.
                this.bodyEl.setPosition({x: 0, y: allocation.y});
                this.bodyEl.setInnerSize({width: allocation.width, height: allocation.height});
                
                // Set child allocation.
                var padding = this.bodyEl.getPadding();
                
                allocation.x  = padding.left;
                allocation.y += padding.top;
                
                child.allocateSize(allocation);
                
                return;
            }
        }
        
        this.bodyEl.setSize({width: 0, height: 0});
    }
});
