// Use strict mode if available.
"use strict";

/**
 * A container with just one child.
 */
Class.define('Bin', {
    extend: 'Container',
    
    /*
     * Private methods; layouting.
     */
    
    getMinimumSize: function()
    {
        // Fetch its request size.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
            {
                var margin = child.margin;
                var size   = child.requestSize();
                
                return {width: size.width + margin.left + margin.right, height: size.height + margin.top + margin.bottom};
            }
        }
        
        return {width: 0, height: 0};
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Set body size.
        this.bodyEl.setInnerSize({width: allocation.width, height: allocation.height});
        
        // Allocate size for child.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
            {
                // Set child allocation.
                var padding = this.bodyEl.getPadding();
                var margin  = child.margin;
                
                allocation.x = margin.left + padding.left;
                allocation.y = margin.top  + padding.top;
                
                allocation.width  -= margin.left + margin.right;
                allocation.height -= margin.top  + margin.bottom;
                
                child.allocateSize(allocation);
            }
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        add: function(widget)
        {
            // Remove existing child.
            if (this.children.length !== 0)
                this.remove(this.children[0]);
            
            Bin.base.add.call(this, widget);
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            Bin.base.remove.call(this, widget);
            
            this.layout();
        },
        /**
         * Gets the child of the bin. Returns `null` if there is no child.
         *
         * @return Widget The child widget of the bin.
         */
        getChild: function()
        {
            return this.children[0] || null;
        }
    }
});
