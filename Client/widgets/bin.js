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
    
    getPreferredSize: function()
    {
        // Fetch its request size.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
                return Util.clone(child.getSizeRequisition(), true);
        }
        
        return {minimum: {width: 0, height: 0}, natural: {width: 0, height: 0}};
    },
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Set body size.
        if (this.bodyEl !== this.el)
            this.bodyEl.setInnerSize({width: allocation.width, height: allocation.height});
        
        // Allocate size for child.
        if (this.children.length)
        {
            var child = this.children[0];
            if (child.visible)
            {
                // Set child allocation.
                var padding = this.bodyEl.getPadding();
                
                allocation.x = padding.left;
                allocation.y = padding.top;
                
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
