// Use strict mode if available.
"use strict";

/*
 * Button box class.
 */

Class.define('ButtonBox', {
    extend: 'Box',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        ButtonBox.base.initialize.call(this);
        
        // Set spacing.
        this.setSpacing(5);
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-button-box x-body" />';
    },
    
    /*
     * Layouting.
     */
    
    // TODO: Preferred size: keep spread in mind (adds spacing to both sides...)
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Create a handy shortcut.
        var horizontal = (this.orientation === Orientation.HORIZONTAL);
        
        // Determine amount of visible children, their total size and their max size.
        var nrVisibleChildren = 0, totalSize = 0, maxPerpSize = 0, maxSize = 0;
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            var child = this.children[i];
            if (child.visible)
            {
                ++nrVisibleChildren;
                
                // Fetch its requisition.
                var childRequisition = child.getSizeRequisition().minimum; // TODO: Do something with natural.
                
                // Keep some statistics.
                var size     = horizontal ? childRequisition.width : childRequisition.height;
                var perpSize = horizontal ? childRequisition.height : childRequisition.width;
                
                maxSize     = Math.max(maxSize, size);
                maxPerpSize = Math.max(maxPerpSize, perpSize);
                totalSize  += size;
            }
        }
        
        if (!nrVisibleChildren)
            return;
        
        // If homogeneous, every child takes the same size.
        if (this.homogeneous)
            totalSize = nrVisibleChildren * maxSize;
        
        // Determine space per gap.
        var gapSize = this.spacing;
        if (this.layoutStyle === ButtonBoxStyle.SPREAD)
            if (horizontal)
                gapSize = (allocation.width - totalSize) / (nrVisibleChildren + 1);
            else
                gapSize = (allocation.height - totalSize) / (nrVisibleChildren + 1);
        else if (this.layoutStyle === ButtonBoxStyle.EDGE)
            if (horizontal)
                gapSize = (allocation.width - totalSize) / (nrVisibleChildren - 1);
            else
                gapSize = (allocation.height - totalSize) / (nrVisibleChildren - 1);
        
        // Setup child allocation.
        var padding = this.bodyEl.getPadding();
        
        var x = padding.left, y = padding.top;
        
        if (horizontal)
        {
            x += (this.layoutStyle === ButtonBoxStyle.SPREAD) ? gapSize : 0;
            y += (allocation.height - maxPerpSize) * 0.5;
        }
        else
        {
            x += (allocation.width - maxPerpSize) * 0.5;
            y += (this.layoutStyle === ButtonBoxStyle.SPREAD) ? gapSize : 0;
        }
        
        // Handle right and center styles.
        if (this.layoutStyle === ButtonBoxStyle.END)
        {
            var temp = totalSize + gapSize * (nrVisibleChildren - 1);
            
            if (horizontal)
                x += allocation.width - temp;
            else
                y += allocation.height - temp;
        }
        else if (this.layoutStyle === ButtonBoxStyle.CENTER)
        {
            var temp = totalSize + gapSize * (nrVisibleChildren - 1);
            
            if (horizontal)
                x += (allocation.width - temp) * 0.5;
            else
                y += (allocation.height - temp) * 0.5;
        }
        
        // Give each child his size.
        for (var i = 0; i < this.children.length; ++i)
        {
            var child = this.children[i];
            if (!child.visible)
                continue;
                
            // Fetch its requisition.
            var childRequisition = child.getSizeRequisition().minimum;
            
            // Determine child allocation.
            if (horizontal)
            {
                var width  = this.homogeneous ? maxSize : childRequisition.width;
                var height = maxPerpSize;
            }
            else
            {
                var height = this.homogeneous ? maxSize : childRequisition.height;
                var width  = maxPerpSize;
            }
            
            // Set position and size.
            child.allocateSize({
                x: Math.floor(x),
                y: Math.floor(y),
                width: width,
                height: height
            });
            
            // Increase location.
            if (horizontal)
                x += width + gapSize;
            else
                y += height + gapSize;
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * How the children of the button box are layed out.
         *
         * @type ButtonBoxStyle
         */
        'layout-style': {
            write: function(layoutStyle)
            {
                this.layoutStyle = layoutStyle;
                
                this.layout();
            },
            read: true,
            defaultValue: ButtonBoxStyle.EDGE
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        /**
         * Adds a widget to the box.
         *
         * @param widget Widget Widget to add.
         */
        add: function(widget)
        {
            ButtonBox.base.add.call(this, widget);
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            var index = ButtonBox.base.remove.call(this, widget);
            
            this.layout();
        }
    }
});
