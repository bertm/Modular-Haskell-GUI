// Use strict mode if available.
"use strict";

/*
 * Alignment class.
 */

Class.define('Alignment', {
    extend: 'Bin',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        Alignment.base.initialize.call(this);
        
        // Set members.
        this.padding = {top: 0, right: 0, bottom: 0, left: 0};
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-alignment x-body" />';
    },
    
    /*
     * Layouting.
     */
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Allocate size for child.
        var child = this.children[0];
        if (child && child.visible)
        {
            // Get child requisition.
            var childRequisition = Util.cloneShallow(child.requestSize());
            
            // Apply child margins.
            var margin = child.margin;
            
            childRequisition.width  += margin.left + margin.right;
            childRequisition.height += margin.top  + margin.bottom;
            
            // Create child allocation.
            var childAllocation = {};
            
            // Determine width.
            if (allocation.width > childRequisition.width)
                childAllocation.width = Math.floor(Util.lerp(this.xScale, childRequisition.width, allocation.width));
            else
                childAllocation.width = allocation.width;
            
            // Determine height.
            if (allocation.height > childRequisition.height)
                childAllocation.height = Math.floor(Util.lerp(this.yScale, childRequisition.height, allocation.height));
            else
                childAllocation.height = allocation.height;
            
            // Determine position.
            childAllocation.x = margin.left + Math.floor(this.xAlign * (allocation.width  - childAllocation.width));
            childAllocation.y = margin.top  + Math.floor(this.yAlign * (allocation.height - childAllocation.height));
            
            // Add margins to dimensions.
            childAllocation.width  += margin.left + margin.right;
            childAllocation.height += margin.top  + margin.bottom;
            
            // Set child its view size.
            child.allocateSize(childAllocation);
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        'x-align': {
            write: function(xAlign)
            {
                this.xAlign = Util.clamp(xAlign, 0, 1);
                
                this.layout();
            },
            read: true,
            defaultValue: 0.5
        },
        'y-align': {
            write: function(yAlign)
            {
                this.yAlign = Util.clamp(yAlign, 0, 1);
                
                this.layout();
            },
            read: true,
            defaultValue: 0.5
        },
        'x-scale': {
            write: function(xScale)
            {
                this.xScale = Util.clamp(xScale, 0, 1);
                
                this.layout();
            },
            read: true,
            defaultValue: 1.0
        },
        'y-scale': {
            write: function(yScale)
            {
                this.yScale = Util.clamp(yScale, 0, 1);
                
                this.layout();
            },
            read: true,
            defaultValue: 1.0
        },
        'top-padding': {
            write: function(topPadding)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.top += topPadding - this.padding.top;
                
                this.padding.top = topPadding;
                
                this.layout();
            },
            read: function()
            {
                return this.padding.top;
            }
        },
        'right-padding': {
            write: function(rightPadding)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.right += rightPadding - this.padding.right;
                
                this.padding.right = rightPadding;
                
                this.layout();
            },
            read: function()
            {
                return this.padding.right;
            }
        },
        'bottom-padding': {
            write: function(bottomPadding)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.bottom += bottomPadding - this.padding.bottom;
                
                this.padding.bottom = bottomPadding;
                
                this.layout();
            },
            read: function()
            {
                return this.padding.bottom;
            }
        },
        'left-padding': {
            write: function(leftPadding)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.left += leftPadding - this.padding.left;
                
                this.padding.left = leftPadding;
                
                this.layout();
            },
            read: function()
            {
                return this.padding.left;
            }
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
    }
});
