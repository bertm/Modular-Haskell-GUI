// Use strict mode if available.
"use strict";

/*
 * Box class.
 */

Class.define('Box', {
    extend: 'Container',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Box.base.initialize.call(this);
        
        // Set members.
        this.expand = [];
        this.fill   = [];
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-box x-body" />';
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        var minSize = {width: 0, height: 0};
        
        var nrVisibleChildren = 0;
        if (this.orientation === Orientation.HORIZONTAL)
        {
            // Take maximum minimum height, sum widths.
            for (var i = this.children.length - 1; i >= 0; --i)
            {
                var child = this.children[i];
                if (child.visible)
                {
                    ++nrVisibleChildren;
                    
                    var size   = child.requestSize();
                    var margin = child.margin;
                    
                    minSize.height = Math.max(minSize.height, size.height + margin.top + margin.bottom);
                    
                    var width = size.width + margin.left + margin.right;
                    
                    if (this.homogeneous)
                        minSize.width = Math.max(minSize.width, width);
                    else
                        minSize.width += width;
                }
            }
        }
        else
        {
            // Take maximum minimum width, sum heights.
            for (var i = this.children.length - 1; i >= 0; --i)
            {
                var child = this.children[i];
                if (child.visible)
                {
                    ++nrVisibleChildren;
                    
                    var size   = child.requestSize();
                    var margin = child.margin;
                    
                    minSize.width = Math.max(minSize.width, size.width + margin.left + margin.right);
                    
                    var height = size.height + margin.top + margin.bottom;
                    
                    if (this.homogeneous)
                        minSize.height = Math.max(minSize.height, height);
                    else
                        minSize.height += height;
                }
            }
        }
        
        if (nrVisibleChildren > 0)
        {
            if (this.homogeneous)
            {
                if (this.orientation === Orientation.HORIZONTAL)
                    minSize.width *= nrVisibleChildren;
                else
                    minSize.height *= nrVisibleChildren;
            }
            
            if (this.orientation === Orientation.HORIZONTAL)
                minSize.width += (nrVisibleChildren - 1) * this.spacing;
            else
                minSize.height += (nrVisibleChildren - 1) * this.spacing;
        }
        
        return minSize;
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Determine amount of visible and expand children.
        var nrVisibleChildren = 0, nrExpandChildren = 0;
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            if (this.children[i].visible)
            {
                ++nrVisibleChildren;
                
                if (this.expand[i])
                    ++nrExpandChildren;
            }
        }
        
        if (!nrVisibleChildren)
            return;
        
        // Determine size.
        var fullSize, size, rest;
        if (this.homogeneous)
        {
            if (this.orientation === Orientation.HORIZONTAL)
                fullSize = allocation.width - (nrVisibleChildren - 1) * this.spacing;
            else
                fullSize = allocation.height - (nrVisibleChildren - 1) * this.spacing;
            
            size = Math.floor(fullSize / nrVisibleChildren);
            rest = fullSize - size * nrVisibleChildren;
        }
        else if (nrExpandChildren > 0)
        {
            var requisition = this.requestSize();
            
            if (this.orientation === Orientation.HORIZONTAL)
                fullSize = allocation.width - requisition.width;
            else
                fullSize = allocation.height - requisition.height;
            
            size = Math.floor(fullSize / nrExpandChildren);
            rest = fullSize - size * nrExpandChildren;
        }
        else
        {
            size = 0;
            rest = 0;
        }
        
        // Setup child allocation.
        var padding = this.bodyEl.getPadding();
        
        var x = padding.left, y = padding.top;
        var childAllocation = {x: x, y: y};
        
        if (this.orientation === Orientation.HORIZONTAL)
            childAllocation.height = allocation.height;
        else
            childAllocation.width = allocation.width;
        
        // Give each child his size.
        for (var i = 0; i < this.children.length; ++i)
        {
            // Get child, and check if it's visible.
            var child = this.children[i];
            if (!child.visible)
                continue;
            
            // Fetch child margin.
            var margin = child.margin;
            
            // Calculate child its size.
            var childSize;
            if (this.homogeneous)
            {
                childSize = size;
                
                if (rest)
                {
                    ++childSize;
                    --rest;
                }
            }
            else
            {
                var childRequisition = child.requestSize();
                
                if (this.orientation === Orientation.HORIZONTAL)
                    childSize = childRequisition.width + margin.left + margin.right;
                else
                    childSize = childRequisition.height + margin.top + margin.bottom;
                
                if (this.expand[i])
                {
                    childSize += size;
                    
                    if (rest)
                    {
                        ++childSize;
                        --rest;
                    }
                }
            }
            
            // Handle fill.
            if (this.fill[i])
            {
                if (this.orientation === Orientation.HORIZONTAL)
                {
                    childAllocation.width = childSize;
                    childAllocation.x     = x;
                }
                else
                {
                    childAllocation.height = childSize;
                    childAllocation.y      = y;
                }
            }
            else
            {
                var childRequisition = child.requestSize();
                
                if (this.orientation === Orientation.HORIZONTAL)
                {
                    childAllocation.width = childRequisition.width;
                    childAllocation.x     = x + Math.floor((childSize - childAllocation.width) / 2);
                }
                else
                {
                    childAllocation.height = childRequisition.height;
                    childAllocation.y      = y + Math.floor((childSize - childAllocation.height) / 2);
                }
            }
            
            // Apply child margin, and set position and size.
            child.allocateSize({
                x: childAllocation.x + margin.left,
                y: childAllocation.y + margin.top,
                width: childAllocation.width - margin.left - margin.right,
                height: childAllocation.height - margin.top - margin.bottom
            });
            
            // Increase location.
            if (this.orientation === Orientation.HORIZONTAL)
                x += childSize + this.spacing;
            else
                y += childSize + this.spacing;
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The orientation of the box.
         *
         * @type Orientation
         */
        orientation: {
            write: function(orientation)
            {
                this.orientation = orientation;
                
                this.layout();
            },
            read: true,
            defaultValue: Orientation.HORIZONTAL
        },
        /**
         * Whether the space is evenly divided between the children.
         *
         * @type bool
         */
        homogeneous: {
            write: function(homogeneous)
            {
                this.homogeneous = homogeneous;
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * Space between each child.
         *
         * @type int
         */
        spacing: {
            write: function(spacing)
            {
                this.spacing = spacing;
                
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
        // Overrides 'add(widget)' action.
        /**
         * Adds a widget to the box.
         *
         * @param widget Widget Widget to add.
         */
        add: function(widget, expand, fill) // TODO: Remove expand (expand), fill (scale).
        {
            Box.base.add.call(this, widget);
            
            this.expand.push(expand !== false);
            this.fill.push(fill !== false);
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            var index = Box.base.remove.call(this, widget);
            
            this.expand.splice(index, 1);
            this.fill.splice(index, 1);
            
            this.layout();
        }
    }
});
