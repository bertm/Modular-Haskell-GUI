// Use strict mode if available.
"use strict";

/*
 * Fixed class.
 */

Class.define('Fixed', {
    extend: 'Container',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        Fixed.base.initialize.call(this);
        
        // Set members.
        this.positions = [];
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-fixed">' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        var minSize = {width: 0, height: 0};
        
        // Take maximum minimum size of visible children.
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            if (this.children[i].visible)
            {
                var size     = this.children[i].requestSize();
                var position = this.positions[i];
                
                minSize.width  = Math.max(minSize.width,  size.width  + position.x);
                minSize.height = Math.max(minSize.height, size.height + position.y);
            }
        }
        
        return minSize;
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Give visible children their requested size.
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            var child = this.children[i];
            
            if (child.visible)
            {
                // Fetch position.
                var position = this.positions[i];
                
                // Fetch child its requested size.
                var childRequisition = child.requestSize();
                
                // Allocate size for child.
                var margin = child.margin;
                
                child.allocateSize({
                    x: position.x + margin.left,
                    y: position.y + margin.top,
                    width: childRequisition.width - margin.left - margin.right,
                    height: childRequisition.height - margin.top  - margin.bottom
                });
            }
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        add: function(widget, x, y)
        {
            Fixed.base.add.call(this, widget);
            
            this.positions.push({x: x, y: y});
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            var index = Fixed.base.remove.call(this, widget);
            
            this.position.splice(index, 1);
            
            this.layout();
        }
    }
});
