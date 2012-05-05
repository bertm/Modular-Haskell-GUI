// Use strict mode if available.
"use strict";

/*
 * Aspect frame class.
 */

Class.define('AspectFrame', {
    extend: 'Frame',
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        var minimumSize = AspectFrame.base.getMinimumSize.call(this);
        
        // TODO: Gtk+ does not do it differently from Frame, but it should if obey-child = false
        
        return minimumSize;
    },
    
    allocateSize: function(allocation)
    {
        // Fetch child and bail out if no child or if it's not visible.
        var child = this.children[0];
        if (!child || !child.visible)
        {
            return AspectFrame.base.allocateSize.call(this, allocation);
        }
        
        // Get requisition.
        var requisition = this.requestSize(false);
        
        // Determine ratio.
        var ratio;
        if (this.obeyChild)
        {
            if (requisition.height !== 0)
            {
                ratio = requisition.width / requisition.height;
                
                if (ratio < AspectFrame.MIN_RATIO)
                    ratio = AspectFrame.MIN_RATIO;
            }
            else if (requisition.width !== 0)
            {
                ratio = AspectFrame.MAX_RATIO;
            }
            else
            {
                ratio = 1.0;
            }
        }
        else
        {
            ratio = this.ratio;
        }
        
        // Determine new allocation.
        var fullSize = {width: allocation.width, height: allocation.height};
        
        if ((ratio * allocation.height) > allocation.width)
            allocation.height = Math.round(allocation.width / ratio);
        else
            allocation.width = Math.round(ratio * allocation.height);
        
        allocation.x += this.xAlign * (fullSize.width  - allocation.width);
        allocation.y += this.yAlign * (fullSize.height - allocation.height);
        
        // Allocate frame.
        AspectFrame.base.allocateSize.call(this, allocation);
    },
    
    statics: {
        MAX_RATIO: 10000.0,
        MIN_RATIO: 0.0001
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
        'ratio': {
            write: function(ratio)
            {
                this.ratio = ratio;
                
                this.layout();
            },
            read: true,
            defaultValue: 1.0
        },
        'obey-child': {
            write: function(obeyChild)
            {
                this.obeyChild = obeyChild;
                
                this.layout();
            },
            read: true,
            defaultValue: true
        }
    }
});
