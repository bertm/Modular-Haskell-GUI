// Use strict mode if available.
"use strict";

/*
 * Scrolled window class.
 */

Class.define('ScrolledWindow', {
    extend: 'Bin',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        ScrolledWindow.base.initialize.call(this);
        
        // Create adjustments.
        this.hAdjustment = new Adjustment({lower: 0, 'step-increment': 10});
        this.vAdjustment = new Adjustment({lower: 0, 'step-increment': 10});
        
        // Add value change handlers.
        this.hAdjustment.connect('value-change', this.onAdjustmentValueChanged, this);
        this.vAdjustment.connect('value-change', this.onAdjustmentValueChanged, this);
        
        // Add scrollbars.
        this.hScrollBar = new ScrollBar({adjustment: this.hAdjustment, visible: true});
        this.vScrollBar = new ScrollBar({adjustment: this.vAdjustment, orientation: Orientation.VERTICAL, visible: true});
        
        // Set their parents.
        this.hScrollBar.setParent(this);
        this.vScrollBar.setParent(this);
        
        // Add them to body.
        this.el.append(this.hScrollBar.el);
        this.el.append(this.vScrollBar.el);
        
        // Add event handlers.
        EventManager.registerHandler(this.el, EventMask.SCROLL, this.onScroll, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-scrolled-window">' +
                '<div class="x-body x-shadow-in" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    layout: function()
    {
        // Do not layout when not visible or not in tree.
        if (!this.getIsVisible())
            return;
        
        // TODO: Check if our own requesition has changed: shadow, policy.
        
        // Get child requisition.
        var child = this.children[0];
        if (child && child.visible)
            var childRequisition = child.requestSize();
        else
            var childRequisition = {width: 0, height: 0};
        
        // Only reallocate when child requisition has changed.
        if ((this.hAdjustment.getUpper() !== childRequisition.width) ||
            (this.vAdjustment.getUpper() !== childRequisition.height))
        {
            this.allocateSize(Util.cloneShallow(this.allocation));
        }
        else
        {
            // Or else, just reallocate child and scrollbars.
            if (child && child.visible)
                child.allocateSize(Util.cloneShallow(child.allocation));
        }
    },
    
    requestSize: function()
    {
        // Fetch scrollbars their size.
        if (!ScrolledWindow.hScrollBarSize)
        {
            ScrolledWindow.hScrollBarSize = this.hScrollBar.requestSize();
            ScrolledWindow.vScrollBarSize = this.vScrollBar.requestSize();
        }
        
        // Fetch size.
        return ScrolledWindow.base.requestSize.call(this);
    },
    
    getMinimumSize: function()
    {
        var hScrollBarSize = ScrolledWindow.hScrollBarSize;
        var vScrollBarSize = ScrolledWindow.vScrollBarSize;
        
        return {
            width: vScrollBarSize.width + hScrollBarSize.width,
            height: vScrollBarSize.height + hScrollBarSize.height
        };
    },
    
    allocateSize: function(allocation)
    {
        // Check for difference.
        //if ((this.allocation.x === allocation.x) &&
        //    (this.allocation.y === allocation.y) &&
        //    (this.allocation.width === allocation.width) &&
        //    (this.allocation.height === allocation.height))
        //{
        //    return;
        //}
        
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Scrollbars are not needed yet.
        var hScrollBarVisible = false, vScrollBarVisible = false;
        
        // Get child its minimum size.
        var child = this.children[0];
        if (child && child.visible)
        {
            // Get child requisition.
            var childRequisition = Util.cloneShallow(child.requestSize());
            
            // Create child allocation.
            var margin = child.margin;
            
            var childAllocation = Util.cloneShallow(childRequisition);
            
            childAllocation.x = margin.left - this.hAdjustment.getValue();
            childAllocation.y = margin.top  - this.vAdjustment.getValue();
            
            // Apply child margins.
            childRequisition.width  += margin.left + margin.right;
            childRequisition.height += margin.top  + margin.bottom;
            
            // Set upper bounds.
            this.hAdjustment.setUpper(childRequisition.width);
            this.vAdjustment.setUpper(childRequisition.height);
            
            // Check if scrollbars are needed.
            if (childRequisition.height > allocation.height)
            {
                allocation.width -= ScrolledWindow.vScrollBarSize.width;
                
                vScrollBarVisible = true;
            }
            
            if (childRequisition.width > allocation.width)
            {
                allocation.height -= ScrolledWindow.hScrollBarSize.height;
                
                hScrollBarVisible = true;
                
                // Check vertical scrollbar again.
                if (!vScrollBarVisible && (childRequisition.height > allocation.height))
                {
                    allocation.width -= ScrolledWindow.vScrollBarSize.width;
                    
                    vScrollBarVisible = true;
                }
            }
            
            childAllocation.width  = Math.max(childAllocation.width, allocation.width - margin.left - margin.right);
            childAllocation.height = Math.max(childAllocation.height, allocation.height - margin.top - margin.bottom);
            
            // Set child its view size.
            child.allocateSize(childAllocation);
        }
        
        // Set body size.
        this.bodyEl.setInnerSize({
            width: allocation.width,
            height: allocation.height
        });
        
        // Allocate space for scrollbars.
        if (hScrollBarVisible)
        {
            this.hScrollBar.show();
            this.hScrollBar.allocateSize({
                width: this.allocation.width - (vScrollBarVisible ? ScrolledWindow.vScrollBarSize.width : 0),
                height: ScrolledWindow.hScrollBarSize.height,
                x: 0,
                y: this.allocation.height - ScrolledWindow.hScrollBarSize.height
            });
        }
        else
        {
            this.hScrollBar.hide();
            this.hAdjustment.setValue(0);
        }
        
        if (vScrollBarVisible)
        {
            this.vScrollBar.show();
            this.vScrollBar.allocateSize({
                width: ScrolledWindow.vScrollBarSize.width,
                height: this.allocation.height - (hScrollBarVisible ? ScrolledWindow.hScrollBarSize.height : 0),
                x: this.allocation.width - ScrolledWindow.vScrollBarSize.width,
                y: 0
            });
        }
        else
        {
            this.vScrollBar.hide();
            this.vAdjustment.setValue(0);
        }
        
        // Set adjustments their page size.
        this.hAdjustment.setPageSize(allocation.width);
        this.vAdjustment.setPageSize(allocation.height);
    },
    
    /*
     * Event handlers.
     */
    
    onScroll: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        this.vAdjustment.setValue(this.vAdjustment.getValue() - e.getDelta() * this.vAdjustment.getStepIncrement());
        
        return true;
    },
    
    onAdjustmentValueChanged: function()
    {
        var child = this.children[0];
        if (child && child.visible)
        {
            // Create child allocation.
            var margin = child.margin;
            
            var childAllocation = {};
            var childSize       = child.getSize();
            
            childAllocation.x = margin.left - this.hAdjustment.getValue();
            childAllocation.y = margin.top  - this.vAdjustment.getValue();
            
            childAllocation.width  = childSize.width;
            childAllocation.height = childSize.height;
            
            // Set child its view size.
            child.allocateSize(childAllocation);
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The horizontal #Adjustment of the scrolled window. You may only change the value of this
         * adjustment, the other values are automatically determined.
         *
         * @type Adjustment
         */
        'h-adjustment': {
            read: true
        },
        /**
         * The vertical #Adjustment of the scrolled window. You may only change the value of this
         * adjustment, the other values are automatically determined.
         *
         * @type Adjustment
         */
        'v-adjustment': {
            read: true
        },
        /**
         * The horizontal scroll bar visibility policy. Determines when to show the horizontal scroll
         * bar.
         *
         * @type Policy
         */
        'h-policy': {
            write: function(hPolicy)
            {
                this.hPolicy = hPolicy;
                
                // Reallocate.
                this.allocateSize(Util.cloneShallow(this.allocation));
            },
            read: true,
            defaultValue: Policy.AUTOMATIC
        },
        /**
         * The vertical scroll bar visibility policy. Determines when to show the vertical scroll
         * bar.
         *
         * @type Policy
         */
        'v-policy': {
            write: function(vPolicy)
            {
                this.vPolicy = vPolicy;
                
                // Reallocate.
                this.allocateSize(Util.cloneShallow(this.allocation));
            },
            read: true,
            defaultValue: Policy.AUTOMATIC
        },
        /**
         * The shadow of the content of the scrolled window.
         *
         * @type ShadowType
         */
        'shadow-type': {
            write: function(shadowType)
            {
                this.bodyEl.replaceClass('x-shadow-' + this.shadowType, 'x-shadow-' + shadowType);
                
                this.shadowType = shadowType;
                
                this.layout();
            },
            read: true,
            defaultValue: ShadowType.IN
        }
    }
});
