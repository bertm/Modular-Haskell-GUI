// Use strict mode if available.
"use strict";

/**
 * The scroll area class provides a scrolling view onto another widget.
 */
Class.define('ScrollArea', {
    extend: 'Bin',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        ScrollArea.base.initialize.call(this);
        
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
            '<div class="x-widget x-scroll-area">' +
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
            var childRequisition = child.getSizeRequisition();
        else
            var childRequisition = {natWidth: 0, natHeight: 0};
        
        // Only reallocate when child requisition has changed.
        if ((this.hAdjustment.getUpper() !== childRequisition.minimum.width) ||
            (this.vAdjustment.getUpper() !== childRequisition.minimum.height))
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
    
    getSizeRequisition: function()
    {
        // Fetch scrollbars their size.
        if (!ScrollArea.hScrollBarSize)
        {
            ScrollArea.hScrollBarSize = this.hScrollBar.getSizeRequisition().natural;
            ScrollArea.vScrollBarSize = this.vScrollBar.getSizeRequisition().natural;
        }
        
        // Fetch size.
        return ScrollArea.base.getSizeRequisition.call(this);
    },
    
    getPreferredSize: function()
    {
        var hScrollBarSize = ScrollArea.hScrollBarSize;
        var vScrollBarSize = ScrollArea.vScrollBarSize;
        
        return {
            minimum: {
                width:  vScrollBarSize.width  + hScrollBarSize.width,
                height: vScrollBarSize.height + hScrollBarSize.height
            },
            natural: {
                width:  Math.max(200, vScrollBarSize.width  + hScrollBarSize.width),
                height: Math.max(200, vScrollBarSize.height + hScrollBarSize.height)
            }
        };
    },
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Scrollbars are not needed yet.
        var hScrollBarVisible = (this.hPolicy === Policy.ALWAYS),
            vScrollBarVisible = (this.vPolicy === Policy.ALWAYS);
        
        // Get child its minimum size.
        var child = this.children[0];
        if (child && child.visible)
        {
            // Get child minimum size.
            var minSize = Util.cloneShallow(child.getSizeRequisition()).minimum;
            
            // Set upper bounds.
            this.hAdjustment.setUpper(minSize.width);
            this.vAdjustment.setUpper(minSize.height);
            
            // Check if scrollbars are needed.
            if (vScrollBarVisible ||
                ((minSize.height > allocation.height) && (this.vPolicy !== Policy.NEVER)))
            {
                allocation.width -= ScrollArea.vScrollBarSize.width;
                
                vScrollBarVisible = true;
            }
            
            if (hScrollBarVisible ||
                ((minSize.width > allocation.width) && (this.hPolicy !== Policy.NEVER)))
            {
                allocation.height -= ScrollArea.hScrollBarSize.height;
                
                hScrollBarVisible = true;
                
                // Check vertical scrollbar again.
                if (!vScrollBarVisible &&
                    (minSize.height > allocation.height) && (this.vPolicy !== Policy.NEVER))
                {
                    allocation.width -= ScrollArea.vScrollBarSize.width;
                    
                    vScrollBarVisible = true;
                }
            }
            
            // Set child its view size.
            child.allocateSize({
                x: -this.hAdjustment.getValue(),
                y: -this.vAdjustment.getValue(),
                width:  Math.max(minSize.width,  allocation.width),
                height: Math.max(minSize.height, allocation.height)
            });
        }
        
        // Set body size.
        this.bodyEl.setInnerSize({
            width:  allocation.width,
            height: allocation.height
        });
        
        // Allocate space for scrollbars.
        var bodyFrame = this.bodyEl.getFrame();
        if (hScrollBarVisible)
        {
            this.hScrollBar.show();
            this.hScrollBar.allocateSize({
                width: allocation.width + bodyFrame.left + bodyFrame.right,
                height: ScrollArea.hScrollBarSize.height,
                x: 0,
                y: allocation.height + bodyFrame.top + bodyFrame.bottom
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
                width: ScrollArea.vScrollBarSize.width,
                height: allocation.height + bodyFrame.top + bodyFrame.bottom,
                x: allocation.width + bodyFrame.left + bodyFrame.right,
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
            // Allocate child.
            var childSize = child.getSize();
            
            child.allocateSize({
                x: -this.hAdjustment.getValue(),
                y: -this.vAdjustment.getValue(),
                width: childSize.width,
                height: childSize.height
            });
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
                if (this.getIsVisible())
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
                if (this.getIsVisible())
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
