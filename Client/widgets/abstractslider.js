// Use strict mode if available.
"use strict";

/**
 * Base class for widgets which visualize an #Adjustment.
 */
Class.define('AbstractSlider', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        AbstractSlider.base.initialize.call(this);
        
        // Set members.
        this.adjustment = new Adjustment();
        this.fraction   = 0;
        
        this.thumbOffset    = 0;
        this.minThumbOffset = 0;
        this.maxThumbOffset = 0;
        this.filledSize     = 0;
        this.trackSize      = 0;
        
        // Get elements.
        this.innerEl  = this.el.find('.x-inner');
        this.trackEl  = this.innerEl.find('.x-track');
        this.thumbEl  = this.innerEl.find('.x-thumb');
        this.filledEl = this.innerEl.find('.x-filled');
        
        // Add event handlers.
        EventManager.registerHandler(this.el, EventMask.SCROLL, this.onScroll, this);
        EventManager.registerHandler(this.el, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(this.thumbEl,
            EventMask.BUTTON_PRESS | EventMask.BUTTON_RELEASE | EventMask.MOTION,
            this.onThumbEvent, this);
        
        // Check for changes on adjustment.
        this.adjustment.connect('change', this.onAdjustmentChange, this);
    },
    
    destroy: function()
    {
        AbstractSlider.base.destroy.call(this);
        
        // TODO: Inverse of the above.
        
        // Remove event handlers.
        Screen.disconnect('motion-event', this.onScreenMotion, this);
        this.adjustment.disconnect('change', this.onAdjustmentChange, this);
    },
    
    /*
     * Sizing.
     */
    
    setThumbOffset: function()
    {
        // Calculate minimum and maximum thumb offset.
        var trackMargin = this.trackEl.getMargin();
        var thumbSize   = this.thumbEl.getSize();
        
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.minThumbOffset = trackMargin.left;
            this.maxThumbOffset = this.trackSize - trackMargin.right - thumbSize.width;
        }
        else
        {
            this.minThumbOffset = trackMargin.top;
            this.maxThumbOffset = this.trackSize - trackMargin.bottom - thumbSize.height;
        }
        
        // Fetch dimensions.
        var thumbSize = this.thumbEl.getSize();
        
        // Calculate offset.
        var inverted      = this.inverted;
        var fraction      = inverted ? (1 - this.fraction) : this.fraction;
        var thumbHalfSize = Math.floor(((this.orientation === Orientation.HORIZONTAL)
                          ? thumbSize.width
                          : thumbSize.height) * 0.5);
        
        // Set some members for use by subclasses.
        this.thumbOffset  = fraction * (this.maxThumbOffset - this.minThumbOffset) + this.minThumbOffset;
        this.filledSize    = (inverted
                             ? (this.maxThumbOffset + this.minThumbOffset - this.thumbOffset)
                             : this.thumbOffset)
                           + thumbHalfSize;
        
        // Set thumb offset and filled size.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.thumbEl.setLeft(Math.round(this.thumbOffset));
            this.filledEl.setWidth(Math.round(this.filledSize));
        }
        else
        {
            this.thumbEl.setTop(Math.round(this.thumbOffset));
            this.filledEl.setHeight(Math.round(this.filledSize));
        }
    },
    
    allocateSize: function(allocation)
    {
        // Fetch dimensions.
        var innerSize  = this.innerEl.getSize();
        var innerFrame = this.innerEl.getFrame();
        
        // Set inner element its size.
        this.innerEl.setSize({width: allocation.width, height: allocation.height});
        this.innerEl.setPosition({x: allocation.x, y: allocation.y});
        
        // Calculate track size and set it.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.trackSize = allocation.width - innerFrame.left - innerFrame.right;
            this.trackEl.setWidth(this.trackSize);
        }
        else
        {
            this.trackSize = allocation.height - innerFrame.top - innerFrame.bottom;
            this.trackEl.setHeight(this.trackSize);
        }
        
        // Set thumb offset.
        this.setThumbOffset();
    },
    
    /*
     * Event handlers.
     */
    
    onScroll: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        var factor = this.inverted ? 1 : -1;
        this.adjustment.setValue(this.adjustment.getValue() + factor * e.getDelta() * this.adjustment.getStepIncrement());
        
        return true;
    },
    
    onButtonPress: function()
    {
        if (!this.getIsSensitive())
            return;
        
        this.focus();
    },
    
    onThumbEvent: function(e)
    {
        if (e.getType() === EventType.BUTTON_RELEASE)
        {
            this.thumbEl.removeClass('x-pressed');
            
            return;
        }
        
        if (!this.getIsSensitive() || !e.hasModifier(EventModifierMask.PRIMARY_BUTTON))
            return;
        
        if (e.getType() === EventType.BUTTON_PRESS)
        {
            this.thumbDragOffset = {
                x: e.getX() - this.thumbEl.getOffset().x,
                y: e.getY() - this.thumbEl.getOffset().y
            };
            
            this.thumbEl.addClass('x-pressed');
            
            return;
        }
        
        if (this.orientation === Orientation.HORIZONTAL)
            var offset = e.getX() - this.thumbEl.getOffset().x + this.thumbEl.getLeft() - this.thumbDragOffset.x;
        else
            var offset = e.getY() - this.thumbEl.getOffset().y + this.thumbEl.getTop() - this.thumbDragOffset.y;
        
        var fraction = (offset - this.minThumbOffset) / this.maxThumbOffset;
        
        if (this.inverted)
            fraction = 1 - fraction;
        
        this.adjustment.setFraction(fraction);
    },
    
    onAdjustmentChange: function(adj)
    {
        // Set new fraction.
        this.fraction = adj.getFraction();
        
        // Set thumb offset.
        if (this.getIsVisible())
            this.setThumbOffset();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The #Adjustment that contains the current value and bounds of this slider.
         *
         * @type Adjustment
         */
        adjustment: {
            write: function(adjustment)
            {
                // Check whether different.
                if (this.adjustment == adjustment)
                    return false;
                
                // Replace signal handler.
                this.adjustment.disconnect('change', this.onAdjustmentChange, this);
                adjustment.connect('change', this.onAdjustmentChange, this);
                
                this.adjustment = adjustment;
                
                // Set new value.
                this.onAdjustmentChange(adjustment);
            },
            read: true
        },
        /**
         * Whether the direction of the slider to increase value is inverted.
         *
         * @type bool
         */
        inverted: {
            write: function(inverted)
            {
                inverted ? this.el.addClass('x-inverted') : this.el.removeClass('x-inverted');
                
                this.inverted = inverted;
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * The orientation of the slider.
         *
         * @type Orientation
         */
        orientation: {
            write: function(orientation)
            {
                this.el.replaceClass('x-orient-' + this.orientation, 'x-orient-' + orientation);
                
                this.orientation = orientation;
                
                this.layout();
            },
            read: true,
            defaultValue: Orientation.HORIZONTAL
        }
    }
});
