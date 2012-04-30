// Use strict mode if available.
"use strict";

/**
 * Base class for widgets which visualize an #Adjustment.
 */
Class.define('Range', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        Range.base.initialize.call(this);
        
        // Set members.
        this.adjustment = new Adjustment();
        this.fraction   = 0;
        
        this.sliderOffset    = 0;
        this.minSliderOffset = 0;
        this.maxSliderOffset = 0;
        this.filledSize      = 0;
        this.trackSize       = 0;
        
        // Get elements.
        this.innerEl  = this.el.find('.x-inner');
        this.trackEl  = this.innerEl.find('.x-track');
        this.sliderEl = this.innerEl.find('.x-slider');
        this.filledEl = this.innerEl.find('.x-filled');
        
        // Add event handlers.
        EventManager.registerHandler(this.el, EventMask.SCROLL, this.onScroll, this);
        EventManager.registerHandler(this.el, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(this.sliderEl,
            EventMask.BUTTON_PRESS | EventMask.BUTTON_RELEASE | EventMask.MOTION,
            this.onSliderEvent, this);
        
        // Check for changes on adjustment.
        this.adjustment.connect('change', this.onAdjustmentChange, this);
    },
    
    destroy: function()
    {
        Range.base.destroy.call(this);
        
        // TODO: Inverse of the above.
        
        // Remove event handlers.
        Screen.disconnect('motion-event', this.onScreenMotion, this);
        this.adjustment.disconnect('change', this.onAdjustmentChange, this);
    },
    
    /*
     * Sizing.
     */
    
    setSliderOffset: function()
    {
        // Calculate minimum and maximum slider offset.
        var trackMargin = this.trackEl.getMargin();
        var sliderSize  = this.sliderEl.getSize();
        
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.minSliderOffset = trackMargin.left;
            this.maxSliderOffset = this.trackSize - trackMargin.right - sliderSize.width;
        }
        else
        {
            this.minSliderOffset = trackMargin.top;
            this.maxSliderOffset = this.trackSize - trackMargin.bottom - sliderSize.height;
        }
        
        // Fetch dimensions.
        var sliderSize = this.sliderEl.getSize();
        
        // Calculate offset.
        var inverted       = this.inverted;
        var fraction       = inverted ? (1 - this.fraction) : this.fraction;
        var sliderHalfSize = Math.floor(((this.orientation === Orientation.HORIZONTAL)
                           ? sliderSize.width
                           : sliderSize.height) * 0.5);
        
        // Set some members for use by subclasses.
        this.sliderOffset  = fraction * (this.maxSliderOffset - this.minSliderOffset) + this.minSliderOffset;
        this.filledSize    = (inverted
                             ? (this.maxSliderOffset + this.minSliderOffset - this.sliderOffset)
                             : this.sliderOffset)
                           + sliderHalfSize;
        
        // Set slider offset and filled size.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.sliderEl.setLeft(Math.round(this.sliderOffset));
            this.filledEl.setWidth(Math.round(this.filledSize));
        }
        else
        {
            this.sliderEl.setTop(Math.round(this.sliderOffset));
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
        
        // Set slider offset.
        this.setSliderOffset();
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
    
    onSliderEvent: function(e)
    {
        if (e.getType() === EventType.BUTTON_RELEASE)
        {
            this.sliderEl.removeClass('x-pressed');
            
            return;
        }
        
        if (!this.getIsSensitive() || !e.hasModifier(EventModifierMask.PRIMARY_BUTTON))
            return;
        
        if (e.getType() === EventType.BUTTON_PRESS)
        {
            this.sliderDragOffset = {
                x: e.getX() - this.sliderEl.getOffset().x,
                y: e.getY() - this.sliderEl.getOffset().y
            };
            
            this.sliderEl.addClass('x-pressed');
            
            return;
        }
        
        if (this.orientation === Orientation.HORIZONTAL)
            var offset = e.getX() - this.sliderEl.getOffset().x + this.sliderEl.getLeft() - this.sliderDragOffset.x;
        else
            var offset = e.getY() - this.sliderEl.getOffset().y + this.sliderEl.getTop() - this.sliderDragOffset.y;
        
        var fraction = (offset - this.minSliderOffset) / this.maxSliderOffset;
        
        if (this.inverted)
            fraction = 1 - fraction;
        
        this.adjustment.setFraction(fraction);
    },
    
    onAdjustmentChange: function(adj)
    {
        // Set new fraction.
        this.fraction = adj.getFraction();
        
        // Set slider offset.
        if (this.getIsVisible())
            this.setSliderOffset();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The #Adjustment that contains the current value and bounds of this scale.
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
         * Whether the direction of the slider to increase range value is inverted.
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
         * The orientation of the scale.
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
