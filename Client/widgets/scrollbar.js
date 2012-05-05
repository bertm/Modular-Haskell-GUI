// Use strict mode if available.
"use strict";

/**
 * A scroll bar.
 */
Class.define('ScrollBar', {
    extend: 'Range',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        ScrollBar.base.initialize.call(this);
        
        // Get elements.
        this.backwardStepperEl = this.el.find('.x-stepper-backward');
        this.forwardStepperEl  = this.el.find('.x-stepper-forward');
        
        // Add event handlers.
        EventManager.registerHandler(this.forwardStepperEl, EventMask.BUTTON_PRESS | EventMask.BUTTON_RELEASE,
            this.onStepperButtonPress, this, this.forwardStepperEl);
        EventManager.registerHandler(this.backwardStepperEl, EventMask.BUTTON_PRESS | EventMask.BUTTON_RELEASE,
            this.onStepperButtonPress, this, this.backwardStepperEl);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-scroll-bar x-orient-horizontal">' +
                '<div class="x-stepper-backward" />' +
                '<div class="x-inner">' +
                    '<div class="x-track" />' +
                    '<div class="x-filled" />' +
                    '<div class="x-slider" />' +
                '</div>' +
                '<div class="x-stepper-forward" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Sizing.
     */
    
    getMinimumSize: function()
    {
        // Fetch dimensions.
        var innerSize = this.innerEl.getSize();
        
        if (this.orientation === Orientation.HORIZONTAL)
            return {width: this.backwardStepperEl.getWidth() + this.forwardStepperEl.getWidth(), height: innerSize.height};
        else
            return {width: innerSize.width, height: this.backwardStepperEl.getHeight() + this.forwardStepperEl.getHeight()};
    },
    
    // Overrides.
    setSliderOffset: function()
    {
        // Set slider its size.
        this.setSliderSize();
        
        ScrollBar.base.setSliderOffset.call(this);
    },
    
    setSliderSize: function()
    {
        // Subtract margin from track size: this is our maximum width.
        var trackMargin = this.trackEl.getMargin();
        
        if (this.orientation === Orientation.HORIZONTAL)
            var trackSize = this.trackSize - trackMargin.left - trackMargin.right;
        else
            var trackSize = this.trackSize - trackMargin.top - trackMargin.bottom;
        
        // Determine adjustment range.
        var pageSize = this.adjustment.getPageSize();
        var range = this.adjustment.getUpper() - this.adjustment.getLower();
        
        // Set slider size.
        if (range === 0)
            var size = trackSize;
        else
            var size = Math.min(trackSize, Math.ceil(trackSize / ((pageSize === 0) ? range : range / pageSize)));
        
        // Hide or show slider, depending on its size.
        if (trackSize < 10) // TODO: Constant.
            this.sliderEl.hide();
        else
            this.sliderEl.show();
        
        // Set a minimum size.
        if (size < 10) // TODO: Constant.
            size = 10;
        
        // Set it on the slider.
        if (this.orientation === Orientation.HORIZONTAL)
            this.sliderEl.setWidth(size);
        else
            this.sliderEl.setHeight(size);
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Vertically center inner element.
        var innerSize = this.innerEl.getSize();
        if (this.orientation === Orientation.HORIZONTAL)
        {
            allocation.x = 0;
            allocation.y = Math.floor((allocation.height - innerSize.height) * 0.5);
            
            allocation.height = innerSize.height;
        }
        else
        {
            allocation.x = Math.floor((allocation.width - innerSize.width) * 0.5);
            allocation.y = 0;
            
            allocation.width = innerSize.width;
        }
        
        // Subtract stepper buttons, and position them.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            allocation.width -= this.backwardStepperEl.getSize().width + this.forwardStepperEl.getSize().width;
            allocation.x     += this.backwardStepperEl.getSize().width;
            
            this.backwardStepperEl.setPosition({x: 0, y: allocation.y});
            this.forwardStepperEl.setPosition({x: allocation.x + allocation.width, y: allocation.y});
        }
        else
        {
            allocation.height -= this.backwardStepperEl.getSize().height + this.forwardStepperEl.getSize().height;
            allocation.y      += this.backwardStepperEl.getSize().height;
            
            this.backwardStepperEl.setPosition({x: allocation.x, y: 0});
            this.forwardStepperEl.setPosition({x: allocation.x, y: allocation.y + allocation.height});
        }
        
        // Allocate size for inner element.
        ScrollBar.base.allocateSize.call(this, allocation);
    },
    
    /*
     * Event handlers.
     */
    
    onStepperButtonPress: function(e, element)
    {
        // Handle release event.
        if (e.getType() === EventType.BUTTON_RELEASE)
        {
            // Remove pressed state from steppers.
            this.backwardStepperEl.removeClass('x-pressed');
            this.forwardStepperEl.removeClass('x-pressed');
            
            return;
        }
        
        // Handle press event.
        if (!this.getIsSensitive() || !e.hasModifier(EventModifierMask.PRIMARY_BUTTON))
            return;
        
        if ((element === this.forwardStepperEl) ^ this.inverted)
        {
            this.adjustment.increment();
            this.forwardStepperEl.addClass('x-pressed');
        }
        else
        {
            this.adjustment.decrement();
            this.backwardStepperEl.addClass('x-pressed');
        }
        
        // TODO: Interval.
    },
    
    onAdjustmentChange: function(adj)
    {
        var backwardStepperActive = (adj.getValue() !== adj.getLower());
        var forwardStepperActive  = (adj.getValue() !== (adj.getUpper() - adj.getPageSize()));
        
        backwardStepperActive ?
            this.backwardStepperEl.addClass('x-active') :
            this.backwardStepperEl.removeClass('x-active');
        
        forwardStepperActive ?
            this.forwardStepperEl.addClass('x-active') :
            this.forwardStepperEl.removeClass('x-active');
        
        (backwardStepperActive || forwardStepperActive) ?
            this.sliderEl.addClass('x-active') :
            this.sliderEl.removeClass('x-active');
        
        ScrollBar.base.onAdjustmentChange.call(this, adj);
    }
});
