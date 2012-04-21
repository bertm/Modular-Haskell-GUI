// Use strict mode if available.
"use strict";

/**
 * A slider widget for selecting a value from a range.
 */
Class.define('Scale', {
    extend: 'Range',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        Scale.base.initialize.call(this);
        
        // Set members.
        this.label = '';
        
        // Get elements.
        this.labelEl = this.el.find('.x-label');
        
        // Add events handlers.
        EventManager.registerHandler(this.el, EventMask.KEY_PRESS, this.onKeyPress, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div tabindex="0" class="x-widget x-scale x-orient-horizontal">' +
                '<div class="x-inner">' +
                    '<div class="x-track" />' +
                    '<div class="x-filled" />' +
                    '<div class="x-slider" />' +
                '</div>' +
                '<div class="x-label" selectable="off"></div>' +
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
        var labelSize = this.labelEl.getSize();
        
        if (this.orientation === Orientation.HORIZONTAL)
            return {width: 150, height: innerSize.height + labelSize.height};
        else
            return {width: innerSize.width + labelSize.width, height: 150};
        
        // TODO: Constants.
        
        /*
        
        TODO: Incorporate text in calculation.
        
        var textWidth = this.showText ? Util.measureTextSize(this.text).width : 0;
        
        if ((this.direction === Direction.EAST) || (this.direction === Direction.WEST))
            return {width: Math.max(textWidth, 150), height: 20};
        else
            return {width: Math.max(textWidth, 20), height: 150};
        */
    },
    
    setSliderOffset: function()
    {
        Scale.base.setSliderOffset.call(this);
        
        this.setLabelOffset();
    },
    
    setLabelOffset: function()
    {
        // Fetch dimensions.
        var sliderSize = this.sliderEl.getSize();
        var innerSize  = this.innerEl.getSize();
        var innerPos   = this.innerEl.getPosition();
        var innerFrame = this.innerEl.getFrame();
        var labelSize  = this.labelEl.getSize();
        
        // Set label position.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            var labelPosition = {
                x: Math.round(Util.clamp(
                    innerFrame.left + this.sliderOffset + Math.floor((sliderSize.width - labelSize.width) * 0.5),
                    0,
                    innerSize.width - labelSize.width
                )),
                y: innerPos.y - labelSize.height
            };
        }
        else
        {
            var labelPosition = {
                x: innerPos.x - labelSize.width,
                y: Math.round(Util.clamp(
                    innerFrame.top + this.sliderOffset + Math.floor((sliderSize.height - labelSize.height) * 0.5),
                    0,
                    innerSize.height - labelSize.height
                ))
            };
        }
        
        this.labelEl.setPosition(labelPosition);
    },
    
    allocateSize: function(allocation)
    {
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Vertically center inner element.
        var labelSize = this.labelEl.getSize();
        var innerSize = this.innerEl.getSize();
        if (this.orientation === Orientation.HORIZONTAL)
        {
            allocation.x = 0;
            allocation.y = Math.floor((allocation.height - innerSize.height + labelSize.height) * 0.5);
            
            allocation.height = innerSize.height;
        }
        else
        {
            allocation.x = Math.floor((allocation.width - innerSize.width + labelSize.width) * 0.5);
            allocation.y = 0;
            
            allocation.width = innerSize.width;
        }
        
        // Allocate size for inner element.
        Scale.base.allocateSize.call(this, allocation);
    },
    
    /*
     * Event handlers.
     */
    
    onKeyPress: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        // Determine methods.
        var incMethod = (e.hasModifier(EventModifierMask.CONTROL) ? 'incrementPage': 'increment');
        var decMethod = (e.hasModifier(EventModifierMask.CONTROL) ? 'decrementPage': 'decrement');
        
        // Check which key.
        switch (e.getKey())
        {
            case Key.HOME:
                this.adjustment.setValue(this.adjustment.getLower());
                break;
                
            case Key.END:
                this.adjustment.setValue(this.adjustment.getUpper());
                break;
                
            case Key.EQUAL:
            case Key.KP_ADD:
                this.adjustment[incMethod]();
                break;
                
            case Key.MINUS:
            case Key.KP_SUBTRACT:
                this.adjustment[decMethod]();
                break;
                
            case Key.PAGE_UP:
                this.adjustment.incrementPage();
                break;
                
            case Key.PAGE_DOWN:
                this.adjustment.decrementPage();
                break;
                
            case Key.LEFT:
            case Key.KP_4:
                // Left.
                if ((this.orientation === Orientation.HORIZONTAL) && this.inverted)
                    this.adjustment[incMethod]();
                else
                    this.adjustment[decMethod]();
                
                break;
                
            case Key.RIGHT:
            case Key.KP_6:
                // Right.
                if ((this.orientation === Orientation.HORIZONTAL) && this.inverted)
                    this.adjustment[decMethod]();
                else
                    this.adjustment[incMethod]();
                
                break;
                
            case Key.UP:
            case Key.KP_8:
                // Up.
                if ((this.orientation === Orientation.VERTICAL) && this.inverted)
                    this.adjustment[incMethod]();
                else
                    this.adjustment[decMethod]();
                
                break;
                
            case Key.DOWN:
            case Key.KP_2:
                // Down.
                if ((this.orientation === Orientation.VERTICAL) && this.inverted)
                    this.adjustment[decMethod]();
                else
                    this.adjustment[incMethod]();
                
                break;
                
            default:
                return;
        }
        
        return true;
    },
    
    onAdjustmentChanged: function(adj)
    {
        // Calculate label.
        var digits = (this.digits < 0) ? 6 : this.digits;
        var factor = Math.pow(10, digits);
        var label  = (Math.round(adj.getValue() * factor) / factor).toFixed(digits);
        
        // Set label.
        this.label = label;
        this.labelEl.setText(label);
        
        // Size label.
        this.labelEl.setSize(Util.measureTextSize(label));
        
        Scale.base.onAdjustmentChanged.call(this, adj);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The number of decimal places that are displayed in the value.
         *
         * @type int
         */
        digits: {
            write: function(digits)
            {
                this.digits = digits;
                
                this.onAdjustmentChanged(this.adjustment);
            },
            read: true,
            defaultValue: 1
        }
    }
});
