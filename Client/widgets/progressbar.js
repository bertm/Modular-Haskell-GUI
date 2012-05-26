// Use strict mode if available.
"use strict";

/*
 * Progress bar class.
 */

Class.define('ProgressBar', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        ProgressBar.base.initialize.call(this);
        
        // Get elements.
        this.barEl       = this.el.find('.x-bar');
        this.barLabelEl  = this.barEl.find('.x-label');
        this.textLabelEl = this.el.find('.x-text .x-label');
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-progress-bar x-orient-horizontal">' +
                '<div class="x-text">' +
                    '<div class="x-label" />' +
                '</div>' +
                '<div class="x-bar">' +
                    '<div class="x-label" />' +
                '</div>' +
            '</div>';
        
        return html;
    },
    
    setLabelText: function(text)
    {
        this.barLabelEl.setText(text);
        this.textLabelEl.setText(text);
    },
    
    /*
     * Sizing.
     */
    
    getMinimumSize: function()
    {
        var textSize = this.showText ? Util.measureTextSize(this.text) : {width: 0, height: 0};
        
        if (this.orientation === Orientation.HORIZONTAL)
            return {width: Math.max(textSize.width, 150), height: Math.max(20, textSize.height)};
        else
            return {width: Math.max(textSize.width, 20), height: Math.max(150, textSize.height)};
        
        // TODO: Constants.
    },
    
    setBarSize: function(allocation)
    {
        // Determine allocation.
        if (!allocation)
        {
            var frameSize = this.getFrameSize();
            
            allocation = {
                width: this.allocation.width - frameSize.width,
                height: this.allocation.height - frameSize.height
            };
        }
        
        // Set bar size.
        var barSize;
        if (this.orientation === Orientation.HORIZONTAL)
        {
            barSize = {
                width:  Math.floor(allocation.width * this.fraction),
                height: allocation.height
            };
        }
        else
        {
            barSize = {
                width:  allocation.width,
                height: Math.floor(allocation.height * this.fraction)
            };
        }
        
        if ((barSize.width <= 0) || (barSize.height <= 0))
        {
            this.barEl.hide();
        }
        else
        {
            this.barEl.show();
            this.barEl.setSize(barSize);
        }
    },
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Set bar its size.
        this.setBarSize(allocation);
        
        // Set line height, Opera need it on text label as well.
        this.el.setStyle('line-height', allocation.height + 'px');
        this.textLabelEl.setStyle('line-height', allocation.height + 'px');
        
        // Set bar label its size.
        var frame = this.barEl.getFrame();
        var barLabelSize = {
            width:  allocation.width  - frame.left,
            height: allocation.height - frame.top
        };
        
        this.barLabelEl.setSize(barLabelSize);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The progress fraction. Should be a value between 0 and 1.
         *
         * @type float
         */
        fraction: {
            write: function(fraction)
            {
                fraction = Util.clamp(fraction, 0, 1);
                
                if (fraction === this.fraction)
                    return false;
                
                this.fraction = fraction;
                
                if (this.getIsVisible())
                    this.setBarSize();
            },
            read: true,
            defaultValue: 0
        },
        /**
         * The text to show in the progress bar. Enable #show-text to show it.
         *
         * @type string
         */
        text: {
            write: function(text)
            {
                text = text.replace(/\n/g, ' ');
                
                if (this.showText)
                    this.setLabelText(text);
                
                this.text = text;
                
                this.layout();
            },
            read: true,
            defaultValue: ''
        },
        /**
         * Whether to show text. Set #text to show it.
         *
         * @type bool
         */
        'show-text': {
            write: function(showText)
            {
                this.showText = showText;
                
                if (showText)
                    this.setLabelText(this.text);
                else
                    this.setLabelText('');
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * The orientation of the progress bar.
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
        },
        /**
         * Whether the progress bar its grow direction is inverted.
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
        }
    }
    
    // TODO: mode, ellipsize (just do it?).
});
