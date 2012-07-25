// Use strict mode if available.
"use strict";

/*
 * Frame class.
 */

Class.define('Frame', {
    extend: 'Bin',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Frame.base.initialize.call(this);
        
        // Fetch elements.
        this.legendEl = this.el.find('.x-legend');
        this.labelEl  = this.el.find('.x-legend .x-label');
    },
    
    getHtml: function()
    {
        var html =
            '<fieldset class="x-widget x-frame x-shadow-etched-in">' +
                '<legend class="x-legend">' +
                    '<div class="x-label" />' + // TODO: Make variable.
                '</legend>' +
                '<div class="x-body" />' + // TODO: Remove.
            '</fieldset>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getPreferredSize: function()
    {
        var prefSize = Frame.base.getPreferredSize.call(this);
        
        var legendWidth = (this.label ? this.legendEl.getWidth() : 0) + 40; // TODO: For now, later, allow widget labels.
        
        prefSize.minimum.width = Math.max(prefSize.minimum.width, legendWidth);
        
        return prefSize;
    },
    
    getFrameSize: function()
    {
        var frameSize = Frame.base.getFrameSize.call(this);
        
        if (this.label) // TODO: For now, later, allow widget labels.
        {
            // Add legend its height.
            frameSize.height += this.legendEl.getHeight();
            
            // Top border does not count for a fieldset.
            frameSize.height -= this.el.getBorder().top;
        }
        
        return frameSize;
    },
    
    /*
     * Properties.
     */
    
    properties: {
        label: {
            write: function(label) // TODO: Widget label support.
            {
                this.labelEl.setText(label);
                
                this.label = label;
            },
            read: true,
            defaultValue: ''
        },
        /**
         * The shadow type of the frame.
         *
         * @type ShadowType
         */
        'shadow-type': {
            write: function(shadowType)
            {
                this.el.replaceClass('x-shadow-' + this.shadowType, 'x-shadow-' + shadowType);
                
                this.shadowType = shadowType;
                
                this.layout();
            },
            read: true,
            defaultValue: ShadowType.ETCHED_IN
        }
    }
});
