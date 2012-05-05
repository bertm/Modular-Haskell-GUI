// Use strict mode if available.
"use strict";

/**
 * A widget that displays a small to medium amount of text.
 *
 * The label widget displays a small amount of text. As the name implies, most labels are used to label another
 * widget such as a #Button or #Frame.
 */
Class.define('Label', {
    extend: 'Misc',
    
    /*
     * Private methods; initialization.
     */
    
    getHtml: function()
    {
        return '<div class="x-widget x-label" />';
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        if (this.wrap)
            return Util.measureTextSize(this.label, Label.getMaxWidth());
        else
            return Util.measureTextSize(this.label);
        
        // TODO: There is also an ellipsize option...
    },
    
    statics: {
        getMaxWidth: function()
        {
            if (!Label.maxWidth)
            {
                var size = Util.measureTextSize('This long string gives a good enough length for any line to have.');
                
                Label.maxWidth = size.width;
            }
            
            return Label.maxWidth;
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        label: {
            write: function(label)
            {
                this.label = label;
                
                this.el.setText(label);
                
                this.layout();
            },
            read: true,
            defaultValue: ''
        },
        /**
         * Whether the label text wraps around.
         *
         * @type bool
         */
        wrap: {
            write: function(wrap)
            {
                this.wrap = wrap;
                
                if (wrap)
                    this.el.addClass('x-wrap');
                else
                    this.el.removeClass('x-wrap');
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * The justification of the label.
         *
         * @type Justify
         */
        justify: {
            write: function(justify)
            {
                this.el.replaceClass('x-justify-' + this.justify, 'x-justify-' + justify);
                
                this.justify = justify;
            },
            read: true,
            defaultValue: Justify.LEFT
        }
    }
});
