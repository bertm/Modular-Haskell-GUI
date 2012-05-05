// Use strict mode if available.
"use strict";

/**
 * A horizontal or vertical line to separate widgets.
 */
Class.define('Separator', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-separator-horizontal">' +
                '<div class="x-inner" />' +
            '</div>';
        
        return html;
    },
    
    getMinimumSize: function()
    {
        // TODO: Implement thickness.
        
        if (this.orientation === Orientation.HORIZONTAL)
            return {width: 1, height: 4};
        else
            return {width: 4, height: 1};
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The orientation of the separator.
         *
         * @type Orientation
         */
        orientation: {
            write: function(orientation)
            {
                this.el.replaceClass('x-separator-' + this.orientation, 'x-separator-' + orientation);
                
                this.orientation = orientation;
                
                this.layout();
            },
            read: true,
            defaultValue: Orientation.HORIZONTAL
        }
    }
});
