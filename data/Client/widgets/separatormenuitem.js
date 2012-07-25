// Use strict mode if available.
"use strict";

/*
 * Separator menu item class.
 */

Class.define('SeparatorMenuItem', {
    extend: 'MenuItem',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        MenuItem.base.initialize.call(this);
        
        // Fetch elements.
        this.innerEl = this.el.find('.x-inner');
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-separator-menu-item">' +
                '<div class="x-inner" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        return this.innerEl.getSize();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'label' property.
        label: {
            write: function(label)
            {
                throw new Error('A separator menu item cannot have a label.');
            }
        },
        // Overrides 'submenu' property.
        submenu: {
            write: function(submenu)
            {
                throw new Error('A separator menu item cannot have a submenu.');
            }
        }
    }
});
