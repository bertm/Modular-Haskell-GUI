// Use strict mode if available.
"use strict";

/*
 * Spinner class.
 */

Class.define('Spinner', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    getHtml: function()
    {
        return '<div class="x-widget x-spinner" />';
    },
    
    getMinimumSize: function()
    {
        return this.el.getSize();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        active: {
            write: function(active)
            {
                active ? this.el.addClass('x-active') : this.el.removeClass('x-active');
                
                this.active = active;
            },
            read: true,
            defaultValue: false
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        start: function()
        {
            this.setActive(true);
        },
        stop: function()
        {
            this.setActive(false);
        }
    }
});
