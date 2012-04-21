// Use strict mode if available.
"use strict";

/*
 * Overlay class.
 */

Class.define('Overlay', {
    extend: 'Object',
    
    /*
     * Public methods.
     */
    
    construct: function()
    {
        var html = '<div class="x-overlay" />';
        
        this.el = new Element(html);
        
        this.el.setOpacity(0.5);
        this.el.setStyle('z-index', Element.getMaxZIndex());
        
    },
    
    destroy: function()
    {
        this.el.destroy();
    },
    
    show: function()
    {
        Element.getBody().append(this.el);
        this.el.show();
    },
    
    hide: function()
    {
        this.el.remove();
    },
    
    moveToFront: function()
    {
        this.el.setStyle('z-index', Element.getMaxZIndex());
    },
    
    setOpacity: function(opacity)
    {
        this.el.setOpacity(opacity);
    }
});
