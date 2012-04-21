// Use strict mode if available.
"use strict";

/*
 * Menu item class.
 */

Class.define('MenuItem', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        MenuItem.base.initialize.call(this);
        
        // Fetch elements.
        this.labelEl = this.el.find('.x-label');
        
        // Attach event handlers.
        EventManager.registerHandler(this.el, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(this.el, EventMask.BUTTON_RELEASE, this.onButtonRelease, this);
    },
    
    destroy: function()
    {
        // Remove event handler from screen.
        Screen.disconnect('button-release-event', this.onScreenButtonRelease, this);
        
        // Break link with submenu.
        if (this.submenu)
            this.submenu.setParentMenuItem(null);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-menu-item">' +
                '<div class="x-label" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Active setter.
     */
    
    setActive: function(active)
    {
        this.active = active;
        
        // Add or remove class.
        active ? this.el.addClass('x-active') : this.el.removeClass('x-active');
        
        // TODO: Have a machanism for this.
        this.signalDispatcher.emit('active-change', this);
        this.signalDispatcher.emit('property-change', this, 'active');
        this.signalDispatcher.emit('change', this);
    },
    
    /*
     * Event handlers.
     */
    
    onButtonPress: function()
    {
        if (!this.getIsSensitive())
            return;
        
        this.el.addClass('x-pressed');
    },
    
    onButtonRelease: function()
    {
        this.el.removeClass('x-pressed');
        
        if (!this.getIsSensitive())
            return;
        
        // Emit activate signal, if we do not have a submenu.
        if (!this.submenu)
            this.signalDispatcher.emit('activate', this);
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        return Util.measureTextSize(this.label);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'visible' property.
        visible: {
            write: function(visible)
            {
                // Hide submenu.
                if (!visible && this.submenu)
                {
                    this.submenu.hide();
                }
                
                MenuItem.base.setVisible.call(this, visible);
            }
        },
        label: {
            write: function(label)
            {
                this.label = label;
                
                this.labelEl.setText(label);
                
                this.layout();
            },
            read: true,
            defaultValue: ''
        },
        active: {
            read: true,
            defaultValue: false
        },
        submenu: {
            write: function(submenu)
            {
                if (!(submenu instanceof Menu))
                    throw new Error('Submenu property must be a menu.');
                
                if (submenu)
                    submenu.setParentMenuItem(this);
                
                if (this.menu)
                    this.submenu.setParentMenuItem(null);
                
                this.submenu = submenu;
            },
            read: true,
            defaultValue: null
        }
    }
});
