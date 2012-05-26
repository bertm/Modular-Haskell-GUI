// Use strict mode if available.
"use strict";

/*
 * Menu bar class.
 */

Class.define('MenuBar', {
    extend: 'Box',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        MenuBar.base.initialize.call(this);
        
        // Attach event handlers.
        EventManager.registerHandler(Element.getBody(), EventMask.BUTTON_PRESS, this.onBodyButtonPress, this);
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-menu-bar x-body" />';
    },
    
    /*
     * Event handlers.
     */
    
    onItemButtonPress: function(item)
    {
        // Check if already active: close menu.
        if (this.active === item)
            return false;
        
        // Set new active item.
        this.setActive(item);
        
        // Show its menu immediately.
        var submenu = item.getSubmenu();
        if (submenu)
        {
            if (item.getIsSensitive())
                submenu.show();
        }
        
        // We handled it.
        return true;
    },
    
    onItemEnter: function(item)
    {
        if (this.active)
        {
            this.onItemButtonPress(item);
        }
    },
    
    onBodyButtonPress: function()
    {
        this.setActive(null);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'visible' property.
        visible: {
            write: function(visible)
            {
                MenuBar.base.setVisible.call(this, visible);
                
                if (!this.visible)
                {
                    // Clear active item.
                    this.setActive(null);
                }
            }
        },
        active: {
            // Hide existing submenu.
            write: function(active)
            {
                // Unset previous item.
                if (this.active)
                {
                    this.active.setActive(false);
                    
                    // Hide its submenu.
                    var submenu = this.active.getSubmenu();
                    if (submenu)
                        submenu.hide();
                }
                
                // Show new item its submenu.
                if (active)
                    active.setActive(true);
                
                // Set new active item.
                this.active = active;
            },
            read: true,
            defaultValue: null
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides add(widget, expand, fill).
        add: function(menuItem, expand, fill) // TODO: Remove expand (expand), fill (scale).
        {
            if (!(menuItem instanceof MenuItem))
                throw new Error('A menu bar can only contain menu items.');
            
            if (expand === undefined)
                expand = false;
            
            MenuBar.base.add.call(this, menuItem, expand, fill);
            
            // Hook into its enter and button press event handlers.
            menuItem.enableEvents(EventMask.ENTER | EventMask.BUTTON_PRESS);
            menuItem.connect('enter-event', this.onItemEnter, this);
            menuItem.connect('button-press-event', this.onItemButtonPress, this);
            
            this.layout();
        },
        
        // Overrides remove(widget).
        remove: function(menuItem)
        {
            var index = Box.base.remove.call(this, menuItem);
            
            // Disconnect enter and button press event handlers.
            menuItem.disconnect('enter-event', this.onItemEnter, this);
            menuItem.disconnect('button-press-event', this.onItemButtonPress, this);
            
            this.layout();
        }
    }
});
