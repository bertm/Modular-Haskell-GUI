// Use strict mode if available.
"use strict";

/*
 * Menu class.
 */

Class.define('Menu', {
    extend: 'Box',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Menu.base.initialize.call(this);
        
        // Set members.
        this.setOrientation(Orientation.VERTICAL);
        
        // Attach event handlers.
        EventManager.registerHandler(this.el, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(Element.getBody(), EventMask.BUTTON_PRESS, this.onBodyButtonPress, this);
        
        // Always show element, its visibility is determined by whether it is in the DOM.
        this.el.show();
    },
    
    destroy: function()
    {
        // Remove us from parent menu item.
        if (this.parentMenuItem)
            this.parentMenuItem.setSubmenu(null);
        
        Menu.base.destroy.call(this);
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-menu x-body" />';
    },
    
    setParent: function(parent)
    {
        throw new Error('Menu cannot be added to another widget.');
    },
    
    setParentMenuItem: function(parentMenuItem)
    {
        // Check for emptying.
        if (parentMenuItem === null)
        {
            delete this.parentMenuItem;
            
            // Hide us.
            this.hide();
            
            return;
        }
        
        // Check parent menu item.
        if (this.parentMenuItem)
            throw new Error('Menu has already been added to a menu item.');
        
        // Set parent menu item.
        this.parentMenuItem = parentMenuItem;
    },
    
    /*
     * Event handlers.
     */
    
    onButtonPress: function(e)
    {
        // Stop propagation.
        return true;
    },
    
    onItemEnter: function(item)
    {
        // Set new active item. Will also stop submenu delay.
        this.setActive(item);
        
        // Show its menu with a delay.
        if (item.getIsSensitive())
        {
            var submenu = item.getSubmenu();
            if (submenu)
            {
                this.submenuDelay = Util.delay(500, // TODO: Constant.
                    function()
                    {
                        submenu.show();
                        delete this.submenuDelay;
                    }, this);
            }
        }
    },
    
    onItemButtonPress: function(item)
    {
        // Set new active item. Will also stop submenu delay.
        this.setActive(item);
        
        // Show its menu immediately.
        var submenu = item.getSubmenu();
        if (submenu)
        {
            if (item.getIsSensitive())
                submenu.show();
            
            // Stop propagation.
            return true;
        }
        
        // Let menus close.
        return false;
    },
    
    onItemLeave: function(item)
    {
        if (this.active && (!this.active.getSubmenu() || !this.active.getSubmenu().getVisible()))
            this.setActive(null);
    },
    
    onBodyButtonPress: function()
    {
        this.hide();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'visible' property.
        visible: {
            write: function(visible)
            {
                this.visible = visible;
                
                if (visible)
                {
                    // Set z-index.
                    this.el.setStyle('z-index', Element.getMaxZIndex());
                    
                    // Append element to body.
                    Element.getBody().append(this.el);
                    
                    // Set position.
                    if (this.parentMenuItem)
                    {
                        // Get menu item its offset and size.
                        var offset = this.parentMenuItem.el.getOffset(); // TODO: Property.
                        var size   = this.parentMenuItem.getSize();
                        
                        // Get menu.
                        var menu = this.parentMenuItem.getParent(); // TODO: Shell?
                        
                        if (menu && (menu instanceof MenuBar))
                        {
                            this.setPosition({x: offset.x, y: offset.y + size.height});
                        }
                        else
                        {
                            this.setPosition({x: offset.x + size.width, y: offset.y});
                        }
                    }
                    else
                    {
                        // TODO: Use cursor position.
                        this.setPosition({x: 0, y: 0});
                    }
                }
                else
                {
                    // Clear active item.
                    this.setActive(null);
                    
                    // Remove element from DOM.
                    this.el.remove();
                }
            }
        },
        // Overrides 'is-visible' property.
        'is-visible': {
            read: function()
            {
                return this.visible;
            }
        },
        // Overrides 'is-sensitive' property.
        'is-sensitive': {
            read: function()
            {
                return this.visible && this.sensitive;
            }
        },
        active: {
            write: function(active)
            {
                // Clear submenu delay.
                if (this.submenuDelay)
                {
                    Util.clearDelay(this.submenuDelay);
                    delete this.submenuDelay;
                }
                
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
        },
        position: { // TODO: Move to widget? See Window, Tooltip.
            write: function(position)
            {
                // TODO: Check value.
                this.allocation = {
                    x: position.x,
                    y: position.y,
                    width: this.allocation.width,
                    height: this.allocation.height
                };
                
                this.layout();
            },
            read: function()
            {
                return {x: this.allocation.x, y: this.allocation.y};
            }
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget, expand, fill)' action.
        add: function(menuItem, expand, fill) // TODO: Remove expand (expand), fill (scale).
        {
            if (!(menuItem instanceof MenuItem))
                throw new Error('A menu bar can only contain menu items.');
            
            if (expand === undefined)
                expand = false;
            
            Menu.base.add.call(this, menuItem, expand, fill);
            
            // Hook into its enter, leave and button press event handlers.
            menuItem.enableEvents(EventMask.ENTER | EventMask.LEAVE | EventMask.BUTTON_PRESS);
            menuItem.connect('enter-event', this.onItemEnter, this);
            menuItem.connect('leave-event', this.onItemLeave, this);
            menuItem.connect('button-press-event', this.onItemButtonPress, this);
            
            this.layout();
        },
        
        // Overrides 'remove(widget)' action.
        remove: function(menuItem)
        {
            var index = Box.base.remove.call(this, menuItem);
            
            // Disconnect enter, leave and button press event handlers.
            menuItem.disconnect('enter-event', this.onItemEnter, this);
            menuItem.disconnect('leave-event', this.onItemLeave, this);
            menuItem.disconnect('button-press-event', this.onItemButtonPress, this);
            
            this.layout();
        }
    }
});
