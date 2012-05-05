// Use strict mode if available.
"use strict";

/*
 * Menu class.
 */

Class.define('Tooltip', {
    extend: 'Bin',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Tooltip.base.initialize.call(this);
        
        // Always show element, its visibility is determined by whether it is in the DOM.
        this.el.show();
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-tooltip x-body" />';
    },
    
    setParent: function(parent)
    {
        throw new Error('Tooltip cannot be added to another widget.');
    },
    
    setParentWidget: function(parentWidget)
    {
        // Check for emptying.
        if (parentWidget === null)
        {
            delete this.parentWidget;
            
            // Hide us.
            this.hide();
            
            return;
        }
        
        // Check parent menu item.
        if (this.parentMenuItem)
            throw new Error('Tooltip has already been added to a widget.');
        
        // Set parent menu item.
        this.parentWidget = parentWidget;
    },
    
    /*
     * Event handlers.
     */
    
    //..
    
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
                    if (this.parentWidget)
                    {
                        // Get menu item its offset and size.
                        var offset = this.parentWidget.el.getOffset(); // TODO: Property.
                        var size   = this.parentWidget.getSize();
                        
                        // Set our position.
                        this.setPosition({x: offset.x, y: offset.y + size.height});
                    }
                    else
                    {
                        // TODO: Use cursor position.
                        this.setPosition({x: 0, y: 0});
                    }
                }
                else
                {
                    // Remove us from the DOM.
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
        /**
         * The label of the tooltip. If set, will add a #Label to the button if it does not contain one yet.
         * If read, will return `null` if the tooltip does not contain a label child.
         *
         * @type string
         */
        label: {
            write: function(label)
            {
                var child = this.children[0];
                if (!child)
                    this.add(new Label({label: label, visible: true}));
                else if (child instanceof Label)
                    child.setLabel(label || '');
            },
            read: function()
            {
                var child = this.children[0];
                if (child && (child instanceof Label))
                    return child.getLabel();
                else
                    return null;
            },
            defaultValue: null
        },
        position: {  // TODO: Move to widget? See Menu, Tooltip.
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
    }
});
