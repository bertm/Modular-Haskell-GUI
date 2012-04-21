// Use strict mode if available.
"use strict";

/*
 * Toggle button class.
 */

Class.define('ToggleButton', {
    extend: 'Button',
    
    /*
     * Private methods; initialization.
     */
    
    getHtml: function()
    {
        var html =
            '<div tabindex="-1" class="x-widget x-button x-togglebutton">' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Toggling.
     */
    
    toggle: function()
    {
        this.setActive(!this.active);
    },
    
    /*
     * Event handlers.
     */
    
    onKeyRelease: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        var key = e.getKey();
        if (key === Key.ENTER)
        {
            if (this.el.hasClass('x-pressed'))
            {
                this.el.removeClass('x-pressed');
                
                this.toggle();
                
                // Handled it.
                return true;
            }
        }
        else if (key === Key.SPACE)
        {
            this.el.removeClass('x-pressed');
            
            this.toggle();
            
            // Handled it.
            return true;
        }
    },
    
    onKeyPress: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        var key = e.getKey();
        if (key === Key.ENTER)
        {
            this.el.addClass('x-pressed');
            
            Util.delay(100,
                function()
                {
                    if (this.el.hasClass('x-pressed'))
                    {
                        this.el.removeClass('x-pressed');
                        
                        this.toggle();
                    }
                }, this);
            
            // Handled it.
            return true;
        }
        else if (key === Key.SPACE)
        {
            this.el.addClass('x-pressed');
            
            // Handles it.
            return true;
        }
    },
    
    onButtonRelease: function()
    {
        this.el.removeClass('x-pressed');
        
        if (this.enterDelay)
        {
            Util.clearDelay(this.enterDelay);
            delete this.enterDelay;
        }
        
        if (!this.getIsSensitive())
            return;
        
        this.toggle();
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
                
                this.signalDispatcher.emit(active ? 'activate' : 'deactivate', this);
                this.signalDispatcher.emit('toggle', this);
            },
            read: true,
            defaultValue: false
        }
    }
});
