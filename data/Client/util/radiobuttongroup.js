// Use strict mode if available.
"use strict";

/**
 * A group of #RadioButton's.
 */
Class.define('RadioButtonGroup', {
    /*
     * Private methods.
     */
    
    // TODO: Base this on some generic List/Set type.
    
    construct: function(properties)
    {
        // Set members.
        this.radioButtons = [];
        
        RadioButtonGroup.base.construct.call(this, properties);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        active: {
            write: function(active)
            {
                // Check for unsetting.
                if (!active)
                {
                    // Set old active inactive.
                    if (this.active && (this.active.getGroup() === this))
                        this.active.setActive(false);
                    
                    this.active = null;
                    
                    return;
                }
                
                // Find radio button.
                for (var i = this.radioButtons.length - 1; i >= 0; --i)
                {
                    if (this.radioButtons[i] === active)
                    {
                        // Set old active inactive.
                        if (this.active && (this.active.getGroup() === this))
                            this.active.setActive(false);
                        
                        // Set new one active.
                        this.active = active;
                        
                        active.setActive(true);
                        
                        return true;
                    }
                }
                
                return false;
            },
            read: true,
            defaultValue: null
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        add: function(radioButton)
        {
            for (var i = this.radioButtons.length - 1; i >= 0; --i)
            {
                if (this.radioButtons[i] === radioButton)
                    return;
            }
            
            this.radioButtons.push(radioButton);
            
            radioButton.setGroup(this);
            radioButton.connect('activate', this.setActive, this);
            
            // Check new radio button is active.
            if (radioButton.getActive())
                this.setActive(radioButton);
            
            this.signalDispatcher.emit('add', this, radioButton);
        },
        
        remove: function(radioButton)
        {
            for (var i = this.radioButtons.length - 1; i >= 0; --i)
            {
                if (this.radioButtons[i] === radioButton)
                {
                    // Remove first, to avoid recursion.
                    this.radioButtons.splice(i, 1);
                    
                    radioButton.setGroup(null);
                    radioButton.disconnect('activate', this.setActive, this);
                    
                    // Check if it was the active radio button in this group.
                    if (radioButton.getActive())
                        this.setActive(null);
            
                    this.signalDispatcher.emit('remove', this, radioButton);
                    
                    return;
                }
            }
        }
    }
});
