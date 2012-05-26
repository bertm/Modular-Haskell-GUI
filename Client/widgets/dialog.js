// Use strict mode if available.
"use strict";

/**
 * A popup window.
 *
 * Usually contains a content part (like a message or input entry) and
 * an action part with some buttons.
 */
Class.define('Dialog', {
    extend: 'Window',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Dialog.base.initialize.call(this);
        
        // Set us non-resizable.
        this.setResizable(false);
        
        // Set members.
        this.contentBox = new Box({orientation: Orientation.VERTICAL});
        this.buttonBox  = new ButtonBox({'layout-style': ButtonBoxStyle.END});
        this.buttons    = {};
        
        // Add content.
        this.contentBox.add(this.buttonBox);
        this.add(this.contentBox);
        
        // NOTE: Dialog has response - widget pairs (buttons).
    },
    
    /*
     * Actions.
     */
    
    actions: {
        addButton: function(response, label)
        {
            if (this.buttons[response])
                throw new Error("A button with that response already exists.");
            
            // Add button.
            var button = new Button({label: label});
            this.buttonBox.add(button);
            this.buttons[response] = button;
            
            // Connect to its signal.
            button.connect('activate', this.onButtonActivate, this);
            
            return button;
        },
        removeButton: function(response)
        {
            if (!this.buttons[response])
                throw new Error("A button with that response does not exist.");
            
            // Remove button.
            this.buttonBox.remove(this.buttons[response]);
            delete this.buttons[response];
        }
    }
});
