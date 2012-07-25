// Use strict mode if available.
"use strict";

/*
 * Radio button class.
 */

Class.define('RadioButton', {
    extend: 'CheckButton',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        RadioButton.base.initialize.call(this);
        
        // Set members.
        this.group = new RadioButtonGroup();
        this.group.add(this);
    },
    
    getHtml: function()
    {
        var html =
            '<div tabindex="-1" class="x-widget x-radiobutton">' +
                '<div class="x-control" />' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Toggling.
     */
    
    toggle: function()
    {
        // A user can only toggle a radio button on.
        if (!this.active)
            this.setActive(true);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * Gets the group of radio buttons of which this radio button is part of.
         * See the #join action for easy 
         */
        group: {
            write: function(group)
            {
                // Check if different, to catch cases of recursion.
                if (group === this.group)
                    return false;
                
                // Remove us from the previous group.
                // Will set our group to null.
                var previousGroup = this.group;
                this.group = null;
                previousGroup.remove(this);
                
                // Check if unsetting.
                if (!group)
                {
                    // Create a new group with just us.
                    this.group = new RadioButtonGroup();
                    this.group.add(this);
                    
                    return;
                }
                else
                {
                    // Set new group, to avoid recursion.
                    this.group = group;
                    
                    // Add us to the new group.
                    // Will set our group to group.
                    group.add(this);
                }
            },
            read: true
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        /**
         * Joins another radio button to the #group of this widget.
         *
         * Joins another radio button to the #group of this widget. Another way of doing this is to set the group
         * of the passed radio button to the group of this widget: `passed.setGroup(radioButton.getGroup())`.
         */
        join: function(radioButton)
        {
            this.group.add(radioButton);
        }
    }
});
