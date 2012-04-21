// Use strict mode if available.
"use strict";

/*
 * Calendar class.
 */

Class.define('Calendar', {
    extend: 'Widget',
    
    /*
     * Private methods.
     */
    
    initialize: function(properties)
    {
        // Set fields.
        this.date = new Date();
        
        Calendar.base.initialize.call(this, properties);
    },
    
    createView: function()
    {
        return Ext.create('Ext.picker.Date', Calendar.base.applyViewConfig.call(this, {
            value: this.date
        }));
    },
    
    getMinimumSize: function()
    {
        return {width: 200, height: 197}; // TODO: ??
    },
    
    /*
     * Properties.
     */
    
    properties: {
        year: {
            write: function(year)
            {
                this.date.setFullYear(year);
                
                if (this.view)
                    this.view.setValue(this.date);
            },
            read: function()
            {
                return this.date.getFullYear();
            }
        },
        month: {
            write: function(month)
            {
                this.date.setMonth(month);
                
                if (this.view)
                    this.view.setValue(this.date);
            },
            read: function()
            {
                return this.date.getMonth();
            }
        },
        day: {
            write: function(day)
            {
                // TODO: 0 means no day selected.
                
                this.date.setDate(day);
                
                if (this.view)
                    this.view.setValue(this.date);
            },
            read: function()
            {
                return this.date.getDate();
            }
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
    }
});
