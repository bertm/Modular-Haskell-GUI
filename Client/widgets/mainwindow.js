// Use strict mode if available.
"use strict";

/**
 * Main application window.
 */
Class.define('MainWindow', {
    extend: 'AbstractWindow',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        // Check for an existing main window.
        if (Application.getMainWindow())
            throw new Error("There can only be one main window.");
        
        MainWindow.base.initialize.call(this);
        
        // Store original document title, and set new title.
        this.originalTitle = document.title || '(untitled)';
        document.title = '(untitled)';
        
        // Add event handlers.
        Screen.connect('size-change', this.onScreenSizeChange, this);
        
        // Set application its main window.
        Application.setMainWindow(this);
    },
    
    destroy: function()
    {
        MainWindow.base.destroy.call(this);
        
        // Remove application its main window.
        Application.setMainWindow(null);
        
        // Set original title.
        document.title = this.originalTitle;
        
        // Remove event handlers.
        Screen.disconnect('size-change', this.onScreenSizeChange, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-main-window">' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    allocateSize: function(allocation)
    {
        // Get minimum allocation.
        var minSize    = this.requestSize();
        var screenSize = Screen.getSize();
        
        var allocation = {
            x: this.margin.left,
            y: this.margin.top,
            width: Math.max(screenSize.width - this.margin.left - this.margin.right,  minSize.width),
            height: Math.max(screenSize.height - this.margin.top - this.margin.bottom, minSize.height)
        };
        
        Window.base.allocateSize.call(this, allocation);
    },
    
    /*
     * Event handlers.
     */
    
    onScreenSizeChange: function(screen, e)
    {
        if (this.visible)
            this.allocateSize({x: 0, y: 0, width: 0, height: 0});
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'title' property.
        title: {
            write: function(title)
            {
                document.title = title;
                
                this.title = title;
            }
        }
    }
});
