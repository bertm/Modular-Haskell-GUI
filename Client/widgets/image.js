// Use strict mode if available.
"use strict";

// Save browser image reference.
var BrowserImage = Image;

/*
 * Image class.
 */

Class.define('Image', {
    extend: 'Misc',
    
    /*
     * Private methods; initialization.
     */
    
    getHtml: function()
    {
        return '<img src="" class="x-widget x-image" />';
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        if ((this.image !== undefined) && this.image.width && this.image.height)
        {
            return {width: this.image.width, height: this.image.height};
        }
        
        return {width: 10, height: 10}; // TODO: Not found picture.
    },
    
    /*
     * Properties.
     */
    
    properties: {
        file: { // TODO: Or source?
            write: function(file)
            {
                this.file = file;
                
                this.image = new BrowserImage();
                this.image.src = file;
                
                var self = this;
                this.image.onload = function()
                {
                    self.el.setProperty('src', self.file);
                    
                    self.layout();
                    
                    self.signalDispatcher.emit('load', this);
                };
                this.image.onerror = function()
                {
                    // TODO: Set file not found image?
                    
                    self.signalDispatcher.emit('error', this);
                };
            },
            read: true,
            defaultValue: ''
        }
        
        // TODO: Stock images with their size
    }
});
