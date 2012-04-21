// Use strict mode if available.
"use strict";

/*
 * Window manager class.
 */

Class.define('WindowManager', {
    /*
     * Public methods.
     */
    
    //..
    
    /*
     * Private methods.
     */
    
    construct: function()
    {
        ;
    },
    
    initialize: function()
    {
    },
    
    statics: {
        // Singleton instance.
        instance: undefined,
        
        /*
         * Public static methods.
         */
        
        // Gets the unique window manager instance.
        getInstance: function()
        {
            if (Application.instance === undefined)
            {
                WindowManager.instance = new WindowManager();
            
                // Initialize here, so that methods that need an instance of the singleton
                // will get it without recursively initializing.
                WindowManager.instance.initialize();
            }
            
            return WindowManager.instance;
        }
    }
});
