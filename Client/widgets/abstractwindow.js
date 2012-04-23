// Use strict mode if available.
"use strict";

/**
 * Top-level widget base class.
 */
Class.define('AbstractWindow', {
    extend: 'Bin',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        AbstractWindow.base.initialize.call(this);
        
        // Set members.
        this.lastFocusChild = null;
        
        // Add event handlers.
        EventManager.registerHandler(this.el, EventMask.CAPTURE_BUTTON_PRESS, this.onCaptureButtonPress, this);
        
        // TODO: Dom is always in body? Or only when visible?
        
        // Append element to body.
        Element.getBody().append(this.el);
    },
    
    setParent: function(parent)
    {
        throw new Error('A window cannot be added to another widget.');
    },
    
    /*
     * Event handlers.
     */
    
    onCaptureButtonPress: function(e)
    {
        // Set us active.
        this.setActive(true);
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
                    this.el.show();
                    
                    if (this.activateOnShow)
                        this.setActive(true);
                    else if (this.modal)
                        Application.setActiveWindow(null);
                }
                else
                {
                    this.el.hide();
                    
                    if (this.active)
                        this.setActive(false);
                }
                
                this.layout();
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
         * The title of the window.
         *
         * @type string
         */
        title: {
            read: true,
            defaultValue: ''
        },
        /**
         * Whether the window widget is active. There can only be one active window.
         *
         * @type bool
         */
        active: {
            write: function(active)
            {
                // Check if deactivating.
                if (!active)
                {
                    // Save last focus child.
                    this.lastFocusChild = this.focusChild;
                    
                    // Blur current focus widget.
                    if (this.focusChild)
                        this.focusChild.blur();
                    
                    // Set active flag first, to prevent recursion.
                    this.active = false;
                    
                    // Unset application its active window.
                    Application.setActiveWindow(null);
                    
                    // Remove active class.
                    this.el.removeClass('x-active');
                    
                    return;
                }
                
                // Check if we are sensitive.
                if (!this.getIsSensitive()) // TODO: Make header buttons also not work.
                    return false;
                
                // Set z-index.
                if (!(this instanceof MainWindow))
                    this.el.setStyle('z-index', Element.getMaxZIndex());
                
                // Set active flag first, to prevent recursion.
                this.active = true;
                
                // Set application its active window.
                Application.setActiveWindow(this);
                
                // Add active class.
                this.el.addClass('x-active');
                
                // Give focus to our focus child.
                if (this.lastFocusChild)
                {
                    this.lastFocusChild.focus();
                    this.lastFocusChild = null;
                }
                else
                {
                    // TODO: Pick a suitable widget.
                }
            },
            read: true,
            defaultValue: false
        },
        /**
         * Whether to set this window #active when made #visible.
         *
         * @type bool
         */
        'activate-on-show': {
            write: function(activateOnShow)
            {
                this.activateOnShow = activateOnShow;
            },
            read: true,
            defaultValue: true
        }
    }
});
