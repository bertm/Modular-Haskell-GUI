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
        
        // Add event handlers.
        EventManager.registerHandler(this.el, EventMask.CAPTURE_BUTTON_PRESS, this.onCaptureButtonPress, this);
        EventManager.registerHandler(this.el, EventMask.KEY_PRESS, this.onKeyPress, this);
        
        // Always show element, its visibility is determined by whether it is in the DOM.
        this.el.show();
    },
    
    setParent: function(parent)
    {
        throw new Error('A window cannot be added to another widget.');
    },
    
    // Will be called by Widget if focus widget has really changed.
    setRealFocusWidget: function(widget)
    {
        if (this.focusWidget !== widget)
        {
            this.focusWidget = widget;
            
            this.emitPropertyChangeSignals('focus-widget');
        }
    },
    
    /*
     * Event handlers.
     */
    
    onCaptureButtonPress: function(e)
    {
        // Set us active.
        this.setActive(true);
    },
    
    onKeyPress: function(e)
    {
        // Determine focus change direction.
        var dir;
        switch (e.getKey())
        {
            case Key.TAB:
                dir = e.hasModifier(EventModifierMask.SHIFT) ?
                    FocusDirection.BACKWARD : FocusDirection.FORWARD;
                break;
                
                /*
            case Key.LEFT:
                dir = FocusDirection.LEFT;
                break;
                
            case Key.RIGHT:
                dir = FocusDirection.RIGHT;
                break;
                
            case Key.UP:
                dir = FocusDirection.UP;
                break;
                
            case Key.DOWN:
                dir = FocusDirection.DOWN;
                break;
                */
                
            default:
                return;
        }
        
        // Move focus.
        if (!this.moveFocus(dir))
        {
            // TODO: Ring bell?
        }
        
        return true;
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
                    // Append element to body.
                    Element.getBody().append(this.el);
                    
                    if (this.activateOnShow)
                        this.setActive(true);
                    else if (this.modal)
                        Application.setActiveWindow(null);
                
                    this.layout();
                }
                else
                {
                    // Remove element from body.
                    this.el.remove();
                    
                    if (this.active)
                        this.setActive(false);
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
                    // Blur current focus widget. Keep local focus.
                    if (this.focusWidget)
                        this.focusWidget.blur(true);
                    
                    // Set active flag first, to prevent recursion.
                    this.active = false;
                    
                    // Unset application its active window.
                    Application.setActiveWindow(null);
                    
                    // Add inactive class.
                    this.el.addClass('x-inactive');
                    
                    return;
                }
                
                // Check if we are visible.
                if (!this.visible)
                    return false;
                
                
                // TODO: Move to Window.
                
                // Set z-index.
                if (!(this instanceof MainWindow))
                {
                    if (this.modal)
                        this.overlay.moveToFront();
                    this.el.setStyle('z-index', Element.getMaxZIndex());
                }
                
                
                
                // Set active flag first, to prevent recursion.
                this.active = true;
                
                // Set application its active window.
                Application.setActiveWindow(this);
                
                // Remove inactive class.
                this.el.removeClass('x-inactive');
                
                // Give focus to our focus child.
                if (this.focusWidget)
                {
                    this.focusWidget.focus();
                }
                else
                {
                    // Or find a good candidate.
                    this.moveFocus(FocusDirection.START);
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
        },
        /**
         * The widget that has the focus within this window. May be `null`.
         *
         * @type Widget
         */
        'focus-widget': {
            write: function(focusWidget)
            {
                if (focusWidget)
                    focusWidget.setIsFocus(true);
                else if (this.focusWidget)
                    this.focusWidget.setIsFocus(false);
                
                // setRealFocusWidget() will be called if it really changed.
                return false;
            },
            read: true,
            defaultValue: null
        }
    },
    
    actions: {
        /**
         * Moves focus within window. Window must be #active for this action to have any effect.
         *
         * @param FocusDirection dir Direction to move focus in.
         *
         * @return bool Whether focus change succeeded.
         */
        moveFocus: function(dir)
        {
            if (this.active && this.children[0])
            {
                return this.children[0].moveFocus(dir, true);
            }
            
            return false;
        }
    }
});
