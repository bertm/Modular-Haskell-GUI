// Use strict mode if available.
"use strict";

/**
 * Widget is the base class all widgets in derive from.
 * 
 * Widget is the base class all widgets in derive from. It has common properties and actions that are shared between
 * all widgets, like visibility, margin, focus management and tooltips.
 */
Class.define('Widget', {
    /*
     * Private methods; initialization.
     */
    
    construct: function(properties)
    {
        // Initialize base class.
        Widget.base.construct.call(this);
        
        // Initialize.
        this.initialize();
        
        // Set properties.
        if (properties)
            this.setProperties(properties);
    },
    
    initialize: function()
    {
        // Set members.
        this.allocation = {width: -1, height: -1, x: -1, y: -1};
        
        // Create element.
        this.el = new Element(this.getHtml());
        
        // Set default properties.
        this.el.setDefaultAttributes();
        
        // Register us as a event source.
        EventManager.registerSource(this.el, this);
    },
    
    getHtml: function()
    {
        throw new Error('Method \'getHtml\' has not been implemented.');
    },
    
    /*
     * Some getters and setters.
     */
    
    getEl: function()
    {
        return this.el;
    },
    
    setParent: function(parent)
    {
        this.parent = parent;
    },
    
    /*
     * Layouting.
     */
    
    /*
    
    setAllocation: function(position, size)
    {
        var requisition = this.requestSize();
        
        if (size.width < requisition.width)
            size.width = requisition.width;
        
        if (size.height < requisition.height)
            size.height = requisition.height;
        
        this.allocateSize({
            width: size.width,
            height: size.height,
            x: position.x,
            y: position.y
        });
    },
    
    getAllocation: function()
    {
        return this.allocation;
    },
    */
    
    /*
     * Tooltip methods.
     */
    
    /*
    createTooltip: function()
    {
        if (this.hasTooltip && this.tooltipText)
        {
            if (!this.tooltip)
            {
                this.tooltip = Ext.create('Ext.tip.ToolTip', {
                    html: Util.escapeText(this.tooltipText),
                    target: this.view ? this.view.getEl() : undefined
                });
            }
            else if (this.view)
            {
                this.tooltip.setTarget(this.view.getEl());
            }
        }
    },
    
    destroyTooltip: function()
    {
        if (this.tooltip)
        {
            this.tooltip.destroy();
            delete this.tooltip;
        }
    },
    */
    
    /*
     * Layouting.
     */
    
    // Requests a layouting of this view.
    // Will also layout parent if requisition is less than allocation.
    layout: function()
    {
        // Do not layout when not visible or not in tree.
        if (!this.getIsVisible())
            return;
        
        // Create a new requisition.
        var oldRequisition = this.requisition;
        this.requisition = undefined;
        var newRequisition = this.requestSize();
        
        // TODO: Indicate a margin change, check for difference.
        
        // Our requisition has changed, so so does our parent's.
        if (this.parent)
        {
            this.parent.layout();
            
            return;
        }
        
        // Check if requisition is larger than current allocation.
        if ((newRequisition.width  > this.allocation.width) ||
            (newRequisition.height > this.allocation.height))
        {
            // Resize top-level widget.
            this.allocateSize({
                x: this.allocation.x,
                y: this.allocation.y,
                width: newRequisition.width,
                height: newRequisition.height
            });
        }
        else
        {
            // Reallocate top-level widget.
            this.allocateSize(Util.cloneShallow(this.allocation));
        }
    },
    
    getMinimumSize: function()
    {
        return {width: 0, height: 0};
    },
    
    getFrameSize: function()
    {
        // Get frame size of top level element.
        var frameSize = this.el.getFrame();
        
        return {width: frameSize.left + frameSize.right, height: frameSize.top + frameSize.bottom};
    },
    
    requestSize: function(useSizeRequest)
    {
        if (!this.visible)
            return {width: 0, height: 0};
        
        if (!this.requisition)
        {
            var frameSize   = this.getFrameSize();
            var minimumSize = this.getMinimumSize();
            
            this.requisition = {
                width:  Math.max(minimumSize.width,  0) + frameSize.width,
                height: Math.max(minimumSize.height, 0) + frameSize.height
            };
        }
        
        if (useSizeRequest === false)
            return this.requisition;
        
        return {
            width:  Math.max(this.sizeRequest.width,  this.requisition.width),
            height: Math.max(this.sizeRequest.height, this.requisition.height)
        };
    },
    
    correctAndStoreAllocation: function(allocation)
    {
        // Store allocation.
        this.allocation = Util.cloneShallow(allocation);
        
        // Subtract frame size.
        var frameSize = this.getFrameSize();
        
        allocation.width  -= frameSize.width;
        allocation.height -= frameSize.height;
        
        // Return corrected allocation.
        return allocation;
    },
    
    allocateSize: function(allocation)
    {
        // Just store allocation.
        this.allocation = allocation;
        
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
    },
    
    /*
     * Focus and blur management.
     */
    
    focus: function()
    {
        // Bail out if we can't be focussed, or we are the global focus widget.
        if (!this.canFocus || !this.getIsSensitive() || this.hasFocus)
            return;
        
        // Get our window, and get active flag.
        var window = this.getWindow();
        var active = window && window.active;
        
        // Get current focus widget within window.
        var current = window && window.getFocusChild();
        
        // Blur current widget.
        if (current)
            current.blur();
        
        // Set focus widget.
        if (active)
        {
            // Set new focus element.
            var focusEl = this.inputEl || this.el;
            if (!EventManager.setFocus(focusEl))
                return false;
            
            // We a now have global focus.
            this.hasFocus = true;
            
            this.el.addClass('x-focus');
        }
        
        // We are now the focus widget.
        this.isFocus = true;
        
        // Set focus chain.
        if (this.parent)
            this.parent.setFocusChild(this);
        
        return true;
    },
    
    blur: function()
    {
        // Bail out if we do not have focus.
        if (!this.isFocus)
            return;
        
        // Unset focus widget.
        if (this.hasFocus)
        {
            if (!EventManager.setFocus(null))
                return false;
            
            // Remove focus class.
            this.el.removeClass('x-focus');
        }
        
        // Unset focus chain.
        if (this.parent)
            this.parent.setFocusChild(null);
        
        // We do not have focus anymore.
        this.isFocus  = false;
        this.hasFocus = false;
        
        return true;
    },
    
    /*
     * Event handlers.
     */
    
    // Event handler.
    onEvent: function(e, data, capturePhase)
    {
        if (capturePhase)
        {
            if (this.signalDispatcher.emit('capture-' + e.type + '-event', this, e) ||
                this.signalDispatcher.emit('capture-event', this, e))
                return true;
        }
        else
        {
            if (this.signalDispatcher.emit(e.type + '-event', this, e) ||
                this.signalDispatcher.emit('event', this, e))
                return true;
        }
        
        return false;
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * Whether the widget is visible.
         *
         * @type int
         * @see #show
         * @see #hide
         */
        visible: {
            write: function(visible)
            {
                this.visible = visible;
                
                visible ? this.el.show() : this.el.hide();
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * The widget's effective visibility, which means it is #visible itself and also its parent widget
         * is visible.
         *
         * @return bool The widget's effective visbility.
         * @see #visible
         */
        'is-visible': {
            read: function()
            {
                return (this.visible && this.parent && this.parent.getIsVisible());
            }
        },
        
        /**
         * The widget's window, or `null` if it has none. Will return itself if widget is a window.
         *
         * @return Widget Window of this widget.
         * @see #Window, #MainWindow
         */
        window: {
            read: function()
            {
                // Fetch top-level window.
                var widget = this;
                while (widget && !((widget instanceof Window) || (widget instanceof MainWindow)))
                {
                    widget = widget.parent;
                }
                
                return widget;
            }
        },
        /**
         * The dimension of the widget. When 
         *
         * @type Dimension
         */
        size: {
            read: function()
            {
                if (!this.getIsVisible())
                    return {width: 0, height: 0};
                
                return {width: this.allocation.width, height: this.allocation.height};
            }
        },
        /**
         * Override for the width request for the widget, or negative if natural size should be used.
         *
         * @type int
         */
        'width-request': { // NOTE: Either this or size-request?
            write: function(widthRequest)
            {
                this.sizeRequest = {width: widthRequest, height: this.sizeRequest.height};
                
                // TODO: Signal (2 extra).
                
                this.layout();
            },
            read: function()
            {
                return this.sizeRequest.width;
            }
        },
        /**
         * Override for the height request for the widget, or negative if natural size should be used.
         *
         * @type int
         */
        'height-request': { // NOTE: Either this or size-request?
            write: function(heightRequest) 
            {
                this.sizeRequest = {width: this.sizeRequest.width, height: heightRequest};
                
                // TODO: Signal (2 extra).
                
                this.layout();
            },
            read: function()
            {
                return this.sizeRequest.height;
            }
        },
        /**
         * Override for the size request for the widget, or negative if natural size should be used.
         *
         * @type Dimension
         */
        'size-request': {
            write: function(sizeRequest)
            {
                this.sizeRequest = sizeRequest;
                
                // TODO: Signal (2 extra).
                
                this.layout();
            },
            read: true,
            defaultValue: {width: -1, height: -1}
        },
        /**
         * Sets all four sides' margin at once. If read, returns max margin on any side.
         *
         * @type int
         */
        margin: {
            write: function(margin)
            {
                this.margin = {top: margin, right: margin, bottom: margin, left: margin};
                
                // TODO: Signaling (4 extra).
                
                this.layout();
            },
            read: function()
            {
                return Math.max(this.margin.top, this.margin.right, this.margin.bottom, this.margin.left);
            },
            defaultValue: {top: 0, right: 0, bottom: 0, left: 0}
        },
        /**
         * Margin on top side of widget.
         *
         * @type int
         */
        'margin-top': {
            write: function(marginTop)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.top = marginTop;
                
                this.layout();
            },
            read: function()
            {
                return this.margin.top;
            }
        },
        /**
         * Margin on right side of widget.
         *
         * @type int
         */
        'margin-right': {
            write: function(marginRight)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.right = marginRight;
                
                this.layout();
            },
            read: function()
            {
                return this.margin.right;
            }
        },
        /**
         * Margin on bottom side of widget.
         *
         * @type int
         */
        'margin-bottom': {
            write: function(marginBottom)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.bottom = marginBottom;
                
                this.layout();
            },
            read: function()
            {
                return this.margin.bottom;
            }
        },
        /**
         * Margin on left side of widget.
         *
         * @type int
         */
        'margin-left': {
            write: function(marginLeft)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.left = marginLeft;
                
                this.layout();
            },
            read: function()
            {
                return this.margin.left;
            }
        },
        /**
         * Whether the widget its #tooltip-text should be shown.
         *
         * @type bool
         */
        'has-tooltip': {
            write: function(hasTooltip)
            {
                // TODO: Implement.
                
                this.hasTooltip = hasTooltip;
                
                // Create or destroy tooltip.
                //if (hasTooltip)
                //    this.createTooltip();
                //else
                //    this.destroyTooltip();
            },
            read: true,
            defaultValue: false
        },
        /**
         * The tooltip text which will be shown if #has-tooltip is `true`.
         *
         * @type string
         */
        'tooltip-text': {
            write: function(tooltipText)
            {
                // TODO: Implement.
                
                this.tooltipText = tooltipText;
                
                // Create tooltip.
                //this.createTooltip();
            },
            read: true,
            defaultValue: ''
        },
        /**
         * The sensitivity of the widget.
         *
         * A widget is sensitive if the user can interact with it. Insensitive widgets are "grayed out" and the user
         * can't interact with them. Insensitive widgets are known as "inactive", "disabled", or "ghosted" in some
         * other toolkits.
         *
         * @type bool
         * @see #is-sensitive
         */
        sensitive: {
            write: function(sensitive)
            {
                sensitive ? this.el.removeClass('x-insensitive') : this.el.addClass('x-insensitive');
                
                this.sensitive = sensitive;
            },
            read: true,
            defaultValue: true
        },
        /**
         * The widget's effective sensitivity, which means it is #sensitive itself and also its parent widget
         * is sensitive.
         *
         * @type bool
         * @see #sensitive
         */
        'is-sensitive': {
            read: function()
            {
                return (this.visible && this.sensitive && this.parent && this.parent.getIsSensitive());
            }
        },
        /**
         * Whether the widget can accept the input focus.
         *
         * @type bool
         */
        'can-focus': {
            write: function(canFocus)
            {
                this.canFocus = canFocus;
                
                // TODO: Override this for entries. Widgets with tabindex=-1 can still be focussed by mouse.
                
                // Check if this widget supports setting focus.
                if (this.el.hasAttribute('tabindex'))
                {
                    if (canFocus)
                        this.el.setAttribute('tabindex', 0);
                    else
                        this.el.setAttribute('tabindex', -1);
                    
                    if (this.hasFocus)
                        this.blur();
                }
            },
            read: true,
            defaultValue: false
        },
        /**
         * Whether the widget has the input focus within its #window.
         *
         * @type bool
         */
        'is-focus': {
            write: function(hasFocus)
            {
                if (hasFocus)
                    this.focus();
                else
                    this.blur();
            },
            read: true,
            defaultValue: false
        },
        /**
         * Whether the widget has the global input focus.
         *
         * @type bool
         */
        'has-focus': {
            write: function(isFocus)
            {
                // Set us as focus widget.
                if (isFocus)
                    this.focus();
                else
                    this.blur();
                
                // Set window as active.
                var window = this.getWindow();
                if (window)
                    window.setActive(true);
            },
            read: true,
            defaultValue: false
        },
        /**
         * Event mask that determines which event signals are enabled.
         *
         * @type int
         */
        events: {
            write: function(events)
            {
                // Register given event types.
                EventManager.registerHandler(this.el, events, this.onEvent, this);
                
                // Set events.
                this.events = events;
            },
            read: true,
            defaultValue: EventMask.NONE
        },
        /**
         * The parent of the widget or `null` if there is none.
         *
         * @type Widget
         */
        'parent': {
            read: true,
            defaultValue: null
        }
    },
    
    /*
     * Actions.
     */

    actions: {
        /**
         * Shows the widget. Does the same as setting the #visible property to `true`.
         */
        show: function()
        {
            this.setVisible(true);
        },
        /**
         * Hides the widget. Does the same as setting the #visible property to `false`.
         */
        hide: function()
        {
            this.setVisible(false);
        },
        /**
         * Shows this widget and all widgets below it in the hierarchy.
         */
        showAll: function()
        {
            this.show();
        },
        /**
         * Hides this widget and all widgets below it in the hierarchy.
         */
        hideAll: function()
        {
            this.hide();
        },
        /**
         * Enables certain #events.
         */
        enableEvents: function(events)
        {
            this.setEvents(this.events | events);
        },
        /**
         * Disables certain #events.
         */
        disableEvents: function(events)
        {
            this.setEvents(this.events & ~events);
        },
        /**
         * Destroys the widget. If the widget is inside a container, it will be removed from it.
         *
         * @throws An #Error if the widget has already been destroyed.
         */
        destroy: function()
        {
            // Destroy widget.
            Widget.base.destroy.call(this);
        
            // TODO: Unregister event source (basically not needed, as element is destroyed).
            
            // Remove widget from parent.
            if (this.parent !== null)
                this.parent.remove(this);
            
            // Destroy element.
            this.el.destroy();
        }
    }
});
