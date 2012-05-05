// Use strict mode if available.
"use strict";

/**
 * Top level widget which can contain other widgets.
 * 
 * A window is a top level widget which can contain other widgets. Windows normally have decorations like a header and
 * a border that allow the user to drag, resize maximize and close it.
 */
Class.define('Window', {
    extend: 'AbstractWindow',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Window.base.initialize.call(this);
        
        // Set members.
        this.overlay = new Overlay();
        
        // Get elements.
        this.headerEl     = this.el.find('.x-header');
        this.titleEl      = this.headerEl.find('.x-label');
        this.deleteEl     = this.headerEl.find('.x-delete');
        this.restoreEl    = this.headerEl.find('.x-restore');
        this.maximizeEl   = this.headerEl.find('.x-maximize');
        this.resizeGripEl = this.el.find('.x-resize-grip');
        
        // Fetch resizers.
        var directions = ['n', 'e', 's', 'w', 'ne', 'se', 'sw', 'nw'];
        
        this.resizers = {};
        for (var i = directions.length - 1; i >= 0; --i)
        {
            var dir = directions[i];
            
            this.resizers[dir] = this.el.find('.x-resizer-' + dir);
        }
        
        // Add events handlers.
        Screen.connect('size-change', this.onScreenSizeChange, this);
        
        var em = EventManager;
        
        em.registerHandler(this.headerEl, EventMask.BUTTON_PRESS, this.onHeaderButtonPress, this);
        em.registerHandler(this.headerEl, EventMask.MOTION, this.onHeaderMotion, this);
        em.registerHandler(this.headerEl, EventMask.BUTTON_RELEASE, this.onHeaderButtonRelease, this);
        
        em.registerHandler(this.deleteEl, EventMask.BUTTON_PRESS, this.onButtonButtonPress, this, this.deleteEl);
        em.registerHandler(this.deleteEl, EventMask.BUTTON_RELEASE, this.onButtonButtonRelease, this, this.deleteEl);
        
        em.registerHandler(this.maximizeEl, EventMask.BUTTON_PRESS, this.onButtonButtonPress, this, this.maximizeEl);
        em.registerHandler(this.maximizeEl, EventMask.BUTTON_RELEASE, this.onButtonButtonRelease, this, this.maximizeEl);
        
        em.registerHandler(this.restoreEl, EventMask.BUTTON_PRESS, this.onButtonButtonPress, this, this.restoreEl);
        em.registerHandler(this.restoreEl, EventMask.BUTTON_RELEASE, this.onButtonButtonRelease, this, this.restoreEl);
        
        for (var dir in this.resizers)
        {
            em.registerHandler(this.resizers[dir], EventMask.BUTTON_PRESS, this.onResizerButtonPress, this, dir);
            em.registerHandler(this.resizers[dir], EventMask.MOTION, this.onResizerMotion, this, dir);
            em.registerHandler(this.resizers[dir], EventMask.BUTTON_RELEASE, this.onResizerButtonRelease, this, dir);
        }
        
        em.registerHandler(this.resizeGripEl, EventMask.BUTTON_PRESS, this.onResizerButtonPress, this, 'se');
        em.registerHandler(this.resizeGripEl, EventMask.MOTION, this.onResizerMotion, this, 'se');
        em.registerHandler(this.resizeGripEl, EventMask.BUTTON_RELEASE, this.onResizerButtonRelease, this, 'se');
    },
    
    destroy: function()
    {
        Window.base.destroy.call(this);
        
        // Destroy overlay.
        this.overlay.destroy();
        
        // TODO: Unregister them all at once.
        
        // Remove event handlers.
        Screen.disconnect('size-change', this.onScreenSizeChange, this);
        /*
        Screen.disconnect('button-release-event', this.onScreenButtonRelease, this);
        
        var em = EventManager;
        
        em.unregisterElementEvent(this.headerEl, EventType.BUTTON_PRESS, this.onHeaderButtonPress, this);
        //em.unregisterElementEvent(this.headerEl, EventType.BUTTON_PRESS, this.onHeaderButtonRelease, this);
        em.unregisterElementEvent(this.deleteEl, EventType.BUTTON_PRESS, this.onButtonButtonPress, this);
        em.unregisterElementEvent(this.deleteEl, EventType.BUTTON_RELEASE, this.onDelete, this);
        em.unregisterElementEvent(this.restoreEl, EventType.BUTTON_PRESS, this.onButtonButtonPress, this);
        em.unregisterElementEvent(this.restoreEl, EventType.BUTTON_RELEASE, function() { this.setMaximized(false); }, this);
        em.unregisterElementEvent(this.maximizeEl, EventType.BUTTON_PRESS, this.onButtonButtonPress, this);
        em.unregisterElementEvent(this.maximizeEl, EventType.BUTTON_RELEASE, function() { this.setMaximized(true); }, this);
        
        for (var dir in this.resizers)
        {
            em.unregisterElementEvent(this.resizers[dir], EventType.BUTTON_PRESS, this.onResizerButtonPress, this);
        }
        
        em.unregisterElementEvent(this.resizeGripEl, EventType.BUTTON_PRESS, this.onResizerButtonPress, this);
        
        */
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-window x-inactive">' +
                '<div class="x-header">' +
                    '<div class="x-label" />' +
                    '<div class="x-delete" />' +
                    '<div class="x-restore" />' +
                    '<div class="x-maximize" />' +
                '</div>' +
                '<div class="x-body" />' +
                '<div class="x-resizer-n" />' +
                '<div class="x-resizer-w" />' +
                '<div class="x-resizer-s" />' +
                '<div class="x-resizer-e" />' +
                '<div class="x-resizer-nw" />' +
                '<div class="x-resizer-sw" />' +
                '<div class="x-resizer-se" />' +
                '<div class="x-resizer-ne" />' +
                '<div class="x-resize-grip" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getCenterPosition: function(size)
    {
        var screenSize = Screen.getSize();
        var elSize     = size || this.getSize();
        
        return {
            x: Math.max(0, Math.floor((screenSize.width  - elSize.width)  * 0.5)),
            y: Math.max(0, Math.floor((screenSize.height - elSize.height) * 0.5))
        };
    },
    
    /*
    setPosition: function(position)
    {
        this.allocation = {
            width: this.allocation.width,
            height: this.allocation.height,
            x: position.x,
            y: position.y
        }
        
        this.allocateSize(this.allocation);
    },
    */
    
    // TODO: MAX(header width, body width)
    //getMinimumSize: function()
    //{
        //var minimumSize = Window.base.getMinimumSize.call(this);
    //},
    
    getFrameSize: function()
    {
        var frameSize = Window.base.getFrameSize.call(this);
        
        // Add header size.
        if (this.decorated)
        {
            var headerSize = this.headerEl.getSize();
            
            frameSize.height += headerSize.height;
        }
        
        return frameSize;
    },
    
    allocateSize: function(allocation)
    {
        // Check for negative allocation.
        if ((allocation.x < 0) || (allocation.y < 0))
        {
            // Center window.
            var center = this.getCenterPosition(allocation);
            
            if (allocation.x < 0)
                allocation.x = center.x;
            if (allocation.y < 0)
                allocation.y = center.y;
        }
        
        Window.base.allocateSize.call(this, allocation);
    },
    
    constrainPosition: function(position)
    {
        // Check maximum position first, in order not to hide the header.
        if (position.x > this.maxPosition.x)
            position.x = this.maxPosition.x;
        if (position.x < 0)
            position.x = 0;
        
        if (position.y > this.maxPosition.y)
            position.y = this.maxPosition.y;
        if (position.y < 0)
            position.y = 0;
    },
    
    constrainAllocation: function(allocation, dir)
    {
        if (allocation.x < 0)
        {
            allocation.width += allocation.x;
            allocation.x = 0;
        }
        
        if (allocation.y < 0)
        {
            allocation.height += allocation.y;
            allocation.y = 0;
        }
        
        var requisition = this.requestSize();
        
        if ((allocation.x + allocation.width) > this.screenSize.width)
            allocation.width = this.screenSize.width - allocation.x;
        
        if ((allocation.y + allocation.height) > this.screenSize.height)
            allocation.height = this.screenSize.height - allocation.y;
        
        if (allocation.width < requisition.width)
        {
            if (dir.indexOf('w') >= 0)
                allocation.x += allocation.width - requisition.width;
            
            allocation.width = requisition.width;
        }
        
        if (allocation.height < requisition.height)
        {
            if (dir.indexOf('n') >= 0)
                allocation.y += allocation.height - requisition.height;
            
            allocation.height = requisition.height;
        }
    },
    
    /*
     * Maximizing and restoring.
     */
    
    maximize: function()
    {
        if (this.maximized)
            return;
        
        this.maximized = true;
        
        // Toggle buttons.
        this.maximizeEl.hide();
        if (this.maximizable)
            this.restoreEl.show();
        
        this.restoreAllocation = Util.cloneShallow(this.allocation);
        
        var screenSize  = Screen.getSize();
        var requesition = this.requestSize();
        
        var newAllocation = {
            x: 0,
            y: 0,
            width: Math.max(requesition.width, screenSize.width),
            height: Math.max(requesition.height, screenSize.height)
        };
        
        this.allocateSize(newAllocation);
        
        for (var dir in this.resizers)
        {
            this.resizers[dir].hide();
        }
        
        this.resizeGripEl.hide();
        
        // For the event handler.
        return false;
    },
    
    restore: function()
    {
        if (!this.maximized)
            return;
        
        this.maximized = false;
        
        // Toggle buttons.
        this.restoreEl.hide();
        if (this.maximizable)
            this.maximizeEl.show();
        
        // Constrain size.
        var requesition = this.requestSize();
        
        if (this.restoreAllocation.width < requesition.width)
            this.restoreAllocation.width = requesition.width;
        if (this.restoreAllocation.height < requesition.height)
            this.restoreAllocation.height = requesition.height;
        
        // Set new allocation.
        this.allocateSize(this.restoreAllocation);
        
        // Simulate a resize to check position and size.
        this.onScreenSizeChange(Screen);
        
        delete this.restoreAllocation;
        
        if (this.resizable && this.decorated)
        {
            for (var dir in this.resizers)
            {
                this.resizers[dir].show();
            }
        
            this.resizeGripEl.show();
        }
        
        // For the event handler.
        return false;
    },
    
    /*
     * Event handlers.
     */
    
    onButtonButtonPress: function(e, button)
    {
        button.addClass('x-pressed');
        
        // Handled it.
        return true;
    },
    
    onButtonButtonRelease: function(e, button)
    {
        switch (button)
        {
            case this.deleteEl:
                this.destroy();
                break;
                
            case this.maximizeEl:
                this.setMaximized(true);
                break;
                
            case this.restoreEl:
                this.setMaximized(false);
                break;
        }
        
        button.removeClass('x-pressed');
                
        // Handled it.
        return true;
    },
    
    onHeaderButtonPress: function(e)
    {
        if (!this.draggable || this.maximized)
            return;
        
        // Show move cursor.
        Cursor.push(CursorShape.MOVE, Cursor.getContextId('window-widget'));
        
        // Calculate mouse offset to element.
        this.mouseOffset = {
            x: e.getX() - this.allocation.x,
            y: e.getY() - this.allocation.y
        };
        
        // Set maximum position.
        var screenSize = Screen.getSize();
        
        this.maxPosition = {
            x: screenSize.width  - this.allocation.width,
            y: screenSize.height - this.allocation.height
        };
    },
    
    onHeaderMotion: function(e)
    {
        // Check if we started dragging.
        if (!this.mouseOffset)
            return;
        
        var position = {
            x: e.getX() - this.mouseOffset.x,
            y: e.getY() - this.mouseOffset.y
        };
        
        this.constrainPosition(position);
        this.el.setPosition(position);
        
        this.allocation.x = position.x;
        this.allocation.y = position.y;
    },
    
    onHeaderButtonRelease: function()
    {
        delete this.mouseOffset;
        delete this.maxPosition;
        
        // Pop move cursor.
        Cursor.pop(Cursor.getContextId('window-widget'));
        
        // TODO: Only if doubleclick.
        //this.setMaximized(!this.maximized);
        
        // Handled it.
        return true;
    },
    
    onResizerButtonPress: function(e, dir)
    {
        // Set body size.
        this.screenSize = Screen.getSize();
        
        // Show resize cursor.
        Cursor.push(Window.cursorShapeByDir[dir], Cursor.getContextId('window-widget'));
    },
    
    onResizerMotion: function(e, dir)
    {
        // Check if we started resizing.
        if (!this.screenSize)
            return;
        
        var mousePosition = e.getPosition();
        
        var newAllocation = Util.cloneShallow(this.allocation);
        if (dir.indexOf('n') >= 0)
        {
            newAllocation.height += newAllocation.y - mousePosition.y;
            newAllocation.y = mousePosition.y;
        }
        
        if (dir.indexOf('w') >= 0)
        {
            newAllocation.width += newAllocation.x - mousePosition.x;
            newAllocation.x = mousePosition.x;
        }
        
        if (dir.indexOf('e') >= 0)
            newAllocation.width = mousePosition.x - newAllocation.x;
        
        if (dir.indexOf('s') >= 0)
            newAllocation.height = mousePosition.y - newAllocation.y;
        
        this.constrainAllocation(newAllocation, dir);
        
        this.allocateSize(newAllocation);
    },
    
    onResizerButtonRelease: function(e, dir)
    {
        // Remove resize cursor.
        Cursor.pop(Cursor.getContextId('window-widget'));
        
        delete this.screenSize;
    },
    
    onScreenSizeChange: function(screen)
    {
        var size = screen.getSize();
        
        var reallocate = false;
        
        var newAllocation = Util.cloneShallow(this.allocation);
        if ((newAllocation.x + newAllocation.width) > size.width)
        {
            newAllocation.x = size.width - newAllocation.width;
            if (newAllocation.x < 0)
            {
                newAllocation.width += newAllocation.x;
                newAllocation.x = 0;
                
                var requisition = this.requestSize();
                if (newAllocation.width < requisition.width)
                    newAllocation.width = requisition.width;
            }
            
            reallocate = true;
        }
        else if (this.maximized)
        {
            newAllocation.width = size.width;
            
            reallocate = true;
        }
        
        if ((newAllocation.y + newAllocation.height) > size.height)
        {
            newAllocation.y = size.height - newAllocation.height;
            if (newAllocation.y < 0)
            {
                newAllocation.height += newAllocation.y;
                newAllocation.y = 0;
                
                var requisition = this.requestSize();
                if (newAllocation.height < requisition.height)
                    newAllocation.height = requisition.height;
            }
            
            reallocate = true;
        }
        else if (this.maximized)
        {
            newAllocation.height = size.height;
            
            reallocate = true;
        }
        
        if (reallocate)
            this.allocateSize(newAllocation);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        // Overrides 'visible' property.
        visible: {
            write: function(visible)
            {
                Window.base.setVisible.call(this, visible);
                
                // Show or hide overlay.
                if (visible && this.modal)
                    this.overlay.show();
                else
                    this.overlay.hide();
            }
        },
        // Overrides 'margin' property.
        margin: {
            write: function(margin)
            {
                this.margin = {top: margin, right: margin, bottom: margin, left: margin};
                
                this.bodyEl.setPadding(Util.cloneShallow(this.margin));
                this.layout();
                
                // TODO: Signal. (4 extra).
            }
        },
        // Overrides 'margin-top' property.
        'margin-top': {
            write: function(marginTop)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.top = marginTop;
                
                this.bodyEl.setPadding(Util.cloneShallow(this.margin));
                this.layout();
                
                // TODO: Signal. (1 extra).
            }
        },
        // Overrides 'margin-right' property.
        'margin-right': {
            write: function(marginRight)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.right = marginRight;
                
                this.bodyEl.setPadding(Util.cloneShallow(this.margin));
                this.layout();
                
                // TODO: Signal. (1 extra).
            }
        },
        // Overrides 'margin-bottom' property.
        'margin-bottom': {
            write: function(marginBottom)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.bottom = marginBottom;
                
                this.bodyEl.setPadding(Util.cloneShallow(this.margin));
                this.layout();
                
                // TODO: Signal. (1 extra).
            }
        },
        // Overrides 'margin-left' property.
        'margin-left': {
            write: function(marginLeft)
            {
                this.margin = Util.cloneShallow(this.margin);
                this.margin.left = marginLeft;
                
                this.bodyEl.setPadding(Util.cloneShallow(this.margin));
                this.layout();
                
                // TODO: Signal. (1 extra).
            }
        },
        // Overrides 'size' property.
        size: {
            write: function(size)
            {
                this.allocation = {
                    width: Math.max(size.width, requisition.width),
                    height: Math.max(size.height, requisition.height),
                    x: this.allocation.x,
                    y: this.allocation.y
                }
                
                if (this.getVisible())
                    this.allocateSize(this.allocation);
            }
        },
        // Overrides 'title' property.
        title: {
            write: function(title)
            {
                this.title = title;
                
                this.titleEl.setText(title);
            },
            read: true,
            defaultValue: ''
        },
        position: { // TODO: Move to widget? Create position and offset? Just the getter though. Override with extra setter.
            write: function(position)
            {
                // TODO: ..
            },
            read: function()
            {
                // TODO: ..
            }
        },
        /**
         * Whether the window is maximized. Setting it will only have an effect if #maximizable is `true`.
         *
         * @type bool
         */
        maximized: {
            write: function(maximized)
            {
                var previousMaximized = this.maximized;
                
                if (maximized)
                    this.maximize();
                else
                    this.restore();
                
                // Maximized may not have been changed, due to 'maximizable' being false.
                return (previousMaximized !== this.maximized);
            },
            read: true,
            defaultValue: false
        },
        /**
         * Whether the window is maximizable. Determines whether to show the maximize/restore button in the header.
         * If the windom is maximized and it maximizable is false, the window will be restored.
         *
         * @type bool
         */
        maximizable: {
            write: function(maximizable)
            {
                this.maximizable = maximizable;
                
                if (!maximizable && this.maximized)
                    this.restore();
                
                maximizable ? this.maximizeEl.show() : this.maximizeEl.hide();
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the window is resizable. Will also determine the visibility of the resize grip.
         *
         * @type bool
         */
        resizable: {
            write: function(resizable)
            {
                this.resizable = resizable;
                
                for (var dir in this.resizers)
                {
                    if (resizable && this.decorated)
                        this.resizers[dir].show();
                    else
                        this.resizers[dir].hide();
                }
                
                if (resizable && this.decorated)
                    this.resizeGripEl.show();
                else
                    this.resizeGripEl.hide();
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the window is deletable. Will determine the visibility of the close button.
         *
         * @type bool
         */
        deletable: {
            write: function(deletable)
            {
                this.deletable = deletable;
                
                deletable ? this.deleteEl.show() : this.deleteEl.hide();
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the window is draggable.
         *
         * @type bool
         */
        draggable: {
            write: function(draggable)
            {
                this.draggable = draggable;
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the window is modal. If the window is modal, other windows will not be usable.
         *
         * @type bool
         */
        modal: {
            write: function(modal)
            {
                this.modal = modal;
            
                if (modal)
                {
                    if (this.visible)
                        this.overlay.show();
                }
                else
                {
                    this.overlay.hide();
                }
                
                this.modal = modal;
            },
            read: true,
            defaultValue: false
        },
        /**
         * The opacity of the window.
         *
         * @type float
         */
        opacity: {
            write: function(opacity)
            {
                this.opacity = Util.clamp(opacity, 0, 1);
                
                this.el.setOpacity(this.opacity);
            },
            read: true,
            defaultValue: 1.0
        },
        /**
         * Whether the window has decorations, like a border and a header. Setting this to false will also disallow
         * manual resizing and the resize grip will be hidden.
         *
         * @type bool
         */
        decorated: {
            write: function(decorated)
            {
                decorated ? this.el.removeClass('x-no-decoration') : this.el.addClass('x-no-decoration');
                
                for (var dir in this.resizers)
                {
                    if (this.resizable && decorated)
                        this.resizers[dir].show();
                    else
                        this.resizers[dir].hide();
                }
                
                if (this.resizable && decorated)
                    this.resizeGripEl.show();
                else
                    this.resizeGripEl.hide();
                
                this.decorated = decorated;
                
                this.layout();
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the window has a resize grip. A resize grip is a grip at the south-east of the window that allows
         * for easy resizing.
         *
         * @type bool
         */
        'has-resize-grip': {
            write: function(hasResizeGrip)
            {
                hasResizeGrip ? this.resizeGripEl.show() : this.resizeGripEl.hide();
                
                this.hasResizeGrip = hasResizeGrip;
            },
            read: true,
            defaultValue: true
        },
        /**
         * The default width of the window, used when initially showing the window.
         *
         * @type int
         */
        'default-width': {
            write: function(defaultWidth)
            {
                this.defaultSize = {width: defaultWidth, height: this.defaultSize.height};
                
                // TODO: Signal. (1 extra).
            },
            read: function()
            {
                return this.defaultSize.width;
            }
        },
        /**
         * The default height of the window, used when initially showing the window.
         *
         * @type int
         */
        'default-height': {
            write: function(defaultHeight)
            {
                this.defaultSize = {width: this.defaultSize.width, height: defaultHeight};
                
                // TODO: Signal. (1 extra).
            },
            read: function()
            {
                return this.defaultSize.height;
            }
        },
        /**
         * The default size of the window, used when initially showing the window.
         *
         * @type Dimension
         */
        'default-size': {
            write: function(defaultSize) // TODO: Either this or default-width/default-height?
            {
                this.defaultSize = defaultSize;
                
                // TODO: Signal. (2 extra)
                
                // TODO: Use it.
            },
            read: true,
            defaultValue: {width: -1, height: -1}
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        /**
         * Moves the window. Does the same as setting the #position.
         *
         * @param x int The new left position of the window.
         * @param y int The new top position of the window.
         */
        move: function(x, y)
        {
            this.setPosition({x: x, y: y});
        },
        /**
         * Resizes the window. Does the same as setting the #size.
         *
         * @param width  int The new width of the window.
         * @param height int The new height of the window.
         */
        resize: function(width, height)
        {
            this.setSize({width: width, height: height});
        },
        /**
         * Centers the window on the screen.
         */
        center: function()
        {
            this.setPosition(this.getCenterPosition());
        }
    },
    
    /*
     * Statics.
     */
    
    statics: {
        cursorShapeByDir: {
            n:  CursorShape.RESIZE_N,
            ne: CursorShape.RESIZE_NE,
            e:  CursorShape.RESIZE_E,
            se: CursorShape.RESIZE_SE,
            s:  CursorShape.RESIZE_S,
            sw: CursorShape.RESIZE_SW,
            w:  CursorShape.RESIZE_W,
            nw: CursorShape.RESIZE_NW
        }
    }
});
