// Use strict mode if available.
"use strict";

/**
 * A widget with two adjustable panes
 */
Class.define('Paned', {
    extend: 'Container',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Paned.base.initialize.call(this);
        
        // Set members.
        this.resize       = [];
        this.splitterSize = 6;
        
        // Get elements.
        this.splitterEl = this.el.find('.x-splitter');
        
        // Attach event handlers.
        EventManager.registerHandler(this.splitterEl, EventMask.BUTTON_PRESS, this.onSplitterButtonPress, this);
        EventManager.registerHandler(this.splitterEl, EventMask.BUTTON_RELEASE, this.onSplitterButtonRelease, this);
        EventManager.registerHandler(this.splitterEl, EventMask.MOTION, this.onSplitterMotion, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-paned x-body x-orient-horizontal">' +
                '<div class="x-splitter" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        var minSize = {width: 0, height: 0};
        
        var nrVisibleChildren = 0;
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            var child = this.children[i];
            if (child.visible)
            {
                ++nrVisibleChildren;
                
                var size   = child.requestSize();
                var margin = child.margin;
                
                if (this.orientation === Orientation.HORIZONTAL)
                {
                    minSize.width += size.width + margin.left + margin.right;
                    minSize.height = Math.max(minSize.height, size.height + margin.top + margin.bottom);
                }
                else
                {
                    minSize.height += size.height + margin.top + margin.bottom;
                    minSize.width   = Math.max(minSize.width, size.width + margin.left + margin.right);
                }
            }
        }
        
        if (nrVisibleChildren === 2)
        {
            if (this.orientation === Orientation.HORIZONTAL)
                minSize.width += this.splitterSize;
            else
                minSize.height += this.splitterSize;
        }
        
        return minSize;
    },
    
    allocateSize: function(allocation)
    {
        // Store previous size.
        var prevSize = ((this.orientation === Orientation.HORIZONTAL) ? this.allocation.width : this.allocation.height)
                     - this.splitterSize;
        
        // Set our size and position.
        this.el.setSize({width: allocation.width, height: allocation.height});
        this.el.setPosition({x: allocation.x, y: allocation.y});
        
        // Correct and store allocation.
        allocation = this.correctAndStoreAllocation(allocation);
        
        // Determine visible children.
        var nrVisibleChildren = 0;
        var availableSize = 0;
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            var child = this.children[i];
            if (child.visible)
                ++nrVisibleChildren;
        }
        
        // Divide size according to amount of visible childs.
        if (nrVisibleChildren === 0)
        {
            // Hide splitter.
            this.splitterEl.hide();
            
            return;
        }
        else if (nrVisibleChildren === 1)
        {
            // Hide splitter.
            this.splitterEl.hide();
            
            // Determine visible child.
            var child = this.children[0].visible ? this.children[0] : this.children[1];
            
            // Fetch child margins.
            var margin = child.margin;
            
            // Allocate size.
            child.allocateSize({
                x: margin.left,
                y: margin.top,
                width:  allocation.width  - margin.left - margin.right,
                height: allocation.height - margin.top  - margin.bottom
            });
            
            return;
        }
        
        // Show splitter.
        this.splitterEl.show();
        
        // Get child requests.
        var firstRequest = this.children[0].requestSize();
        var firstMargin  = this.children[0].margin;
        
        var secondRequest = this.children[1].requestSize();
        var secondMargin  = this.children[1].margin;
        
        var firstRequestSize, secondRequestSize;
        if (this.orientation === Orientation.HORIZONTAL)
        {
            firstRequestSize  = firstRequest.width  + firstMargin.left  + firstMargin.right;
            secondRequestSize = secondRequest.width + secondMargin.left + secondMargin.right;
        }
        else
        {
            firstRequestSize  = firstRequest.height  + firstMargin.top  + firstMargin.bottom;
            secondRequestSize = secondRequest.height + secondMargin.top + secondMargin.bottom;
        }
        
        // Get available size.
        var availableSize = (this.orientation === Orientation.HORIZONTAL)
                          ? allocation.width
                          : allocation.height;
        
        availableSize -= this.splitterSize;
        
        // Set minimum and maximum position.
        var minPosition = Math.max(this.minSplitterPosition, firstRequestSize);
        var maxPosition = Math.min(this.maxSplitterPosition, availableSize - secondRequestSize);
        
        // Set position.
        if (this.splitterPosition < 0)
        {
            // Position it halfway.
            var position = availableSize * 0.5;
        }
        else
        {
            // Check if both childs resize.
            if (!(this.resize[0] ^ this.resize[1]))
                var position = this.splitterPosition * availableSize / prevSize;
            else if (this.resize[0])
                var position = availableSize - (prevSize - this.splitterPosition);
            else // if (this.resize[1])
                var position = this.splitterPosition;
        }
        
        // Clamp, round and set position.
        position = Math.round(Util.clamp(position, minPosition, maxPosition));
        
        this.setSplitterPosition(position);
        
        // Set children their sizes.
        // Set splitter its position and size.
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.children[0].allocateSize({
                x: firstMargin.left,
                y: firstMargin.top,
                width:  position - firstMargin.left - firstMargin.right,
                height: allocation.height
            });
            
            this.children[1].allocateSize({
                x: secondMargin.left + position + this.splitterSize,
                y: secondMargin.top,
                width:  (availableSize - position) - secondMargin.left - secondMargin.right,
                height: allocation.height
            });
            
            this.splitterEl.setPosition({x: position, y: 0});
            this.splitterEl.setSize({width: this.splitterSize, height: allocation.height});
        }
        else
        {
            this.children[0].allocateSize({
                x: firstMargin.left,
                y: firstMargin.top,
                width: allocation.width,
                height: position - firstMargin.top - firstMargin.bottom,
            });
            
            this.children[1].allocateSize({
                x: secondMargin.left,
                y: secondMargin.top + position + this.splitterSize,
                width:  allocation.width,
                height: (availableSize - position) - secondMargin.top - secondMargin.bottom
            });
            
            this.splitterEl.setPosition({x: 0, y: position});
            this.splitterEl.setSize({width: allocation.width, height: this.splitterSize});
        }
    },
    
    /*
     * Event handlers.
     */
    
    onSplitterButtonPress: function(e)
    {
        this.dragging = true;
        
        // Show cursor.
        if (this.orientation === Orientation.HORIZONTAL)
            Cursor.push(CursorShape.RESIZE_H, Cursor.getContextId('paned-widget'));
        else
            Cursor.push(CursorShape.RESIZE_V, Cursor.getContextId('paned-widget'));
    },
    
    onSplitterMotion: function(e)
    {
        if (!this.dragging)
            return;
        
        var offset = this.el.getOffset();
        if (this.orientation === Orientation.HORIZONTAL)
        {
            this.setSplitterPosition(Math.max(0, e.getX() - offset.x - this.splitterSize * 0.5));
        }
        else
        {
            this.setSplitterPosition(Math.max(0, e.getY() - offset.y - this.splitterSize * 0.5));
        }
    },
    
    onSplitterButtonRelease: function(e)
    {
        this.dragging = false;
        
        // Remove cursor.
        Cursor.pop(Cursor.getContextId('paned-widget'));
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The orientation of the paned.
         *
         * @type Orientation
         */
        orientation: {
            write: function(orientation)
            {
                this.el.replaceClass('x-orient-' + this.orientation, 'x-orient-' + orientation);
                
                this.orientation = orientation;
                
                this.layout();
            },
            read: true,
            defaultValue: Orientation.HORIZONTAL
        },
        'splitter-position': {
            write: function(splitterPosition)
            {
                // Store old value, and set new one.
                var oldSplitterPosition = this.splitterPosition;
                
                // Do not recurse. Layout will set real splitter position.
                if (!this.layouting)
                {
                    this.layouting = true;
                    
                    this.splitterPosition = splitterPosition;
                    
                    if (this.getIsVisible())
                        this.allocateSize(Util.cloneShallow(this.allocation));
                    
                    delete this.layouting;
                    
                    // Has it really changed?
                    return (oldSplitterPosition === this.splitterPosition);
                }
                else
                {
                    return false;
                }
            },
            read: true,
            defaultValue: -1
        },
        'min-splitter-position': {
            write: function(minSplitterPosition)
            {
                this.minSplitterPosition = minSplitterPosition;
                
                this.layout();
            },
            read: true,
            defaultValue: 0
        },
        'max-splitter-position': {
            write: function(maxSplitterPosition)
            {
                this.maxSplitterPosition = maxSplitterPosition;
                
                this.layout();
            },
            read: true,
            defaultValue: Number.MAX_VALUE
        }
        
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        add: function(widget, resize)
        {
            if (this.children.length === 2)
                throw new Error('Paned container can only contain two children.');
            
            Paned.base.add.call(this, widget);
            
            this.resize.push(resize !== false);
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            var index = Paned.base.remove.call(this, widget);
            
            this.resize.splice(index, 1);
            
            this.layout();
        }
    }
});
