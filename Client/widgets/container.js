// Use strict mode if available.
"use strict";

/**
 * Base class for widgets which contain other widgets.
 */
Class.define('Container', {
    extend: 'Widget',
    
    /*
     * Private methods.
     */
    
    initialize: function()
    {
        Container.base.initialize.call(this);
        
        // Set members.
        this.children = [];
        
        // Get body element.
        if (this.el.hasClass('x-body'))
            this.bodyEl = this.el;
        else
            this.bodyEl = this.el.find('.x-body');
    },
    
    getBody: function()
    {
        return this.bodyEl;
    },
    
    setFocusChild: function(widget)
    {
        if (this.focusChild !== widget)
        {
            // Reset old focus chain.
            if (this.focusChild && (this.focusChild instanceof Container))
            {
                // Make sure that we do not go all the way to the top.
                var oldFocusChild = this.focusChild;
                this.focusChild = null;
                oldFocusChild.setFocusChild(null);
            }
            
            this.focusChild = widget;
            
            this.emitPropertyChangeSignals('focus-child');
            
            if (this.parent)
                this.parent.setFocusChild(widget ? this : null);
        }
    },
    
    /*
     * Focus and blur management.
     */
    
    moveFocus: function(dir, trap)
    {
        if (this.visible && this.sensitive)
        {
            // Determine whether to search fowards or backwards.
            var backward = ((dir === FocusDirection.BACKWARD) || (dir === FocusDirection.END));
            
            // Find the index of the first child to try.
            if (dir === FocusDirection.START)
            {
                var index = 0;
            }
            else if (dir === FocusDirection.END)
            {
                var index = this.children.length - 1;
            }
            else if (this.focusChild)
            {
                if (this.focusChild.moveFocus(dir))
                    return true;
                
                for (var i = this.children.length - 1; i >= 0; --i)
                {
                    if (this.children[i] === this.focusChild)
                        break;
                }
                
                var index = i;
            }
            else
            {
                var index = backward ? (this.children.length - 1) : 0;
            }
            
            if (backward)
            {
                // Walk backwards.
                for (var i = index; i >= 0; --i)
                {
                    if (this.children[i].moveFocus(dir))
                        return true;
                }
                
                // Try to focus us. Do not try twice.
                if (this.focus())
                    return true;
                
                // If focus is trapped, rotate.
                if (trap)
                {
                    for (var i = this.children.length - 1; i >= 0; --i)
                    {
                        if (this.children[i].moveFocus(FocusDirection.END))
                            return true;
                    }
                }
            }
            else
            {
                // Try to focus us first.
                if (!this.focusChild && this.focus())
                    return true;
                
                // Walk forwards.
                for (var i = index; i < this.children.length; ++i)
                {
                    if (this.children[i].moveFocus(dir))
                        return true;
                }
                
                // If focus is trapped, rotate.
                if (trap)
                {
                    // Try to focus us. Do not try twice.
                    if (this.focusChild && this.focus())
                        return true;
                    
                    for (var i = 0; i < this.children.length; ++i)
                    {
                        if (this.children[i].moveFocus(FocusDirection.START))
                            return true;
                    }
                }
            }
        }
        
        // Stop looking we want to trap focus, and we already had focus.
        return trap && this.focusChild;
    },
    
    /*
     * Layouting.
     */
    
    getFrameSize: function()
    {
        // Get view frame size.
        var frameSize = Container.base.getFrameSize.call(this);
        
        // Check for case where body is the main element.
        if (this.bodyEl === this.el)
            return frameSize;
        
        // Get frame size of the body element.
        var bodyFrameSize = this.bodyEl.getFrame();
        
        return {
            width:  frameSize.width  + bodyFrameSize.left + bodyFrameSize.right,
            height: frameSize.height + bodyFrameSize.top  + bodyFrameSize.bottom
        };
    },
    
    getPreferredSize: function()
    {
        throw new Error('Method \'getPreferredSize\' has not been implemented.');
    },
    
    allocateSize: function(allocation)
    {
        throw new Error('Method \'allocateSize\' has not been implemented.');
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * Child of this container that contains or is the focus widget.
         *
         * @type Widget
         */
        'focus-child': {
            read: true,
            defaultValue: null
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'showAll()' action.
        showAll: function()
        {
            for (var i = this.children.length - 1; i >= 0; --i)
                this.children[i].showAll();
            
            this.show();
        },
        // Overrides 'hideAll()' action.
        hideAll: function()
        {
            for (var i = this.children.length - 1; i >= 0; --i)
                this.children[i].hideAll();
            
            this.hide();
        },
        /**
         * Adds widget to container. A widget may be added to only one container at a time; you can't place the same
         * widget inside two different containers.
         *
         * @param widget Widget A widget to be placed inside the container.
         */
        add: function(widget)
        {
            if (widget.getParent() !== null)
                throw new Error('Widget has already been added to a container.');
            
            widget.setParent(this);
            
            this.bodyEl.append(widget.el);
            
            this.children.push(widget);
        },
        /**
         * Removes widget from container. widget must be inside container. If you also want to destroy the widget, you
         * should call #Widget.destroy(), which will automatically remove the widget from its parent container.
         *
         * @param widget Widget A current child of the container.
         *
         * @throws An #Error if widget was not found in this container.
         * @see #Widget.destroy()
         */
        remove: function(widget)
        {
            for (var i = this.children.length - 1; i >= 0; --i)
            {
                if (this.children[i] === widget)
                {
                    widget.setParent(null);
                    this.bodyEl.remove(widget.el);
                    this.children.splice(i, 1);
                    
                    return i;
                }
            }
            
            throw new Error('Could not find child.');
        },
        
        // TODO: insert, indexOf.
        
        removeAll: function()
        {
            for (var i = this.children.length - 1; i >= 0; --i)
            {
                var widget = this.children[i];
                
                widget.setParent(null);
                this.bodyEl.remove(widget.el);
            }
            
            this.children = [];
            
            this.layout();
        },
        
        removeAt: function(index)
        {
            var widget = this.children[index];
            if (widget !== undefined)
                this.remove(widget);
            
            throw new Error('Could not find child at index ' + index + '.');
        },
        
        getAt: function(index)
        {
            var widget = this.children[index];
            if (widget === undefined)
                throw new Error('Could not find child at index ' + index + '.');
            
            return widget;
        },
        
        count: function()
        {
            return this.children.length;
        },
        
        // NOTE: An action or a setter/getter? Remove?
        getChildren: function()
        {
            return this.children;
        }
    }
});
