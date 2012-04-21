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
        this.focusChild = widget;
        
        if (this.parent)
            this.parent.setFocusChild(widget);
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
    
    getMinimumSize: function()
    {
        throw new Error('Method \'getMinimumSize\' has not been implemented.');
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
         * Child of this container that has focus.
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
        // Overrides 'showAll' action.
        showAll: function()
        {
            for (var i = this.children.length - 1; i >= 0; --i)
                this.children[i].showAll();
            
            this.show();
        },
        // Overrides 'hideAll' action.
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
        
        // TODO: Prepend?, insert?
        
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
        
        // NOTE: An action or a setter/getter?
        getChildren: function()
        {
            return this.children;
        }
    }
});
