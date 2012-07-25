// Use strict mode if available.
"use strict";

/*
 * Element class.
 */

Class.define('Element', {
    extend: 'Object',
    
    /*
     * Public methods.
     */
    
    construct: function(html)
    {
        this.dom   = $(html).first();
        this.el    = this.dom.get(0);
        this.slots = {};
    },
    
    destroy: function()
    {
        this.dom.remove();
    },
    
    invalidate: function()
    {
        delete this.shown;
        delete this.margin;
        delete this.border;
        delete this.padding;
        delete this.size;
        delete this.position;
    },
    
    /*
     * Selectors.
     */
    
    find: function(selector)
    {
        var result = this.dom.find(selector);
        
        return result.length ? new Element(result) : undefined;
    },
    
    /*
     * Styling.
     */
    
    show: function()
    {
        if (this.shown === true)
            return;
        
        this.el.style.display = 'block';
        
        this.invalidate();
        
        this.shown = true;
    },
    
    hide: function()
    {
        if (this.shown === false)
            return;
        
        this.el.style.display = 'none';
        
        this.invalidate();
        
        this.shown = false;
    },
    
    setStyle: function(name, value)
    {
        this.dom.css(name, value);
        
        // NOTE: No invalidation, use the setters for margin/border/padding/size.
    },
    
    getStyle: function(name)
    {
        return this.dom.css(name);
    },
    
    addClass: function(name)
    {
        this.dom.addClass(name);
        
        this.invalidate();
    },
    
    removeClass: function(name)
    {
        this.dom.removeClass(name);
        
        this.invalidate();
    },
    
    hasClass: function(name)
    {
        return this.dom.hasClass(name);
    },
    
    toggleClass: function(name)
    {
        this.dom.toggleClass(name);
        
        this.invalidate();
    },
    
    getClass: function()
    {
        return this.el.className;
    },
    
    replaceClass: function(oldName, newName)
    {
        this.removeClass(oldName);
        this.addClass(newName);
        
        this.invalidate();
    },
    
    setOpacity: function(opacity)
    {
        this.el.style.filter  = 'alpha(opacity=' + (opacity * 100) + ')';
        this.el.style.opacity = opacity;
    },
    
    getOffset: function()
    {
        var offset = this.dom.offset();
        
        return {x: offset.left, y: offset.top};
    },
    
    setPosition: function(position)
    {
        this.el.style.left = position.x + 'px';
        this.el.style.top  = position.y + 'px';
        
        this.position = position;
    },
    
    getPosition: function()
    {
        if (!this.position)
        {
            this.position = {
                x: parseFloat(this.el.style.left),
                y: parseFloat(this.el.style.top)
            };
        }
        
        return this.position;
    },
    
    setLeft: function(left)
    {
        this.el.style.left = left + 'px';
        
        if (this.position)
            this.position.x = left;
    },
    
    setTop: function(top)
    {
        this.el.style.top = top + 'px';
        
        if (this.position)
            this.position.y = top;
    },
    
    getLeft: function()
    {
        if (this.position)
            return this.position.x;
        
        return parseFloat(this.el.style.left);
    },
    
    getTop: function()
    {
        if (this.position)
            return this.position.y;
        
        return parseFloat(this.el.style.top);
    },
    
    setSize: function(size)
    {
        var frameInfo = this.getFrame();
        
        this.el.style.width  = Math.max(0, size.width  - frameInfo.left - frameInfo.right)  + 'px';
        this.el.style.height = Math.max(0, size.height - frameInfo.top  - frameInfo.bottom) + 'px';
        
        this.size = size;
    },
    
    getSize: function()
    {
        if (!this.size)
        {
            var frameInfo = this.getFrame();
            
            this.size = {
                width:  this.dom.width()  + frameInfo.left + frameInfo.right,
                height: this.dom.height() + frameInfo.top  + frameInfo.bottom
            };
        }
        
        return this.size;
    },
    
    getWidth: function()
    {
        if (this.size)
            return this.size.width;
        
        var frameInfo = this.getFrame();
        
        return this.dom.width() + frameInfo.left + frameInfo.right;
    },
    
    setWidth: function(width)
    {
        var frameInfo = this.getFrame();
        
        this.el.style.width = Math.max(0, width - frameInfo.left - frameInfo.right) + 'px';
        
        if (this.size)
            this.size.width = width;
    },
    
    getHeight: function()
    {
        if (this.size)
            return this.size.height;
        
        var frameInfo = this.getFrame();
        
        return this.dom.height() + frameInfo.top  + frameInfo.bottom;
    },
    
    setHeight: function(height)
    {
        var frameInfo = this.getFrame();
        
        this.el.style.height = Math.max(0, height - frameInfo.top - frameInfo.bottom) + 'px';
        
        if (this.size)
            this.size.height = height;
    },
    
    setInnerSize: function(size)
    {
        this.el.style.width  = Math.max(0, size.width)  + 'px';
        this.el.style.height = Math.max(0, size.height) + 'px';
    },
    
    getInnerSize: function()
    {
        return {width: this.dom.width(), height: this.dom.height()};
    },
    
    getInnerWidth: function()
    {
        return this.dom.width();
    },
    
    setInnerWidth: function(width)
    {
        this.el.style.width = Math.max(0, width) + 'px';
    },
    
    getInnerHeight: function()
    {
        return this.dom.height();
    },
    
    setInnerHeight: function(height)
    {
        this.el.style.height = Math.max(0, height) + 'px';
    },
    
    getPadding: function()
    {
        if (!this.padding)
            this.padding = this.dom.padding();
        
        return this.padding;
    },
    
    setPadding: function(padding)
    {
        this.dom.padding(padding);
        
        this.padding = padding;
        
        delete this.size;
    },
    
    getBorder: function()
    {
        if (!this.border)
            this.border = this.dom.border();
        
        return this.border;
    },
    
    getMargin: function()
    {
        if (!this.margin)
            this.margin = this.dom.margin();
        
        return this.margin;
    },
    
    setMargin: function(margin)
    {
        this.dom.margin(margin);
        
        delete this.size;
        
        this.margin = margin;
    },
    
    getFrame: function(padding, border, margin)
    {
        var info = {top: 0, right: 0, bottom: 0, left: 0};
        
        if (padding !== false)
        {
            padding = this.getPadding();
            
            info.top    += padding.top;    info.right += padding.right;
            info.bottom += padding.bottom; info.left  += padding.left;
        }
        
        if (border !== false)
        {
            border = this.getBorder();
            
            info.top    += border.top;    info.right += border.right;
            info.bottom += border.bottom; info.left  += border.left;
        }
        
        if (margin !== false)
        {
            margin = this.getMargin();
            
            info.top    += margin.top;    info.right += margin.right;
            info.bottom += margin.bottom; info.left  += margin.left;
        }
        
        return info;
    },
    
    /*
     * Attributes and properties.
     */
    
    setDefaultAttributes: function()
    {
        var setAttributes = function(node)
        {
            //if (node.getAttribute('tabIndex'))
            //{
                node.hideFocus = true;
                //node.setAttribute('hidefocus', 'true'); // TODO: Only IE 7.
            //}
            
            //if (!node.hasAttribute('unselectable'))
            //{
            if (node.nodeName.toLowerCase() !== 'input')
            {
                node.setAttribute('unselectable', 'on'); // TODO: Only IE 7/8.
            }
            //};
            
            node.onselectstart = function() { return false; }; // TODO: Leaking.
            
            for (node = node.firstChild; node; node = node.nextSibling)
            {
                if (node.nodeType === 1) // Node.ELEMENT_NODE = 1
                    setAttributes(node);
            }
        };
        
        setAttributes(this.el);
    },
    
    setAttribute: function(name, value)
    {
        this.dom.attr(name, value);
    },
    
    getAttribute: function(name)
    {
        return this.dom.attr(name);
    },
    
    hasAttribute: function(name)
    {
        return (this.dom.attr(name) !== undefined);
    },
    
    setProperty: function(name, value)
    {
        this.dom.prop(name, value);
    },
    
    getProperty: function(name)
    {
        return this.dom.prop(name);
    },
    
    /*
     * Content methods.
     */
    
    setText: function(text)
    {
        // Set HTML.
        this.el.innerHTML = Util.escapeText(text);
        
        // Invalidate size.
        delete this.size;
    },
    
    prepend: function(element)
    {
        this.el.insertBefore(element.el, this.el.firstChild);
    },
    
    append: function(element)
    {
        this.el.appendChild(element.el);
    },
    
    insert: function(element, index)
    {
        this.el.insertBefore(element.el, this.el.childNodes[index]);
    },
    
    remove: function(element)
    {
        if (element !== undefined)
            element.remove();
        else
            this.dom.detach();
    },
    
    getNode: function()
    {
        return this.el;
    },
    
    /*
     * Event handling.
     */
    
    connect: function(name, method, scope)
    {
        if ((name === 'mousewheel') && $.browser.mozilla)
            name = 'DOMMouseScroll';
        
        var newMethod = function(e) { return method.call(scope, e || event); };
        
        var listeners = this.slots[name];
        if (listeners === undefined)
            listeners = this.slots[name] = [];
        
        listeners.push({name: name, method: method, newMethod: newMethod, scope: scope});
        
        if (this.el.addEventListener)
            this.el.addEventListener(name, newMethod, false);
        else
            this.el.attachEvent('on' + name, newMethod);
    },
    
    disconnect: function(name, method, scope)
    {
        if ((name === 'mousewheel') && $.browser.mozilla)
            name = 'DOMMouseScroll';
        
        var listeners = this.slots[name];
        if (listeners === undefined)
            return;
        
        for (var i = listeners.length - 1; i >= 0; --i)
        {
            if ((listeners[i].method === method) && (listeners[i].scope === scope))
            {
                if (this.el.removeEventListener)
                    this.el.removeEventListener(name, listeners[i].newMethod, false);
                else
                    this.el.detachEvent('on' + name, listeners[i].newMethod);
                
                listeners.splice(i, 1);
                
                return;
            }
        }
    },
    
    emit: function(name)
    {
        this.dom.triggerHandler(name);
    },
    
    /*
     * Actions.
     */
    
    focus: function()
    {
        this.el.focus();
    },
    
    blur: function()
    {
        this.el.blur();
    },
    
    /*
     * Statics.
     */
    
    statics: {
        maxZIndex: 10,
        
        getMaxZIndex: function()
        {
            ++Element.maxZIndex;
            
            return Element.maxZIndex;
        },
        
        getBody: function()
        {
            if (!Element.body)
            {
                if (!document.body) debugger;
                
                Element.body = new Element(document.body);
                
                Element.body.setDefaultAttributes();
            }
            
            return Element.body;
        },
        
        getDocument: function()
        {
            if (!Element.document)
            {
                Element.document = new Element(document);
            }
            
            return Element.document;
        }
    }
});
