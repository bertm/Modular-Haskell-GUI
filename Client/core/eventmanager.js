// Use strict mode if available.
"use strict";

/*
 * Event manager class.
 */

Singleton.define('EventManager', {
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        // Connect to events on document or body.
        var body = Element.getBody();
        var doc  = Element.getDocument();
        for (var type in this.eventNameByType)
        {
            var name = this.eventNameByType[type]
            if ((type === EventType.MOTION) || (type === EventType.BUTTON_RELEASE))
                doc.connect(name, this.onEvent, this);
            else
                body.connect(name, this.onEvent, this);
        }
        
        body.connect('mousedown', function(e)
        {
            // TODO: What about a, elements with tabindex?
            // TODO: Make this work in IE 7/8.
            // TODO: Only for selections within input/selectable labels, but not for focus?

            
            // Prevent default focus on mousedown.
            //var target = e.target ? e.target : e.srcElement;
            //if (target.nodeName !== 'INPUT')
            //{
            //    e.preventDefault ? e.preventDefault() : (e.returnValue = false);
            //}
        }, this);
        
        // Listen for focus events on any node.
        var self = this;
        var onFocus = function(e) { self.onFocus(e || event); };
        
        if (window.addEventListener)
            document.body.addEventListener('focus', onFocus, true);
        else
            window.attachEvent('onfocusin', onFocus);
        
        // TODO: Blur?
        
        /*
        
        On focus event (focusin on IE, focus capture in other browsers):
            - Check if it's our focus element.
            - If not, focus our focus element.
        
        On window focus:
            - Focus our focus element.
        
        
        */
        
        // Set modifiers.
        this.modifiers = EventModifierMask.NONE;
        
        // Set grabbers.
        this.mouseGrabNode    = null;
        this.keyboardGrabNode = null;
    },
    
    /*
     * Public methods.
     */
    
    registerHandler: function(element, mask, method, scope, data)
    {
        // Fetch node, and create handlers array.
        var node = element.getNode();
        
        if (!node._handlers)
            node._handlers = [];
        
        // Check if handler already exists.
        var cumulativeMask = mask;
        for (var i = node._handlers.length - 1; i >= 0; --i)
        {
            var handler = node._handlers[i];
            if ((handler.method === method) && (handler.scope === scope))
            {
                // Remove handler, or change mask and data.
                if (!mask)
                {
                    node.handlers.splice(i, 1);
                    if (!node.handlers.length)
                        delete node.handlers;
                    
                    continue;
                }
                else
                {
                    handler.mask = mask;
                    handler.data = data;
                }
            }
            
            // Add mask.
            cumulativeMask |= handler.mask;
        }
        
        // Add handler, and set new mask.
        if (mask)
            node._handlers.push({mask: mask, method: method, scope: scope, data: data});
        
        node._mask = cumulativeMask;
    },
    
    registerSource: function(element, source)
    {
        // Set source of the DOM element.
        element.getNode()._source = source;
    },
    
    triggerEvent: function(element, event)
    {
        // Get source.
        var node   = element.getNode();
        var source = this.getSource(node);
        
        // Handle event chain.
        return this.handleEventChain(event, node);
    },
    
    setFocus: function(element)
    {
        if (element)
        {
            // Focus element, we want keyboard input.
            document.body.focus();
            
            element.focus();
            
            // Set keyboard grab.
            this.keyboardGrabNode = element.getNode();
            
            // TODO: Set element?
            // TODO: Remove focus/blur from element.
            
            // 
            
            // Needed?
            
            /*
            if (!this.triggerEvent(element, new FocusChangeEvent(foo, this.modifiers, true)))
            {
                this.focusElement = element;
                
                // foo.el.connect('blur', this.onFooBlur, this);
                
                return true;
            }
            */
            
            return true;
        }
        else if (this.keyboardGrabNode) // TODO: Set element?
        {
            // Blur element.
            this.keyboardGrabNode.blur();
            
            // Unset keyboard grab.
            this.keyboardGrabNode = null;
            
            // TODO: Set element?
            
            /*
            if (!this.triggerEvent(this.focusElement, new FocusChangeEvent(foo, this.modifiers, false)))
            {
                // this.focusElement.disconnect('blur', this.onFooBlur, this);
                
                this.focusElement = null;
                
                return true;
            }
            */
            
            return true;
        }
        else
        {
            return true;
        }
        
        return false;
    },
    
    /*
     * Private methods.
     */
    
    // Called when an element in the body gets focus.
    onFocus: function(e)
    {
        var target = e.target ? e.target : e.srcElement;
        
        if (this.keyboardGrabNode)
        {
            if (target === this.keyboardGrabNode)
                return;
            
            // Focus the keyboard grab node instead.
            this.keyboardGrabNode.focus();
        }
        else
        {
            // Blur target: no element has the keyboard grab.
            target.blur();
        }
    },
    
    /*
    // TODO: ..
    onFooBlur: function()
    {
        // TODO: Blur new element, as it does not deserve focus.
        // TODO: We should do this at focus.
        
        Application.setActiveWindow(null);
    },
    */
    
    getSource: function(node)
    {
        // Walk node tree, looking for a source.
        while (node && !node._source)
        {
            node = node.parentNode;
        }
        
        return node ? node._source : null;
    },
    
    /*
     * Generic event handler.
     */
    
    onEvent: function(e)
    {
        // Get type.
        var type = this.eventTypeByName[e.type];
        
        // Fetch event node.
        var node = e.target ? e.target : e.srcElement;
        switch (type)
        {
            case EventType.MOTION:
            case EventType.BUTTON_PRESS:
            case EventType.BUTTON_RELEASE:
            case EventType.SCROLL:
                node = this.pointerGrabNode || node;
                break;
            
            case EventType.KEY_PRESS:
            case EventType.KEY_RELEASE:
                node = this.keyboardGrabNode;
                break;
        }
        
        // Get source.
        var source = this.getSource(node);
        
        // Create event.
        var event = this.eventCreators[type].call(this, e, source, node);
        if (!event)
            return;
        
        // Handle event chain.
        var handled = this.handleEventChain(event, node);
        
        // Prevent default action if event has been handled.
        if (handled)
            e.preventDefault ? e.preventDefault() : (e.returnValue = false);
    },
    
    /*
     * Specific event handlers and creators.
     */
    
    handleButtonEvent: function(e, press, source, node)
    {
        var button = e.which;
        
        // Determine mouse position.
        var x = e.pageX || e.clientX;
        var y = e.pageY || e.clientY;
        
        // TODO: IE6/7/8 cursor 2px bug.
        
        // Determine modifiers.
        if (!button && (e.button !== undefined))
            button = ((e.button & 1) ? 1 : ((e.button & 2) ? 3 : ((e.button & 4) ? 2 : 0)));
    
        if (button)
            press ? (this.modifiers |= 1 << button) : (this.modifiers &= ~(1 << button));
        
        // Grab mouse to send all mouse events to node.
        var buttons = this.modifiers & EventModifierMask.BUTTONS;
        if (press && buttons)
            this.pointerGrabNode = node;
        
        // Release grab if no buttons are pressed.
        if (!buttons)
            this.pointerGrabNode = null;
        
        // Create event.
        return new ButtonEvent(source, this.modifiers, x, y, press, button);
    },
    
    handleKeyEvent: function(e, press)
    {
        // TODO: Do this for every event, except move: handleModifiers(..).
        // TODO: Trigger own event if alt key is suddenly on.
        
        // Fetch key code.
        var key = e.keyCode;
        
        // Handle special keys.
        if (key === 16) // Shift.
            press ? (this.modifiers |= EventModifierMask.SHIFT) : (this.modifiers &= ~EventModifierMask.SHIFT);
        else if (key === 17) // Control.
            press ? (this.modifiers |= EventModifierMask.CONTROL) : (this.modifiers &= ~EventModifierMask.CONTROL);
        else if (key === 18) // Alt.
            press ? (this.modifiers |= EventModifierMask.ALT) : (this.modifiers &= ~EventModifierMask.ALT);
        else if ((key === 91) || (key === 92)) // Super.
            press ? (this.modifiers |= EventModifierMask.SUPER) : (this.modifiers &= ~EventModifierMask.SUPER);
        
        if (e.shiftKey !== undefined)
            e.shiftKey ? (this.modifiers |= EventModifierMask.SHIFT) : (this.modifiers &= ~EventModifierMask.SHIFT);
        
        if (e.ctrlKey !== undefined)
            e.ctrlKey ? (this.modifiers |= EventModifierMask.CONTROL) : (this.modifiers &= ~EventModifierMask.CONTROL);
            
        if (e.altKey !== undefined)
            e.altKey ? (this.modifiers |= EventModifierMask.ALT) : (this.modifiers &= ~EventModifierMask.ALT);
        
        return key;
    },
    
    handleCrossingEvent: function(e, enter, source)
    {
        return null; // TODO: We do not have a source.
        
        // Fetch target.
        var target = element.getNode(); //e.target ? e.target : e.srcElement;
        //if (target !== source.getNode()) // TODO: Check if not a link or something. (Safari bug, see Quirksmode)
        //    return false;
        
        // Get target that mouse went to.
        var relatedTarget = e.relatedTarget ? e.relatedTarget : (enter ? e.fromElement : e.toElement);
        while (relatedTarget && (relatedTarget !== target) && (relatedTarget.nodeName !== 'BODY'))
            relatedTarget = relatedTarget.parentNode;
        
        // Check if mouse did not leave us.
        if (relatedTarget === target)
            return false;
        
        // Create new element from related target.
        return new Element(relatedTarget);
    },
    
    eventCreators: (function()
    {
        var creators = {};
        
        creators[EventType.MOTION] = function(e, source)
        {
            // Determine target.
            //source  = this.mouseGrabWidget  || source;
            //element = this.mouseGrabElement || element;
            
            // Determine mouse position.
            var x = e.pageX || e.clientX;
            var y = e.pageY || e.clientY;
            
            // Return motion event.
            return new MotionEvent(source, this.modifiers, x, y);
        };
        
        creators[EventType.SCROLL] = function(e, source)
        {
            // Determine target.
            //source  = this.mouseGrabWidget  || source;
            //element = this.mouseGrabElement || element;
            
            // Determine mouse position.
            var x = e.pageX || e.clientX;
            var y = e.pageY || e.clientY;
            
            // Determine amount of scrolling.
            //var delta = Math.round(e.wheelDelta ? e.wheelDelta / 120 : (e.detail || 0) / -3);
            var delta = Math.round(1.33333333 * (e.detail ? -e.detail : (e.wheelDelta * 0.025)));
            
            return new ScrollEvent(source, this.modifiers, x, y, delta);
        };
        
        creators[EventType.ENTER] = function(e, source)
        {
            // Determine mouse position.
            var x = e.pageX || e.clientX;
            var y = e.pageY || e.clientY;
            
            var related = this.handleCrossingEvent(e, true, source);
            if (related === false)
                return;
            
            return new CrossingEvent(source, this.modifiers, x, y, true, related);
        };
        
        creators[EventType.LEAVE] = function(e, source)
        {
            // Determine mouse position.
            var x = e.pageX || e.clientX;
            var y = e.pageY || e.clientY;
            
            var related = this.handleCrossingEvent(e, false, source);
            if (related === false)
                return;
            
            return new CrossingEvent(source, this.modifiers, x, y, false, related);
        };
        
        creators[EventType.BUTTON_PRESS] = function(e, source, node)
        {
            return this.handleButtonEvent(e, true, source, node);
        };
        
        creators[EventType.BUTTON_RELEASE] = function(e, source, node)
        {
            return this.handleButtonEvent(e, false, source, node);
        };
        
        creators[EventType.KEY_PRESS] = function(e, source)
        {
            // Determine origin.
            //source = this.focusWidget;
            
            // TODO: Keyboard grab.
            
            var key = this.handleKeyEvent(e, true);
            
            return new KeyEvent(source, this.modifiers, true, key);
        };
        
        creators[EventType.KEY_RELEASE] = function(e, source)
        {
            // Determine origin.
            //source = this.focusWidget;
            
            // TODO: Keyboard grab.
            
            var key = this.handleKeyEvent(e, false);
            
            return new KeyEvent(source, this.modifiers, false, key);
        };
        
        return creators;
    })(),
    
    handleEventChain: function(event, node)
    {
        // Determine event masks.
        var bubbleMask     = this.eventMaskByType[event.type];
        var captureMask    = bubbleMask << 1;
        var cumulativeMask = bubbleMask | captureMask;
        
        // Determine chain of events.
        var captureChain = [];
        var bubbleChain  = [];
        
        while (node)
        {
            if (node._mask & cumulativeMask)
            {
                for (var i = node._handlers.length - 1; i >= 0; --i)
                {
                    var handler = node._handlers[i];
                    
                    if (handler.mask & captureMask)
                        captureChain.push(handler);
                    
                    if (handler.mask & bubbleMask)
                        bubbleChain.push(handler);
                }
            }
            
            node = node.parentNode;
        }
        
        // Capture phase.
        for (var i = captureChain.length - 1; i >= 0; --i)
        {
            var handler = captureChain[i];
            if (handler.method.call(handler.scope, event, handler.data, true))
                return true;
        }
        
        // Bubble phase.
        var length = this.eventBubbles[event.type] ? bubbleChain.length : (bubbleChain.length ? 1 : 0);
        for (var i = 0; i < length; ++i)
        {
            var handler = bubbleChain[i];
            if (handler.method.call(handler.scope, event, handler.data, false))
                return true;
        }
        
        return false;
    },
    
    /*
     * Private members.
     */
    
    // Browser event names by their type.
    eventNameByType: (function()
        {
            var names = {};
            
            names[EventType.MOTION] = 'mousemove';
            names[EventType.SCROLL] = 'mousewheel';
            
            names[EventType.ENTER] = 'mouseover';
            names[EventType.LEAVE] = 'mouseout';
            
            names[EventType.KEY_PRESS]   = 'keydown';
            names[EventType.KEY_RELEASE] = 'keyup';
            
            names[EventType.BUTTON_PRESS]   = 'mousedown';
            names[EventType.BUTTON_RELEASE] = 'mouseup';
            
            return names;
        })(),
    
    // Event type by their browser name.
    eventTypeByName: (function()
        {
            var types = {};
            
            types['mousemove']  = EventType.MOTION;
            types['mousewheel'] = types['DOMMouseScroll'] = EventType.SCROLL;
            
            types['mouseover'] = EventType.ENTER;
            types['mouseout']  = EventType.LEAVE;
            
            types['keydown'] = EventType.KEY_PRESS;
            types['keyup']   = EventType.KEY_RELEASE;
            
            types['mousedown'] = EventType.BUTTON_PRESS;
            types['mouseup']   = EventType.BUTTON_RELEASE;
            
            return types;
        })(),
    
    // Event mask by their type.
    eventMaskByType: (function()
        {
            var masks = {};
            
            masks[EventType.MOTION] = EventMask.MOTION;
            masks[EventType.SCROLL] = EventMask.SCROLL;
            
            masks[EventType.ENTER] = EventMask.ENTER;
            masks[EventType.LEAVE] = EventMask.LEAVE;
            
            masks[EventType.KEY_PRESS]   = EventMask.KEY_PRESS;
            masks[EventType.KEY_RELEASE] = EventMask.KEY_RELEASE;
            
            masks[EventType.BUTTON_PRESS]   = EventMask.BUTTON_PRESS;
            masks[EventType.BUTTON_RELEASE] = EventMask.BUTTON_RELEASE;
            
            masks[EventType.FOCUS] = EventMask.FOCUS;
            masks[EventType.BLUR]  = EventMask.BLUR;
            
            return masks;
        })(),
    
    // Whether an event bubbles by type.
    eventBubbles: (function()
        {
            var bubbles = {};
            
            // Enter, leave, focus and blur events do not bubble.
            bubbles[EventType.ENTER] = bubbles[EventType.LEAVE] =
            bubbles[EventType.FOCUS] = bubbles[EventType.BLUR] = false;
            
            // The rest of them do.
            bubbles[EventType.MOTION] = bubbles[EventType.SCROLL] =
            bubbles[EventType.KEY_PRESS] = bubbles[EventType.KEY_RELEASE] =
            bubbles[EventType.BUTTON_PRESS] = bubbles[EventType.BUTTON_RELEASE] = true;
            
            return bubbles;
        })()
});
